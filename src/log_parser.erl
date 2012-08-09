-module(log_parser).
-behaviour(gen_server).

-export([
        start_link/0,
        parse/1
]).

-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
]).


-include("logging.hrl").


-record(state, {
          server,
          connection,
          db,
          collection,
          file_for_rejected_data
}).



%% ===================================================================
%% APIs
%% ===================================================================
start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


parse(LogFileName) ->
   gen_server:cast(?MODULE, {parse, LogFileName}).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([]) ->
   {ok, MongoServer} = application:get_env(parse_mongo_logs, mongo_server),
   {ok, MongoDb} = application:get_env(parse_mongo_logs, mongodb_dbname),
   {ok, MongoCollection} = application:get_env(parse_mongo_logs, collection),
   {ok, FileForRejectedData} = application:get_env(parse_mongo_logs, file_for_rejected_data),

   case mongo:connect(MongoServer) of
      {ok, Connection} ->
      {ok, #state{server = MongoServer, connection = Connection, db = MongoDb,
           collection = MongoCollection, file_for_rejected_data = FileForRejectedData}};
      {error, Reason} ->
         ?log_error("Error Occured. Reason:~p. Connection settings: ~p~n", [Reason, MongoServer])
   end.


handle_cast({parse, LogFileName}, State) ->
   ?log_info("parsing started", []),

   case file:open(LogFileName, [read]) of
      {ok, Device} ->
         init_storage_file(State#state.file_for_rejected_data),
         parse_logfile_line_by_line(Device, "", State),
         ?log_info("parsing ended", []),
         file:close(Device),

         {noreply, State};
      {error, Reason} ->
         ?log_error("Can't access file: ~p~n", [LogFileName]),
         {stop, Reason, State}
   end;

handle_cast(Request, State) ->
   {stop, {bad_arg, Request}, State}.
  

handle_info(Info, State) ->
   {stop, {bad_arg, Info}, State}.


handle_call(_Request, _From, State) ->
   {noreply, State}.


terminate(_Reason, _State) ->
   ok.


code_change(_OldSvn, State, _Extra) ->
   {ok, State}.



%% ===================================================================
%% Internal
%% ===================================================================

%
% extracting messages from log file
%
parse_logfile_line_by_line(Device, Accum, State) ->
   case io:get_line(Device, "") of
      eof ->
         ok;
      Line ->
         {Term, UnhandledData} = try_to_get_term(Accum ++ Line, string:chr(Line, $.)),
         reinsert_message(State, Term),
         parse_logfile_line_by_line(Device, UnhandledData, State)
   end.


%
% trying to get Term from raw string
%
try_to_get_term(Text, 0) -> {undefined, Text};

try_to_get_term(Text, _Position) ->
   RefinedString = remove_spaces_and_brakes(Text),
   Position = string:chr(RefinedString, $.),

   UnhandledData = string:substr(RefinedString, Position + 1, length(RefinedString) - Position),

   StringToParse = string:substr(RefinedString, 1, Position),
   {ok, Tokens, _EndLine} = erl_scan:string(StringToParse),
   {ok, Terms} = erl_parse:parse_term(Tokens),
   {Binary, _Trash} = Terms,

   {binary_to_term(Binary), UnhandledData}.


%
% saving data into file.
% when "" passed file should be cleansed
%
init_storage_file(FileName) ->
   file:write_file(FileName, "", []).

save_into_file(FileName, Data) ->
   file:write_file(FileName, io_lib:fwrite("~p.\n", [Data]), [append]).


%
% Replacing spaces and new lines with ""
%
remove_spaces_and_brakes(Text) ->
   lists:filter(fun(C) -> not lists:member(C, "$ $\n") end, Text).


%
% re-insert data in MongoDB or in file (depending it was fired or not)
%
reinsert_message(_State, undefined) -> ok;

reinsert_message(_State = #state{connection = Connection, db = Db, collection = Collection},
                _Data = {task, _RecipCnt, Msisdns, TranSid, _Tag, _Error}) ->
   {ok, ok} = mongo:do(safe, master, Connection, Db, fun() ->
      lists:foreach(fun(Msisdn) ->
         ok = mongo:repsert(Collection, {'_id', Msisdn}, { '$push', { v, TranSid } })
      end, Msisdns)
   end);

reinsert_message(_State = #state{file_for_rejected_data = FileForRejectedData}, Data = {_Message, _UnpackedMessage}) ->
   save_into_file(FileForRejectedData, Data).