-module(log_parser).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([parse/1]).


-record(state, 
          {server,
           connection, 
           db, 
           collection, 
           file_for_rejected_data, 
           unfired_message_counts, 
           fired_message_counts}).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%
% init
%
init([]) ->
	{ok, MongoServer} = application:get_env(parse_mongo_logs, mongo_server),
	{ok, MongoDb} = application:get_env(parse_mongo_logs, mongodb_dbname),
	{ok, MongoCollection} = application:get_env(parse_mongo_logs, collection),
	{ok, FileForRejectedData} = application:get_env(parse_mongo_logs, file_for_rejected_data),

	{ok, Connection} = mongo:connect(MongoServer),

	{ok, #state{server = MongoServer, connection = Connection, db = MongoDb, 
					collection = MongoCollection, file_for_rejected_data = FileForRejectedData, fired_message_counts = 0, unfired_message_counts = 0}}.


%
% handling "parse" message.
%
handle_cast({parse, LogFileName}, State) ->
  log_message("parsing started"),

  case file:open(LogFileName, [read]) of
    {ok, Device} ->
      save_into_file(State#state.file_for_rejected_data, ""),
      parse_logfile(Device, "", 0, State),
      log_message("parsing ended"),
      file:close(Device),

      {noreply, State};
    {error, Reason} -> 
      {stop, Reason, State}
  end;

handle_cast(Request, State) ->
	{stop, {bad_arg, Request}, State}.
	
handle_info(Info, State) ->
	{stop, {bad_arg, Info}, State}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

terminate(_Reason, _State = #state{server = _Server, connection = Connection}) ->
  mongo:disconnect(Connection),
	ok.

code_change(_OldSvn, State, _Extra) ->
	{ok, State}.


%
% sending message
%
parse(LogFileName) ->
  gen_server:cast(?MODULE, {parse, LogFileName}).


%
% replace spaces and carriage returns with ''
%
replace_spaces_and_returns(String) ->
    lists:filter(
         fun(C) -> 
            if (C == $ ) or (C == $\n) ->
               false;
            true ->
               true
            end
         end, String).


%
% "{<<TEXT>>, {SOMETHING}}. --> "<<TEXT>>"
%
extract_unpacked_message(Data) ->
  DotPos = string:chr(Data, $.),
  RawMessage = string:substr(Data, 1, DotPos - 1),
  StartPos = string:chr(RawMessage, $<),
  EndPos = string:chr(RawMessage, $>),

  replace_spaces_and_returns(string:substr(RawMessage, StartPos, EndPos - StartPos + 2)).


%
% saving data into file.
% when "" passing it clense file
%
save_into_file(FileName, "") ->
   file:write_file(FileName, "", []);

save_into_file(FileName, Data) ->
   file:write_file(FileName, io_lib:fwrite("~p\n", [Data]), [append]).


%
% insert data in MongoDB or in file (depending it was fired or not)
%
insert_message(_State = #state{connection = Connection, db = Db, collection = Collection}, 
                _Data = {_task, _RecipCnt, Msisdns, TranSid, _Tag, _Error}) ->
  % State#state.unfired_message_counts = State#state.unfired_message_counts + 1,

  {ok, ok} = mongo:do(safe, master, Connection, Db, fun() ->
     lists:foreach(fun(Msisdn) ->
        ok = mongo:repsert(Collection, {'_id', Msisdn}, { '$push', { v, TranSid } })
      end, Msisdns)
    end);

insert_message(_State = #state{file_for_rejected_data = FileForRejectedData}, Data) ->  
  % State#state.fired_message_counts = State#state.fired_message_counts + 1,
  save_into_file(FileForRejectedData, Data).
  % State#state{fired_message_counts}


%
% extracting messages from log file
%
parse_logfile(Device, Accum, Position, State) ->
   case file:read(Device, 200) of
      eof -> file:close(Device), Accum;
      {_, Data} -> 
         AllData = Accum ++ Data,
         DotPosition = string:chr(Data, $.),
         if (DotPosition > 0) ->
            DotPos = string:chr(AllData, $.),

            Message = extract_unpacked_message(AllData),

            {ok, T, _} = erl_scan:string(Message ++ "."),
            {ok, Binary} = erl_parse:parse_term(T),     
            Term = binary_to_term(Binary),

            insert_message(State, Term),
            
            RestOfData = string:substr(AllData, DotPos + 1, string:len(AllData) - DotPos);
         true ->
            RestOfData = AllData
         end,
         
         show_progress(Position),
            
         parse_logfile(Device, RestOfData, Position + 200, State)
   end.


%
% show progress in console (Mb of raw text)
% can be deprecated and removed
%
show_progress(AmountOfRawDataParsed) ->
  Progress = AmountOfRawDataParsed rem (1000 * 1000),
  if (Progress == 0) ->
     io:format("~.2fM of raw data processed~n", [AmountOfRawDataParsed / 1024 / 1024]);
  true ->
     true
  end.


log_message(Message) ->
  io:format("~p~n", [Message]).