-module(parser).
-export([parse_logs/1, do/0]).
-include("deps/mongodb/include/mongo_protocol.hrl").

%
% sample call
%
% parser:parse_logs("data/test").
%


-define(FIRED, 1).
-define(UNFIRED, 0).
-define(DEFAULT_CONFIG, "config/default.cfg").


-record(task, {
  rcnt    :: integer(),
  msisdns :: list(),
  sid     :: integer(),
  dtag    :: integer(),
  err = 0 :: byte() | err | ok
}).


-record(mongo_options,
	 {host="localhost",
	 port=27017,
	 db=test,
	 fired_messages_collection=fired,
	 unfired_messages_collection=unfired
	 }).

	
	 
%
%  get default mongo config
%
get_mongodb_config()->
   {ok, Config} = file:consult("../../" ++ ?DEFAULT_CONFIG),
   list_to_tuple([mongo_options|[proplists:get_value(X, Config) || X <- record_info(fields, mongo_options)]]).


   
%
% show progress in console (in Mb of raw text)
% can be deprecated and removed
%
show_progress(AmountOfRawDataParsed) ->
  Progress = AmountOfRawDataParsed rem (1000 * 1000),
  if (Progress == 0) ->
     io:format("~.2fM of raw data processed      (~p)~n", [AmountOfRawDataParsed / 1024 / 1024, erlang:localtime()]);
  true ->
     true
  end.         

  
  
%
% choosing proper collection if job had been executed or not
%
choose_collection(Options, ?FIRED) ->
   Options#mongo_options.fired_messages_collection;
   
choose_collection(Options, ?UNFIRED) ->
   Options#mongo_options.unfired_messages_collection.
   

   
%
% wrapper for insert job in mongo
%
insert_message(MongoOptions, Connection, IsFiredMessage, 
    Data = {_task, RecipCnt, Msisdns, TranSid, _Tag, _Error}) ->


    %  = #task{rcnt = RecipCnt, msisdns = Msisdns, sid = TranSid}


  %% {ok, _Message} = mongo:do(safe, master, Connection, MongoOptions#mongo_options.db, fun() ->
  %%	mongo:save(choose_collection(MongoOptions, IsFiredMessage), {Data}) end).

  % ?log_debug("inserting...~nRecipCnt: ~p~nMsisdns: ~p~nTranSid: ~p", [RecipCnt, Msisdns, TranSid]),


% -record(task, {
%   rcnt    :: integer(),
%   msisdns :: list(),
%   sid     :: integer(),
%   dtag    :: integer(),
%   err = 0 :: byte() | err | ok
% }).


% handle_cast(process_task, State = #state{connection=Conn,dbname = DbName, task =  Task}) ->
%   #task{rcnt = RecipCnt, msisdns = Msisdns, sid = TranSid} = Task,
%   ?log_debug("inserting...~nRecipCnt: ~p~nMsisdns: ~p~nTranSid: ~p", [RecipCnt, Msisdns, TranSid]),

%   {ok, ok} = mongo:do(safe, master, Conn, DbName, fun() ->
%       lists:foreach(fun(Msisdn) ->
%       ok = mongo:repsert(rbs_Storage, {'_id', Msisdn}, { '$push', { v, TranSid } })
%       end, Msisdns)
%     end),

%   rbs_dispatcher:give_task(_ToPid = self()),
%   rbs_rabbit_mgr:ack(Task),
%   {noreply, State#state{task = undef}};


%   {task,1,[974550999930],98975327,540,10}

io:format("  ~p ~p~n", [Msisdns, TranSid]),


   {ok, ok} = mongo:do(safe, master, Connection, test, fun() ->
       lists:foreach(fun(Msisdn) ->
       ok = mongo:repsert(default, {'_id', Msisdn}, { '$push', { v, TranSid } })
       end, Msisdns)
     end).


  	
%
% remove some chars from input
%
replaceUselessChars(String) ->
    lists:filter(
         fun(C) -> 
            if (C == $ ) or (C == $\n) ->
               false;
            true ->
               true
            end
         end, String).

      
      
%
% wrapper function
% parsing logs from "LogFile": should be full path + name
%
do() ->
  parse_logs("/home/svart_ravn/work/projects/trash/parse_mongo_logs/data/test").


parse_logs(LogFileFullPath) -> 
   MongoOpts = get_mongodb_config(),

   io:format("~p~n~n~n", [MongoOpts]),
   
   application:start(mongodb),
   
   case file:open(LogFileFullPath, [read]) of
     {ok, Device} ->
        {ok, Connection} = mongo:connect({MongoOpts#mongo_options.host, MongoOpts#mongo_options.port}),
        get_data_from_file(Device, "", 0, MongoOpts, Connection),
        mongo:disconnect(Connection),
        file:close(Device);
     {error, Reason} -> 
        io:format("Exit because of: ~p~n", [Reason])
   end.

 
   
%
% parsing raw data from data file
%
get_data_from_file(Device, Accum, Position, MongoOpts, Connection) ->
   case file:read(Device, 200) of
      eof -> file:close(Device), Accum;
      {_, Data} -> 
         AllData = Accum ++ Data,
         DotPosition = string:chr(Data, $.),
         if (DotPosition > 0) ->
            DotPos = string:chr(AllData, $.),
            RawMessage = string:substr(AllData, 1, DotPos - 1),
            StartPos = string:chr(RawMessage, $<),
            EndPos = string:chr(RawMessage, $>),

            Message = replaceUselessChars(string:substr(RawMessage, StartPos, EndPos - StartPos + 2)),
            
            {ok, T, _} = erl_scan:string(Message ++ "."),
            {ok, Binary} = erl_parse:parse_term(T),     
            Term = binary_to_term(Binary),
            TaskType = element(1, Term),
            
            if (TaskType == task) ->
                io:format("~p~n~n--------------------------------~n", [Term]),
               insert_message(MongoOpts, Connection, ?UNFIRED, Term);
            true ->
               ok
            end,
            
            RestOfData = string:substr(AllData, DotPos + 1, string:len(AllData) - DotPos);
         true ->
            RestOfData = AllData
         end,
         
         show_progress(Position),
            
         get_data_from_file(Device, RestOfData, Position + 200, MongoOpts, Connection)
   end.