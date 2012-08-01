-module(test).
-export([conn/0, foo/1, bar/0]).
-include("deps/mongodb/include/mongo_protocol.hrl").


-record(rec, {x=1, y}).

-record(mongo_options,
	 {host="localhost",
	 port=27017,
	 db=test,
	 fired_messages_collection=fired,
	 unfired_messages_collection=unfired
	 }).


conn() ->
  application:start(mongodb),
  {ok, Conn} = mongo:connect({localhost, 27017}),
  io:format("Connect is: ~p~n", [Conn]),
  {ok, _Docs} = mongo:do(safe, master, Conn, test, fun() ->
  	mongo:save(unfired_jobs, {1}) end),
  mongo:disconnect(Conn),
  ok.
  
  
foo(Opt) ->
  R = #rec{x=element(1, Opt), y=element(2, Opt)},
  io:format("~p~n", [R]).
  
  
bar() ->
%   io:format("~p~n", [Dir ++ "/config/default.cfg"]),
   {ok, Config} = file:consult("../../" ++ "config/default.cfg"),
   
   R = list_to_tuple([mongo_options|[proplists:get_value(X, Config) || X <- record_info(fields, mongo_options)]]),

   % R = #mongo_options(host=
   io:format("~p~n~n", [R]).
