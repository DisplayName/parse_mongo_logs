-module(parse_mongo_logs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("starting: ~p~n", [?MODULE]),
	application:start(mongodb),

   parse_mongo_logs_sup:start_link().

stop(_State) ->
    ok.