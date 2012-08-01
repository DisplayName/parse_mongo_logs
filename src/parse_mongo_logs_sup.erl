-module(parse_mongo_logs_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    io:format("starting: ~p~n", [?MODULE]),

    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy    = one_for_one,
    MaxRestarts        = 5,
    MaxTimeBetRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

    ChildSpecs =
    [
     {test,
      {test, start_link, []},
      permanent,
      infinity,
      worker,
      [test]}
     ],
    
    % {ok, {SupFlags, ChildSpecs}}.


    {ok, { {one_for_one, 5, 10}, [
        {log_parser, {log_parser, start_link, []}, permanent, infinity, worker, [log_parser]}

    ]} }.




% -module(parse_mongo_logs_sup).

% -behaviour(supervisor).

% %% API
% -export([start_link/0]).

% %% Supervisor callbacks
% -export([init/1]).

% %% Helper macro for declaring children of supervisor
% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

% %% ===================================================================
% %% API functions
% %% ===================================================================

% start_link() ->
%     supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% %% ===================================================================
% %% Supervisor callbacks
% %% ===================================================================

% init([]) ->
%     io:format("init MAIN~n", []),

%     {ok, { {one_for_one, 5, 10}, [
%         {log_parser1, {log_parser, start_link, []}, permanent, 6000, worker, [log_parser]}

%     ]} }.    

