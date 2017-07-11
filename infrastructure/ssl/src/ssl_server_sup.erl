-module(ssl_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
  %  io:format("~p~n",[{?M1ODULE,?LINE}]),
    supervisor:start_child(?SERVER, []).

init([]) ->
    Server = {ssl_server, {ssl_server, start, []},
              permanent, brutal_kill, worker, [ssl_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
