-module(template_ssl_p_sup).

-behaviour(supervisor).

%% API
-export([start_link/2, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(LSock,CallBackModule) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock,CallBackModule]).

start_child() ->
    supervisor:start_child(?SERVER, []).

init([LSock,CallBackModule]) ->
    Server = {template_ssl_p_server, {template_ssl_p_server, start_link, [LSock,CallBackModule]},
              temporary, brutal_kill, worker, [template_ssl_p_server]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
