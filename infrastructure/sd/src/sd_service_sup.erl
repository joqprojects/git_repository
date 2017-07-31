-module(sd_service_sup).

-behaviour(supervisor).

-include("sd_service.hrl"). %To be changed for new services
%% API
-export([start_link/2, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

start_link(LSock,CallBackModule) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock,CallBackModule]).

start_child() ->
    supervisor:start_child(?MODULE, []).
    
init([LSock,CallBackModule]) ->
    Server = {?SERVICE_SERVER, 
	      {?SERVICE_SERVER,start_link, [LSock,CallBackModule]},
              permanent, brutal_kill, worker, [?SERVICE_SERVER]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 100,1000 },
    {ok, {RestartStrategy, Children}}.
