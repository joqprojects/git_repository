-module(oam_service_sup).

-behaviour(supervisor).

-include("oam_service.hrl"). %To be changed for new services
%% API
-export([start_link/4, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

start_link(LSock,CallBackModule,EbinPath,NetworkConfigFile) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock,CallBackModule,EbinPath,NetworkConfigFile]).

start_child() ->
    supervisor:start_child(?MODULE, []).
    
init([LSock,CallBackModule,EbinPath,NetworkConfigFile]) ->
    Server = {?SERVICE_SERVER, 
	      {?SERVICE_SERVER,start_link, [LSock,CallBackModule,EbinPath,NetworkConfigFile]},
              permanent, brutal_kill, worker, [?SERVICE_SERVER]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 100,1000 },
    {ok, {RestartStrategy, Children}}.
