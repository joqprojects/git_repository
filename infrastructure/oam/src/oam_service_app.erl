-module(oam_service_app).  %To be changed for new services
-behaviour(application).
-export([start/2, stop/1]).

-include("oam_service.hrl"). %To be changed for new services

start(_StartType, _StartArgs) ->
    {ok, Port}  = application:get_env(?SERVICE, port),
    {ok,SetUp}= application:get_env(?SERVICE, clientsetup),
    {ok,CallBackModule}= application:get_env(?SERVICE, callBackModule),
    {ok,MaxNumServers}= application:get_env(?SERVICE, maxNumServers),
    {ok,NetworkConfigFile}= application:get_env(?SERVICE, networkConfigFile),
    {ok,EbinPath}= application:get_env(?SERVICE, ebinPath),
   
    {ok, LSock} = ssl:listen(Port,SetUp ),
   {ok, Pid}=?SERVICE_SUP:start_link(LSock,CallBackModule,EbinPath,NetworkConfigFile),
    start_children(MaxNumServers),
    {ok, Pid}.

stop(_State) ->
    ok.

start_children(0)->
    ok;
start_children(N) ->
  %  io:format(" ~p~n",[{?MODULE,?LINE}]),
    ?SERVICE_SUP:start_child(),
    start_children(N-1).
