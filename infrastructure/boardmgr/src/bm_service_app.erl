-module(bm_service_app).  %To be changed for new services
-behaviour(application).
-export([start/2, stop/1]).

-include("include/bm_service.hrl"). %To be changed for new services

start(_StartType, _StartArgs) ->
    {ok, Port}  = application:get_env(?SERVICE, port),
    {ok,SetUp}= application:get_env(?SERVICE, clientsetup),
    {ok,CallBackModule}= application:get_env(?SERVICE, callBackModule),
    {ok,MaxNumServers}= application:get_env(?SERVICE, maxNumServers),

    {ok, LSock} = ssl:listen(Port,SetUp ),
    case bm_service_sup:start_link(LSock,CallBackModule) of
        {ok, Pid} ->
	    start_children(MaxNumServers),
            %template_ssl_p_sup:start_child(),
            {ok, Pid};
        Other ->
	    crash={?MODULE,?LINE},
            {error, Other}
    end.

stop(_State) ->
    ok.

start_children(0)->
    ok;
start_children(N) ->
  %  io:format(" ~p~n",[{?MODULE,?LINE}]),
    bm_service_sup:start_child(),
    start_children(N-1).
