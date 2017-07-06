-module(microservice_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_SERVER_SETUP,[binary,{packet,4},{reuseaddr,true},{active,true}]).
-define(DEFAULT_PORT, 1055).

start(_StartType, _StartArgs) ->

  %  io:format("~p~n",[{?MODULE,?LINE,Port,ServerSetUp,MicroService}]),
%    {ok, LSock} = gen_tcp:listen(Port,ServerSetUp),
    {ok,_Pid}= microservice_sup:start_link().

stop(_State) ->
    ok.
