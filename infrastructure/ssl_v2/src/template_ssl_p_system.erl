-module(template_ssl_p_system).

-export([start/4, stop/1]).

start(ServiceName,Port,Setup,CallBackModule) ->
    ok=ssl:start(),
    ok=application:set_env(ServiceName,port, Port),
    ok=application:set_env(ServiceName,clientsetup,Setup),
    ok=application:set_env(ServiceName,callBackModule,CallBackModule),
    application:load(ServiceName),
    application:start(ServiceName),
    ok.

stop(ServiceName) ->
    application:stop(ServiceName),
    application:unload(ServiceName),
    ok.
