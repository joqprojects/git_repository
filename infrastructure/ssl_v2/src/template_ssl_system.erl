-module(template_ssl_system).

-export([start/3, stop/1]).

start(ServiceName,Port,Setup) ->
    ok=ssl:start(),
    ok=application:set_env(ServiceName,port, Port),
    ok=application:set_env(ServiceName,clientsetup,Setup),
    application:load(ServiceName),
    application:start(ServiceName),
    ok.

stop(ServiceName) ->
    application:stop(ServiceName),
    application:unload(ServiceName),
    ok.
