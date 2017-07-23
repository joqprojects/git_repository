-module(template_ssl_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1155).

start(_StartType, _StartArgs) ->
    Port = case application:get_env(template_ssl, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    {ok,SetUp}= application:get_env(template_ssl, clientsetup),
    {ok, LSock} = ssl:listen(Port, SetUp),
    case template_ssl_sup:start_link(LSock) of
        {ok, Pid} ->
            template_ssl_sup:start_child(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
