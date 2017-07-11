-module(ezwebframe_demos).
-export([start/0]).

%% This demo assumes that all your code
%% the code paths to ezwebframe and simple_demo are correct
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("").

start() ->
%    Port =?PORT_WEB,
    Port=5555,
    io:format("ezwebframe started port ~p~n",[Port]),
    ezwebframe:start_link(fun dispatch/1, Port).

%% dispatch maps names in the HTML onto fixed paths 

dispatch(F) ->
    F1 = dispatch1(F),
%    io:format("ezwebframe_demos::dispatch ~s => ~s~n",[F,F1]),
    F1.

dispatch1("/ezwebframe/" ++ F) ->
    Dir = dir(2, code:which(ezwebframe)) ++ "/priv/",
    Dir ++ F;
dispatch1("/" ++ F) ->
    Dir = dir(2,code:which(?MODULE)) ++ "/",
    Dir ++ F.

dir(0, F) -> F;
dir(K, F) -> dir(K-1, filename:dirname(F)).