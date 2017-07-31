%% 
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
%% Copyright (C) 2002, Joe Armstrong
%% File    : lib_tcp_server.erl
%% Author  : Joe Armstrong (joe@sics.se)
%% Purpose : Keeps track of a number of TCP sessions
%% Time-stamp: <2012-10-12 08:33:25 joe>

-module(infra_test).

-export([start/0]).

start()->
    EUnitTest=[sd_test,bm_test,oam_test],
    Result=do_test(EUnitTest,[]),
    timer:sleep(1000),
       io:format("~n"),   io:format("~n"),   io:format("~n"),   io:format("~n"),
    io:format("*********************************~n"),
    io:format("-------  Test result ------~n"),
    io:format("   ~p~n",[{date(),time()}]),
   io:format("~n"),
    print(Result),
   io:format("~n"),
    io:format("-------  End Test    ------~n").

print([])->
    ok;
print([Result|T])->
    io:format("~p~n",[Result]),
    print(T).

do_test([],Acc)->
    Acc;
do_test([Service|T],Acc)->
    case rpc:call(node(),Service,test,[]) of
	{badrpc,Err}->
	    Acc1=[{badrpc,Service, time()}|Acc];
	Result->
	    Acc1=[{Result,Service,time()}|Acc]
    end,
    do_test(T,Acc1).
