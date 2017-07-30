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

-module(math_test).

-include_lib("eunit/include/eunit.hrl").
-include("include/math_service.hrl").
-include("include/service_register.hrl").


-export([]).

% Basic tests on a singelton process

start_system_test()->
    ok=math_service_if:start(?MATH_PORT,1),
    ok.

basic_connect_disconnect_test()->
    {ok,S1}=math_service_if:connect_timeout(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
    Msg1={?SERVICE,add,[20,21],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
    {{ok,41},0}=math_service_if:rec(S1,1000),    
    math_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    Msg2={?SERVICE,add,[20,22],0},
    {error,closed}=ssl:send(S1,term_to_binary(Msg2)),
    ok.

stop_system_test()->
    ok=math_service_if:stop(?SERVICE),
    ok.
restart_system_test()->
    ok=math_service_if:start(?MATH_PORT,1),
    ok.

% Explict math services exposed via interface funtion
add_services_test()->
    PacketNum=10,
    {ok,S1}=math_service_if:connect(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT),
    ok=math_service_if:add(20,21,S1,PacketNum),
    {{ok,41},PacketNum}=math_service_if:rec(S1,1000),
    math_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.

service_crash_test()->
    PacketNum=10,
    {ok,S1}=math_service_if:connect(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT),
    ok=math_service_if:divi(20,0,S1,PacketNum),
    {{error,Err},PacketNum}=math_service_if:rec(S1,1000),
    math_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.    

wrong_service_test()->    
    {ok,S1}=math_service_if:connect(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT),
    Msg1={wrongService,add,[20,21],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
    {{error,{notImplemented,wrongService}},0}=math_service_if:rec(S1,1000),
    math_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.

no_function_test()->
    {ok,S1}=math_service_if:connect(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT),
    Msg1={?SERVICE,noFunction,[20,21],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
    {{error,{badrpc,_ErrMsg}},0}=math_service_if:rec(S1,1000),
    math_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.
crash_test()->
    {ok,S1}=math_service_if:connect(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT),
    Msg1={?SERVICE,divi,[20,0],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
    {{error,{badrpc,_ErrMsg}},0}=math_service_if:rec(S1,1000),
    math_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.

% Singelton process tests

max_server_test()->
    {ok,S1}=math_service_if:connect_timeout(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
    {error,timeout}=math_service_if:connect_timeout(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
    math_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    timer:sleep(1000),
    ok.

singleton_multi_test()->
   {ok,S1}=math_service_if:connect_timeout(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT,1000),
  %  {ok,S2}=math_service_if:connect(Addr,Port,ClientSetUp,TimeOut),
    {error,timeout}=math_service_if:connect_timeout(?MATH_IP_ADDR,?MATH_PORT,?CONNECT_TIMEOUT),
    Msg1={?SERVICE,add,[20,21],0},
%    Msg2={?SERVICE,add,[20,22],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
   % ok=ssl:send(S2,term_to_binary(Msg2)),
    {{ok,41},0}=math_service_if:rec(S1,1000),
    %{{ok,42},0}=math_service_if:rec(S2,1000),

    math_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
   % math_service_if:disconnect(S2),
    ok.
stop_singleton_test()->
    ok=math_service_if:stop(?SERVICE),
    ok.

% Multi process tests
start_multi_test()->
    ok=math_service_if:start(?MATH_PORT,3),
    ok.    


multi_multi_test()->
    {ok,S1}=math_service_if:connect_timeout(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
    {ok,S2}=math_service_if:connect_timeout(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),

    Msg1={?SERVICE,add,[20,21],0},
    Msg2={?SERVICE,add,[20,22],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
    ok=ssl:send(S2,term_to_binary(Msg2)),
    {{ok,41},0}=math_service_if:rec(S1,1000),
    {{ok,42},0}=math_service_if:rec(S2,1000),

    math_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    math_service_if:disconnect(S2,?SYSTEM_TIMEOUT),
    ok.

stop_test()->
    ok=math_service_if:stop(?SERVICE),
    ok.
