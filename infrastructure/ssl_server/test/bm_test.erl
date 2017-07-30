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

-module(bm_test).

-include_lib("eunit/include/eunit.hrl").
-include("include/bm_service.hrl").
-include("include/service_register.hrl").

-export([]).

% Basic tests on a singelton process

start_system_test()->
    ok=math_service_if:start(math_service,?MATH_PORT,math_service,1),
    bm_service_if:start(bm_service,?BM_PORT,bm_service,1),
    ok.

basic_connect_disconnect_test()->
    PacketIdReqService=5,
    PacketIdBm=42,
    {ok,S1}=bm_service_if:connect_timeout(?BM_IP_ADDR,?BM_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
    ReqMsg={math_service,add,[20,21],PacketIdReqService},
    BmMsg={bm_service,call_service,[ReqMsg],PacketIdBm},
    ok=bm_service_if:send(S1,BmMsg),
    {{{ok,41},PacketIdReqService},PacketIdBm}=bm_service_if:rec(S1,3000),    
    bm_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    {error,closed}=bm_service_if:send(S1,BmMsg),
    ok.

service_crash_test()->
    PacketIdReqService=5,
    PacketIdBm=42,
    {ok,S1}=bm_service_if:connect_timeout(?BM_IP_ADDR,?BM_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
    ReqMsg={math_service,divi,[20,0],PacketIdReqService},
    BmMsg={bm_service,call_service,[ReqMsg],PacketIdBm},
    ok=bm_service_if:send(S1,BmMsg),
    {{{error,_Err},PacketIdReqService},PacketIdBm}=bm_service_if:rec(S1,3000), 
    bm_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.  

wrong_service_Service_test()->    
    PacketIdReqService=5,
    PacketIdBm=42,
    {ok,S1}=bm_service_if:connect_timeout(?BM_IP_ADDR,?BM_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
    ReqMsg={wrongService,add,[20,21],PacketIdReqService},
    BmMsg={bm_service,call_service,[ReqMsg],PacketIdBm},
    ok=bm_service_if:send(S1,BmMsg),
    {{{error,wrongService,PacketIdReqService}},PacketIdBm}=bm_service_if:rec(S1,3000),
    bm_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.

no_function_service__test()->    
    PacketIdReqService=5,
    PacketIdBm=42,
    {ok,S1}=bm_service_if:connect_timeout(?BM_IP_ADDR,?BM_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
    ReqMsg={math_service,no_function,[20,21],PacketIdReqService},
    BmMsg={bm_service,call_service,[ReqMsg],PacketIdBm},
    ok=bm_service_if:send(S1,BmMsg),
    {{{error,_Err},PacketIdReqService},PacketIdBm}=bm_service_if:rec(S1,3000),
    bm_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.


stop_singleton_test()->
    ok=bm_service_if:stop(bm_service),
    ok=math_service_if:stop(math_service),
    ok.

% Multi process tests
start_multi_test()->
    ok=math_service_if:start(math_service,?MATH_PORT,math_service,3),
    bm_service_if:start(bm_service,?BM_PORT,bm_service,3),
    ok.    


multi_multi_test()->
    PacketIdReqService=0,
    PacketIdBm=10,
    {ok,S1}=bm_service_if:connect_timeout(?BM_IP_ADDR,?BM_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
    {ok,S2}=bm_service_if:connect_timeout(?BM_IP_ADDR,?BM_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
    ReqMsg1={math_service,add,[20,21],PacketIdReqService},
    ReqMsg2={math_service,add,[20,22],PacketIdReqService+1},
    BmMsg1={bm_service,call_service,[ReqMsg1],PacketIdBm},
    BmMsg2={bm_service,call_service,[ReqMsg2],PacketIdBm+1},

    ok=bm_service_if:send(S1,BmMsg1),
    ok=bm_service_if:send(S2,BmMsg2),

    {{{ok,41},PacketIdReqService},PacketIdBm}=bm_service_if:rec(S1,3000),
    {{{ok,42},1},11}=bm_service_if:rec(S2,3000),

    math_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    math_service_if:disconnect(S2,?SYSTEM_TIMEOUT),
    ok.


stop_test()->
    ok=bm_service_if:stop(bm_service),
    ok=math_service_if:stop(math_service),
    ok.
