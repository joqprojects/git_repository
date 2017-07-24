%% ---
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

-module(template_ssl_p_test).

-include_lib("eunit/include/eunit.hrl").

-export([]).

init_test()->
    Port=10066,
    PacketFormat=[binary,{packet,4},{reuseaddr, true}, {active, true}],
    ServiceName=template_ssl_p,
    CertFile= "ebin/certificate.pem",
    KeyFile = "ebin/key.pem",
    CallBackModule=template_math,
    SetUp=lists:append(PacketFormat,[{certfile,CertFile},{keyfile,KeyFile}]),
    ok=template_ssl_p_system:start(ServiceName,Port,SetUp,CallBackModule),
    ok.

connect_disconnect_test()->
    Addr="localhost",
    Port=10066,
    ClientSetUp=[binary,{packet,4}],
    {ok,S1}=template_ssl_client:connect(Addr,Port,ClientSetUp),
    template_ssl_client:disconnect(S1),
    Msg1={template_math,add,[20,22],0},
    {error,closed}=ssl:send(S1,term_to_binary(Msg1)),
    ok.

single_test()->
    Addr="localhost",
    Port=10066,
    ClientSetUp=[binary,{packet,4}],
    {ok,S1}=template_ssl_client:connect(Addr,Port,ClientSetUp),
    Msg1={template_math,add,[20,21],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
    {{ok,41},0}=template_ssl_client:rec(S1,1000),
    template_ssl_client:disconnect(S1),
    ok.

mulit_test()->
    Addr="localhost",
    Port=10066,
    ClientSetUp=[binary,{packet,4}],
    {ok,S1}=template_ssl_client:connect(Addr,Port,ClientSetUp),
    {ok,S2}=template_ssl_client:connect(Addr,Port,ClientSetUp),
    Msg1={template_math,add,[20,21],0},
    Msg2={template_math,add,[20,22],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
    ok=ssl:send(S2,term_to_binary(Msg2)),
    {{ok,41},0}=template_ssl_client:rec(S1,1000),
    {{ok,42},0}=template_ssl_client:rec(S2,1000),
    template_ssl_client:disconnect(S1),
    template_ssl_client:disconnect(S2),
    ok.

wrong_service_test()->
    Addr="localhost",
    Port=10066,
    ClientSetUp=[binary,{packet,4}],
    {ok,S1}=template_ssl_client:connect(Addr,Port,ClientSetUp),
    Msg1={wrongService,add,[20,21],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
    {{error,{notImplemented,wrongService}},0}=template_ssl_client:rec(S1,1000),
    template_ssl_client:disconnect(S1),
    ok.

no_function_test()->
    Addr="localhost",
    Port=10066,
    ClientSetUp=[binary,{packet,4}],
    {ok,S1}=template_ssl_client:connect(Addr,Port,ClientSetUp),
    Msg1={template_math,noFunction,[20,21],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
    {{error,{badrpc,_ErrMsg}},0}=template_ssl_client:rec(S1,1000),
    template_ssl_client:disconnect(S1),
    ok.


crash_test()->
    Addr="localhost",
    Port=10066,
    ClientSetUp=[binary,{packet,4}],
    {ok,S1}=template_ssl_client:connect(Addr,Port,ClientSetUp),
    Msg1={template_math,divi,[20,0],0},
    ok=ssl:send(S1,term_to_binary(Msg1)),
    {{error,{badrpc,_ErrMsg}},0}=template_ssl_client:rec(S1,1000),
    template_ssl_client:disconnect(S1),
    ok.

stop_test()->
    ServiceName=template_ssl_p,
    ok=template_ssl_system:stop(ServiceName),
    ok.
