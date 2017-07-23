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

-module(template_ssl_test).

-include_lib("eunit/include/eunit.hrl").

-export([]).

init_test()->
    Port=10055,
    PacketFormat=[binary,{packet,4},{reuseaddr, true}, {active, true}],
    ServiceName=template_ssl,
    CertFile= "ebin/certificate.pem",
    KeyFile = "ebin/key.pem",
    SetUp=lists:append(PacketFormat,[{certfile,CertFile},{keyfile,KeyFile}]),
    ok=template_ssl_system:start(ServiceName,Port,SetUp),
    ok.

t0_test()->
    Addr="localhost",
    Port=10055,
    ClientSetUp=[binary,{packet,4}],
    {ok,S1}=template_ssl_client:connect(Addr,Port,ClientSetUp),
    ok=ssl:send(S1,term_to_binary(S1)),
    S1=template_ssl_client:rec(S1,1000),
    ok.
stop_test()->
    ServiceName=template_ssl,
    ok=template_ssl_system:stop(ServiceName),
    ok.
