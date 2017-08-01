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

-module(oam_test_1).

-include_lib("eunit/include/eunit.hrl").
-include("oam_service.hrl").
-include("service_register.hrl").


-export([]).

% Basic tests on a singelton process


basic_connect_disconnect_test()->
    {ok,S1}=oam_service_if:connect_timeout(?OAM_IP_ADDR,?OAM_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT*5),
    oam_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.

add_delete_test()->
     IP="192.168.0.1",
    Port=1001,
    PacketNum=0,

    {ok,S1}=oam_service_if:connect_timeout(?OAM_IP_ADDR,?OAM_PORT,
					   ?CLIENT_PACKET_FORMAT,
					   ?CONNECT_TIMEOUT*5),
    ok=oam_service_if:send(S1,{?SERVICE,add_node,[IP,Port],PacketNum}),
    ok=oam_service_if:send(S1,{?SERVICE,get_all_nodes,[],PacketNum+1}),
    {{ok,[{"192.168.0.1",1001}]},1}=oam_service_if:rec(S1,?CALL_TIMEOUT),   
    ok=oam_service_if:send(S1,{?SERVICE,delete_node,[IP,Port],PacketNum+2}),    
    ok=oam_service_if:send(S1,{?SERVICE,get_all_nodes,[],PacketNum+3}),
    {{ok,[]},3}=oam_service_if:rec(S1,?CALL_TIMEOUT),    
    oam_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.

config_test()->
    PacketNum=0,
    {ok,S1}=oam_service_if:connect_timeout(?OAM_IP_ADDR,?OAM_PORT,
					   ?CLIENT_PACKET_FORMAT,
					   ?CONNECT_TIMEOUT*5),
    ok=oam_service_if:send(S1,{?SERVICE,get_config,["Asus"],PacketNum}),
    {{ok,{"80.216.90.144",10110,[dets_db_service],[math_service]}},0}=oam_service_if:rec(S1,?CALL_TIMEOUT),      

    ok=oam_service_if:send(S1,{?SERVICE,get_config,["No Entry"],PacketNum+1}),
    {{error,{{boardId_not_exists,"No Entry"}}},1}=oam_service_if:rec(S1,?CALL_TIMEOUT), 
    oam_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.

file_handling_test()->
    PacketNum=0,
    {ok,S1}=oam_service_if:connect_timeout(?OAM_IP_ADDR,?OAM_PORT,
					   ?CLIENT_PACKET_FORMAT,
					   ?CONNECT_TIMEOUT*5),
    ok=oam_service_if:send(S1,{?SERVICE,get_file_list,[],PacketNum}),
    {{ok,[{"oam_service.app",_},{"network.config",_}]},0}=oam_service_if:rec(S1,?CALL_TIMEOUT),   

    ok=oam_service_if:send(S1,{?SERVICE,get_file,["oam_service.app"],PacketNum+1}),
    {{ok,{"oam_service.app",File}},1}=oam_service_if:rec(S1,?CALL_TIMEOUT),

    ok=oam_service_if:send(S1,{?SERVICE,get_file,["no_file"],PacketNum+1}),
    {{error,{{no_entry,"no_file"}}},1}=oam_service_if:rec(S1,?CALL_TIMEOUT),

    oam_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.
