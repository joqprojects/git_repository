%% 

-module(sd_test).

-include_lib("eunit/include/eunit.hrl").
-include("include/sd_service.hrl").
-include("include/service_register.hrl").


-export([]).

% Basic tests on a singelton process

start_system_test()->
    ok=sd_service_if:start(?SD_PORT,1),
    ok.

basic_connect_disconnect_test()->
    {ok,S1}=sd_service_if:connect_timeout(?SD_IP_ADDR,?SD_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT*5),
    sd_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.

add_local_test()->
    PacketNum=0,
    {ok,S1}=sd_service_if:connect_timeout(?SD_IP_ADDR,?SD_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT*5),
    ok=oam_service_if:send(S1,{?SERVICE,add_local_resource,[math_service,{?MATH_IP_ADDR,?MATH_PORT}],PacketNum}),
    ok=oam_service_if:send(S1,{?SERVICE,get_local,[],PacketNum+1}),
    {{ok,[{math_service,[{"localhost",10066}]}]},1}=oam_service_if:rec(S1,?CALL_TIMEOUT), 	   
    sd_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.

add_target_test()->
     PacketNum=0,
    {ok,S1}=sd_service_if:connect_timeout(?SD_IP_ADDR,?SD_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT*5),
    ok=oam_service_if:send(S1,{?SERVICE,add_target_service,[math_service],PacketNum}),
    ok=oam_service_if:send(S1,{?SERVICE,get_target,[],PacketNum+1}),
    {{ok,[math_service]},1}=oam_service_if:rec(S1,?CALL_TIMEOUT), 	   
    sd_service_if:disconnect(S1,?SYSTEM_TIMEOUT),
    ok.

stop_test()->
    ok=sd_service_if:stop(?SERVICE),
    ok.
