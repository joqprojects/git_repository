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

-module(ssl_server_test).

-include_lib("eunit/include/eunit.hrl").

-export([add/3]).

add(A,B,T)->
    timer:sleep(T),
    A+B.

%% PacketFormat=[binary,{packet,4},{reuseaddr, true}, {active, true}],
%% CertFile= {certfile,"/home/joq/erlang/code_examples/ssl/ebin/certificate.pem"}
%% KeyFile ={keyfile, "/home/joq/erlang/code_examples/ssl/ebin/key.pem"}

end_2_test()->
    os:cmd("rm ssl_server_portinfo"),
    os:cmd("rm ssl_server_portinfo"),
    ok.

init_test()->
    ssl:start(),
    ok=application:load(ssl_server_interface),
    ok=application:start(ssl_server_interface).

start_servers_test()-> 
    PacketFormat=[binary,{packet,4},{reuseaddr, true}, {active, true}],
    CertFile= "ebin/certificate.pem",
    KeyFile = "ebin/key.pem",
    ok=ssl_server_interface:start(1055,PacketFormat,CertFile,KeyFile, ms_1,1),
    ok=ssl_server:start_server(1155,PacketFormat,CertFile,KeyFile, ms_1,1),
    ok=ssl_server:start_server(1255,PacketFormat,CertFile,KeyFile, ms_2,2),
    ok=ssl_server:start_server(1355,PacketFormat,CertFile,KeyFile, ms_1,3),
    ok.

one_packet_test()->
    ClientSetUp=[binary,{packet,4}],
    Addr="localhost",
    TimeOut=infinity,
    PacketNumStart=0,

    {ok,SessionId_1}=ssl_server_interface:connect(Addr,1155,ClientSetUp,TimeOut),
    {ok,SessionId_2}=ssl_server_interface:connect(Addr,1255,ClientSetUp,TimeOut),

    {call_ack,11,0}=ssl_server_interface:call(SessionId_1,{ms_1,add,[10,1]},TimeOut,PacketNumStart),
    {call_ack,10,0}=ssl_server_interface:call(SessionId_1,{ms_2,multi,[10,1]},TimeOut,PacketNumStart),

     ssl_server_interface:disconnect(SessionId_1),
     ssl_server_interface:disconnect(SessionId_2),
    ok. 

multi_packet_test()->
    ClientSetUp=[binary,{packet,4}],
    Addr="localhost",
    TimeOut=infinity,
    PacketNumStart=0,
    
    {ok,SessionId_1}=ssl_server_interface:connect(Addr,1155,ClientSetUp,TimeOut),

    {call_ack,11,PacketNumStart}=ssl_server_interface:call(SessionId_1,{ms_1,add,[10,1]},TimeOut,PacketNumStart),
    P1=PacketNumStart+1,
    {call_ack,12,P1}=ssl_server_interface:call(SessionId_1,{ms_1,add,[10,2]},TimeOut,PacketNumStart+1),

  
     ssl_server_interface:disconnect(SessionId_1),
    

    MyPid=self(),
    Pid0=spawn(fun()->test_1055(1000,MyPid) end),
    Pid2=spawn(fun()->test_1255(1000,MyPid) end),   
    Pid21=spawn(fun()->test_1255(600,MyPid) end),
    3=check_progress([Pid0,Pid2,Pid21],0),
    ok.  

t1_test()->
    ClientSetUp=[binary,{packet,4}],
    Addr="localhost",
    TimeOut=300,
    {ok,SessionId}=ssl_server_interface:connect(Addr,1255,ClientSetUp,TimeOut),
    {call_ack,9}=ssl_server_interface:call(SessionId,{ms_2,sub,[10,1]}),
     ssl_server_interface:disconnect(SessionId),
    ok.


t3_test()->
    MyPid=self(),
    Pid0=spawn(fun()->test_1055(1000,MyPid) end),
    Pid2=spawn(fun()->test_1255(1000,MyPid) end),   
    Pid21=spawn(fun()->test_1255(600,MyPid) end),
    3=check_progress([Pid0,Pid2,Pid21],0),
    ok.  

t4_test()->
    ClientSetUp=[binary,{packet,4}],
    Addr="localhost",
    TimeOut=infinity,
    PacketNumStart=0,

    {ok,SessionId_1}=ssl_server_interface:connect(Addr,1155,ClientSetUp,TimeOut),
    {call_ack,11,0}=ssl_server_interface:call(SessionId_1,{ms_1,add,[10,1]},TimeOut,PacketNumStart),
    {error,timeout,SessionId_1}=ssl_server_interface:call(SessionId_1,{ms_1,add,[10,1]},300,PacketNumStart+1),
    % ssl_server_interface:clear_inbox(SessionId_1,PacketNumStart+1)
    receive
	{SessionId_1,R}->
	    {server_reply,{call_ack,11,1}}=R
    end,
    ssl_server_interface:disconnect(SessionId_1),
   
    {ok,SessionId_2}=ssl_server_interface:connect(Addr,1155,ClientSetUp,TimeOut),
    {call_ack,22,0}=ssl_server_interface:call(SessionId_2,{ms_1,add,[20,2]},TimeOut,PacketNumStart),
     ssl_server_interface:disconnect(SessionId_2),
    ok.

t5_test()->
    ClientSetUp=[binary,{packet,4}],
    Addr="localhost",
    TimeOut=infinity,
    PacketNumStart=0,

    {ok,SessionId_1}=ssl_server_interface:connect(Addr,1155,ClientSetUp,TimeOut),
    {call_ack,51,0}=ssl_server_interface:call(SessionId_1,{ms_1,add,[50,1]},TimeOut,PacketNumStart),
    {error,timeout,SessionId_1}=ssl_server_interface:call(SessionId_1,{ms_1,add,[20,1]},300,PacketNumStart+1),
    {call_ack,21,1}=ssl_server_interface:check_msg_queue(SessionId_1,TimeOut),
    {error,timeout,SessionId_1}=ssl_server_interface:call(SessionId_1,{ms_1,add,[30,1]},300,PacketNumStart+2),
    
    {call_ack,31,2}=ssl_server_interface:check_msg_queue(SessionId_1,TimeOut),

    {call_ack,11,3}=ssl_server_interface:call(SessionId_1,{ms_1,add,[10,1]},TimeOut,PacketNumStart+3),
    {call_ack,22,4}=ssl_server_interface:call(SessionId_1,{ms_1,add,[20,2]},TimeOut,PacketNumStart+4),
     ssl_server_interface:disconnect(SessionId_1),
    ok.

t6_test()->
    ClientSetUp=[binary,{packet,4}],
    Addr="localhost",
    TimeOut=infinity,
    PacketNumStart=0,

    {ok,SessionId_1}=ssl_server_interface:connect(Addr,1155,ClientSetUp,TimeOut),
    {badrpc,_Reason,0}=ssl_server_interface:call(SessionId_1,{ms_1,badrpc,[50,1]},TimeOut,PacketNumStart),
     ssl_server_interface:disconnect(SessionId_1),
    ok.


end_test()->
    misc_dets:delete(ssl_server_portinfo),
    misc_dets:delete(ssl_server_pidsport),
    ok=application:stop(ssl_server_interface),
    application:unload(ssl_server_interface),
    ok.



check_progress([],Acc)->
    Acc;
check_progress(PidList,N)->
    receive
	{Pid,R}->
	    ok=R,
	    NewList=lists:delete(Pid,PidList)
    end,
    check_progress(NewList,N+1).

test_1055(N,Pid)->
    ClientSetUp=[binary,{packet,4}],
    Addr="localhost",
    TimeOut=300,
    {ok,SessionId}=ssl_server_interface:connect(Addr,1055,ClientSetUp,TimeOut),
    test_1055(N,SessionId,Pid,0).

test_1055(0,SessionId,Pid,_PacketNum)-> 
   ssl_server_interface:disconnect(SessionId),
   Pid!{self(),ok};
test_1055(N,SessionId,Pid,PacketNum)->
    {call_ack,5.0,PacketNum}=ssl_server_interface:call(SessionId,{ms_1,divi,[10,2]},infinity,PacketNum),
    test_1055(N-1,SessionId,Pid,PacketNum+1). 


test_1255(N,Pid)->
    ClientSetUp=[binary,{packet,4}],
    Addr="localhost",
    TimeOut=300,
    {ok,SessionId}=ssl_server_interface:connect(Addr,1255,ClientSetUp,TimeOut),
    test_1255(N,SessionId,Pid,0).  
 
test_1255(0,SessionId,Pid,_PacketNum)->
   ssl_server_interface:disconnect(SessionId),
   Pid!{self(),ok};
test_1255(N,SessionId,Pid,PacketNum)->
    {call_ack,9,PacketNum}=ssl_server_interface:call(SessionId,{ms_2,sub,[10,1]},infinity,PacketNum),
    test_1255(N-1,SessionId,Pid,PacketNum+1).    

    
