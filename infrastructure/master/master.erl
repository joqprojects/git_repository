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

-module(master).

-export([start/0]).

start()->
    {ok,MasterData}=file:consult("ebin/master.config"),
    
    {serviceName,ServiceName_Oam}=lists:keyfind(serviceName,1,MasterData),
    {port,Port_Oam}=lists:keyfind(port,1,MasterData),
    {callBackModule,CallBackModule_Oam}=lists:keyfind(callBackModule,1,MasterData),
    {maxNumServers,MaxNumServers_Oam}=lists:keyfind(maxNumServers,1,MasterData),
    {ebinPath,EbinPath}=lists:keyfind(ebinPath,1,MasterData),
    {networkConfigFile,NetworkConfigFile}=lists:keyfind(networkConfigFile,1,MasterData),

    ok=oam_service_if:start(ServiceName_Oam,Port_Oam,
			    CallBackModule_Oam,MaxNumServers_Oam,
			    EbinPath,NetworkConfigFile),
    ok=oam_test_1:test(),

    {ok,BoardData}=file:consult("ebin/board.config"),
    {serviceName,ServiceName_BM}=lists:keyfind(serviceName,1,BoardData),
    {port,Port_BM}=lists:keyfind(port,1,BoardData),
    {callBackModule,CallBackModule_BM}=lists:keyfind(callBackModule,1,BoardData),
    {maxNumServers,MaxNumServers_BM}=lists:keyfind(maxNumServers,1,BoardData),

     ok=bm_service_if:start(ServiceName_BM,Port_BM,
			    CallBackModule_BM,MaxNumServers_BM),

    
    ok=bm_test_1:test(),

    ok=oam_service_if:stop(ServiceName_Oam),
    ok=bm_service_if:stop(ServiceName_BM),
    ok.
    

