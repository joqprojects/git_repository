-module(bm_service_if).

-export([connect/2,connect/3,connect_timeout/3,connect_timeout/4,send/2,rec/2,disconnect/1,disconnect/2]).

-export([start/2,start/4, stop/0,stop/1]).


 
% Service specific parts - To be changed for new services
-include("include/bm_service.hrl"). 
-export([call_service/3]).

call_service({ReqService,F,A,PacketIdService},SslSocket_BM,PacketNum)->
    ReqServiceInfo={ReqService,F,A,PacketIdService},
    {ok,CallBackModule}= application:get_env(?SERVICE, callBackModule),
    Msg={CallBackModule,call_service,[ReqServiceInfo],PacketNum},
    send(SslSocket_BM,Msg).

%% Generic parts interface part - Do not change !
start(Port,MaxNumServers)->
    ok=start(?SERVICE,Port,?CALLBACKMODULE,MaxNumServers).

start(ServiceName,Port,CallBackModule,MaxNumServers) ->
    RunningApps=application:which_applications(),
    case lists:keyfind(ssl,1,RunningApps) of
	false->
	    ssl:start();
	_->
	    ssl_already_started
    end,
    SetUp=lists:append(?PACKET_FORMAT,[{certfile,?CERTFILE},{keyfile,?KEYFILE}]),
    ok=application:set_env(ServiceName,port, Port),
    ok=application:set_env(ServiceName,clientsetup,SetUp),
    ok=application:set_env(ServiceName,callBackModule,CallBackModule),
    ok=application:set_env(ServiceName,maxNumServers,MaxNumServers),
    application:load(ServiceName),
    application:start(ServiceName),
    ok.

stop()->
    stop(?SERVICE).
stop(ServiceName) ->
    application:stop(ServiceName),
    application:unload(ServiceName),
    ok.


%% Client part 
send(Socket,Msg)->
    ssl:send(Socket,term_to_binary(Msg)).

rec(Socket,TimeOut)->
    receive
	{ssl,Socket, ReplyBin}->
	    Reply=binary_to_term(ReplyBin);
	{ssl_closed, Socket}->
	    Reply={ssl_closed, Socket};
	{ssl,{sslsocket,_Z1,_Z2},IoListData}->
	    io:format("Client received Binary: ~p~n",[{?MODULE,?LINE,IoListData}]),
	    ReplyBin=iolist_to_binary(IoListData),
	    Reply=binary_to_term(ReplyBin);	    
	Unmatched ->
	    Reply=Unmatched,
	    io:format("Unmatched Signal ~p~n",[{?MODULE,?LINE,Unmatched}])
    after TimeOut->
	    Reply={error,timeout}
    end,
    Reply.


%%%----------------------------------------------------------------------------
connect(Addr,Port)->
    Reply=connect(Addr,Port,?CLIENT_PACKET_FORMAT),
    Reply.
    

connect(Addr,Port,SetUp)->
    Reply=ssl:connect(Addr,Port,SetUp),
    Reply.


connect_timeout(Addr,Port,TimeOut)->
    Reply=connect_timeout(Addr,Port,?CLIENT_PACKET_FORMAT,TimeOut),
    Reply.

connect_timeout(Addr,Port,SetUp,TimeOut)->
    MyPid=self(),
    P=spawn(fun()->conn(Addr,Port,SetUp,MyPid) end),
    receive
	{P,{ok,SslSocket}}->
	    Reply={ok,SslSocket};
	{P,{error,Err}}->
	    Reply={error,Err};
	{P,Err}->
	    Reply={error,Err}
    after TimeOut->
	    Reply={error,timeout},
	    exit(P,kill)
    end,
    Reply.

conn(Addr,Port,SetUp,CallerPid)->
    case ssl:connect(Addr,Port,SetUp) of
	{ok,SslSocket}->
	    ok=ssl:controlling_process(SslSocket,CallerPid ),
	    CallerPid!{self(),{ok,SslSocket}};
	{error,Err}->
	    CallerPid!{self(),{error,Err}};
	X->
	    CallerPid!{self(),X}
    end.
    
disconnect(Socket,TimeOut)->
    ssl:close(Socket),
    timer:sleep(TimeOut),
    ok.

disconnect(Socket)->
    ssl:close(Socket),
    ok.
