-module(sd_service_if).

-export([connect/2,connect/3,connect_timeout/3,connect_timeout/4,send/2,rec/2,disconnect/1,disconnect/2]).

-export([start/2,start/4, stop/0,stop/1]).



% Service specific parts - To be changed for new services
-include("sd_service.hrl"). 
-export([get_file_list/2,get_file/3,get_all_nodes/2,
	 add_node/4,delete_node/4,
	 get_myip/3,get_config/3]).

get_config(BoardId,SslSocket,PacketNum)->
    {ok,CallBackModule}= application:get_env(?SERVICE, callBackModule),
    Msg={CallBackModule,get_config,[BoardId],PacketNum},
    send(SslSocket,Msg).

get_file_list(SslSocket,PacketNum)->
    {ok,CallBackModule}= application:get_env(?SERVICE, callBackModule),
    Msg={CallBackModule,get_file_list,[],PacketNum},
    send(SslSocket,Msg).

get_file(FileName,SslSocket,PacketNum)->
    {ok,CallBackModule}= application:get_env(?SERVICE, callBackModule),
    Msg={CallBackModule,get_file,[FileName],PacketNum},
    send(SslSocket,Msg).

get_myip(BoardId,SslSocket,PacketNum)->
    {ok,CallBackModule}= application:get_env(?SERVICE, callBackModule),
    Msg={CallBackModule,get_myip,[BoardId],PacketNum},
    send(SslSocket,Msg).

add_node(IP,Port,SslSocket,PacketNum)->
    {ok,CallBackModule}= application:get_env(?SERVICE, callBackModule),
    Msg={CallBackModule,add_node,[IP,Port],PacketNum},
    send(SslSocket,Msg).

delete_node(IP,Port,SslSocket,PacketNum)->
    {ok,CallBackModule}= application:get_env(?SERVICE, callBackModule),
    Msg={CallBackModule,delete_node,[IP,Port],PacketNum},
    send(SslSocket,Msg).

get_all_nodes(SslSocket,PacketNum)->
    {ok,CallBackModule}= application:get_env(?SERVICE, callBackModule),
    Msg={CallBackModule,get_all_nodes,[],PacketNum},
    send(SslSocket,Msg).


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
    ok=application:set_env(ServiceName,ebinPath,"repository"),
    ok=application:set_env(ServiceName,networkConfigFile,"ebin/network.config"),
    ok=application:load(ServiceName),
    ok=application:start(ServiceName),
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
