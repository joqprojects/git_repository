%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% This server manage several tcp servers
%%% Creates pools of servers and keep the number as
%%% Each server has following data
%%%
%%% Generic data = {ListenPort,LSock,NumberOfInstances,ServerSetUp,CallBackModule}
%%% PoolInfo = [Generic data]
%%% pool = [{ListenPort_1,[Pid_1,Pid_2},{ListenPort_2,[Pid_1,Pid_2}]
%%% The pool of servers is a list of server types
%%% Information is stored in a database
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(ssl_server).

-behaviour(gen_server).



%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([start/0,stop/0]).
-export([start_server/6]).
-export([get_pids_info/0,get_pool_info/0,divi/2]).


%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pidsport,portinfo}).

%% ====================================================================
%% External functions
%% ====================================================================
start() ->
    gen_server:start_link({local,?MODULE},?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


start_server(Port,PacketFormat,CertFile,Keyfile,CallbackModule,NumInstances)->
    gen_server:call(?MODULE, {start_server,Port,PacketFormat,CertFile,Keyfile,CallbackModule,NumInstances},infinity).
    
get_pids_info()->
    gen_server:call(?MODULE, {get_pids_info},infinity).
get_pool_info()->
    gen_server:call(?MODULE, {get_pool_info},infinity).

divi(A,B)->
    gen_server:call(?MODULE, {divi,A,B},infinity).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->

    %should be
    DbasePidsPort=list_to_atom(atom_to_list(?MODULE)++"_"++"pidsport"),
    case misc_dets:create(DbasePidsPort,set) of
	{error,create_file_already_exist}->
	    % kill all processes 
	    % delete file and recreate a new
	    ok;
	{ok,created}->
	    doNothing
		
    end,

    DbasePortInfo=list_to_atom(atom_to_list(?MODULE)++"_"++"portinfo"),
    case misc_dets:create(DbasePortInfo,set) of
	{error,create_file_already_exist}->
	    create_servers(DbasePortInfo);
	{ok,created}->
	    doNothing
		
    end,
    io:format(" ~p~n",[{?MODULE,?LINE}]),
    {ok, #state{pidsport=DbasePidsPort,portinfo=DbasePortInfo}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------
handle_call({start_server,Port,PacketFormat,CertFile,Keyfile,CallbackModule,NumInstances}, _From,#state{pidsport=DbasePidsPort,portinfo=DbasePortInfo}=State) ->

%% PacketFormat=[binary,{packet,4},{reuseaddr, true}, {active, true}],
%% CertFile= "/home/joq/erlang/code_examples/ssl/ebin/certificate.pem"
%% KeyFile ="/home/joq/erlang/code_examples/ssl/ebin/key.pem"

    SetUp=lists:append(PacketFormat,[{certfile,CertFile},{keyfile,Keyfile}]),
    {ok, LSock} = ssl:listen(Port, SetUp), 
  %  {ok,LSock}=gen_tcp:listen(Port,SetUp),
    _PortPool=start_servers(DbasePidsPort,Port,LSock,CallbackModule,NumInstances),
  %  {ok,set}=misc_dets:set(?MODULE,Port,PortPool),
    case misc_dets:get(DbasePortInfo,portInfo) of
	{error,no_entry}->
	    {ok,set}=misc_dets:set(DbasePortInfo,portInfo,[{Port,{LSock,SetUp,CallbackModule,NumInstances}}]);
	 {ok,PortInfo}->
	    case lists:keyfind(Port,1,PortInfo) of
		{Port,_ServerInfo}->
		    NewPortInfo=lists:keyreplace(Port,1,PortInfo,{Port,{LSock,SetUp,CallbackModule,NumInstances}});
		false ->
		    NewPortInfo=[{Port,{LSock,SetUp,CallbackModule,NumInstances}}|PortInfo]
	    end,
	    {ok,set}=misc_dets:set(DbasePortInfo,portInfo,NewPortInfo)
    end,
    Reply=ok,
    {reply, Reply, State};

handle_call({get_pids_info}, _From, #state{pidsport=DbasePidsPort}=State) ->
    Reply=misc_dets:all(DbasePidsPort),
    {reply, Reply, State};

handle_call({get_pool_info}, _From,  #state{portinfo=DbasePortInfo}=State) ->
    Reply=misc_dets:get(DbasePortInfo,portInfo),
    {reply, Reply, State};

handle_call({divi,A,B}, _From, State) ->
    Reply = A/B,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,time()}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'DOWN',Ref,process,Pid,normal}, #state{pidsport=DbasePidsPort}=State) ->
    {ok,{Port,LSock,CallbackModule,Ref}}=misc_dets:get(DbasePidsPort,Pid),
    {ok,remove}=misc_dets:remove(DbasePidsPort,Pid),

    {NewPid,NewRef}=spawn_monitor(fun()->connect(LSock,CallbackModule) end),
    {ok,set}=misc_dets:set(DbasePidsPort,NewPid,{Port,LSock,CallbackModule,NewRef}),
    {noreply, State};

handle_info({'DOWN',_Ref,process,_Pid,Reason}, State) ->
    io:format(" ~p~n",[{?MODULE,?LINE,{'DOWN',_Ref,process,_Pid,Reason}}]),
  %  {_NewPid,_NewRef}=spawn_monitor(fun()->par_connect(LSock,CallbackModule) end),
    {noreply, State};

handle_info(Info, State) ->
    io:format("unmatched signal ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: create_servers(DetsFileName)
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
create_servers(_DetsFile)->
    ok.
    



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

start_servers(DbasePidsPort,Port,LSock,CallbackModule,NumInstances)->
    PortPool=start_servers(DbasePidsPort,Port,LSock,CallbackModule,NumInstances,[]),
    PortPool.
    
start_servers(_DbasePidsPort,_Port,_LSock,_CallbackModule,0,Acc)->
    Acc;
start_servers(DbasePidsPort,Port,LSock,CallbackModule,NumInstances,Acc)->
    {Pid,Ref}=spawn_monitor(fun()->connect(LSock,CallbackModule) end),
    {ok,set}=misc_dets:set(DbasePidsPort,Pid,{Port,LSock,CallbackModule,Ref}),
    %Acc1=[{Pid,Port}|Acc],
    start_servers(DbasePidsPort,Port,LSock,CallbackModule,NumInstances-1,Acc).

connect(LSock,CallbackModule)->

    case ssl:transport_accept(LSock) of
	{ok,Socket}->
	    ok= ssl:ssl_accept(Socket),
	    server_loop(Socket,CallbackModule);
	{error,closed}->
	    terminate
    end.

server_loop(Socket,CallbackModule)->
  %  io:format("~p~n",[{?MODULE,?LINE,self(),Socket}]),
%    ssl:setopts(Socket, [{active, once}]),
    receive
	{ssl,Socket, RawData}->
%	    io:format("~p~n",[{?MODULE,?LINE,Socket}]),
	    handle_data(Socket,RawData,CallbackModule),
	    server_loop(Socket,CallbackModule);
	{ssl_closed, Socket}->
	    ok
	   % io:format("closed ~p~n",[{?MODULE,?LINE,Socket}])
	    
    end.

handle_data(Socket,Bin,_CallbackModule)->
 %   io:format("~p~n",[{?MODULE,?LINE,CallbackModule}]),
    {Type,Args}=binary_to_term(Bin),
    case {Type,Args} of
	{connect_req,[]}->
	    ssl:send(Socket,term_to_binary(connect_ack));
	{call_non_blocking,[CallbackModule,F,A,SessionId,PacketNum]}->
	    case rpc:call(node(),erlang,apply,[CallbackModule,F,A]) of
		{badrpc,Reason}->
		    Reply={badrpc,Reason,SessionId,PacketNum};
		R->
		    Reply={call_non_blocking_ack,R,SessionId,PacketNum}
	    end,
            ssl:send(Socket, term_to_binary(Reply));	    
	{call,[CallbackModule,F,A,PacketNum]}->
	    case rpc:call(node(),erlang,apply,[CallbackModule,F,A]) of
		{badrpc,Reason}->
		    Reply={badrpc,Reason,PacketNum};
		R->
		    Reply={call_ack,R,PacketNum}
	    end,
	    ssl:send(Socket, term_to_binary(Reply));
	{call,[CallbackModule,F,A]}->
	    Reply=rpc:call(node(),erlang,apply,[CallbackModule,F,A]),
	    ssl:send(Socket, term_to_binary({call_ack,Reply}));
	{cast,[CallbackModule,F,A]}->
	    rpc:cast(node(),erlang,apply,[CallbackModule,F,A]);
	X ->
	    ssl:send(Socket,term_to_binary({call_ack,{error,unmatchedSignal,X}}))
    end.
