%%% -------------------------------------------------------------------
%%% Author  : uabjle
%% Description : 
%%% Template for parallell ssl server
%%% -------------------------------------------------------------------
-module(oam_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include
%% --------------------------------------------------------------------
%% External exports

-export([start_link/4,stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {lsock,callBackModule,
	       nodesDict,networkConfigFile,ebinPath}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(LSock,CallBackModule,EbinPath,NetworkConfigFile) ->
    gen_server:start_link(?MODULE, [LSock,CallBackModule,EbinPath,NetworkConfigFile], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



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
init([LSock,CallBackModule,EbinPath,NetworkConfigFile]) ->
    io:format("Starting ~p~n",[{?MODULE,?LINE}]),
    NodesDict=dict:new(),
    {ok, #state{lsock = LSock,callBackModule=CallBackModule,
	       nodesDict=NodesDict,networkConfigFile=NetworkConfigFile,ebinPath=EbinPath}, 0}.

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
handle_info({ssl,Socket, BinData}, State) ->
    NewState = handle_data(Socket, BinData, State),
    {noreply, NewState};

handle_info({ssl_closed, _Socket}, #state{lsock = LSock} = State) ->
    {ok,SslSocket}=ssl:transport_accept(LSock),
    case ssl:ssl_accept(SslSocket) of
	ok->
	    Next={noreply, State};
	{error,closed}->
	    Next={stop, normal, State}
    end,
    Next;

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok,SslSocket}= ssl:transport_accept(LSock),
    case ssl:ssl_accept(SslSocket) of
	ok->
	    Next={noreply, State};
	{error,closed}->
	    Next={stop, normal, State}
    end,
    Next;

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

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
handle_data(Socket,BinData,State)->
    CallBackModule=State#state.callBackModule,
    {_M,F,A,PacketNum}=binary_to_term(BinData),
    case F of
	get_file->
	    [FileName]=A,
	    EbinPath=State#state.ebinPath,
	    case rpc:call(node(),CallBackModule,F,[FileName,EbinPath]) of
		{error,Err}->
		    NewState=State,
		    {Reply,Result}={reply,{error,{Err}}};
		{badrpc,Err}->
		    NewState=State,
		    {Reply,Result}={reply,{error,{badrpc,Err}}};
		{FileName,File}->
		    NewState=State,
		    {Reply,Result}={reply,{ok,{FileName,File}}}
	    end;
	get_file_list->
	    EbinPath=State#state.ebinPath,
	    case rpc:call(node(),CallBackModule,F,[EbinPath]) of
		{badrpc,Err}->
		    NewState=State,
		    {Reply,Result}={reply,{error,{badrpc,Err}}};
		FileList->
		    NewState=State,
		    {Reply,Result}={reply,{ok,FileList}}
	    end;	    
	get_config->
	    [BoardId]=A,
	    NetworkConfigFile=State#state.networkConfigFile,
	    case rpc:call(node(),CallBackModule,F,[BoardId,NetworkConfigFile]) of
		{error,Err}->
		    NewState=State,
		    {Reply,Result}={reply,{error,{Err}}};
		{badrpc,Err}->
		    NewState=State,
		    {Reply,Result}={reply,{error,{badrpc,Err}}};
		Configuration->
		    NewState=State,
		    {Reply,Result}={reply,{ok,Configuration}}
	    end;
	get_all_nodes->
	    NodesDict=State#state.nodesDict,
	    case rpc:call(node(),CallBackModule,F,[NodesDict]) of
		{badrpc,Err}->
		    NewState=State,
		    {Reply,Result}={reply,{error,{badrpc,Err}}};
		AllNodesList->
		    NewState=State,
		    {Reply,Result}={reply,{ok,AllNodesList}}
	    end;
	add_node->
	    [IpAddr,Port]=A,
	    NodesDict=State#state.nodesDict,
	    case rpc:call(node(),CallBackModule,F,[IpAddr,Port,NodesDict]) of
		{badrpc,Err}->
		    NewState=State,
		    {Reply,Result}={reply,{error,{badrpc,Err}}};
		NewDict->
		    NewState=State#state{nodesDict=NewDict},
		    {Reply,Result}={noreply,noreply}
	    end;
	delete_node->
	    [IpAddr,Port]=A,
	    NodesDict=State#state.nodesDict,
	    case rpc:call(node(),CallBackModule,F,[IpAddr,Port,NodesDict]) of
		{badrpc,Err}->
		    NewState=State,
		    {Reply,Result}={reply,{error,{badrpc,Err}}};
		NewDict->
		    NewState=State#state{nodesDict=NewDict},
		    {Reply,Result}={noreply,noreply}
	    end;
	    
	Err->
	    NewState=State,
	    {Reply,Result}={reply,{error,{notImplemented,Err}}}
    end,

%%
    case Reply of
	reply->
	    BinReply=term_to_binary({Result,PacketNum}),
	    ssl:send(Socket,BinReply);
	noreply ->
	    doNothing
    end,
    NewState.
