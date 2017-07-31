%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% Template for parallell ssl server
%%% -------------------------------------------------------------------
-module(bm_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include
%% --------------------------------------------------------------------
%% External exports

-export([start_link/2,stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {lsock,callBackModule}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(LSock,CallBackModule) ->
    gen_server:start_link(?MODULE, [LSock,CallBackModule], []).
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
init([LSock,CallBackModule]) ->
    io:format("starting ~p~n",[{?MODULE,?LINE}]),
    {ok, #state{lsock = LSock,callBackModule=CallBackModule}, 0}.

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
    {M,F,A,PacketNum}=binary_to_term(BinData),
    case M of
	CallBackModule->
	    case rpc:call(node(),CallBackModule,F,A) of
		{badrpc,Err}->
		    {Reply,Result}={reply,{error,{badrpc,Err}}};
		{Reply,R}->
		    {Reply,Result}={Reply,R}
	    end;
	Err->
	    {Reply,Result}={reply,{error,{notImplemented,Err}}}
    end,
    case Reply of
	reply->
	    BinReply=term_to_binary({Result,PacketNum}),
	    ssl:send(Socket,BinReply);
	noreply ->
	    doNothing
    end,
    State.
