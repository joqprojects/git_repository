%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : genserver template with jle embedded extensions 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(tellstick_controller).

-behaviour(gen_server).

-define(VERSION,'1.0.0').

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("include/tellstick.hlr").
%% --------------------------------------------------------------------
%% External exports
-export([refresh/1,cmd/2,start/0,stop/0,ver/0]).
%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).
ver()->  {?MODULE,?VERSION}.

cmd(Id,Action)->
    gen_server:call(?MODULE,{cmd,Id,Action},infinity).

refresh(Id)->
    gen_server:call(?MODULE,{refresh,Id},infinity).

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
    io:format("Starting ~p~n",[{?MODULE,?VERSION}]),
    {ok, #state{}}.

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
handle_call({restart}, _From, State) ->
    Reply={ok,telldus_restarted},
    {reply, Reply, State};

handle_call({cmd,Id,Action}, _From, State) ->
    case dbase_tuple:get(Id,type,?DBASE_FILE) of
	{type,tellstick_switch}->
	    case dbase_tuple:get(Id,Action,?DBASE_FILE) of
		{Action,[{tdtool,Tdtool_cmd},_,_]}->
		    io:format("~p~n",[{?MODULE,?LINE,Action,Tdtool_cmd}]),
		    {Action,[{tdtool,Cmd},_,_]}=dbase_tuple:get(Id,Action,?DBASE_FILE),
		    dbase_tuple:store(Id,current_status,Action,?DBASE_FILE),
		    send({tellstick_switch,[Cmd]}),
		    NextState=in_session;	
		X->
		    io:format("unmatched signal ~p~n",[{?MODULE,?LINE,X}]),
		    NextState=in_session
	    end;
	{type,switch}->
	    dbase_tuple:store(Id,current_status,Action,?DBASE_FILE),
	    NextState=in_session;
	X->
	    io:format("unmatched signal ~p~n",[{?MODULE,?LINE,X}]),
	    NextState=in_session
    end,
    {reply,NextState,  State};

handle_call({refresh,Id}, _From, State) ->
    case dbase_tuple:get(Id,type,?DBASE_FILE) of
	{type,tellstick_switch}->
	    io:format("refresh ~p~n",[{?MODULE,?LINE,Id}]),
	    {current_status,Status}=dbase_tuple:get(Id,current_status,?DBASE_FILE),
	    {Status,[{tdtool,Tdtool_cmd},_,_]}=dbase_tuple:get(Id,Status,?DBASE_FILE),
	    send({tellstick_switch,[Tdtool_cmd]}),
	    NextState=in_session;	
	X->
	    io:format("unmatched signal ~p~n",[{?MODULE,?LINE,Id,X}]),
	    NextState=in_session
    end,
    {reply,NextState,  State};

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
handle_info(_Info, State) ->
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
send(MsgTerm)->
    Reply=tcp_client:send(MsgTerm,?ADDRESS_TELLSTICK,?PORT_TELLSTICK,?CLIENTSETUP_TELLSTICK,true),
    io:format(" ~p~n",[{?MODULE,?LINE,Reply}]).
