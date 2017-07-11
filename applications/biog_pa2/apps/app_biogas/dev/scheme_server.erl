%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : genserver template with jle embedded extensions 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(scheme_server).

-behaviour(gen_server).

-define(VERSION,'2.0.0').


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("include/tellstick.hlr").
%% --------------------------------------------------------------------
%% External exports
-export([tick/0,start/0,stop/0,ver/0]).
%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pid_tick,filename,pos}).

%% ====================================================================
%% External functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).
ver()->  {?MODULE,?VERSION}.


tick()-> gen_server:cast(?MODULE, {tick}).

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
    Pid=spawn_link(fun()-> tick(?UPDATE_TIME) end),
    RecordList=dbase_tuple:all(?DBASE_FILE),
    Pos=lists:flatlength(RecordList),
    {ok, #state{pid_tick=Pid,filename=?SCHEMA_FILE,pos=Pos}}.

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
handle_call({stop}, _From, State) ->
    #state{pid_tick=Pid}=State,
    Pid!{self(),{stop}},
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
handle_cast({tick},#state{filename=File,pos=Pos}=State) ->
    NextPos=auto_update_devices(Pos),
    scheme(File),
    NewState=State#state{pos=NextPos},
    {noreply, NewState};
   
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
%% --------------------------------------------------------------------
%% Function: tick/1
%% Description:if needed creates dets file with name ?MODULE, and
%% initates the debase
%% Returns: non
%% --------------------------------------------------------------------
tick(Sec)->
    tick(Sec,false).

tick(_Sec,true)->
    ok;
tick(Sec,false)->
    receive
	{_Pid,{stop}}->   
	    Exit=true
    after Sec*1000 ->
	    scheme_server:tick(),
	    Exit=false
    end,
tick(Sec,Exit).

%% --------------------------------------------------------------------
%% Function: scheme/1
%% Description:if needed creates dets file with name ?MODULE, and
%% initates the debase
%% Returns: non
%% --------------------------------------------------------------------
scheme(FileName)->
    {ok,SchemeTerm}=file:consult(FileName),
    Reply=do_scheme(SchemeTerm),
    Reply.

%% --------------------------------------------------------------------
%% Function: do_scheme/1
%% Description:if needed creates dets file with name ?MODULE, and
%% initates the debase
%% Returns: non
%  [{lamp,[{on,daily,{17,0}},{off,daily,{23,0}}]},
%   {lamp_1,[{on,daily,{12,34}},{off,daily,{18,19}},{on,sat,{8,0}},{off,sat,{22,37}}]}]}
%   
%  date2day;convert()={Day,{H,M}}
%  
%
%% --------------------------------------------------------------------
do_scheme([])->
    ok;
do_scheme([H|T])->
    {Id,ActionList}=H,   
    Action=action(ActionList,no_action),
    case Action of
	no_action->
	    ok;
	on->
	    tellstick_server:cmd(Id,on),
	    do_on;
	off->
	    tellstick_server:cmd(Id,off),
	    do_off
    end,
    do_scheme(T).
action([],Acc)->
    Acc;
action([{State,Day,{H,M}}|T],Acc)->
    {C_Day,{C_H,C_M}}=date2day:convert(),
    case Day of
	daily->
	    case {C_H,C_M}== {H,M} of
		true->
		    Acc1=State,
		    T1=[];
		false->
		    Acc1=Acc,
		    T1=T
	    end;
	_->
	    case {C_Day,{C_H,C_M}}=={Day,{H,M}} of
		true->
		    Acc1=State,
		    T1=[];
		false ->
		    Acc1=Acc,
		    T1=T
	    end
    end,
    action(T1,Acc1).

%% --------------------------------------------------------------------
%% Function: auto_update_devices/0
%% Description:if Secure that devices are in right state
%% 
%% Returns: non
%% --------------------------------------------------------------------
auto_update_devices(0)->
    RecordList=dbase_tuple:all(?DBASE_FILE),
    Pos=lists:flatlength(RecordList),
    Pos;
auto_update_devices(Position)->
    RecordList=dbase_tuple:all(?DBASE_FILE),
    {Id,_}=lists:nth(Position, RecordList),
    tellstick_server:refresh(Id),
    timer:sleep(3000),
    Position-1.
