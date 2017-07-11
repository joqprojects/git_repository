%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : Test 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(db_dets).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([create/2,get/2,all/1,set/3,remove/2,delete/1]).
-export([start/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start()-> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

create(DetsFile,Type)->
    gen_server:call(?MODULE,{create,DetsFile,Type},infinity).

get(DetsFile,Key)->
    gen_server:call(?MODULE,{get,DetsFile,Key},infinity).

set(DetsFile,Key,Value)->
    gen_server:call(?MODULE,{set,DetsFile,Key,Value},infinity).

remove(DetsFile,Key)->
    gen_server:call(?MODULE,{remove,DetsFile,Key},infinity).

delete(DetsFile)->
    gen_server:call(?MODULE,{delete,DetsFile},infinity).

all(DetsFile)->
    gen_server:call(?MODULE,{all,DetsFile},infinity).


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

handle_call({create,DetsFile,Type}, _From, State) ->
    case filelib:is_file(DetsFile) of 
	true->
	    Reply = {error,create_file_already_exist};
	false->
	    {ok,Descriptor}=dets:open_file(DetsFile,[{type,Type}]),
	    dets:close(Descriptor),
	    Reply = {ok,created}
    end,
    {reply, Reply, State};

handle_call({get,DetsFile,Key}, _From, State)->
    {ok,Descriptor}=dets:open_file(DetsFile),
    case dets:lookup(Descriptor, Key) of
	[]->
	    Reply = [];
	X->
	    Reply=X
    end,
    dets:close(Descriptor),
    {reply, Reply, State};


handle_call({set,DetsFile,Key,Value}, _From, State) ->
    {ok,Descriptor}=dets:open_file(DetsFile),
    ok=dets:insert(Descriptor, {Key,Value}),
    dets:close(Descriptor),
    Reply={ok,set},
    {reply, Reply, State};

handle_call({remove,DetsFile,Key}, _From, State) ->
  {ok,Descriptor}=dets:open_file(DetsFile),
    case dets:lookup(Descriptor, Key) of
	[]->
	    Reply = {error,no_entry};
	_->
	    ok=dets:delete(Descriptor, Key),
	    Reply={ok,remove}
    end,
    dets:close(Descriptor),
    {reply, Reply, State};

handle_call({delete,DetsFile}, _From, State) ->
     ok=file:delete(DetsFile),
    Reply={ok,delete},
    {reply, Reply, State};

handle_call({all,DetsFile}, _From, State) ->
    {ok,Descriptor}=dets:open_file(DetsFile),
    Key=dets:first(Descriptor),
    Reply=get_all(Descriptor,Key,[]),
    dets:close(Descriptor),
    {reply, Reply, State};

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
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unknown_call,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
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
terminate(_Reason,_State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State,_Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Func:
%% Purpose:  
%% Returns: 
%% --------------------------------------------------------------------
get_all(_Desc,'$end_of_table',Acc)->
    Acc;
get_all(Desc,Key,Acc)->  
    Status=dets:lookup(Desc, Key),
    Acc1=lists:append(Status,Acc),
    Key1=dets:next(Desc,Key),
    get_all(Desc,Key1,Acc1).
