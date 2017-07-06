%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% dets_server creates an abstraction of the dets functionality. 
%%% The implmentation is state less and the module takes care of the 
%%% dets file.
%%% 
%%
%%% Created : 10 dec 2012
%%% -------------- -----------------------------------------------------
-module(misc_dets).


-define(VERSION,'0.0.1').
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

% -include("").
%% --------------------------------------------------------------------
%% External exports
-export([create/2,get/2,all/1,set/3,remove/2,delete/1]).


%% ====================================================================
%% External functions
%% ====================================================================

create(DetsFile,Type)->
    case filelib:is_file(DetsFile) of 
	true->
	    Reply = {error,create_file_already_exist};
	false->
	    {ok,Descriptor}=dets:open_file(DetsFile,[{type,Type}]),
	    dets:close(Descriptor),
	    Reply = {ok,created}
    end,
    Reply.

get(DetsFile,Key)->
    {ok,Descriptor}=dets:open_file(DetsFile),
    case dets:lookup(Descriptor, Key) of
	[]->
	    Reply = {error,no_entry};
	X->
	    [{Key,Value}]=X,
	    Reply={ok,Value}
    end,
    dets:close(Descriptor),
    Reply.

set(DetsFile,Key,Value)->
    {ok,Descriptor}=dets:open_file(DetsFile),
    ok=dets:insert(Descriptor, {Key,Value}),
    dets:close(Descriptor),
    Reply={ok,set},
    Reply.

all(DetsFile)->
    {ok,Descriptor}=dets:open_file(DetsFile),
    Key=dets:first(Descriptor),
    Reply=get_all(Descriptor,Key,[]),
    dets:close(Descriptor),
    Reply.


remove(DetsFile,Key)->
    {ok,Descriptor}=dets:open_file(DetsFile),
    case dets:lookup(Descriptor, Key) of
	[]->
	    Reply = {error,no_entry};
	_->
	    ok=dets:delete(Descriptor, Key),
	    Reply={ok,remove}
    end,
    dets:close(Descriptor),
    Reply.

delete(DetsFile)->
    ok=file:delete(DetsFile),
    Reply={ok,delete},
    Reply.


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/0
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Func: dets_exists(Name,State)
%% Purpose: check if detsfile Name exists in the current dir 
%% Returns: true|false
%% --------------------------------------------------------------------
get_all(_Desc,'$end_of_table',Acc)->
    {ok,Acc};
get_all(Desc,Key,Acc)->  
    [Status]=dets:lookup(Desc, Key),
    Acc1=[Status|Acc],
    Key1=dets:next(Desc,Key),
    get_all(Desc,Key1,Acc1).
