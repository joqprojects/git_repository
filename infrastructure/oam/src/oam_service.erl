%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%% 1. Master node that keep track of all information about all cluster nodes
%%% 2. When a nodes starts/restarts 
%%%    a. Get all needed information from Master node
%%%    b. Using service discovery to share what services it has (local) and 
%%%       and ask for needed services (target)
%%%    c. Starts all the nodes local services
%%% 
%%% Master Configuration files
%%%  cluster.config
%%%  Information about all boards services and addresses
%%%  {{board_id,"davidsDator"},{address,"80.216.90.144",10100},
%%%    {local_resources,      [mymath]},{target_resources,[dbase]}}.
%%%  {{board_id,"Asus"},{address,"80.216.90.144",10110},
%%%    {local_resources,[dbase]},{target_resources,[]}}.
%%%  {{board_id,"AsasHP"},{address,"80.216.90.144",10120},
%%%    {local_resources,[]},{target_resources,[mymath,dbase]}}.
%%%  board.config
%%%  Information about the specific board 
%%%  Ip address and port to Service master
%%%  IP address and Allocated ports for this board
%%%  List of specific servcies for the board, ex HW controller telldus or a dbase
%%%  [{IP,Port,Service1},{IP,Port,Service2}]
%%%  Port 
%%%  XXXX: Board_mgr port
%%%  XXXX+N: Service ports
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(oam_service).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("kernel/include/file.hrl").
%% --------------------------------------------------------------------
-define(POLL_TIME,2000).
%-define(SW_MGR_BYT,3009).  % skall bytas mot config
-record(state, {lsock,callBackModule,
	       nodesDict,networkConfigFile,ebinPath}).

%% External exports
-export([get_file_list/1,get_file/2,get_all_nodes/1,
	 add_node/3,delete_node/3,
	 get_myip/2,get_config/2]).


%% ====================================================================
%% External functions
%% ====================================================================

%{{board_id,"192.168.0.100"},{address,"localhost",10100},{local_resources,[mymath]},{target_resources,[dbase,telldus]}}.
get_config(BoardId,NetworkConfigFile)->
    {ok,ConfigInfo}=file:consult(NetworkConfigFile),
    case lists:keyfind({board_id,BoardId},1,ConfigInfo) of
	{{board_id,BoardId},{address,IP,Port},{local_resources,LocalList},{target_resources,TargetList}}->
	    Reply ={IP,Port,LocalList,TargetList};
	false->
	    Reply={error,{boardId_not_exists,BoardId}}
    end,
    Reply.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
get_myip(BoardId,NetworkConfig)->
    {{board_id,BoardId},{address,IP,Port},_,_}=lists:keyfind({board_id,BoardId},1,NetworkConfig),
    {IP,Port}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

get_file_list(Ebin)->
    {ok,Files}=file:list_dir(Ebin),
    FileList=create_file_list(Files,Ebin),
    FileList.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

get_file(FileName,Ebin)->
    FullFileName=filename:join(Ebin,FileName),
    case filelib:is_file(FullFileName) of
	true->
	    {ok,Bin}=file:read_file(FullFileName),
	    Reply={FileName,Bin};
	false->
	    Reply={error,{no_entry,FileName}}
    end,
    Reply.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

add_node(IP,Port,NodesDict)->
    case dict:find(nodes,NodesDict) of
	{ok,NodeList}->
	    NewList=[{IP,Port}|lists:delete({IP,Port},NodeList)],
	    NewNodeDict=dict:store(nodes,NewList,NodesDict);
	error ->
	    NewNodeDict=dict:store(nodes,[{IP,Port}],NodesDict)
    end,
    NewNodeDict.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

delete_node(IP,Port,NodesDict)->
    case dict:find(nodes,NodesDict) of
	{ok,NodeList}->
	    NewList=lists:delete({IP,Port},NodeList),
	    NewNodeDict=dict:store(nodes,NewList,NodesDict);
		error ->
	    NewNodeDict=NodesDict
    end,
    NewNodeDict.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

get_all_nodes(NodesDict)->
    case dict:find(nodes,NodesDict) of
	{ok,AllNodesList}->
	    Reply=AllNodesList;
	error ->
	    Reply=[]
    end,
    Reply.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
create_file_list([],_)->
    {error,empty_directory};
create_file_list(Files,Path) ->
    FileList=create_file_list(Files,Path,[]),
    FileList.

create_file_list([],_Path,Acc)->
    Acc;
create_file_list([BaseName|T],Path,Acc) ->
    FullName=filename:join(Path,BaseName),
    {ok,Facts}=file:read_file_info(FullName),
    Acc1=[{BaseName,Facts}|Acc],
    create_file_list(T,Path,Acc1).
