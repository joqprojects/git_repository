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
-module(sd_service).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("kernel/include/file.hrl").
%% --------------------------------------------------------------------
-define(POLL_TIME,2000).
%-define(SW_MGR_BYT,3009).  % skall bytas mot config
-record(state, {lsock,callBackModule,
		target_resource_types,  % I want part
	        local_resource_tuples,  % I have part
	        found_resource_tuples  % Local cache of found resources
	       }).

%% External exports
%-export([add_target_resource_type/1,
%	 add_local_resource/2,
%	 fetch_resources/1,
%	 trade_resources/4,
%	 trade_resources/1,
%	 get_local/0,
%	 get_target/0,
%	 get_found/0
%	]).


%% ====================================================================
%% External functions
%% ====================================================================



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------



%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
