%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(math_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/1]).


%%
%% API Function
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
start(Port)->
    PacketFormat=[binary,{packet,4},{reuseaddr, true}, {active, true}],
    CertFile= "ebin/certificate.pem",
    KeyFile = "ebin/key.pem",
    {ok,Pid}=ssl_server:start_seq_server(Port,PacketFormat,CertFile,KeyFile),
    wait_accept(Pid).

wait_accept(Pid)->
 %   io:format("~p~n",[{?MODULE,?LINE}]),
    receive
	{Pid,{ok,Socket}}->
	    ok
    end,
 %   io:format("~p~n",[{?MODULE,?LINE}]),
    loop(Pid,Socket).

loop(ServerPid,Socket)->
    receive
	{ServerPid,{Socket,msg,Data}}->
%	   io:format("~p~n",[{?MODULE,?LINE,Data}]),
	    {Cmd,Args}=Data,
	    case Cmd of
		add->
		    [A,B]=Args,
		    Reply=A+B;
		X->
		    Reply={error,{not_implemented,X}}
	    end,
	    ssl:send(Socket,term_to_binary(Reply)),
	    loop(ServerPid,Socket);
	{ServerPid,{Socket,ssl_closed}}->
	    end_session;
%	    io:format("~p~n",[{?MODULE,?LINE,ssl_closed}]);
	Unmatched ->
%	    io:format("~p~n",[{?MODULE,?LINE,Unmatched}]),
	    loop(ServerPid,Socket)
    end.
