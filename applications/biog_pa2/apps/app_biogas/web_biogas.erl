%% Author: uabjle
%% Created: 2016-01-21
%% Description:
%% Implements the web interface for the distributed remote controller
%% 
%%
%%
-module(web_biogas).

%%
%% Include files
%%
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("").
%%

%% Exported Functions
%%
-export([start/1]).

%%
%% API Functions
%%

%% --------------------------------------------------------------------
%% Function: start
%% Description: The html and joes ezweb calls the start to initiate a new session
%% Returns: NoN
%% --------------------------------------------------------------------
start(Browser) ->
    io:format("~p~n",[{?MODULE,?LINE,Browser}]),    
    loop(Browser,1),
    ok.

loop(Browser,N)->
    B=integer_to_list(1+N)++" st",
    G=float_to_list(N*0.0092, [{decimals, 3}, compact])++" Nm3",
    S=float_to_list(N*0.096, [{decimals, 1}, compact])++" km",
  %  io:format("~p~n",[{?MODULE,?LINE,in_session}]),
    receive 
	X->
	    ok
    after 3000  ->
	    io:format("~p~n",[{?MODULE,?LINE,B,G,S}]),  
	    Browser ! [{cmd,fill_div}, {id,banan}, {txt,list_to_binary(B )}],
	    Browser ! [{cmd,fill_div}, {id,tanka}, {txt,list_to_binary(G )}],
	    Browser ! [{cmd,fill_div}, {id,dist}, {txt,list_to_binary(S)}]
    end,
    loop(Browser,N+1).

%% --------------------------------------------------------------------
%% Function: login(Browser)
%% Description: login session
%% Returns: message to send to logic
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:clicked
%% Description: creates a message based on a button action
%% Returns: message to send to logic
%% --------------------------------------------------------------------

