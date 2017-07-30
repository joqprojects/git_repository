%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(bm_service).

%% --------------------------------------------------------------------
%% Include files
-include("include/bm_service.hrl").
-include("include/service_register.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([call_service/1]).

call_service({ReqService,F,A,PacketIdService})->
    case ReqService of
	math_service->
	    {ok,S}=math_service_if:connect_timeout(?MATH_IP_ADDR,?MATH_PORT,?CLIENT_PACKET_FORMAT,?CONNECT_TIMEOUT),
	    Msg={ReqService,F,A,PacketIdService},
	    ok=ssl:send(S,term_to_binary(Msg)),
	    {Result,PacketIdService}=math_service_if:rec(S,?SYSTEM_TIMEOUT),    
	    math_service_if:disconnect(S,?SYSTEM_TIMEOUT),
	    Reply={reply,{Result,PacketIdService}};
	NotImplemented ->
	    Reply={reply,{{error,NotImplemented,PacketIdService}}}
    end,
    Reply.


%%
%% API Function
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% ------------------------------------------------------------------
