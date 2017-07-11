-module(ssl_server_interface).

-export([start/6,stop/0]).
-export([connect/3,connect/4,check_msg_queue/2,call/2,call/4,cast/2,disconnect/1,call_non_blocking/3,poll/1]).


start(Port,PacketFormat,CertFile,Keyfile,CallbackModule,NumInstances) ->
    application:load(?MODULE),
    application:start(?MODULE),
    ssl_server:start_server(Port,PacketFormat,CertFile,Keyfile,CallbackModule,NumInstances).

stop()->
    application:stop(?MODULE),    
    application:unload(?MODULE).

%% Client part 
session(CallerId,Addr,Port,SetUp)->
    case ssl:connect(Addr,Port,SetUp) of
       	{ok,Socket}->
	    CallerId!{self(),ok},
	    session_loop(CallerId,Socket);
	{error,ErrMsg} ->	
	 %   io:format("~p~n",[{?MODULE,?LINE,{error,ErrMsg}}]),
	    CallerId!{self(),{error,ErrMsg}}
    end.

session_loop(CallerId,Socket)->
    receive
	{ssl,{sslsocket,_Z1,_Z2},IoListData}->
	    io:format("Client received Binary: ~p~n",[{?MODULE,?LINE,IoListData}]),
	    ReplyBin=iolist_to_binary(IoListData),
%	    io:format("W1: ~p~n",[{?MODULE,?LINE,W1}]),
%	    W2=binary_to_term(W1),
	    CallerId!{self(),{server_reply,binary_to_term(ReplyBin)}},
	    session_loop(CallerId,Socket);
	    
	{ssl,Socket, ReplyBin}->
	    CallerId!{self(),{server_reply,binary_to_term(ReplyBin)}},
	    session_loop(CallerId,Socket);
	{CallerId,{connect_req,[]}}->
	    MsgBin=term_to_binary({connect_req,[]}),
	    ok=ssl:send(Socket,MsgBin),
	    session_loop(CallerId,Socket);
	{CallerId,{call,[M,F,A,PacketNum]}}->
	    MsgBin=term_to_binary({call,[M,F,A,PacketNum]}),
	    ok=ssl:send(Socket,MsgBin),
	    session_loop(CallerId,Socket);
	{CallerId,{call,[M,F,A]}}->
	    MsgBin=term_to_binary({call,[M,F,A]}),
	    ok=ssl:send(Socket,MsgBin),
	    session_loop(CallerId,Socket);
       {CallerId,{call_non_blocking,[M,F,A,SessionId,Num]}}->
	    MsgBin=term_to_binary({call_non_blocking,[M,F,A,SessionId,Num]}),
	    ok=ssl:send(Socket,MsgBin),
	    session_loop(CallerId,Socket);
	{CallerId,{cast,{M,F,A}}}->
	    MsgBin=term_to_binary({cast,{M,F,A}}),
	    ok=ssl:send(Socket,MsgBin),
	    session_loop(CallerId,Socket);
	{CallerId,end_session}->
	    ok=ssl:close(Socket);
	{ssl_closed, Socket}->
	    SessionId=self(),
	    CallerId!{self(),{session_ended,SessionId}};
	Unmatched ->
	    ok=Unmatched,
	    io:format("Unmatched Signal ~p~n",[{?MODULE,?LINE,Unmatched}]),
	    session_loop(CallerId,Socket)
    end.


%%%----------------------------------------------------------------------------

connect(Addr,Port,SetUp,TimeOut)->
    CallerId=self(),
    SessionId=spawn(fun()->session(CallerId,Addr,Port,SetUp) end),
    receive
	{SessionId,R}->
	    SessionId!{self(),{connect_req,[]}},
	    receive
		{SessionId,{server_reply,connect_ack}}->
		    Reply={ok,SessionId};
		Unmatched ->
		    Reply={error,{unmatched_Signal,{?MODULE,?LINE,Unmatched}}},
		    io:format("Unmatched Signal ~p~n",[{?MODULE,?LINE,Unmatched}])
	    end
	after TimeOut->
		Reply={error,timeout,SessionId}
	end,
    Reply.

%    SessionId!{self(),{connect_req,[]}},
 %   receive
%	{SessionId,{server_reply,connect_ack}}->
%	    Reply={ok,SessionId};
%	Unmatched1 ->
%	    Reply={error,unmatched,Unmatched1}
 %   after TimeOut->
%	    Reply={error,timeout,SessionId}
 %   end,
 %   Reply.
    

connect(Addr,Port,SetUp)->
    CallerId=self(),
    SessionId=spawn(fun()->session(CallerId,Addr,Port,SetUp) end),

    receive
	{SessionId,R}->
	    {R,SessionId};
	Unmatched ->
	    io:format("Unmatched Signal ~p~n",[{?MODULE,?LINE,Unmatched}]),
	    SessionId={error,unmatched,Unmatched}
    end,
  
   SessionId!{self(),{connect_req,[]}},
   receive
        {SessionId,{server_reply,connect_ack}}->
	    Reply={ok,SessionId};
	 {error,Err} ->
	   Reply={error,Err};
       X ->
	   Reply=X
    end,
    Reply.  


disconnect(SessionId)->
    SessionId!{self(),end_session},
    ok.


check_msg_queue(SessionId,TimeOut)->
     receive
	{SessionId,{server_reply,Reply}}->
	    ok
    after TimeOut->
	    Reply={no_message}
    end,
    Reply.


call(SessionId,{M,F,A},TimeOut,PacketNum)->
    SessionId!{self(),{call,[M,F,A,PacketNum]}},
    receive
	{SessionId,{server_reply,Reply}}->
	    ok
    after TimeOut->
	    Reply={error,timeout,SessionId}
    end,
    Reply.

call(SessionId,{M,F,A})->
    SessionId!{self(),{call,[M,F,A]}},
    receive
	{SessionId,{server_reply,Reply}}->
	    ok;
	X ->
	    io:format("unmatched ~p~n",[{?MODULE,?LINE,X}]),
	    Reply=X
    end,
    Reply.

call_non_blocking(SessionId,{M,F,A},Num)->
    SessionId!{self(),{call_non_blocking,[M,F,A,SessionId,Num]}}.

poll(SessionId)->
    receive
	{SessionId,{server_reply,Reply}}->
	    ok;
	X->
	    io:format("unmatched ~p~n",[{?MODULE,?LINE,X}]),
	    Reply=X
    end,
    Reply.
    
cast(SessionId,{M,F,A})->
    SessionId!{self(),{cast,[M,F,A]}}.



mycrash()->
    S=self(),
    Pid=spawn(fun()->mycrash(S) end),
    receive
	{Pid,R}->
	    Reply=R
    after 100->
	    Reply=timeout
    end,
    Reply.
mycrash(Pid)->
    Pid!{self(),crash}.
