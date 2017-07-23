-module(template_ssl_client).

-export([connect/3,send/2,rec/2,disconnect/1]).


%% Client part 
send(Socket,Msg)->
    ssl:send(Socket,term_to_binary(Msg)).

rec(Socket,TimeOut)->
    receive
	{ssl,{sslsocket,_Z1,_Z2},IoListData}->
	    io:format("Client received Binary: ~p~n",[{?MODULE,?LINE,IoListData}]),
	    ReplyBin=iolist_to_binary(IoListData),
	    Reply=binary_to_term(ReplyBin);	    
	{ssl,Socket, ReplyBin}->
	    Reply=binary_to_term(ReplyBin);
	{ssl_closed, Socket}->
	    Reply={ssl_closed, Socket};
	Unmatched ->
	    Reply=Unmatched,
	    io:format("Unmatched Signal ~p~n",[{?MODULE,?LINE,Unmatched}])
    after TimeOut->
	    Reply={error,timeout}
    end,
    Reply.


%%%----------------------------------------------------------------------------

connect(Addr,Port,SetUp)->
    case ssl:connect(Addr,Port,SetUp) of
       	{ok,Socket}->
	    Reply={ok,Socket};
	{error,ErrMsg} ->	
	 %   io:format("~p~n",[{?MODULE,?LINE,{error,ErrMsg}}]),
	    Reply={error,ErrMsg} 
    end,
    Reply.
    
disconnect(Socket)->
    ssl:close(Socket),
    ok.
