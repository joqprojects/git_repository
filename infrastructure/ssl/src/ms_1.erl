-module(ms_1).

-export([network_congestion_10s/0,node_dead/0,add/2,divi/2]).


node_dead()->
    receive
	infinity->
	    ok
    end.

network_congestion_10s()->
    timer:sleep(10000),
    reply_from_node.


add(A,B)->
    timer:sleep(500),
    A+B.

divi(A,B)->
  %  io:format("rev 1.0.0 ~n"),
    Reply= case B of
	0->
	   {error,badarith,B};
	_->
	    A/B
    end,
    Reply.
