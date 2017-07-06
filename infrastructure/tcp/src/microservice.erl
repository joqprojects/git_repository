-module(microservice).

-export([add/2,divi/2]).

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
