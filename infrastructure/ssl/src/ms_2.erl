-module(ms_2).

-export([sub/2,multi/2]).

sub(A,B)->
    A-B.

multi(A,B)->
  %  io:format("rev 1.0.0 ~n"),
   A*B.
