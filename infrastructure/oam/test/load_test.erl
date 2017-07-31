%% 
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
%% Copyright (C) 2002, Joe Armstrong
%% File    : lib_tcp_server.erl
%% Author  : Joe Armstrong (joe@sics.se)
%% Purpose : Keeps track of a number of TCP sessions
%% Time-stamp: <2012-10-12 08:33:25 joe>

-module(load_test).

-export([load/0]).

load()->
    case oam_test:test() of
	ok->
	   io:format("loaded sucessfull ~n");
	error->
	    io:format("failed ~n")
    end.
