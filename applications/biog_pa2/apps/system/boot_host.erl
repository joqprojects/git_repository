%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : Test 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(boot_host).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0]).


%% ====================================================================
%% External functions
%% ====================================================================
start()->
    io:format(" load biogas ~p~n",[ application:load(biogas)]),
   io:format(" start biogas ~p~n",[ application:start(biogas)]).


%chromium-browser --app=http://example.com --start-fullscreen
 %   os:cmd("firefox http://localhost:2000/app_tellstick/biogas.html").
