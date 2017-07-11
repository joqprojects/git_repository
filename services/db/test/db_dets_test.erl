%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 2017-07-11
%%% Revsion : 1.0.0 :  Created
%%% Description :
%%% EUnit test for db_dets module.Simple key value db based on Erlang dets module
%%% -------------------------------------------------------------------
-module(db_dets_test).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).
%%
%% API Functions
%%


%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------

init_test()->
    {ok,_}=db_dets:start(),
%    {error,{already_started,_Pid}}=db_dets:start(),
    ok.
create_test()->
    {ok,created}=db_dets:create(file_set,set),
    {ok,created}=db_dets:create(file_bag,bag),
    {ok,created}=db_dets:create(file_duplicatedBag,duplicate_bag),
    {error,create_file_already_exist}=db_dets:create(file_set,set),
    ok.

set_test()->
    []=db_dets:all(file_set),

    {ok,set}=db_dets:set(file_set,joakim,{62,male,erlang}),
    {ok,set}=db_dets:set(file_set,david,{95,male,javasrcipt}),
    {ok,set}=db_dets:set(file_set,erika,{98,female,c}),
    
    [{joakim,{62,male,erlang}}]=db_dets:get(file_set,joakim),
    [{david,{95,male,javasrcipt}}]=db_dets:get(file_set,david),
    [{erika,{98,female,c}}]=db_dets:get(file_set,erika),
    []=db_dets:get(file_set,no_entry),

    [{joakim,{62,male,erlang}},
     {david,{95,male,javasrcipt}},
     {erika,{98,female,c}}]=db_dets:all(file_set),

    ok.
    
bag_test()->
    []=db_dets:all(file_bag),

    {ok,set}=db_dets:set(file_bag,joakim,{highSchool,thorildsplan}),
    [{joakim,{highSchool,thorildsplan}}]=db_dets:get(file_bag,joakim),
    
    {ok,set}=db_dets:set(file_bag,joakim,{highSchool,thorildsplan}),
    [{joakim,{highSchool,thorildsplan}}]=db_dets:get(file_bag,joakim),   

    {ok,set}=db_dets:set(file_bag,joakim,{university,chalmers}),
    [{joakim,{highSchool,thorildsplan}},
     {joakim,{university,chalmers}}]=db_dets:get(file_bag,joakim),

    {ok,set}=db_dets:set(file_bag,david,{95,male,javasrcipt}),
    {ok,set}=db_dets:set(file_bag,erika,{98,female,c}),

    [{joakim,{highSchool,thorildsplan}},
     {joakim,{university,chalmers}},
     {david,{95,male,javasrcipt}},
     {erika,{98,female,c}}]=db_dets:all(file_bag),
    ok.
    
duplicatedbag_test()->
    []=db_dets:all(file_duplicatedBag),
    
    {ok,set}=db_dets:set(file_duplicatedBag,joakim,{highSchool,thorildsplan}),
    [{joakim,{highSchool,thorildsplan}}]=db_dets:get(file_duplicatedBag,joakim),
    
    {ok,set}=db_dets:set(file_duplicatedBag,joakim,{highSchool,thorildsplan}),
    [{joakim,{highSchool,thorildsplan}},
     {joakim,{highSchool,thorildsplan}}]=db_dets:get(file_duplicatedBag,joakim),

    {ok,set}=db_dets:set(file_duplicatedBag,joakim,{university,chalmers}),
    [{joakim,{highSchool,thorildsplan}},
     {joakim,{highSchool,thorildsplan}},
     {joakim,{university,chalmers}}]=db_dets:get(file_duplicatedBag,joakim),

    {ok,set}=db_dets:set(file_duplicatedBag,david,{95,male,javasrcipt}),
    {ok,set}=db_dets:set(file_duplicatedBag,erika,{98,female,c}),

    [{joakim,{highSchool,thorildsplan}},
     {joakim,{highSchool,thorildsplan}},
     {joakim,{university,chalmers}},
     {david,{95,male,javasrcipt}},
     {erika,{98,female,c}}]=db_dets:all(file_duplicatedBag),
    ok.



remove_test()->
    {ok,remove}=db_dets:remove(file_set,joakim),
    [{david,{95,male,javasrcipt}},
     {erika,{98,female,c}}]=db_dets:all(file_set),
    {error,no_entry}=db_dets:remove(file_set,joakim),

    {ok,remove}=db_dets:remove(file_bag,joakim),
    [{david,{95,male,javasrcipt}},
     {erika,{98,female,c}}]=db_dets:all(file_bag),   

    {ok,remove}=db_dets:remove(file_duplicatedBag,joakim),
    [{david,{95,male,javasrcipt}},
     {erika,{98,female,c}}]=db_dets:all(file_duplicatedBag),   
    
    ok.

cleanUp_test()->
    db_dets:delete(file_set),
    db_dets:delete(file_bag),
    db_dets:delete(file_duplicatedBag),
    shutdown_ok=db_dets:stop(),
    ok. 
%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
