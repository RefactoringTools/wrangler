-module(dets_eqc).

-behaviour(general).

-export([table_name/0, module_name/0, particular_next_state_data/5,
	 particular_refactoring/1, delete_postcondition/0,
	 insert_postcondition/0, set_up/0, parallel_set_up/0,
	 get_contents/1, init_state/1, opened/1, open_file/2]).

-ifndef(BUG_OPEN_FILE).
-define(BUG_OPEN_FILE,false).
-endif.

-record(state,{name,type,contents=[]}).

table_name() -> dets_table.

module_name() -> dets.

%% Definition of the states. Each state is represented by a function, 
%% listing the transitions from that state, together with generators 
%% for the calls to make each transition.
init_state(_S) ->
    [{opened,{call,?MODULE,open_file,[table_name(),[{type,eqc_gen:oneof([set,bag])}]]}}
     ].

opened(S) ->
    [ {init_state,{call,dets,close,[dets_table]}},
      {opened,{call,?MODULE,open_file,[dets_table,[{type,S#state.type}]]}},
     {opened,{call,dets,insert,[dets_table,eqc_gen:oneof([general:object(),eqc_gen:list(general:object())])]}},
     {opened,{call,dets,insert_new,[dets_table,eqc_gen:oneof([general:object(),eqc_gen:list(general:object())])]}},
     {opened,{call,dets,delete,[dets_table,eqc_gen:nat()]}},
     {opened,{call,?MODULE,get_contents,[dets_table]}}
     ].

%% Next state transformation for state data.
%% S is the current state, From and To are state names
particular_next_state_data(_From,_To,S,V,{call,_,open_file,[_,[{type,T}]]}) ->
    S#state{name=V,type=T};
particular_next_state_data(_From,_To,S,_V,{call,_,insert,[_,Objs]}) ->
    S#state{contents=general:model_insert(S#state.type,S#state.contents,Objs)};
particular_next_state_data(_From,_To,S,_V,{call,_,insert_new,[_,Objs]}) ->
    case general:any_exist(Objs,S#state.contents) of
	true ->
	    S;
	false ->
	    S#state{contents=general:model_insert(S#state.type,S#state.contents,Objs)}
    end;
particular_next_state_data(_From,_To,S,_V,{call,_,delete,[_,K]}) ->
    S#state{contents=general:model_delete(S#state.contents,K)};
particular_next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

particular_refactoring(From) -> From == init_state orelse ?BUG_OPEN_FILE.


delete_postcondition() -> ok.

insert_postcondition() -> ok.

set_up() ->
    dets:close(table_name()),
    general:file_delete(table_name()).

parallel_set_up() ->
    %% Make sure the table is not left open by a previous test.
    [dets:close(table_name()) || _ <- "abcdefghijkl"],
    %% Make sure the table is initially empty.
    general:file_delete(table_name()).

%% Operations for inclusion in test cases

get_contents(Name) ->
    dets:traverse(Name,fun(X) -> {continue,X} end).


open_file(Name,Args) ->
    {ok,N} = dets:open_file(Name,Args),
    N.
