-module(dets_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).

-ifndef(BUG_OPEN_FILE).
-define(BUG_OPEN_FILE,false).
-endif.

-ifndef(BUG_INSERT_NEW).
-define(BUG_INSERT_NEW,true).
-endif.

-record(state,{name,type,contents=[]}).

-behaviour(common).

module_name() -> dets.

table_name() -> dets_table.

init_state(S) -> common:init_state(?MODULE, S).

opened(S) -> common:common_5(?MODULE, S).

object() -> common:object(?MODULE).

pattern() -> common:pattern(?MODULE).

%% Identify the initial state
initial_state() ->
    init_state.

%% Initialize the state data
initial_state_data() ->
    #state{}.

%% Next state transformation for state data.
%% S is the current state, From and To are state names
next_state_data(_From,_To,S,V,{call,_,open_file,[_,[{type,T}]]}) ->
    common:common_1(S, T, V);
next_state_data(_From,_To,S,_V,{call,_,insert,[_,Objs]}) ->
    common:common_8(?MODULE, Objs, S);
next_state_data(_From,_To,S,_V,{call,_,insert_new,[_,Objs]}) ->
    common:common_3(?MODULE, Objs, S);
next_state_data(_From,_To,S,_V,{call,_,delete,[_,K]}) ->
    common:common_10(?MODULE, K, S);
next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

precondition(From, To, S, X4) ->
    common:precondition(?MODULE, From, To, S, X4).

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>) 
postcondition(_From,_To,_S,{call,_,close,_},Res) ->
    common:common_9(Res);  %% not_owner in parallel tests
postcondition(_From,_To,_S,{call,_,delete,_},Res) ->
    Res==ok;
postcondition(_From,_To,_S,{call,_,insert,_},Res) ->
    Res==ok;
postcondition(_From,_To,S,{call,_,insert_new,[_,Objs]},Res) ->
    common:common_7(?MODULE, Objs, Res, S);
postcondition(_From,_To,S,{call,_,get_contents,_},Res) ->
    common:common_2(Res, S);
postcondition(_From,_To,_S,{call,_,_,_},_Res) ->
    true.

%% Model

model_insert(T, S, Obj) -> common:model_insert(?MODULE, T, S, Obj).

any_exist(Obj, S) -> common:any_exist(?MODULE, Obj, S).

model_delete(S, K) -> common:model_delete(S, K).

%% Top level property

prop_dets() -> common:common_6(?MODULE, Cmds).

%% Parallel testing property

prop_parallel() -> common:prop_parallel(?MODULE).

weight(From, To, X3) -> common:weight(From, To, X3).

open_file(Name,Args) ->
    {ok,N} = dets:open_file(Name,Args),
    N.

%% Operations for inclusion in test cases

get_contents(Name) ->
    dets:traverse(Name,fun(X)->{continue,X}end).
			       
file_delete(Name) -> common:common_4(?MODULE, Name).



callback_1() -> insert_new.

callback_2() -> close.

callback_3(S) -> [{type,S#state.type}].

callback_4() ->
    [{opened,{call,module_name(),delete,[table_name(),nat()]}},
     {opened,{call,?MODULE,get_contents,[table_name()]}}
      ].

callback_5() -> oneof([object(),list(object())]).

callback_6() -> ?MODULE.

callback_7() -> open_file.

callback_8() -> insert.

callback_9(Cmds) ->
    dets:close(table_name()),
    file_delete(table_name()),
    {H,S,Res} = run_commands(?MODULE,Cmds),
    common:common_11(?MODULE, Cmds, H, Res, S).

callback_10() -> ?MODULE.

callback_11() -> open_file.

callback_12() -> [{type,oneof([set,bag])}].

callback_13(From) -> From == init_state orelse ?BUG_OPEN_FILE.

callback_14(Par, Seq) ->
    %% Make sure the table is not left open by a previous test.
    [dets:close(table_name()) || _ <- "abcdefghijkl"],
    %% Make sure the table is initially empty.
    file_delete(table_name()),
    %% Run the test case.
    {H,ParH,Res} = run_parallel_commands(?MODULE,{Seq,Par}),
    common:common_12(?MODULE, H, Par, ParH, Res).