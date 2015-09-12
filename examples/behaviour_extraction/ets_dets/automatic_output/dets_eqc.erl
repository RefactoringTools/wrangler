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

module_name() -> common:module_name(?MODULE).

table_name() -> common:table_name(?MODULE).

init_state(S) -> common:init_state(?MODULE, S).

opened(S) -> common:common_13(?MODULE, S).

object() -> common:object().

pattern() -> common:pattern().

initial_state() -> common:initial_state().

initial_state_data() -> common:initial_state_data().

%% Next state transformation for state data.
%% S is the current state, From and To are state names
next_state_data(_From,_To,S,V,{call,_,open_file,[_,[{type,T}]]}) ->
    common:common_5(?MODULE, S, T, V);
next_state_data(_From,_To,S,_V,{call,_,insert,[_,Objs]}) ->
    common:common_7(?MODULE, Objs, S);
next_state_data(_From,_To,S,_V,{call,_,insert_new,[_,Objs]}) ->
    case common:common_8(?MODULE, Objs, S) of
	true ->
	    S;
	false -> common:common_1(?MODULE, Objs, S)
    end;
next_state_data(_From,_To,S,_V,{call,_,delete,[_,K]}) ->
    common:common_11(?MODULE, K, S);
next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

precondition(From, To, S, X4) ->
    common:precondition(?MODULE, From, To, S, X4).

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>) 
postcondition(_From,_To,_S,{call,_,close,_},Res) ->
    common:common_2(Res);  %% not_owner in parallel tests
postcondition(_From,_To,_S,{call,_,delete,_},Res) ->
    common:common_9(Res);
postcondition(_From,_To,_S,{call,_,insert,_},Res) ->
    common:common_3(Res);
postcondition(_From,_To,S,{call,_,insert_new,[_,Objs]},Res) ->
    common:common_6(?MODULE, Objs, Res, S);
postcondition(_From,_To,S,{call,_,get_contents,_},Res) ->
    common:common_10(?MODULE, Res, S);
postcondition(_From,_To,_S,{call,_,_,_},_Res) ->
    true.

%% Model

model_insert(T, S, Obj) -> common:model_insert(?MODULE, T, S, Obj).

any_exist(Obj, S) -> common:any_exist(?MODULE, Obj, S).

model_delete(S, K) -> common:model_delete(S, K).

%% Top level property

prop_dets() -> common:common_12(?MODULE).

%% Parallel testing property

prop_parallel() -> common:prop_parallel(?MODULE).

weight(From, To, X3) -> common:weight(From, To, X3).

open_file(Name,Args) ->
    {ok,N} = dets:open_file(Name,Args),
    N.

%% Operations for inclusion in test cases

get_contents(Name) -> common:get_contents(?MODULE, Name).
			       
file_delete(Name) -> common:common_4(?MODULE, Name).



callback_1(V) -> V.

callback_2(Res) -> Res.

callback_3() -> dets.

callback_4() -> dets_table.

callback_5() -> common:common_17(?MODULE).

callback_6() -> ?MODULE.

callback_7() -> open_file.

callback_8(From) ->
    common:common_15(?MODULE, From, none) orelse ?BUG_OPEN_FILE.

callback_9(Par, Seq) ->
    %% Make sure the table is not left open by a previous test.
    [dets:close(table_name()) || _ <- "abcdefghijkl"],
    %% Make sure the table is initially empty.
    file_delete(table_name()),
    %% Run the test case.
    {H,ParH,Res} = run_parallel_commands(?MODULE,{Seq,Par}),
    ?WHENFAIL((io:format("History: ~p\nParallel: ~p\n\nRes: ~p\n",common:common_22(?MODULE,
                                                                                   H,
                                                                                   ParH,
                                                                                   Res,
                                                                                   none))),
              (collect(length(Par),
                       aggregate([length(P) || P <- Par],
                                 collect(length([ok
                                                 || P <- Par,
                                                    {set,_,{call,_,open_file,_}} <- P]),common:common_16(?MODULE,
                                                                                                         Res)))))).

callback_10(Name) -> dets:traverse(Name,fun (X) -> {continue,X} end).

callback_11(Res) -> Res.

callback_12(Cmds) ->
    dets:close(table_name()),
    file_delete(table_name()),
    {H,S,Res} = run_commands(?MODULE,Cmds),
    common:common_23(?MODULE, Cmds, H, Res, S, none, none).

callback_13() -> insert_new.

callback_14() -> ?MODULE.

callback_15() -> open_file.

callback_16(S) -> [{type,S#state.type}].

callback_17() -> close.

callback_18() -> insert.

callback_19() -> oneof([object(),list(object())]).

callback_20() -> common:common_18(?MODULE).

callback_21(Res) -> Res.

callback_22() -> ok.

callback_23(From, _Res) -> From.

callback_24() -> init_state.

callback_25(Res) -> Res.

callback_26() -> ok.

callback_27() -> type.

callback_28() -> common:common_19().

callback_29() -> {opened,{call,?MODULE,get_contents,[table_name()]}}.

callback_30() ->
    {opened,common:common_20(?MODULE)
                                                            }.

callback_31() -> module_name().

callback_32() -> delete.

callback_33() -> common:common_21(?MODULE).

callback_34() -> nat().

callback_35() -> table_name().

callback_36(ParH, _S) -> ParH.

callback_37(Res) -> [Res].

callback_38() -> "History: ~p\nState: ~p\nRes: ~p\n".

callback_39(S, _ParH) -> S.

callback_40(Cmds, Res, _Par) ->
    aggregate(command_names(Cmds),common:common_14(?MODULE, Res)).