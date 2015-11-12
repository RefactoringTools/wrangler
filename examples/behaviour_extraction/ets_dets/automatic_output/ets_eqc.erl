-module(ets_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).

-ifndef(BUG_INSERT_NEW).
-define(BUG_INSERT_NEW,true).
-endif.

-record(state,{name,type,contents=[]}).

-behaviour(common).

module_name() -> common:module_name(?MODULE).

table_name() -> common:table_name(?MODULE).

init_state(S) -> common:init_state(?MODULE, S).

opened(_S) -> common:common_13(?MODULE, none).

object() -> common:object().

pattern() -> common:pattern().

initial_state() -> common:initial_state().

initial_state_data() -> common:initial_state_data().

%% Next state transformation for state data.
%% S is the current state, From and To are state names
next_state_data(_From,_To,S,V,{call,_,new,[_,[public,named_table,T]]}) ->
    common:common_5(?MODULE, S, T, V);
next_state_data(_From,_To,S,_V,{call,_,insert,[_,Objs]}) ->
    common:common_1(?MODULE, Objs, S);
next_state_data(_From,_To,S,_V,{call,_,insert_new,[_,Objs]}) ->
    case common:common_8(?MODULE, Objs, S) of
	true ->
	    S;
	false -> common:common_7(?MODULE, Objs, S)
    end;
next_state_data(_From,_To,S,_V,{call,_,delete,[_,K]}) ->
    common:common_11(?MODULE, K, S);
next_state_data(_From,_To,S,_V,{call,_,delete,[_]}) ->
    S#state{contents=[], type=undefined};
next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

precondition(From, To, S, X4) ->
    common:precondition(?MODULE, From, To, S, X4).

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>) 
postcondition(_From,_To,_S,{call,_,close,_},Res) ->
    common:common_2(Res);  %% not_owner in parallel tests
postcondition(_From,_To,_S,{call,_,delete,[_,_]},Res) ->
    common:common_15(?MODULE, none, Res);
postcondition(_From,_To,_S,{call,_,delete,[_]},Res) ->
    common:common_16(?MODULE, Res);
postcondition(_From,_To,_S,{call,_,insert,_},Res) ->
    common:common_14(?MODULE, Res);
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

prop_ets() -> common:common_12(?MODULE).

%% Parallel testing property

prop_parallel() -> common:prop_parallel(?MODULE).

weight(From, To, X3) -> common:weight(From, To, X3).

%% Operations for inclusion in test cases

get_contents(Name) -> common:get_contents(?MODULE, Name).
			       
file_delete(Name) -> common:common_4(?MODULE, Name).



callback_1(V) -> V.

callback_2(Res) -> Res.

callback_3() -> ets.

callback_4() -> ets_table.

callback_5() -> [public | common:common_21(?MODULE)].

callback_6() -> module_name().

callback_7() -> new.

callback_8(From) -> From == init_state.

callback_9(Par, Seq) ->
    %% Make sure the table is not left open by a previous test.
    catch ets:delete(table_name()),
    %% Run the test case.
    {H,ParH,Res} = run_parallel_commands(?MODULE,{Seq,Par}),
    common:common_23(?MODULE, none, H, Res, none, Par, ParH).

callback_10(Name) -> ets:foldl(fun (X,Y) -> [X | Y] end,[],Name).

callback_11(Res) -> Res.

callback_12(Cmds) ->
    catch ets:delete(table_name()),
    {H,S,Res} = run_commands(?MODULE,Cmds),
    ?WHENFAIL((io:format("History: ~p\nState: ~p\nRes: ~p\n",common:common_22(?MODULE,
                                                                              H,
                                                                              none,
                                                                              Res,
                                                                              S))),
              (aggregate(command_names(Cmds),common:common_9(Res)))).

callback_13() -> delete.

callback_14() -> module_name().

callback_15() -> insert.

callback_16(_S) -> oneof(common:common_18(?MODULE)).

callback_17() -> delete.

callback_18() -> insert_new.

callback_19() -> nat().

callback_20() -> common:common_17(?MODULE).

callback_21(Res) -> Res.

callback_22() -> true.

callback_23(_From, Res) -> Res.

callback_24() -> true.

callback_25(Res) -> Res.

callback_26() -> true.

callback_27() -> opened.

callback_28() -> common:common_20(?MODULE).

callback_29() -> list(object()).

callback_30() -> object().

callback_31() -> ?MODULE.

callback_32() -> get_contents.

callback_33() -> [table_name()].

callback_34() -> common:common_19().

callback_35() -> named_table.

callback_36(_ParH, S) -> S.

callback_37(Res) -> [Res].

callback_38() -> "History: ~p\nParallel: ~p\n\nRes: ~p\n".

callback_39(_S, ParH) -> ParH.

callback_40(_Cmds, Res, Par) ->
    collect(length(Par),
            aggregate([length(P) || P <- Par],
                      collect(length([ok
                                      || P <- Par,
                                         {set,_,{call,_,open_file,_}} <- P]),common:common_3(Res)))).