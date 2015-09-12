

-module(common).

-export([any_exist/3, behaviour_info/1, common_1/3, common_10/3,
         common_11/3, common_12/1, common_13/2, common_14/2, common_15/3,
         common_16/2, common_17/1, common_18/1, common_19/0, common_2/1,
         common_20/1, common_21/1, common_22/5, common_23/7, common_3/1,
         common_4/2, common_5/4, common_6/4, common_7/3, common_8/3, common_9/1,
         get_contents/2, init_state/2, initial_state/0, initial_state_data/0,
         model_delete/2, model_insert/4, module_name/1, object/0, pattern/0,
         precondition/5, prop_parallel/1, table_name/1, weight/3]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-ifndef(BUG_INSERT_NEW).
-define(BUG_INSERT_NEW,true).
-endif.

-record(state,{name,type,contents=[]}).

behaviour_info(callbacks) ->
    [{any_exist, 2}, {callback_1, 1}, {callback_10, 1}, {callback_11, 1},
     {callback_12, 1}, {callback_13, 0}, {callback_14, 0}, {callback_15, 0},
     {callback_16, 1}, {callback_17, 0}, {callback_18, 0}, {callback_19, 0},
     {callback_2, 1}, {callback_20, 0}, {callback_21, 1}, {callback_22, 0},
     {callback_23, 2}, {callback_24, 0}, {callback_25, 1}, {callback_26, 0},
     {callback_27, 0}, {callback_28, 0}, {callback_29, 0}, {callback_3, 0},
     {callback_30, 0}, {callback_31, 0}, {callback_32, 0}, {callback_33, 0},
     {callback_34, 0}, {callback_35, 0}, {callback_36, 2}, {callback_37, 1},
     {callback_38, 0}, {callback_39, 2}, {callback_4, 0}, {callback_40, 3},
     {callback_5, 0}, {callback_6, 0}, {callback_7, 0}, {callback_8, 1},
     {callback_9, 2}, {file_delete, 1}, {model_delete, 2}, {model_insert, 3},
     {module_name, 0}, {object, 0}, {table_name, 0}];
behaviour_info(_Other) -> undefined.

common_1(Module, Objs, S) ->
    S#state{contents = Module:model_insert(S#state.type,S#state.contents,Objs)}.

common_2(Res) -> Res == ok orelse Res == {error,not_owner}.

common_3(Res) -> Res == ok.

common_4(Module, Name) ->
    case file:delete(Name) of
        {error,enoent} ->
            ok;
        _Bad ->
            Module:file_delete(Name)
    end.

common_5(Module, S, T, V) ->
    S#state{name = Module:callback_1(V),type = T}.

common_6(Module, Objs, Res, S) ->
    Module:callback_2(Res) == not Module:any_exist(Objs,S#state.contents).

common_7(Module, Objs, S) ->
    S#state{contents = Module:model_insert(S#state.type,S#state.contents,Objs)}.

module_name(Module) -> Module:callback_3().

table_name(Module) -> Module:callback_4().

%% Definition of the states. Each state is represented by a function, 
%% listing the transitions from that state, together with generators 
%% for the calls to make each transition.
init_state(Module,_S) ->
    [{opened,{call,Module:callback_6(),Module:callback_7(),[Module:table_name(),Module:callback_5()]}}
     ].

object() ->
    {nat(),nat()}.

pattern() ->
    {oneof(['$1',nat()]),oneof(['$1','$2',nat()])}.

%% Identify the initial state
initial_state() ->
    init_state.

%% Initialize the state data
initial_state_data() ->
    #state{}.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(Module,From,_To,S,{call,_,open_file,[_,[{type,T}]]}) ->
    lists:member(S#state.type,[undefined,T])
       andalso
      Module:callback_8(From);
precondition(_Module,_From,_To,_S,{call,_,insert_new,_}) ->
    ?BUG_INSERT_NEW;
precondition(_Module,_From,_To,_S,{call,_,_,_}) ->
    true.

model_insert(_Module,set,S,{K,_V}=Obj) ->
    lists:keydelete(K,1,S)++[Obj];
model_insert(_Module,bag,S,{_K,_V}=Obj) ->
     (S -- [Obj]) ++ [Obj];   % surprise! why can't Obj appear twice?
model_insert(Module,T,S,[Obj|Objs]) ->
    Module:model_insert(T,Module:model_insert(T,S,Obj),Objs);
model_insert(_Module,_,S,[]) ->
    S.

any_exist(Module,Obj,S) when is_tuple(Obj) ->
    Module:any_exist([Obj],S);
any_exist(_Module,Objs,S) ->
    [K || {K,_} <- Objs,
	  lists:keymember(K,1,S)]
	/= [].

model_delete(S,K) ->
    [O || O={K1,_} <- S,
	  K1/=K].

prop_parallel(Module) ->
    fails(
    %% Number of attempts to make each test case fail. When searching
    %% for a failing example, we run each test once. When searching for
    %% a way to shrink a test case, we run each candidate shrinking
    %% 100 times.
    ?FORALL(Attempts,?SHRINK(1,[100]),
      ?FORALL({Seq,Par}, (parallel_commands(Module)),
	?ALWAYS(Attempts,
	  ?TIMEOUT(1000,(Module:callback_9(Par, Seq))))))
     ).


%% Weight for transition (this callback is optional).
%% Specify how often each transition should be chosen
weight(_From,_To,{call,_,_,_}) ->
    1.

get_contents(Module,Name) -> Module:callback_10(Name).

common_8(Module, Objs, S) -> Module:any_exist(Objs,S#state.contents).

common_9(Res) -> Res == ok.

common_10(Module, Res, S) ->
    lists:sort(Module:callback_11(Res)) == lists:sort(S#state.contents).

common_11(Module, K, S) ->
    S#state{contents = Module:model_delete(S#state.contents,K)}.

common_12(Module) ->
    ?FORALL(Cmds,(more_commands(3,commands(Module))),
            ?TRAPEXIT(Module:callback_12(Cmds))).

common_13(Module, S) ->
    [{init_state,{call,Module:module_name(),Module:callback_17(),[Module:table_name()]}},
     {opened,{call,Module:callback_14(),Module:callback_15(),[Module:table_name(),Module:callback_16(S)]}},
   {opened,{call,Module:module_name(),Module:callback_18(),[Module:table_name(),oneof([Module:object(),list(Module:object())])]}},
   {opened,{call,Module:module_name(),Module:callback_13(),[Module:table_name(),Module:callback_19()]}}
     | Module:callback_20()].

common_14(Module, Res) ->
    Module:callback_21(Res) == Module:callback_22().

common_15(Module, From, Res) ->
    Module:callback_23(From, Res) == Module:callback_24().

common_16(Module, Res) ->
    Module:callback_25(Res) == Module:callback_26().

common_17(Module) -> [{Module:callback_27(), Module:callback_28()}].

common_18(Module) -> [Module:callback_30(), Module:callback_29()].

common_19() -> oneof([set,bag]).

common_20(Module) ->
    {call,Module:callback_31(),Module:callback_32(),Module:callback_33()
                                                   }.

common_21(Module) -> [Module:callback_35(), Module:callback_34()].

common_22(Module, H, ParH, Res, S) ->
    [H,Module:callback_36(ParH, S) | Module:callback_37(Res)].

common_23(Module, Cmds, H, Res, S, Par, ParH) ->
    ?WHENFAIL((io:format(Module:callback_38(),[H,Module:callback_39(S,
                                                                    ParH),Res])),(Module:callback_40(Cmds,
                                                                                                     Res,
                                                                                                     Par))).