

-module(common).

-export([any_exist/3, behaviour_info/1, common_1/3, common_10/3,
         common_11/5, common_12/5, common_2/2, common_3/3, common_4/2,
         common_5/2, common_6/2, common_7/4, common_8/3, common_9/1,
         init_state/2, model_delete/2, model_insert/4, object/1, pattern/1,
         precondition/5, prop_parallel/1, weight/3]).

-ifndef(BUG_INSERT_NEW).
-define(BUG_INSERT_NEW,true).
-endif.

-record(state,{name,type,contents=[]}).

behaviour_info(callbacks) ->
    [{aggregate, 2}, {any_exist, 2}, {callback_1, 0}, {callback_10, 0},
     {callback_11, 0}, {callback_12, 0}, {callback_13, 1}, {callback_14, 2},
     {callback_2, 0}, {callback_3, 1}, {callback_4, 0}, {callback_5, 0},
     {callback_6, 0}, {callback_7, 0}, {callback_8, 0}, {callback_9, 1},
     {collect, 2}, {command_names, 1}, {commands, 1}, {fails, 1},
     {file_delete, 1}, {list, 1}, {model_delete, 2}, {model_insert, 3},
     {module_name, 0}, {more_commands, 2}, {nat, 0}, {object, 0}, {oneof, 1},
     {parallel_commands, 1}, {table_name, 0}];
behaviour_info(_Other) -> undefined.

common_1(S, T, V) -> S#state{name = V,type = T}.

common_2(Res, S) -> lists:sort(Res) == lists:sort(S#state.contents).

common_3(Module, Objs, S) ->
    case Module:any_exist(Objs,S#state.contents) of
        true ->
            S;
        false ->
            S#state{contents = Module:model_insert(S#state.type,S#state.contents,Objs)}
    end.

common_4(Module, Name) ->
    case file:delete(Name) of
        {error,enoent} ->
            ok;
        _Bad ->
            Module:file_delete(Name)
    end.

common_5(Module, S) ->
    [{init_state,{call,Module:module_name(),Module:callback_2(),[Module:table_name()]}},
     {opened,{call,Module:callback_6(),Module:callback_7(),[Module:table_name(),Module:callback_3(S)]}},
   {opened,{call,Module:module_name(),Module:callback_8(),[Module:table_name(),Module:oneof([Module:object(),Module:list(Module:object())])]}},
   {opened,{call,Module:module_name(),Module:callback_1(),[Module:table_name(),Module:callback_5()]}}
     | Module:callback_4()].

common_6(Module, Cmds) ->
    ?FORALL(Cmds,(Module:more_commands(3,Module:commands(Module))),
            ?TRAPEXIT(Module:callback_9(Cmds))).

%% Definition of the states. Each state is represented by a function, 
%% listing the transitions from that state, together with generators 
%% for the calls to make each transition.
init_state(Module,_S) ->
    [{opened,{call,Module:callback_10(),Module:callback_11(),[Module:table_name(),Module:callback_12()]}}
     ].

object(Module) ->
    {Module:nat(),Module:nat()}.

pattern(Module) ->
    {Module:oneof(['$1',Module:nat()]),Module:oneof(['$1','$2',Module:nat()])}.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(Module,From,_To,S,{call,_,open_file,[_,[{type,T}]]}) ->
    lists:member(S#state.type,[undefined,T])
       andalso
      Module:callback_13(From);
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
    Module:fails(%% Number of attempts to make each test case fail. When searching
                 %% for a failing example, we run each test once. When searching for
                 %% a way to shrink a test case, we run each candidate shrinking
                 %% 100 times.
                 ?FORALL(Attempts, ?SHRINK(1,[100]),
                   ?FORALL({Seq,Par}, (Module:parallel_commands(Module)),
                     ?ALWAYS(Attempts,
	               ?TIMEOUT(1000,(Module:callback_14(Par, Seq))))))).


%% Weight for transition (this callback is optional).
%% Specify how often each transition should be chosen
weight(_From,_To,{call,_,_,_}) ->
    1.

common_7(Module, Objs, Res, S) ->
    Res == not Module:any_exist(Objs,S#state.contents).

common_8(Module, Objs, S) ->
    S#state{contents = Module:model_insert(S#state.type,S#state.contents,Objs)}.

common_9(Res) -> Res == ok orelse Res == {error,not_owner}.

common_10(Module, K, S) ->
    S#state{contents = Module:model_delete(S#state.contents,K)}.

common_11(Module, Cmds, H, Res, S) ->
    ?WHENFAIL((io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res])),
              (Module:aggregate(Module:command_names(Cmds),
                                Res == ok))).

common_12(Module, H, Par, ParH, Res) ->
    ?WHENFAIL((io:format("History: ~p\nParallel: ~p\n\nRes: ~p\n",
                         [H,ParH,Res])),
              (Module:collect(length(Par),
                              Module:aggregate([length(P) || P <- Par],
                                               Module:collect(length([ok
                                                                      || P <- Par,
                                                                         {set,_,{call,_,open_file,_}} <- P]),
                                                      Res == ok))))).