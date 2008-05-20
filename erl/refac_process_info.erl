%% ============================================================================================
%% Refactoring: Annotate the program with process information.
%%
%% Copyright (C) 2006-2008  Huiqing Li, Simon Thompson


%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.


%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.


%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%


%% TODO: data flow analysis to get the values actual parameters of spawn.
%%  handling of send expressions (to detive the initial fun info of the dest process.


-module(refac_process_info).

-export([ann_process_info/1]).

-export([fun_typesig/1, env/0, counter/0,is_spawn_app/1, ann_process_in_a_file/1, is_send_expr/1, is_register_app/1]).

-compile(export_all).

-include("../hrl/wrangler.hrl").

ann_process_in_a_file(FName) ->
    [{FName, AST}] = ann_process_info([FName]),
    ok.
    
ann_process_info(DirList) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    SortedFuns = sort_funs(DirList),
    Funs = lists:map(fun ({Fun, _FunDef}) -> Fun end, SortedFuns),
    Funs1 = lists:map(fun(F) ->annotate_special_fun_apps(F) end, SortedFuns),
  %%   Pid =
%% 	start_fun_typesig_process(Funs),            %% Refactor this:  register a process, and remove the uses of Pid.
   %%  SortedFuns1 = annotate_process_info(SortedFuns, Pid),
   %%  Pid ! stop,
    lists:map(fun (File) -> {File, update_function(File, Funs1)} end, Files).
    

annotate_process_info(Funs, Pid) ->
    Funs1 = bottom_up_ann(Funs, Pid),
    lists:map(fun (F) -> topdown_prop(F, Pid) end, lists:reverse(Funs1)).
    

bottom_up_ann(Funs, TypeSigPid) ->
    lists:map(fun (F) -> bottom_up_ann_1(F, TypeSigPid) end,
	      Funs).

bottom_up_ann_1({{ModName, FunName, Arity}, FunDef}, TypeSigPid) ->
    EnvPid = start_env_process(),
    FunDef1 = annotate_special_fun_apps({{ModName, FunName, Arity}, EnvPid, FunDef}),
    %% Note: two traversals are necessary.
    FunDef2 = refac_util:full_buTP(fun annotate_within_fun/2, FunDef1, {ModName, FunName, Arity, EnvPid, TypeSigPid}),
    FunDef3 = refac_util:full_buTP(fun annotate_within_fun/2, FunDef2, {ModName, FunName, Arity, EnvPid, TypeSigPid}),
    EnvPid ! stop,  %% => env ! stop,
    {{ModName, FunName, Arity}, FunDef3}.

topdown_prop({{ModName, FunName, Arity}, FunDef}, TypeSigPid) ->
    EnvPid = start_env_process(),
    FunDef2 = do_topdown_propagate(FunDef, {ModName, FunName, Arity, EnvPid, TypeSigPid}),
    FunDef3 = refac_util:full_buTP(fun annotate_within_fun_1/2, FunDef2, {ModName, FunName, Arity, EnvPid, TypeSigPid}),
    EnvPid ! stop,  %% => env ! stop.
    {{ModName, FunName, Arity}, FunDef3}.


update_function(File, FunList) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(File, true, []),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    F = fun (Node, []) ->
		case refac_syntax:type(Node) of
		  function ->
		      FunName = refac_syntax:data(refac_syntax:function_name(Node)),
		      Arity = refac_syntax:function_arity(Node),
		      case lists:keysearch({ModName, FunName, Arity}, 1, FunList) of
			{value, {{ModName, FunName, Arity}, FunDef}} -> {FunDef, true};
			_ -> {Node, false}
		      end;
		  _ -> {Node, false}
		end
	end,
    {AnnAST1, _} = refac_util:stop_tdTP(F, AnnAST, []),
    AnnAST1.


annotate_within_fun(Node, {_ModName, FunName, Arity, Pid, TypeSigPid}) ->
    case refac_syntax:type(Node) of
      variable ->
	  Ann = refac_syntax:get_ann(Node),
	  case lists:keysearch(def, 1, Ann) of
	    {value, {def, DefinePos}} ->
		Pid ! {self(), get, {def, DefinePos}},
		receive
		  {Pid, value, Value} -> refac_util:update_ann(Node, Value);
		  {Pid, false} -> Node
		end;
	    _ -> Node
	  end;
      match_expr ->
	  P = refac_syntax:match_expr_pattern(Node),
	  B = refac_syntax:match_expr_body(Node),
	  Ann = refac_syntax:get_ann(B),
	  case lists:keysearch(pid, 1, Ann) of           %% Ingnore process name first.
	    {value, {pid, Value}} ->
		P1 = refac_util:update_ann(P, {pid, Value}),
		case refac_syntax:type(P) of
		  variable ->
		      Ann1 = refac_syntax:get_ann(P),
		      {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
		      Pid ! {add, {{def, DefinePos}, {pid, Value}}};
		  _ -> ok                %%% What about the complex pattern matches?
		end,
		refac_syntax:copy_attrs(Node, refac_syntax:match_expr(P1, B));
	    _ -> Node %% The body is not a pid
	  end;
      application ->
	  Operator = refac_syntax:application_operator(Node),
	  Ann = refac_syntax:get_ann(Operator),
	  case lists:keysearch(fun_def, 1, Ann) of
	    {value, {fun_def, {Mod, F, A, _, _}}} ->
		Args = refac_syntax:application_arguments(Node),
		TypeSigPid ! {self(), get, {Mod, F, A}},
		Args1 = receive
			    {TypeSigPid, value, {ParSig, RtnSig}} ->
				lists:map(fun ({Arg, T}) ->
						  case T of
						      {process, Value} ->
							  case refac_syntax:type(Arg) of
							      variable ->
								  Ann1 = refac_syntax:get_ann(Arg),
								  case lists:keysearch(process, 1, Ann1) of
								      {value, {process, _Value1}} -> Arg;
								      false ->
									  {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
									  Pid ! {add, {{def, DefinePos}, {process, Value}}},
									  refac_util:update_ann(Arg, {process, Value})
								  end;
							      _ ->
								  Ann1 = refac_syntax:get_ann(Arg),
								  case lists:keysearch(process, 1, Ann1) of
								      {value, {process, _Value}} -> Arg;
								      false -> refac_util:update_ann(Arg, {process, Value})
								  end
							  end;
						      _ -> Arg
						  end
					  end,
					  lists:zip(Args, ParSig));
			    {TypeSigPid, false} -> Args
			end,
		refac_syntax:copy_attrs(Node, refac_syntax:application(Operator, Args1));
	    _ -> Node
	  end;
      function ->
	  Ann = refac_syntax:get_ann(Node),
	  {value, {fun_def, {Mod, FunName, Arity, _, _}}} = lists:keysearch(fun_def, 1, Ann),
	  Cs = refac_syntax:function_clauses(Node),
	  lists:map(fun (C) ->
			    Ps = refac_syntax:clause_patterns(C),
			    LastExp = lists:last(refac_syntax:clause_body(C)),
			    ArgsInfo = lists:map(fun (P) ->
							 Ann1 = refac_syntax:get_ann(P),
							 case lists:keysearch(process, 1, Ann1) of
							   {value, {process, Value}} -> {process, Value};
							   _ -> any
							 end
						 end,
						 Ps),
			    Res = case refac_syntax:type(LastExp) of
				    match_expr -> refac_syntax:match_expr_pattern(LastExp);
				    _ -> LastExp
				  end,
			    ResInfo = case lists:keysearch(process, 1, refac_syntax:get_ann(Res)) of
					{value, {process, Value}} -> {process, Value};
					_ -> any
				      end,
			    Info = {{Mod, FunName, Arity}, {ArgsInfo, ResInfo}},
			    TypeSigPid ! {add, Info}  %% This is only right when the function has only one clause.
		    end,
		    Cs),
	  Node;
      _ -> Node
    end.

do_topdown_propagate(FunDef, {_ModName, FunName, Arity, EnvPid, TypeSigPid}) ->
    F = fun (Pat, Typ) ->
		Ann = refac_syntax:get_ann(Pat),
		case lists:keysearch(process, 1, Ann) of
		  {value, {process, Value}} ->
		      case Typ of
			any -> {process, Value};
			{process, []} -> {process, Value};
			{process, Value1} -> {process, lists:usort(Value ++ Value1)}
		      end;
		  false -> Typ
		end
	end,
    F2 = fun (Node, _Others) ->
		 case refac_syntax:type(Node) of
		   application ->
		       Operator = refac_syntax:application_operator(Node),
		       Ann = refac_syntax:get_ann(Operator),
		       case lists:keysearch(fun_def, 1, Ann) of
			 {value, {fun_def, {M1, F1, A1, _P1, _P2}}} ->
			     Args = refac_syntax:application_arguments(Node),
			     TypeSigPid ! {self(), get, {M1, F1, A1}},
			     receive
			       {TypeSigPid, value, {ParSig, _RtnSig}} ->
				   ArgsInfo = lists:map(fun ({A, P}) -> F(A, P) end, lists:zip(Args, ParSig)),
				   Info = {{M1, F1, A1}, {ArgsInfo, any}}, %% any is for the return type of the function.
				   TypeSigPid ! {add, Info};
			       {TypeSigPid, false} -> ok
			     end,
			     Node;
			 _ -> Node
		       end;
		   variable ->
		       Ann = refac_syntax:get_ann(Node),
		       case lists:keysearch(def, 1, Ann) of
			 {value, {def, DefinePos}} ->
			     EnvPid ! {self(), get, {def, DefinePos}},
			     receive
			       {EnvPid, value, Value} -> refac_util:update_ann(Node, Value);
			       {EnvPid, false} -> Node
			     end;
			 _ -> Node
		       end;
		   _ -> Node
		 end
	 end,
    Ann = refac_syntax:get_ann(FunDef),
    {value, {fun_def, {Mod, FunName, Arity, _, _}}} = lists:keysearch(fun_def, 1, Ann),
    TypeSigPid ! {self(), get, {Mod, FunName, Arity}},
    receive
      {TypeSigPid, value, {ParsSig, _RtnSig}} ->
	  FunName1 = refac_syntax:function_name(FunDef),
	  Cs = refac_syntax:function_clauses(FunDef),
	  Cs1 = lists:map(fun (C) ->
				  Ps = refac_syntax:clause_patterns(C),
				  B = refac_syntax:clause_body(C),
				  G = refac_syntax:clause_guard(C),
				  Ps1 = lists:map(fun ({P, T}) ->  %% what about complex parameters.
							  case refac_syntax:type(P) of
							    variable ->
								case T of
								  {process, Value} ->
								      Ann1 = refac_syntax:get_ann(P),
								      {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
								      EnvPid ! {add, {{def, DefinePos}, {process, Value}}},
								      refac_util:update_ann(P, {process, Value});
								  _ -> P
								end;
							    _ -> P
							  end
						  end,
						  lists:zip(Ps, ParsSig)),
				  C1 = refac_syntax:copy_attrs(C, refac_syntax:clause(Ps1, G, B)),
				  refac_util:full_buTP(F2, C1, [])
			  end,
			  Cs),
	  refac_syntax:copy_attrs(FunDef, refac_syntax:function(FunName1, Cs1));
      {TypeSigPid, false} -> FunDef
    end.

annotate_within_fun_1(Node, {_ModName, _FunName, _Arity, Pid, _TypeSigPid}) ->
    case refac_syntax:type(Node) of
      variable ->
	  Ann = refac_syntax:get_ann(Node),
	  case lists:keysearch(def, 1, Ann) of
	    {value, {def, DefinePos}} ->
		Pid ! {self(), get, {def, DefinePos}},
		receive
		  {Pid, value, Value} -> refac_util:update_ann(Node, Value);
		  {Pid, false} -> Node
		end;
	    _ -> Node
	  end;
      match_expr ->
	  P = refac_syntax:match_expr_pattern(Node),
	  B = refac_syntax:match_expr_body(Node),
	  Ann = refac_syntax:get_ann(B),
	  case lists:keysearch(process, 1, Ann) of
	    {value, {process, Value}} ->
		P1 = refac_util:update_ann(P, {process, Value}),
		case refac_syntax:type(P) of
		  variable ->
		      Ann1 = refac_syntax:get_ann(P),
		      {value, {def, DefinePos}} = lists:keysearch(def, 1, Ann1),
		      Pid ! {add, {{def, DefinePos}, {process, Value}}};
		  _ -> ok                %%% What about the complex pattern matches?
		end,
		refac_syntax:copy_attrs(Node, refac_syntax:match_expr(P1, B));
	    _ -> Node
	  end;
      _ -> Node
    end.


start_counter_process() ->
     register(counter, spawn_link(refac_process_info, counter, [])).

counter() ->
     counter_loop({1,1}).

counter_loop({Spawn, Self}) ->
    receive
	{From, next_spawn} ->
	    From ! {counter, Spawn},
	    counter_loop({Spawn+1, Self});
	{From, next_self} ->
	    From ! {counter, Self},
	    counter_loop({Spawn, Self+1});
	stop ->
	    ok
    end.

%%% TO think: any other problems/restrictions with thus fun?
annotate_special_fun_apps({{ModName, FunName, Arity},FunDef}) ->
    start_counter_process(),
    Res = refac_util:full_buTP(fun do_annotate_special_fun_apps/2, FunDef, {ModName, FunName, Arity,FunDef}),
    counter ! stop,
    Res.


do_annotate_special_fun_apps(Node, {ModName, FunName, Arity,FunDef}) ->
    HandleSpawn = fun ({_Mod, _Fun, _Ar}, Args) ->
			  case Args of
			    [M, F, A] ->  
				  {_, M1} = evaluate_expr(M, FunDef),
				  {_, F1} = evaluate_expr(F, FunDef),
				  {_, A1} = evaluate_args(A, FunDef),
				  {M1, F1, A1};
			      [_N, M, F, A] ->
				  {_, M1} = evaluate_expr(M, FunDef),
				  {_, F1} = evaluate_expr(F, FunDef), 
				  {_, A1} = evaluate_args(A, FunDef),
				  {M1, F1, A1};
			      [_N, M, F, A, _O] ->
				  {_, M1} = evaluate_expr(M, FunDef),
				  {_, F1} = evaluate_expr(F, FunDef),
				  {_, A1} = evaluate_args(A, FunDef),
				  {M1, F1, A1};
			      [Fun] ->
				  case refac_syntax:type(Fun) of
				      fun_expr -> refac_prettypr:format(Fun);
				      _ -> unknown_initial_fun
				  end;
			      [_N, Fun] ->
				  case refac_syntax:type(Fun) of
				      fun_expr -> refac_prettypr:format(Fun);
				      _ -> unknown_initial_fun
				  end
			  end
		  end,
    case refac_syntax:type(Node) of
	application ->
	    case is_spawn_app(Node) of
		true ->
		    Operator = refac_syntax:application_operator(Node),
		    Ann = refac_syntax:get_ann(Operator),
		    {value, {fun_def, {Mod, Fun, Ari, _, _}}} = lists:keysearch(fun_def, 1, Ann),
		    Arguments = refac_syntax:application_arguments(Node),
		    InitialFun = HandleSpawn({Mod, Fun, Ari}, Arguments),
		    counter ! {self(), next_spawn},
		    receive
			{counter, N} -> N
		    end,
		    Node1 = refac_util:update_ann(Node,
						  {pid, {{spawn, ModName, FunName,Arity, N}, {initial_fun, [InitialFun]}}}),
		    io:format("Node:\n~p\n", [Node1]),
		    Node1;
		_ ->
		    case is_self_app(Node) of
			true ->
			    CurrentFun = {ModName, FunName, Arity},
			    counter ! {self(), next_self},
			    receive
				{counter, N} -> N
			    end,
			    %% TODO: NEED TO FIND OUT THE REAL INITIAL FUN.
			    Node1 = refac_util:update_ann(Node, {pid, {{self, ModName, FunName, Arity, N}, {initial_fun, []}}}),
			    io:format("Node:\n~p\n", [Node1]),
			    Node1;
			_ -> Node
		    end
	    end;
	_ -> Node
    end.



is_register_app(T) ->
    case refac_syntax:type(T) of
      application ->
	  Operator = refac_syntax:application_operator(T),
	  Ann = refac_syntax:get_ann(Operator),
	  case lists:keysearch(fun_def, 1, Ann) of
	    {value, {fun_def, {erlang, register, 2, _, _}}} -> true;
	    _ -> false
	  end;
      _ -> false
    end.

is_self_app(T) ->
    case refac_syntax:type(T) of
      application ->
	  Operator = refac_syntax:application_operator(T),
	  Ann = refac_syntax:get_ann(Operator),
	  case lists:keysearch(fun_def, 1, Ann) of
	    {value, {fun_def, {erlang, self, 0, _, _}}} -> true;
	    _ -> false
	  end;
      _ -> false
    end.

is_spawn_app(Tree) ->
    SpawnFuns1 = [{erlang, spawn, 1}, {erlang, spawn, 2}, {erlang, spawn, 3}, {erlang, spawn, 4},
		  {erlang, spawn_link, 1}, {erlang, spawn_link, 2}, {erlang, spawn_link, 3}, {erlang, spawn_link, 4},
		  {erlang, spawn_opt, 3}, {erlang, spawn_opt, 5}],
    SpawnFuns2 = [{erlang, spawn_monitor, 1}, {erlang, spawn_monitor, 3}, {erlang, spawn_opt, 2},
		  {erlang, spawn_opt, 4}],
    case refac_syntax:type(Tree) of
      application ->
	  Operator = refac_syntax:application_operator(Tree),
	  Ann = refac_syntax:get_ann(Operator),
	  case lists:keysearch(fun_def, 1, Ann) of
	    {value, {fun_def, {Mod, Fun, Arity, _, _}}} -> lists:member({Mod, Fun, Arity}, SpawnFuns1);
	    _ -> false
	  end;
      _ -> false
    end.

is_send_expr(Tree) ->
    SendFuns = [{erlang, send, 2}, {erlang, send, 3}, {erlang, send_after, 3}, {erlang, send_nosuspend, 2},
		{erlang, send_nosuspend, 3}],
    case refac_syntax:type(Tree) of
      infix_expr ->
	  Op = refac_syntax:infix_expr_operator(Tree),
	  case refac_syntax:type(Op) of
	    operator -> refac_syntax:operator_name(Op) == '!';
	    _ -> false
	  end;
      application ->
	  Operator = refac_syntax:application_operator(Tree),
	  Ann = refac_syntax:get_ann(Operator),
	  case lists:keysearch(fun_def, 1, Ann) of
	    {value, {fun_def, {Mod, Fun, Arity, _, _}}} -> lists:member({Mod, Fun, Arity}, SendFuns);
	    _ -> false
	  end;
      _ -> false
    end.

%% sort functions according to calling relationship and remove functions which are not process related.
sort_funs(Files) ->
    CallGraph = refac_util:build_call_graph(Files, []),
    #callgraph{scc_order = Sccs, external_calls = _E} = refac_callgraph:construct(CallGraph),
    CallerCallee = lists:map(fun ({{Caller, _CallerDef}, Callee}) -> {Caller, Callee} end, CallGraph),
    TrimmedSccs = trim_scc(Sccs, CallerCallee, [], []),
    lists:concat(TrimmedSccs).

trim_scc([], _CallerCallee, _PFunAcc, Acc) -> lists:reverse(Acc);
trim_scc([Scc | Sccs], CallerCallee, PFunAcc, Acc) ->
    SccFuns = lists:map(fun ({Fun, _FunDef}) -> Fun end, Scc),
    IsProcessScc = lists:any(fun ({_Fun, FunDef}) -> is_process_related_fun(FunDef) end, Scc),
    CalledFuns = lists:usort(lists:flatmap(fun (Fun) ->
						   case lists:keysearch(Fun, 1, CallerCallee) of
						     {value, {Fun, Called}} -> Called;
						     _ -> []
						   end
					   end,
					   SccFuns)),
    PFunsCalled = length(lists:subtract(CalledFuns, PFunAcc)) < length(CalledFuns),
    case IsProcessScc orelse PFunsCalled of
      true -> trim_scc(Sccs, CallerCallee, SccFuns ++ PFunAcc, [Scc | Acc]);
      _ -> trim_scc(Sccs, CallerCallee, PFunAcc, Acc)
    end.

is_process_related_fun(FunDef) ->
    ProcessFuns = [{erlang, register, 2}, {erlang, self, 0}, {erlang, spawn, 1}, {erlang, spawn, 2}, {erlang, spawn, 3},
		   {erlang, spawn, 4}, {erlang, spawn_link, 1}, {erlang, spawn_link, 2}, {erlang, spawn_link, 3}, {erlang, spawn_link, 4},
		   {erlang, send, 2}, {erlang, send, 3}, {erlang, send_after, 3}, {erlang, send_nosuspend, 2}, {erlang, send_nosuspend, 3}],
    F = fun (Node, _Others) ->
		case refac_syntax:type(Node) of
		  infix_expr ->
		      case is_send_expr(Node) of
			true -> {true, true};
			_ -> {[], false}
		      end;
		  receive_expr -> {true, true};
		  application ->
		      Operator = refac_syntax:application_operator(Node),
		      Arity = length(refac_syntax:application_arguments(Node)),
		      case refac_syntax:type(Operator) of
			atom ->
			    Op = refac_syntax:atom_value(Operator),
			    {value, {fun_def, {M, Op, A, _, _}}} = lists:keysearch(fun_def, 1, refac_syntax:get_ann(Operator)),
			    case lists:member({M, Op, A}, ProcessFuns) of
			      true -> {true, true};
			      _ -> {[], false}
			    end;
			module_qualifier ->
			    Mod = refac_syntax:module_qualifier_argument(Operator),
			    Body = refac_syntax:module_qualifier_body(Operator),
			    case {refac_syntax:type(Mod), refac_syntax:type(Body)} of
			      {atom, atom} ->
				  M = refac_syntax:atom_value(Mod),
				  Op = refac_syntax:atom_value(Body),
				  case lists:member({M, Op, Arity}, ProcessFuns) of
				    true -> {true, true};
				    _ -> {[], false}
				  end;
			      _ -> {[], false}
			    end;
			_ -> {[], false}
		      end;
		  _ -> {[], false}
		end
	end,
    case refac_util:once_tdTU(F, FunDef, []) of
      {_, false} -> false;
      {_R, true} -> true
    end.

start_fun_typesig_process(Funs) ->
    spawn(refac_process_info, fun_typesig, [Funs]).

fun_typesig(Funs) ->
    fun_typesig_loop([]).

fun_typesig_loop(Env) ->
    receive
	{From, get, Fun} ->
	    case lists:keysearch(Fun, 1, Env) of
		{value, {Fun, TypeSig}} -> 
		    From ! {self(), value, TypeSig};
		false -> From ! {self(), false}
	    end,
	    fun_typesig_loop(Env);
	{From, getenv} -> From ! {self(), Env}, fun_typesig_loop(Env);
	stop ->  
	    ok
    end.

   
start_env_process() ->
    Pid = spawn_link(refac_process_info, env, []), Pid.

env() -> env_loop([]).

env_loop(Env) ->
    receive
      {From, get, Key} ->
	  case lists:keysearch(Key, 1, Env) of
	    {value, {Key, Value}} -> From ! {self(), value, Value};
	    false -> From ! {self(), false}
	  end,
	  env_loop(Env);
      {add, {Key, Value}} ->
	  case lists:keysearch(Key, 1, Env) of
	    {value, {Key, _}} ->
		lists:keyreplace(Key, 1, Env, {Key, Value}), env_loop(Env);
	    false -> env_loop([{Key, Value} | Env])
	  end;
      stop ->
     	  ok
    end.





%%==========================================================================================
%% Given a function and an expression within the function body, this function computes the
%% program slice that could potentially affect the value of the expression.
%% If the resulted program slice is closed and only uses Erlang built-in function, we
%% are then able to calcuate the actual value of this expression.

%% Need to make user the last point of the execution of the slice result is Expr or {error, ...}.

evaluate_expr(Expr, FunDef) ->
    Exprs = case refac_util:get_free_vars(Expr) of 
		[] -> [Expr];
		_ -> backward_slice(Expr, FunDef)
	    end,
    try_evaluation(Exprs).

evaluate_args(A, FunDef) ->
    case refac_syntax:type(A) of 
	list ->
	    refac_syntax:list_length(A);
	_  -> 
	    Exprs = backward_slice(A, FunDef),
	    case try_evaluation(Exprs) of 
		{value, V} ->
		    case is_list(V) of 
			true ->{value, length(V)};
		        _ ->
			    {error, V}
		    end;
		_  ->
		    Expr1 = unfold_vars_in_expr(A, FunDef),
		    case refac_syntax:type(Expr1) of 
			list ->
			    {value, refac_syntax:list_length(Expr1)};
			_ -> case try_evaluation(Expr1) of 
				 {value, V} ->
				     case is_list(V) of
					 true -> {value, length(V)};
					 false ->
					     {error, V}
				     end;
				 _ -> {error, Expr1}
			     end
		    end 
	    end
    end.

try_evaluation(Exprs) ->
    case catch erl_eval:exprs(lists:map(fun (E) -> refac_syntax:revert(E) end, Exprs), []) of
      {value, V, _} -> {value, V};
      _ -> {error, Exprs}
    end.


backward_slice(Expr, FunDef) ->
    FunName = refac_syntax:function_name(FunDef),
    {S, E} = refac_util:get_range(Expr),
    FunClauses = refac_syntax:function_clauses(FunDef),
    Pred = fun (Node) ->
		   {StartPos, EndPos} = refac_util:get_range(Node),
		   S >= StartPos andalso E =< EndPos
	   end,
    %% Get the function clause to which the expression belongs.
    C = hd(lists:filter(fun (Clause) -> Pred(Clause) end, FunClauses)),
    C1 = process_a_clause(C, Expr),
    NewFun = refac_syntax:function(FunName, C1),
    %% to keep the annotation info correct.
    NewFun1 = refac_syntax_lib:annotate_bindings(refac_util:reset_attrs(NewFun), []),
    Body = refac_syntax:clause_body(hd(refac_syntax:function_clauses(NewFun1))),
    %% io:format("Body:\n~p\n", [Body]),
    rm_unused_exprs(Body).
    
process_a_clause(C, Expr) ->
    Patterns = refac_syntax:clause_patterns(C),
    Guard = refac_syntax:clause_guard(C),
    Body = refac_syntax:clause_body(C),
    NewBody = process_body(Body, Expr),
    FreeVars = refac_util:get_free_vars(Expr),
    case NewBody == [refac_syntax:tuple([refac_syntax:atom(error), refac_syntax:atom("Error with evaluation")])] of
      true -> [];
      _ ->
	  BoundVars = lists:concat(lists:map(fun (P) -> refac_util:get_bound_vars(P) end, Patterns)),
	  case FreeVars -- BoundVars =/= FreeVars of
	    true ->  %% Expr uses some of the vars declared in Patterns.
		C1 = refac_syntax:clause(Patterns, none, NewBody),
		{Bound1, Free1} = lists:foldl(fun (P, {Bd, Fr}) ->
						      {Bd1, Fr1} = {refac_util:get_bound_vars(P), refac_util:get_free_vars(P)},
						      {ordsets:union(Bd, Bd1), ordsets:union(Fr, Fr1)}
					      end,
					      {[], []}, Patterns),
		{Bound2, Free2} = lists:foldl(fun (E, {Bd, Fr}) ->
						      {Bd1, Fr1} = {refac_util:get_bound_vars(E), refac_util:get_free_vars(E)},
						      {ordsets:union(Bd, Bd1), ordsets:union(Fr, ordsets:subtract(Fr1, Bd))}
					      end,
					      {[], []}, NewBody),
		Bound = ordsets:union(Bound1, Bound2),
		Free = ordsets:union(Free1, ordsets:subtract(Free2, Bound1)),
		C2 = refac_util:update_ann(refac_util:update_ann(C1, {bound, Bound}), {free, Free}),
		[C2];
	    _ -> %% Expr does not use any of the vars declared in Patterns.
		{Bound, Free} = lists:foldl(fun (E, {Bd, Fr}) ->
						    {Bd1, Fr1} = {refac_util:get_bound_vars(E), refac_util:get_free_vars(E)},
						    {ordsets:union(Bd, Bd1), ordsets:union(Fr, ordsets:subtract(Fr1, Bd))}
					    end,
					    {[], []}, NewBody),
		C1 = refac_syntax:clause([refac_syntax:underscore()], none, NewBody),  %% replace patterns with undersocre.
		C2 = refac_util:update_ann(refac_util:update_ann(C1, {bound, Bound}), {free, Free}),
		[C2]
	  end
    end.

%% If Expr belongs to Body, then remove those expressions that will be evaluated after Expr, since 
%% those expressions do not contribute to the value of Expr.
process_body(Body, Expr) ->
    {S, E} = refac_util:get_range(Expr),
    FreeVars = refac_util:get_free_vars(Expr),
    FstExp = hd(Body),
    LstExp = lists:last(Body),
    {S1, _} = refac_util:get_range(FstExp),
    {_, E1} = refac_util:get_range(LstExp),
    case S1 =< S andalso E =< E1 of
      true ->
	  %% Expr is part of body.
	  case FreeVars of
	    [] -> Expr;        %% The selected expr does not has any free vars.
	    _ ->
		FreeVarDefLocs = lists:map(fun ({_V, DefLoc}) -> DefLoc end, FreeVars),
		LastLoc = lists:last(lists:sort(FreeVarDefLocs)),
		Exprs1 = lists:takewhile(fun (BodyExpr) ->
						 {StartPos, EndPos} = refac_util:get_range(BodyExpr),
						 (EndPos =< S) or (S >= StartPos andalso E =< EndPos)
					 end,
					 Body),
		LastExpr1 = lists:last(Exprs1), %% The expression that contains Expr.
		{LastExprStartPos, _} = refac_util:get_range(LastExpr1),
		LastExpr = case LastLoc >= LastExprStartPos of
			     false -> Expr;  %% The last expr does not declare any free vars of Expr
			     true -> %% some of the free vars in Expr are introduced in the LastExpr1.
				 %% This function needs to make sure Expr or {error, error with evalution} is 
				 %% the lasted expression to evaluate in the processed LastExpr1.
				 process_expr(LastExpr1, Expr)  %% LastExpr1 is a complex expr, such as case/if/receive exprs.
			   end,
		NewExprs = lists:reverse(tl(lists:reverse(Exprs1))) ++ [LastExpr], 
		rm_unused_exprs(NewExprs)
	  end;
      %% Expr is not part of Body.
      false -> [refac_syntax:tuple([refac_syntax:atom(error), refac_syntax:atom("Error with evaluation")])]
    end.

%% Expr is part of LastExpr. This function tries to simplify LastExpr to remove those parts that do not 
%% affect the value of Expr.
%% IMPORTANT:
%% 1) Make sure that the last evaluated expression is either Expr or {error, "Error with evaluation"}.
%% 2) The slicing process should not change the binding structure of variables.
process_expr(LastExpr, Expr) ->
    GetExprBody = fun (E) ->
			  case refac_syntax:type(E) of
			    match_expr -> get_match_expr_body(E);
			    _ -> E
			  end
		  end,
    E = GetExprBody(LastExpr),
    case refac_syntax:type(E) of
	case_expr ->
	    Args = refac_syntax:case_expr_argument(E),
	    {Bound1, Free1} = {refac_util:get_bound_vars(Args), refac_util:get_free_vars(Args)},
	    Clauses = refac_syntax:case_expr_clauses(E),
	    NewClauses = lists:concat(lists:map(fun (C) -> process_a_clause(C, Expr) end, Clauses)), %% process each case clause.
	    {Bound2, Free2} = lists:foldl(fun (C, {Bd, Fr}) ->
						  {Bd1, Fr1} = {refac_util:get_bound_vars(C), refac_util:get_free_vars(C)},
						  {ordsets:intersection(Bd, Bd1), ordsets:union(Fr, Fr1)}
					  end,
					  {[], []}, NewClauses),
	    Bound = ordsets:union(Bound1, Bound2),
	    Free = ordsets:union(Free1, Free2),
	    E1 = refac_syntax:case_expr(Args, NewClauses),
	    %% updated the annotation.
	    E2 = refac_util:update_ann(refac_util:update_ann(E1, {bound, Bound}), {free, Free}),
	    E2;
	block_expr ->
	    Body = refac_syntax:block_expr_body(E),
	    NewBody = process_body(Body, Expr),
	    {Bound, Free} = lists:foldl(fun (E, {Bd, Fr}) ->
						{Bd1, Fr1} = {refac_util:get_bound_vars(E), refac_util:get_free_vars(E)},
						{ordsets:union(Bd, Bd1), ordsets:union(Fr, ordsets:subtract(Fr1, Bd))}
					end,
					{[], []}, NewBody),
	    BE = refac_syntax:block_expr(NewBody),
	    refac_util:update_ann(refac_util:update_ann(BE, {bound, Bound}), {free, Free});
	if_expr ->
	    Clauses = refac_syntax:if_expr_clauses(E),
	    NewClauses = lists:concat(lists:map(fun (C) -> process_a_clause(C, Expr) end, Clauses)),
	    {Bound, Free} = lists:foldl(fun (C, {Bd, Fr}) ->
						  {Bd1, Fr1} = {refac_util:get_bound_vars(C), refac_util:get_free_vars(C)},
						  {ordsets:intersection(Bd, Bd1), ordsets:union(Fr, Fr1)}
					  end,
					  {[], []}, NewClauses),
	    IE = refac_syntax:if_expr(NewClauses),          
	    refac_util:update_ann(refac_util:update_ann(IE, {bound, Bound}), {free, Free});
%%	receive_expr -> LastExpr;
	%%fun_expr ->  %% IMPORTANT: fun exprs need more attection, as it is a function closure. 
	%% lists comprehension is another problem. (find the example !!)
	%% catch_expr ->
	%% Any other possibilities?
	_ -> refac_syntax:tuple([refac_syntax:atom(error), refac_syntax:atom("Error with evaluation")])
    end.


%% this is the function that does the backward slicing.
rm_unused_exprs([]) -> [];
rm_unused_exprs(Exprs) ->
    %%io:format("Initial:\n"),
    %%io:format(lists:concat(lists:map(fun(B) ->refac_prettypr:format(B) end, Exprs))),
    LastExpr = lists:last(Exprs),
    FreeVars = refac_util:get_free_vars(LastExpr),
    %%io:format("LastExpr:\n~p\n", [refac_prettypr:format(LastExpr)]),
    %%io:format("FreeVars:\n~p\n", [FreeVars]),
    ReversedPrevExprs = tl(lists:reverse(Exprs)),
    Res = rm_unused_exprs_1(ReversedPrevExprs, FreeVars, [LastExpr]),
    %%io:format("Result:\n"),
    %%io:format(lists:concat(lists:map(fun(B) ->refac_prettypr:format(B) end, Res))),
    Res.

rm_unused_exprs_1([], _FreeVars, Acc) -> Acc;
rm_unused_exprs_1([E | Exprs], FreeVars, Acc) ->
    ExportedVars = refac_util:get_var_exports(E),
    case FreeVars -- ExportedVars =/= FreeVars of
      true ->
	  FreeVarsInE = refac_util:get_free_vars(E),
	  NewFreeVars = lists:usort((FreeVars -- ExportedVars) ++ FreeVarsInE),
	  rm_unused_exprs_1(Exprs, NewFreeVars, [E | Acc]);
      false -> rm_unused_exprs_1(Exprs, FreeVars, Acc)
    end.


%%================================================================================================
unfold_vars_in_expr(Expr, FunBody) ->
    FreeVars = refac_util:get_free_vars(Expr),
    case FreeVars of 
	[] ->
	    Expr;
	[V|Vars] -> case unfold_a_var_in_expr(V, Expr, FunBody) of 
			 {expr, Expr1} ->
			    case Expr1 =/= Expr of 
				true ->
				    unfold_vars_in_expr(Expr1, FunBody);
				_ -> Expr
			    end;
			 {error, _} ->
			    Expr
		    end
    end.

unfold_a_var_in_expr(Var, Expr, FunBody) ->    
    case refac_util:once_tdTU(fun var_to_match_body/2, FunBody, Var) of 
	{_, false} -> {error, match_expr_unfound};
	{B, true} -> 
	    {expr, replace_var_with_body(Var, B, Expr)}
    end.

replace_var_with_body(Var, B, Expr) ->
    refac_util:full_buTP(fun do_replace_var_with_body/2, Expr, {Var, B}).

do_replace_var_with_body(Node, {{VarName, DefPos}, B}) ->
    case refac_syntax:type(Node) of 
	variable ->
	    As = refac_syntax:get_ann(Node),
	    case lists:keysearch(def, 1, As) of 
		{value, {def, DefPos1}} ->
		    case [DefPos] == DefPos1 of 
			true ->
			    B;
			_ -> Node
		    end;
		_ -> Node
	    end;
	_ -> Node
    end.

var_to_match_body(Node, Var) ->
    case refac_syntax:type(Node) of 
	match_expr ->
	    P = refac_syntax:match_expr_pattern(Node),
	    case refac_syntax:type(P) of 
		variable -> 
		    case hd(refac_util:get_var_exports(P)) of 
			Var -> {refac_syntax:match_expr_body(Node), true};
			_ -> {[], false}
		    end;		    
		_ -> {[], false}
	    end;
	_ -> {[], false}
    end.

get_match_expr_body(E) ->
    Body = refac_syntax:match_expr_body(E),
    case Body of
      match_expr -> get_match_expr_body(Body);
      _ -> Body
    end.



