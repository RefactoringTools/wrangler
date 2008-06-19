%% =====================================================================
%% Refactoring: Rename a variable name.
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
%% =====================================================================
%%
%% @doc This refactoring register a process id, Pid say, with a name, RegName say, and replace
%% the uses of Pid ! Msg with  RegName ! Msg if possible.
%% @end
-module(refac_register_pid).

-export([register_pid/5, register_pid_eclipse/5]).

-export([sliced_funs/0]).  

-include("../hrl/wrangler.hrl").


%% This function associates a name, which must be an atom, with a pid, and replaces the uses of this pid with the process process in the send operator.
%% 
%% Rationale for this refactoring: If we want to send a message to a process, then we need to know its PID. This is often inconvenient since the 
%% PID has to be sent to all the processes in the system that want to comminicate with this process. Registering a process with a name allows any 
%% process in the system to  communicate with this process without knowing its PID.
%%
%% To perform this refactoring, the uses needs to select a match expression whose righthand is a spawn/self expression, and whose lefthand side 
%% is a Pid variable.
%%
%%
%% Side conditions for this refactorings:
%% 1. The process name provided by the uses should be lexically valid. (no problem).
%% 2. The process name should not have been used as a process name. (need to handle dynamically create process names)
%% 2. The same process cannot be registered more than once. (check the registration og the generated Pid, and self()).
%% 3. At any time during the running of the system, only one process can be associated with a particular process name.  Since statically we 
%% cannot decide when a process starts/dies, we use a rather stronger condition, that is the function containing the registration is only 
%% called once. 
%%   3.1) the function should not be a recursive function either directly or indirectly.
%%   3.2) the function is only called at one place of the program.
%%   3.3) the function should not be used in a map/fold.
%%   3.4) the registeration should not be in a receive expression.

%% To do the transformation, Wrangler needs to know, for each Pid!Msg expression, where the Pid is spawned. This Pid is only replacable if 
%% it is only assoicated with the spawn expression selected by the user.

%% Why do I need slicing? mosting for reduced the number of unclear registration expressions.

%% =====================================================================
%% @spec rename_var(FileName::filename(), Line::integer(), Col::integer(), NewName::string(),SearchPaths::[string()])-> term()
%%
register_pid(FName, Start, End, RegName,  SearchPaths) ->
    register_pid(FName, Start, End, RegName, SearchPaths, emacs).

register_pid_eclipse(FName, Start, End, RegName, SearchPaths) ->
    register_pid(FName, Start, End, RegName, SearchPaths, eclipse).

register_pid(FName, Start, End, RegName, SearchPaths, Editor) ->
    io:format("\n[CMD: register_pid, ~p, ~p, ~p, ~p,~p]\n",  [FName, Start, End, RegName, SearchPaths]),
    case refac_util:is_fun_name(RegName) of
	true ->
	    case refac_util:parse_annotate_file(FName, false, SearchPaths) of   %% preprocessor is not bypassed.
		{ok, {_AnnAST, Info}} ->
		    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
		    RegName1 = list_to_atom(RegName), 
		    case refac_util:parse_annotate_file(FName, true, SearchPaths) of  %% preprocess is bypassed.
			{ok, {AnnAST2, _Info1}} ->
			    AnnAST3 = refac_annotate_pid:ann_pid_in_a_file(FName),  %% Interface needs to be changed!
			    case pos_to_match_expr(AnnAST3, Start, End) of
				{ok, MatchExpr} ->
				    case pre_cond_check(FName, ModName,AnnAST3, Start, MatchExpr, RegName1, Info) of 
					ok -> 
					    Pid = refac_syntax:match_expr_pattern(MatchExpr),
					    case do_register(AnnAST3, MatchExpr, Pid, RegName1) of 
						{ok, AnnAST4} ->
						    case Editor of 
							emacs ->
							    refac_util:write_refactored_files([{{FName, FName}, AnnAST4}]),
							    {ok, [FName]};
							eclipse ->
							    {ok, [{FName, FName, refac_prettypr:print_ast(AnnAST4)}]}
						    end;
						{error, Reason} -> {error, Reason}
					    end;						    
					{registered, Regs} -> {registered, Regs};   %% CHECK THIS!!!
					{error, Reason} -> {error, Reason}
				    end;
				{error,Reason} -> 
				    {error, Reason}
			    end;
			{error, Reason} -> {error, Reason}
		    end;
		{error, Reason} -> {error, Reason}
	    end;
	false -> {error, "Invalid process name."}
    end.

pos_to_match_expr(AnnAST, Start, End) ->
    Message = "You have not selected a match expression whose left-hand side is a PID, and right-hand side is a spawn expression!",
    case refac_util:pos_to_expr(AnnAST, Start, End) of 
	{ok, Expr} ->
	    case refac_syntax:type(Expr) of 
		match_expr -> 
		    P = refac_syntax:match_expr_pattern(Expr),
		    B = refac_syntax:match_expr_body(Expr),
		    case {is_spawn_app(B), refac_syntax:type(P) == variable} of 
			{true, true} ->
			    {ok, Expr};
			_ -> {error, Message}
		    end;
		_ -> {error, Message}
	    end;
	_ -> {error, Message}    
    end.


%% pre_cond_check(FileName, ModName, AnnAST, Start, MatchExpr, RegName, Info) ->
%%     MatchExprBody = refac_syntax:match_expr_body(MatchExpr),
%%     {ok, FunDef} = refac_util:pos_to_fun_def(AnnAST, Start),
%%     case process_spawn(FileName, AnnAST, Info, FunDef, MatchExprBody) of 
%% 	{ok, ReachedFuns} ->
%% 	    io:format("ReachedFuns:\n~p\n", [ReachedFuns]);
%% 	{error, Reason} -> {error, Reason}
%%     end.
	

%%     RegExprs = check_registered_process(FileName, ModName, AnnAST, Start, MatchExpr),
%%     case RegExprs of 
%% 	[] ->
%% 	    ok;
%% 	_ -> io:format("\nThe following registration of processes might cause conflict, please check!\n"),
%% 	     lists:map(fun(Reg) -> {StartPos,_} = refac_util:get_range(Reg),
%% 				   io:format("Location:~p, ~p:  ", [FileName, StartPos]),
%% 				   io:format(refac_prettypr:format(Reg)++"\n") end, RegExprs),
%% 	     {registered, []}
%%     end.

%% TODO: ALSO NEED TO CHECK the slice of self().

pre_cond_check(FileName, ModName, AnnAST, Start, MatchExpr, RegName, Info) ->		 
    Res = check_registered_process(FileName, ModName, AnnAST, Start, MatchExpr),
    io:format("Register results:\n~p\n", [Res]),
    case Res of 
	 ok ->
	     ok;
	{registered, RegExpr} -> {{Line,_Col}, _} = refac_util:get_range(RegExpr),
				 io:format("Line:\n~p\n", [RegExpr]),
				 {error, "The selected process is already registered at line "++ integer_to_list(Line)};
	{unsure, RegExprs} ->
	    io:format("\nThe following registration of processes might cause conflict, please check!\n"),
	    lists:map(fun(Reg) -> {StartPos,_} = refac_util:get_range(Reg),
				  io:format("\nLocation: (~p, ~p):  ", [FileName, StartPos]),
				  io:format(refac_prettypr:format(Reg)++"\n") end, RegExprs),
	    {registered, RegExprs}
    end.
    
check_registered_process(FileName, ModName, AnnAST, Start, MatchExpr) ->
    start_env_process(),
    {ok, FunDef} = refac_util:pos_to_fun_def(AnnAST, Start),
    Res = forward_slice([FileName], AnnAST, ModName, {FunDef, MatchExpr}),
    io:format("Slcing Result:\n~p\n", [Res]),
    RegExprs = collect_register_exprs(Res),
    stop_env_process(),
    Pid = refac_syntax:match_expr_pattern(MatchExpr),
    {value, {pid, PidInfo}} = lists:keysearch(pid,1, refac_syntax:get_ann(Pid)),
    case RegExprs of 
	[] ->
	    ok;
	_ -> RegExprs1 = lists:filter(fun(R) ->
					      [RegName, Pid1] = refac_syntax:application_arguments(R),
					      case lists:keysearch(pid,1, refac_syntax:get_ann(Pid1)) of 
						  {value, {pid, PidInfo1}}->
						      PidInfo -- PidInfo1 =/= PidInfo;
						  _ -> false
					      end
				      end, RegExprs),
	     case RegExprs1 of 
		 [] ->{unsure, RegExprs};
		 _ -> {registered, RegExprs1}
	     end
    end. 
	

collect_register_exprs(Funs) ->
    F = fun(Node, Acc) ->
		case refac_syntax:type(Node) of 
		    application ->
			case is_register_app(Node) of 
			    true ->
				[Node |Acc];
			    _ -> Acc
			end;
		    _ -> Acc
		end
	end,			   
    lists:flatmap(fun({Key, FunDef}) ->refac_syntax_lib:fold(F, [], FunDef) end, Funs).
    
is_direct_recursive_fun(ModName, FunName, Arity, FunDef) ->
    F = fun(Node, {ModName, FunName, Arity}) ->
		case refac_syntax:type(Node) of 
		    application ->
			Op = refac_syntax:application_operator(Node),
			case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of 
			    {value, {fun_def, {ModName, FunName, Arity, _, _}}} ->
				{true, true};
			    _ -> {[],false}
			end;
		    _ -> {[], false}
		end
	end,	   
    R = refac_util:once_tdTU(F,  FunDef, {ModName, FunName, Arity}),
    case R of 
	{_, true} ->
	     true;
	_ -> false
    end.
is_recursive_fun(Files, {ModName, FunName, Arity, FunDef}) ->
    case is_direct_recursive_fun(ModName, FunName, Arity, FunDef) of 
	true -> 
	    true;
	false ->
	    CallGraph= refac_util:build_call_graph(Files, []),
	    #callgraph{scc_order = Sccs, external_calls = _E} = refac_callgraph:construct(CallGraph),
	    Sccs1 =[[Fun||{Fun, _FunDef}<-Scc]||Scc<-Sccs],
	    lists:any(fun(E)-> (length(E)>1) andalso (lists:member({ModName, FunName, Arity}, E)) end,
		      Sccs1)   
    end.
	
   
collect_reg_info(Files, ModName, AnnAST) ->
    F = fun (Node, {Acc, FunDef}) ->    
		case refac_syntax:type(Node) of 
		    application ->
			case is_register_app(Node) of 
			    true -> 
				[RegName, Pid] = refac_syntax:application_arguments(Node),
				Res = evaluate_expr(Files, ModName, AnnAST, FunDef, RegName),
				case Res of 
				    {value, V} -> {[V | Acc], FunDef};
				    _ -> {[RegName | Acc], FunDef}
				end;		    
			    _ -> {Acc, FunDef}
			end; 
		    _ ->  {Acc,FunDef}
		end
	end,
    F1 = fun(Node, Acc) ->
		case refac_syntax:type(Node) of 
		    function ->
			{Acc1, _} =refac_syntax_lib:fold(F, {Acc, Node}, Node),
			Acc1;
		    _-> Acc 
		end
	end,
    Acc =refac_syntax_lib:fold(F1, [], AnnAST),
    lists:usort(Acc).


do_register(AnnAST, MatchExpr, Pid, RegName) ->
    Ann = refac_syntax:get_ann(Pid),
   {value, PidInfo} = lists:keysearch(pid, 1, Ann),
   {AnnAST1, Modified} = add_register_expr(AnnAST, MatchExpr, RegName),
   case Modified of 
       true ->  Res = refactor_send_exprs(AnnAST1, PidInfo, RegName),
	       {ok, Res};	    
       _ -> {error, "Wrangler failed to add the registration expression."}
   end.
 


refactor_send_exprs(AnnAST, PidInfo, RegName) ->
    refac_util:full_buTP(fun do_refactor_send_exprs/2, AnnAST, {PidInfo, RegName}).

do_refactor_send_exprs(Node, {PidInfo, RegName}) ->
    case refac_syntax:type(Node) of 
	infix_expr -> Dest = refac_syntax:infix_expr_left(Node),
		      Op = refac_syntax:infix_expr_operator(Node),
		      Msg = refac_syntax:infix_expr_right(Node),
		      Ann = refac_syntax:get_ann(Dest),
		      case lists:keysearch(pid, 1, Ann) of 
			  {value, PidInfo} ->
			      Node1 = refac_syntax:infix_expr(refac_syntax:atom(RegName), Op, Msg),
			      refac_syntax:copy_attrs(Node, Node1);
			  _ -> Node
		      end;
	_  -> Node %% TODO: Need to handle uses of send/2.
    end.
			  
	    
    

add_register_expr(AnnAST, MatchExpr,RegName) ->
    Pid = refac_syntax:match_expr_pattern(MatchExpr),
    RegExpr = refac_syntax:application(refac_syntax:atom(register),
				       [refac_syntax:atom(RegName), Pid]),
    refac_util:stop_tdTP(fun do_add_register_expr/2, AnnAST, {MatchExpr, RegExpr}).
    
    


do_add_register_expr(Node, {MatchExpr, RegExpr}) ->
    F = fun(Body) ->
		lists:flatmap(fun(E) -> case E == MatchExpr of 
					   true -> [E, RegExpr];
					    _ -> [E]
					end
			      end, Body)
	end,
    case refac_syntax:type(Node) of 
	clause -> 
	    P = refac_syntax:clause_patterns(Node),
	    B = refac_syntax:clause_body(Node),
	    G = refac_syntax:clause_guard(Node),
	    B1 = F(B),
	    case length(B1) == length(B) of
		true -> {Node, false};
		_ -> Node1 = refac_syntax:clause(P,G, B1),
		     {Node1, true}
	    end;
	_  -> {Node, false}
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


returns_undefined(FunDef) ->
    F= fun(C) ->
	       Body = refac_syntax:clause_body(C),
	       Expr = lists:last(Body),
	       case refac_syntax:type(Expr) of 
		   atom -> refac_syntax:atom_value(Expr) == undefined;
		   _ -> false
	       end
       end,
    FunClauses = refac_syntax:function_clauses(FunDef),
    lists:all(fun(C) ->
		      F(C) end, FunClauses).
    
    
start_env_process() ->
    
    case erlang:whereis(sliced_funs) of 
	undefined -> ok;
	_         -> erlang:unregister(sliced_funs)
    end,
    register(sliced_funs, spawn_link(refac_register_pid, sliced_funs, [])).

stop_env_process() ->
    sliced_funs!stop.

get_all_sliced_funs() ->
    sliced_funs!{self(), get_all},
    receive 
	{sliced_funs, State} ->
	    State
    end.
	

sliced_funs() ->
    sliced_funs([]).

sliced_funs(State) ->
    receive
	{From, get, Key} ->
	    case lists:keysearch(Key, 1, State) of
		{value, {Key, Value}} ->
		    From ! {sliced_funs, value, Value};
		false -> From ! {sliced_funs, false}
	    end,
	    sliced_funs(State);
	{add, {Key, Value}} ->
	    case lists:keysearch(Key, 1, State) of 
		    {value, {Key, _}} ->  %% This should not happen.
			State1 =lists:keyreplace(Key,1, State, {Key, Value}), 
			sliced_funs(State1);
		    false -> sliced_funs([{Key, Value}|State])
		end;
	 {From, get_all} ->
	    io:format("\nAll sliced funs:\n"),
	    lists:map(fun({Key, Value}) ->
			      io:format(refac_prettypr:format(Value)++"\n\n"),
			      io:format("Fun:\n~p\n", [Value]) end, State),
	    From ! {sliced_funs, State},
	    sliced_funs(State);
	 stop ->
		ok
	end.



%% Forward slicing: returns the parts of the program that are potentially affected by the value of the selected expression.
%% TODO: think about how to handle implicit functions, recursive functions and message passing.

forward_slice(Files, AnnAST, ModName, {FunDef, Expr}) ->
    FunName = refac_syntax:function_name(FunDef),
    FunName1 = refac_syntax:data(FunName),
    Arity = refac_syntax:function_arity(FunDef),
    FunClauses = refac_syntax:function_clauses(FunDef),
    NewFunClauses = lists:map(fun(Cs) ->
			    process_a_clause(AnnAST, ModName,Cs, Expr) end, FunClauses),
    NewFunDef = refac_syntax:function(FunName, NewFunClauses),
    sliced_funs ! {add, {{ModName, FunName1, Arity}, NewFunDef}},
    case returns_undefined(NewFunDef) of 
	true ->    %% None of the variables depending on the selected expression is exported.
	    get_all_sliced_funs();
	false -> CallerFuns = get_caller_funs(Files, {ModName, FunName1, Arity}),
		 F = fun(T,Acc) -> case refac_syntax:type(T) of 
				       application ->
					   Op = refac_syntax:application_operator(T),
					   Ann = refac_syntax:get_ann(Op),
					   case lists:keysearch(fun_def,1, Ann) of
					       {value, {fun_def, {ModName, FunName1, Arity, _, _}}} ->
						   [T|Acc];
					       _ ->  Acc
					   end;
				       _ -> Acc
				   end
		     end,	 
		 case CallerFuns of 
		     [] -> [];
		     _ ->  SliceCriterion = lists:flatmap(fun(FunDef) -> AppExprs = refac_syntax_lib:fold(F, [], FunDef),
								     lists:map(fun(E) -> {FunDef, E} end, AppExprs)
						      end, CallerFuns),
			   %% io:format("SliceCriterion:\n~p\n", [SliceCriterion]),
			   lists:flatmap(fun(SC) ->forward_slice(Files, AnnAST, ModName,SC) end, SliceCriterion)
		 end		 
    end.



get_caller_funs(Files, {ModName, FunName, Arity}) ->
    CallGraph = refac_util:build_call_graph(Files, []),
    #callgraph{scc_order = Sccs, external_calls = _E} = refac_callgraph:construct(CallGraph),
    lists:flatmap(fun ({{Caller, CallerDef}, Callee}) -> 
			  case lists:member({ModName, FunName, Arity}, Callee) of
			      true -> [CallerDef];
			      _ -> []
			  end
		  end, CallGraph).
   
%% process_a_clause(AnnAST, ModName, C, {app, {ModName, FunName, Arity}}) ->
%%     Patterns = refac_syntax:clause_patterns(C),
%%     Body = refac_syntax:clause_body(C),
%%     ExportedVars = get_exported_vars(C, {ModName, FunName, Arity}),
%%     Body1 = rm_unrelated_exprs(AnnAST, ModName, Body, ExportedVars),
%%     refac_syntax:clause(Patterns, none, Body1); 


process_a_clause(AnnAST, ModName,C, Expr) ->
    ExportedVars = get_exported_vars(C, Expr),
    Patterns = refac_syntax:clause_patterns(C),
    Body = refac_syntax:clause_body(C),
    Body1 = rm_unrelated_exprs(AnnAST, ModName, Body, ExportedVars),
    io:format("Exported Vars:\n~p\n", [ExportedVars]),
    refac_syntax:clause(Patterns, none, Body1).

get_exported_vars(Body, Expr) ->
    F1 =fun(Node, _Others) ->
		case Node of 
		    Expr ->
			{true, true};
		    _ ->
			{[], false}
		end
	end,		    
    F = fun(T,S) ->
		case refac_syntax:type(T) of 
		    match_expr ->                              %% TO THINK: assume only match expressions export variables, is this correct?
			case refac_util:once_tdTU(F1, T, []) of 
			    {_, true} -> 
				S++refac_util:get_var_exports(T);
			    _ -> S
			end;
		    _ -> S
		end
	end,
    refac_syntax_lib:fold(F, [], Body).

rm_unrelated_exprs(AnnAST, ModName, [], Vars) ->
    [];
rm_unrelated_exprs(AnnAST, ModName, [E], Vars) ->
    FreeVars = refac_util:get_free_vars(E),
    ExportedVars = refac_util:get_var_exports(E),
    case (ExportedVars -- Vars =/= ExportedVars) of 
	true -> [E];
	false -> case FreeVars -- Vars =/= FreeVars of 
		     true ->
			 [E];
		     _ ->
			 [refac_syntax:atom(undefined)]
		 end
    end;
rm_unrelated_exprs(AnnAST, ModName,[E |Exprs], Vars) ->
    FreeVars = refac_util:get_free_vars(E),
    ExportedVars = refac_util:get_var_exports(E),
    case (ExportedVars -- Vars =/= ExportedVars) of 
	true -> 
	    [E | rm_unrelated_exprs(AnnAST, ModName,Exprs, lists:usort(Vars++ExportedVars))];
	false -> case FreeVars -- Vars =/= FreeVars of 
		     true ->
			 Env = refac_util:get_env_vars(E),
			 E1 = process_fun_applications(AnnAST, ModName, E, Vars),
			 E2 = refac_syntax_lib:annotate_bindings(refac_util:reset_attrs(E1), Env),
			 FreeVars1 = refac_util:get_free_vars(E2),
			 case FreeVars1 --Vars =/= FreeVars1 of 
			     true ->
				 [E2 | rm_unrelated_exprs(AnnAST, ModName, Exprs, lists:sort(Vars++refac_util:get_var_exports(E2)))];
			     _ -> rm_unrelated_exprs(AnnAST, ModName, Exprs, Vars)
			 end;
		     _ ->
			 rm_unrelated_exprs(AnnAST, ModName, Exprs, Vars)
		 end
    end.

	    
  
intra_fun_forward_slice(AnnAST, ModName, FunDef, PatIndex) ->
    FunName = refac_syntax:function_name(FunDef),
    Cs = refac_syntax:function_clauses(FunDef),
    Cs1 = lists:map(fun(C) ->
			    process_a_clause_1(AnnAST, ModName,C, PatIndex) end, Cs),
    refac_syntax:function(FunName, Cs1).

process_a_clause_1(AnnAST, ModName, C, PatIndex) ->
    Patterns = refac_syntax:clause_patterns(C),
    Body = refac_syntax:clause_body(C),
    Vars =refac_util:get_var_exports(lists:nth(PatIndex, Patterns)),
    Body1 = process_fun_body(AnnAST, ModName, Body, Vars),
    refac_syntax:clause(Patterns, none, Body1).    

process_fun_body(AnnAST, ModName, [], Vars) ->			    
    [];
process_fun_body(AnnAST, ModName, [E], Vars) ->
    E1 = process_fun_applications(AnnAST, ModName, E, Vars),
    FreeVars = refac_util:get_free_vars(E1),
    ExportedVars = refac_util:get_var_exports(E1),
    case (ExportedVars -- Vars =/= ExportedVars) of 
	true -> [E];
	false -> case FreeVars -- Vars =/= FreeVars of 
		     true ->
			   %% check free/exported vars again?
			 [E1];
		     _ ->
			 [refac_syntax:atom(undefined)]
		 end
    end;
process_fun_body(AnnAST, ModName,[E |Exprs], Vars) ->
    E1 = process_fun_applications(AnnAST, ModName, E, Vars),
    FreeVars = refac_util:get_free_vars(E1),
    ExportedVars = refac_util:get_var_exports(E1),
    case (ExportedVars -- Vars =/= ExportedVars) of 
	true -> [E | process_fun_body(AnnAST, ModName,Exprs, lists:usort(Vars++ExportedVars))];
	false -> case FreeVars -- Vars =/= FreeVars of 
		     true ->
			 [E1 | process_fun_body(AnnAST, ModName, Exprs, lists:sort(Vars++refac_util:get_var_exports(E1)))];
		     _ ->
			 process_fun_body(AnnAST, ModName, Exprs, Vars)
		 end
    end.


process_fun_applications(AnnAST, ModName, E, Vars) ->
    refac_util:full_buTP(fun do_process_fun_applications/2, E, {AnnAST, ModName, Vars}).

do_process_fun_applications(Node, {AnnAST, ModName, Vars}) ->
    case refac_syntax:type(Node) of 
	application ->
	    FreeVars = refac_util:get_free_vars(Node),
	    case FreeVars -- Vars =/= FreeVars of   %% the function application makes use of some of the variables in Vars;
		true -> Operator = refac_syntax:application_operator(Node),
			Ann = refac_syntax:get_ann(Operator),
			case lists:keysearch(fun_def, 1, Ann) of 
			    {value, {fun_def, {ModName, F, A, _, DefPos}}} ->   %% TO CHANGE. temperoary only check functions defined in this module;
				sliced_funs ! {self(), get, {ModName, F, A}},
				receive 
				    {value, {{ModName, F, A}, FunDef1}} ->
					case returns_undefined(FunDef1) of 
					    true ->refac_syntax:atom(undefined);
					    _ ->Node
					end;
				    _ -> case refac_util:pos_to_fun_def(AnnAST, DefPos) of 
					     {ok, FunDef} -> FunDef1= intra_fun_forward_slice(AnnAST, ModName, FunDef, 1), %% TOCHANGE: use 1 temporally;
							     sliced_funs ! {add, {{ModName, F, A}, FunDef1}},
							     case returns_undefined(FunDef1) of 
								 true -> refac_syntax:atom(undefined);
								 _ -> Node
							     end;
					     _ -> Node
					 end
				end;
			    _ -> Node
			end;			    
		_  -> refac_syntax:atom(undefined)
	    end;
	_ -> Node
    end.

evaluate_expr(Files, ModName, AnnAST, FunDef, Expr) ->
    Exprs = case refac_util:get_free_vars(Expr) of 
		[] -> [Expr];
		_ -> Res = backward_slice(Files, AnnAST, ModName, FunDef, Expr),
		     %%io:format("Res:\n~p\n", [Res]),
		     case Res of 
			 [] -> Res;
			 _ ->
			     %%io:format("backward slice result:\n~p\n", [hd(Res)]),
			     hd(Res)   %% NEED TO CHANGE TO TAKE ALL LIST ELEMENTS INTO ACCOUNT.		    
		     end
	    end,
    Value = refac_process_info:try_evaluation(Exprs),
    Value.





backward_slice(Files,AnnAST, ModName, FunDef, Expr) ->
    FunName = refac_syntax:data(refac_syntax:function_name(FunDef)),
    Arity= refac_syntax:function_arity(FunDef),
    NewFunDef1 = refac_process_info:backward_slice(Expr, FunDef),
    NewFunDef2 = unfold_fun_defs(Files, AnnAST, ModName, NewFunDef1),
    C = hd(refac_syntax:function_clauses(NewFunDef2)),
    Body = refac_syntax:clause_body(C),
    Patterns = refac_syntax:clause_patterns(C),
    {_Bound2, FreeVarsInBody} = lists:foldl(fun (E, {Bd, Fr}) ->
						    {Bd1, Fr1} = {refac_util:get_bound_vars(E), refac_util:get_free_vars(E)},
						    {ordsets:union(Bd, Bd1), ordsets:union(Fr, ordsets:subtract(Fr1, Bd))}
					    end,
					    {[], []}, Body),
    case FreeVarsInBody of 
	[] ->
	    [Body];
	_ -> 
	    SlicePoints = collect_app_sites(AnnAST, ModName, FunName, Arity),
	    %% io:format("SlicesPoints:\n~p\n", [SlicePoints]),
	    Slices = lists:flatmap(fun({Fun, E}) ->
				       backward_slice(Files, AnnAST, ModName, Fun, E) end, SlicePoints),
	    %% io:format("Slices:\n~p\n", [Slices]),
	    FunExpr = refac_syntax:fun_expr(
			[refac_syntax:clause(lists:map(fun({V, _}) -> refac_syntax:variable(V) end, FreeVarsInBody), none, Body)]),  
	    %% IMPORTANT: ORDER OF VARIABLES MATTERS HERE.
	    Res = lists:map(fun(S) ->
				    [refac_syntax:application(FunExpr, S)] end, Slices),
	    Res
    end. 
		 
	    
collect_app_sites(AnnAST, ModName, FunName, Arity) ->    			
    F1 = fun(T,Acc) ->
		case refac_syntax:type(T) of 
		    application ->
			Op = refac_syntax:application_operator(T),
			Ann = refac_syntax:get_ann(Op),
			case lists:keysearch(fun_def,1, Ann) of
			    {value, {fun_def, {ModName, FunName, Arity, _, _}}} ->
				Args = refac_syntax:application_arguments(T),
				case Args of 
				    [] -> Acc;
				    _ -> Acc ++ [hd(Args)]  %% TODO: THIS NEED TO BE CHANGED, TEMORALLY ASSUME THE FIRST ARGUMENT.
				    end;
			    _ ->
				Acc
			end;
		    _ -> Acc
		end
	 end,			
    F = fun(T, Acc) ->
		case refac_syntax:type(T) of 
		    function ->
		       Acc1 = refac_syntax_lib:fold(F1,[], T),
		       case Acc1 of 
			   [] -> Acc;
			   _ -> Acc ++ lists:map(fun(E) -> {T, E} end, Acc1)
		       end;
		    _ -> Acc
		end
	end,
    %%io:format("Slice\n~p\n", [{ModName, FunName, Arity}]),
    refac_syntax_lib:fold(F, [], AnnAST).
			

unfold_fun_defs(Files, AnnAST, ModName, FunDef) -> %% How about recursive functions?
    F = fun(Node, _Others) ->
		case refac_syntax:type(Node) of 
		    application ->
			Operator = refac_syntax:application_operator(Node),
			Ann = refac_syntax:get_ann(Operator),
			%%io:format("Value:\n~p\n", [lists:keysearch(fun_def,1,Ann)]),
			case lists:keysearch(fun_def,1,Ann) of 
			    {value, {fun_def, {ModName, F, A, _, DefPos}}} ->  %% TOCHANGE: temporaly assume the function is local.
				case refac_util:pos_to_fun_def(AnnAST, DefPos) of 
				    {ok, Def} -> 
					Cs = refac_syntax:function_clauses(Def),
					FunExpr = refac_syntax:fun_expr(Cs),
					Args = refac_syntax:application_arguments(Node),
					{refac_syntax:application(FunExpr, Args),true};
				    _ -> {Node, false}
				end;
			    _ -> {Node, false}
			end;
		    _ -> {Node, false}
		end
	end,	
    {FunDef1, _} = refac_util:stop_tdTP(F, FunDef, []),
    FunDef2 = refac_syntax_lib:annotate_bindings(reset_attrs(FunDef1), []),
    FunDef2.
    
    
reset_attrs(Node) ->
    refac_util:full_buTP(fun (T, _Others) ->  
				 As =refac_syntax:get_ann(T),
				 As0 = lists:keydelete(free, 1, As),
				 As1 = lists:keydelete(bound, 1, As0),
				 As2 = lists:keydelete(env,1,As1),
				 refac_syntax:set_ann(T, As2)				 
			 end, Node, {}).

	    
process_spawn(FileName, AnnAST, Info, FunDef, SpawnApp) -> 
    {AnnAST1,_} = refac_add_a_tag:remove_spawn_pars(AnnAST),
    CallGraph = refac_add_a_tag:build_call_graph(AnnAST1, Info, FileName),
    CallGraph1 = lists:map(fun({{Caller,_CallerDef}, Called})->
				   {Caller, Called} end, CallGraph),    
    Operator = refac_syntax:application_operator(SpawnApp),
    Args = refac_syntax:application_arguments(SpawnApp),
    case Args of
	[M, F, A] ->  
	    {_, M1} = refac_process_info:evaluate_expr(M, FunDef),
	    {_, F1} = refac_process_info:evaluate_expr(F, FunDef),
	    {_, A1} = refac_process_info:evaluate_args(A, FunDef),
	    case is_atom(M1) andalso is_atom(F1) andalso is_integer(A1) of
		true -> 
		    {ok, reached_funs([{M1, F1, A1}], CallGraph1, Info)};
		_ -> {error, "Wrangler could not decide the initial function of the spawned process."}
	    end;
	[_N, M, F, A] ->
	    {_, M1} = refac_process_info:evaluate_expr(M, FunDef),
	    {_, F1} = refac_process_info:evaluate_expr(F, FunDef), 
	    {_, A1} = refac_process_info:evaluate_args(A, FunDef),
	    case is_atom(M1) andalso is_atom(F1) andalso is_integer(A1) of
		true -> {ok, reached_funs([{M1, F1, A1}], CallGraph1, Info)};
		_ -> {error, "Wrangler could not decide the initial function of the spawned process."}
	    end;
	[_N, M, F, A, _O] ->
	    {_, M1} = refac_process_info:evaluate_expr(M, FunDef),
	    {_, F1} = refac_process_info:evaluate_expr(F, FunDef),
	    {_, A1} = refac_process_info:evaluate_args(A, FunDef),
	    case is_atom(M1) andalso is_atom(F1) andalso is_integer(A1) of
		true -> {ok, reached_funs([{M1, F1, A1}], CallGraph1, Info)};
		_ -> {error, "Wrangler could not decide the initial function of the spawned process."}
	    end;
	[Fun] ->
	    {ok, reached_funs([Fun], CallGraph1, Info)};
	[_N, Fun] ->
	    {ok, reached_funs([Fun], CallGraph1, Info)}
    end.



    
   
reached_funs([], _Callgraph,  _Info) -> [];
reached_funs(InitialFuns, CallGraph, Info) ->
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
     Inscope_Funs = [{erlang, Fun, Arity} || {Fun, Arity} <- refac_util:auto_imported_bifs()] ++
		     refac_util:inscope_funs(Info),  %% NOTE: orders matters here.
     F2 = fun (T, S) ->
		 case refac_syntax:type(T) of
		   application ->
		       Operator = refac_syntax:application_operator(T),
		       Arguments = refac_syntax:application_arguments(T),
		       Arity = length(Arguments),
		       case refac_syntax:type(Operator) of
			 atom ->
			     Op = refac_syntax:atom_value(Operator),
			     R = lists:filter(fun ({_M, F, A}) -> (F == Op) and (A == Arity) end, Inscope_Funs),
			     if R == [] ->
				    ordsets:add_element({unknown, Op, Arity},
							S);  %% Should we give an error message here?
				true ->
				    {M, Op, Arity} = hd(R),
				     ordsets:add_element({M, Op, Arity}, S)
				end;
			   module_qualifier ->
			       Mod = refac_syntax:module_qualifier_argument(Operator),
			       Body = refac_syntax:module_qualifier_body(Operator),
			       case {refac_syntax:type(Mod), refac_syntax:type(Body)} of
				   {atom, atom} ->
				       Mod1 = refac_syntax:atom_value(Mod),
				       Op = refac_syntax:atom_value(Body),
				       ordsets:add_element({Mod1, Op, Arity}, S);
				   _ -> S
			       end
			   end;
		     arity_qualifier ->
			 Fun = refac_syntax:arity_qualifier_body(T),
			 A = refac_syntax:arity_qualifier_argument(T),
			 case {refac_syntax:type(Fun), refac_syntax:type(A)} of
			     {atom, integer} ->
				 FunName = refac_syntax:atom_value(Fun),
				 Arity = refac_syntax:integer_value(A),
				 ordsets:add_element({ModName, FunName, Arity}, S);
			     _ -> S
			 end;
		     _ -> S
		 end
	  end,
    F  = fun(InitialFun) ->
		 case InitialFun of 
		     {Mod, Fun, Arity} -> [{Mod,Fun, Arity}];
		     FunExpr ->
			 lists:usort(refac_syntax_lib:fold(F2, [], FunExpr))
		 end
	 end,		 
    ReachedFuns = lists:zip(InitialFuns, lists:map(fun(InitialFun) -> 
							   reached_funs(CallGraph, F(InitialFun)) end, 
						   InitialFuns)),
    ReachedFuns.

 
reached_funs(CallGraph, Acc) ->
    Res = lists:usort(lists:concat(lists:map(fun({Mod, Fun, Args}) ->
					 case lists:keysearch({Mod, Fun, Args}, 1, CallGraph) of 
					     {value, {{Mod, Fun, Args}, CalledFuns}} ->
						 CalledFuns;
					     _ ->[]
					 end
				 end, Acc))),
     case lists:usort(Res++Acc) == Acc of 
	true -> Res;
	_ -> reached_funs(CallGraph, lists:usort(Res++Acc)) 
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
   
