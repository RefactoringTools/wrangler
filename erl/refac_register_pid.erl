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

-export([register_pid/5, register_pid_eclipse/5, register_pid_1/5, register_pid_2/5]).

-export([ast_server/2, get_ast/1, get_call_graph/1, call_graph_server/1]).

-include("../hrl/wrangler.hrl").


 
%% Rationale for this refactoring: If we want to send a message to a process, then we need to know its PID. This is often inconvenient since the 
%% PID has to be sent to all the processes in the system that want to comminicate with this process. Registering a process with a name allows any 
%% process in the system to communicate with this process without knowing its PID.
%%
%% To perform this refactoring, the user needs to select a match expression whose right-hand side is a spawn expression, and whose left-hand side 
%% is a Pid variable.
%%

%% Side conditions for this refactorings:
%% 1. The process name provided by the user should be lexically valid. (done).
%% 2. The name provided by the user should not have been used as a process name. (need to handle dynamically create process names) (done).

%% 2. The same process cannot be registered more than once. (check the registration of the generated Pid, and self()). (done).

%% 3. At any time during the running of the system, only one process can be associated with a particular process name.  Since statically we 
%% cannot decide when a process starts/dies, we use a rather stronger condition, that is the function containing the registration is only 
%% called once. 
%%   3.1) the function should not be a recursive function either directly or indirectly. (done).
%%   3.2) the function is only called at one place of the program.
%%   3.3) the function should not be used in a map/fold.
%%   3.4) the registeration should not be in a receive expression, list comprehension (done)

%% To do the transformation, Wrangler needs to know, for each Pid!Msg expression, where the Pid is spawned. This Pid is only replacable if 
%% it is only assoicated with the spawn expression selected by the user.

%% Why do I need slicing? mostly for reduced the number of unclear registration expressions.

%% ==============================================================================================================
%% @spec register_pid(FileName::filename(), Start::Pos, End::Pos, RegName::string(),SearchPaths::[dir()()])-> term()
%% @doc This function associates a name, which must be an atom, with a pid, and replaces the uses of this pid 
%%      with the process name in send expressions.
register_pid(FName, Start, End, RegName,  SearchPaths) ->
    register_pid(FName, Start, End, RegName, SearchPaths, emacs).

%%@private
%% @spec register_pid(FileName::filename(), Start::Pos, End::Pos, RegName::string(),SearchPaths::[dir()()])-> term()
register_pid_eclipse(FName, Start, End, RegName, SearchPaths) ->
    register_pid(FName, Start, End, RegName, SearchPaths, eclipse).

register_pid(FName, Start, End, RegName, SearchPaths, Editor) ->
    io:format("\n[CMD: register_pid, ~p, ~p, ~p, ~p,~p]\n",  [FName, Start, End, RegName, SearchPaths]),
    start_ast_server_process(SearchPaths),
    start_call_graph_server(),
    case refac_util:is_fun_name(RegName) of
	true ->
	    case get_ast(FName) of  %% preprocessor is not bypassed.
		{ok, {AnnAST, Info}} ->
		    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
		    RegName1 = list_to_atom(RegName), 
		    _Res = refac_annotate_pid:ann_pid_info(SearchPaths),  %% Interface needs to be changed!
		    {ok, {AnnAST2, Info}} = get_ast(FName),
		    case pos_to_spawn_match_expr(AnnAST2, Start, End) of
			{ok, MatchExpr} ->
			    case pre_cond_check(FName, ModName,AnnAST2, Start, MatchExpr, RegName1, Info, SearchPaths) of 
				ok -> 
				    Pid = refac_syntax:match_expr_pattern(MatchExpr),
				    case do_register(AnnAST2, MatchExpr, Pid, RegName1) of 
					{ok, AnnAST4} ->
					    case Editor of 
						emacs ->
						    refac_util:write_refactored_files([{{FName, FName}, AnnAST4}]),
						    ast_server ! stop,
						    call_graph_server ! stop,
						    {ok, [FName]};
						eclipse ->
						    {ok, [{FName, FName, refac_prettypr:print_ast(AnnAST4)}]}
					    end;
					{error, Reason} -> {error, Reason}
				    end;	
				{unknown_value, UnKnowns} -> {unknown_value, UnKnowns};
				{error, Reason} -> {error, Reason}
			    end;
			{error,Reason} -> 
			    {error, Reason}
		    end;
		{error, Reason} -> {error, Reason}
	    end;
	false -> {error, "Invalid process name."}
    end.

register_pid_1(FName, Start, End, RegName, SearchPaths) ->
    ModName = list_to_atom(filename:basename(FName, ".erl")),
    AnnAST = refac_annotate_pid:ann_pid_in_a_file(FName),  %% Interface needs to be changed!
    {ok, MatchExpr} = pos_to_spawn_match_expr(AnnAST, Start, End),
    Pid = refac_syntax:match_expr_pattern(MatchExpr),
    RegName1 = list_to_atom(RegName), 
    Res = check_registered_pids(FName, ModName, AnnAST, Start, MatchExpr),
    io:format("Register results:\n~p\n", [Res]),
    case Res of 
 	 ok ->  case do_register(AnnAST, MatchExpr, Pid, RegName1) of 
		    {ok, AnnAST2} ->
			refac_util:write_refactored_files([{{FName, FName}, AnnAST2}]),
			{ok, [FName]};
		    {error, Reason} -> {error, Reason}
		end;
  	{registered, RegExpr} -> {{Line,_Col}, _} = refac_util:get_range(RegExpr),
 				 {error, "The selected process is already registered at line "++ integer_to_list(Line)};
 	{unsure, RegExprs} ->
 	    io:format("\nThe following registration of processes might cause conflict, please check!\n"),
 	    lists:map(fun(Reg) -> {StartPos,_} = refac_util:get_range(Reg),
 				  io:format("\nLocation: (~p, ~p):  ", [FName, StartPos]),
 				  io:format(refac_prettypr:format(Reg)++"\n") end, RegExprs),
 	    {registered, RegExprs}
    end.
    
register_pid_2(FName, Start, End, RegName, SearchPaths) ->
    ModName = list_to_atom(filename:basename(FName, ".erl")),
    AnnAST = refac_annotate_pid:ann_pid_in_a_file(FName),  %% Interface needs to be changed!
    {ok, MatchExpr} = pos_to_spawn_match_expr(AnnAST, Start, End),
    Pid = refac_syntax:match_expr_pattern(MatchExpr),
    RegName1 = list_to_atom(RegName),
    case do_register(AnnAST, MatchExpr, Pid, RegName1) of 
	{ok, AnnAST2} ->
	    refac_util:write_refactored_files([{{FName, FName}, AnnAST2}]),
	    {ok, [FName]};
	{error, Reason} -> {error, Reason}
    end.

pos_to_spawn_match_expr(AnnAST, Start, End) ->
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


%% TODO: REFACTOR THE FOLLOWING TWO FUNCTIONS.
pos_to_receive_expr(FunDef, Start) ->
    F = fun(T, Acc) ->
		case refac_syntax:type(T)==receive_expr of 
		    true -> [T|Acc];
		    _ -> Acc
		end
	end,
    ReceiveExprs = refac_syntax_lib:fold(F, [], FunDef),
    lists:any(fun(E) ->
		      {Start1, End1} = refac_util:get_range(E),
		      (Start1 =< Start) andalso (Start =< End1)
	      end, ReceiveExprs).
    

pos_to_list_comp_expr(FunDef, Start) ->
    F = fun(T, Acc) ->
		case refac_syntax:type(T) of 
		    list_comp -> [T|Acc];
		    _ -> Acc
		end
	end,
    ReceiveExprs = refac_syntax_lib:fold(F, [], FunDef),
    lists:any(fun(E) ->
		      {Start1, End1} = refac_util:get_range(E),
		      (Start1 =< Start) andalso (Start =< End1)
	      end, ReceiveExprs).


%% TODO: ALSO NEED TO CHECK the slice of self().

pre_cond_check(FileName, ModName, AnnAST, Start, MatchExpr, RegName, Info, SearchPaths) ->		 
    {ok, FunDef} = refac_util:pos_to_fun_def(AnnAST, Start),
    FunName = refac_syntax:data(refac_syntax:function_name(FunDef)),
    Arity = refac_syntax:function_arity(FunDef),
    case is_recursive_fun(SearchPaths, {ModName, FunName, Arity, FunDef}) of 
	true -> {error, "The function containing the spawn  expression is a recursive function"};
	_ -> case pos_to_receive_expr(FunDef, Start) of 
		 true -> {error, "Wrangler do not supported register a process spawned in a received expression\n"};
		 _ -> case pos_to_list_comp_expr(FunDef, Start) of
			  true -> {error, "The spawn expression selected in part of a list comprehension expression\n"};
			  _ -> {ExistingProcessNames, UnKnowns} = collect_process_names(SearchPaths),
			       
			       io:format("registed:\n~p\n", [{ExistingProcessNames, UnKnowns}]),
			       case lists:member(RegName, ExistingProcessNames) of 
				   true -> {error, "The process name provided is already in use, please choose another name."};
				   _ -> case UnKnowns of 
					    [] ->
						Res = check_registered_pids(FileName, ModName, AnnAST, Start, MatchExpr),
						case Res of 
						    ok -> ok;
						    {registered, RegExprs1} ->
							{{M, F, A}, R} = hd(RegExprs1),
							{error, "The process is already registered in function "++ atom_to_list(F)++"/"++integer_to_list(A)++"\n"};
						    {unsure, _} -> ok
						end;
					    _ -> io:format("Wrangler could not fully decide the process name used by the following register expressions:\n"),
						 UnKnowns1 = lists:map(fun({_, V}) -> V end, UnKnowns),
						 lists:map(fun({M, F,A, L}) -> io:format("\n Location: (module: ~p, function:~p/~p, line:~p)\n", [M, F, A, L])
							  end, UnKnowns1),
						 {unknown_value, UnKnowns}
				       end
			      end
		     end
	     end
    end.
     
check_registered_pids(FileName, ModName, AnnAST, Start, MatchExpr) ->
    {ok, FunDef} = refac_util:pos_to_fun_def(AnnAST, Start),
    Res = refac_slice:forward_slice([FileName], AnnAST, ModName, FunDef, MatchExpr),
    RegExprs = collect_register_exprs(Res),
    io:format("RegExprs\n~p\n",[RegExprs]),
    Pid = refac_syntax:match_expr_pattern(MatchExpr),
    {value, {pid, PidInfo}} = lists:keysearch(pid,1, refac_syntax:get_ann(Pid)),
    case RegExprs of 
	[] ->
	    ok;
	_ -> RegExprs1 = lists:filter(fun({{M, F, A},R}) ->
					      [RegName, Pid1] = refac_syntax:application_arguments(R),
					      case lists:keysearch(pid,1, refac_syntax:get_ann(Pid1)) of 
						  {value, {pid, PidInfo1}}->
						      PidInfo -- PidInfo1 =/= PidInfo;
						  _ -> false
					      end
				      end, RegExprs),
	     case RegExprs1 of 
		 [] ->{unsure, RegExprs};  %% CHECK This about undeciable processes.
		 _ -> {registered, RegExprs1}
	     end
    end. 
	


collect_register_exprs(Funs) ->
    F = fun(Node, {Acc, {M, F, A}}) ->
		case refac_syntax:type(Node) of 
		    application ->
			case is_register_app(Node) of 
			    true ->
				{[{{M, F, A},Node} |Acc], {M, F, A}};
			    _ -> {Acc, {M, F, A}}
			end;
		    _ -> {Acc, {M,F,A}}
		end
	end,		
    lists:flatmap(fun({Key, FunDef}) ->
			  {value, {fun_def, {ModName, FunName, Arity, _,_}}} = 
			      lists:keysearch(fun_def, 1, refac_syntax:get_ann(FunDef)),
			  refac_syntax_lib:fold(F, {[], {ModName, FunName, Arity}}, FunDef) end, Funs).
    
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
	    {_CallerCallee, Sccs, _E} = get_call_graph(Files),
	    Sccs1 =[[Fun||{Fun, _FunDef}<-Scc]||Scc<-Sccs],
	    lists:any(fun(E)-> (length(E)>1) andalso (lists:member({ModName, FunName, Arity}, E)) end,
		      Sccs1)   
    end.
	
   
collect_process_names(DirList) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    F = fun(File, FileAcc) ->
		 case get_ast(File) of 
		     {ok, {AnnAST, Info}} ->
			 {value, {module, ModName}} = lists:keysearch(module, 1, Info),
			 F1 = fun(Node, ModAcc) ->
				      case refac_syntax:type(Node) of 
					  function ->
					      FunDef = Node, 
					      F2= fun (Node1, FunAcc) ->    
							  case refac_syntax:type(Node1) of 
							      application ->
								  case is_register_app(Node1) of 
								      true -> 
									  [RegName, Pid] = refac_syntax:application_arguments(Node1),
									    Res = evaluate_expr(Files, ModName, AnnAST, FunDef, RegName),
									    Res++FunAcc;
									_ -> FunAcc
								    end; 
								_ ->  FunAcc
							    end
						    end,
					      refac_syntax_lib:fold(F2, [], FunDef)++ModAcc;
					  _-> ModAcc 
				      end
			      end,
			 refac_syntax_lib:fold(F1, [], AnnAST) ++ FileAcc;
		     {error, Reason} -> erlang:error({error, Reason})
		 end
	 end,			 
    Acc =lists:foldl(F, [], Files),
    {Names, UnKnowns} = lists:partition(fun({Tag,_V})-> Tag==value end, Acc),
    {lists:usort(lists:map(fun({value, P}) -> P end, Names)), lists:usort(UnKnowns)}.
    

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
	application -> Operator = refac_syntax:application_operator(Node),
		       Ann = refac_syntax:get_ann(Operator),
		       case lists:keysearch(fun_def,1,Ann) of 
			   {value, {fun_def, {erlang, send, 2, _, _}}} ->
			       [ReceiverPid, Msg]=refac_syntax:application_arguments(Node),
			       Ann = refac_syntax:get_ann(ReceiverPid),
			       case lists:keysearch(pid,1,Ann) of 
				   {value, PidInfo} ->
				       Node1 = refac_syntax:application(Operator, [refac_syntax:atom(RegName), Msg]),
				       refac_syntax:copy_attrs(Node, Node1);
				   _ -> Node
			       end;
			   _ -> Node
		       end;
	_  -> Node
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



evaluate_expr(Files, ModName, AnnAST, FunDef, Expr) ->
    F = fun(Exprs) ->
		%%io:format(lists:concat(lists:map(fun(B) ->refac_prettypr:format(B)++"\n" end, Exprs))),
		Exprs1 = lists:map(fun(E) -> refac_syntax:revert(E) end, Exprs),
		case catch erl_eval:exprs(Exprs1, []) of 
		    {value, V, _} -> {value, V};
		    _ ->
			FunName = refac_syntax:data(refac_syntax:function_name(FunDef)),
			Arity = refac_syntax:function_arity(FunDef),
			{StartPos, _} = refac_util:get_range(Expr),
			{unknown_value, {ModName, FunName, Arity, StartPos}}
		end
	end,
    ExprsList = case refac_util:get_free_vars(Expr) of 
		[] -> [[Expr]];
		_ -> Res = refac_slice:backward_slice(Files, AnnAST, ModName, FunDef, Expr),
		     %%io:format("Res:\n~p\n", [Expr]),
		     case Res of 
			 [] -> Res;
			 _ ->
			     Res 		    
		     end
	    end,
    Values = lists:map(F, ExprsList),
    Values.


start_ast_server_process(SearchPaths) ->
    Pid = spawn_link(refac_register_pid, ast_server, [[], SearchPaths]),
    register(ast_server, Pid).

get_ast(FileName) ->
    ast_server ! {self(), get, FileName},
    receive 
	{ast_server, Res} ->
	    Res
    end.
	    
ast_server(Env, SearchPaths) ->
    receive
	{From, get, FileName} ->
	    case lists:keysearch(FileName, 1, Env) of
		{value, {FileName, {AnnAST, Info}}} ->
		    From ! {ast_server, {ok, {AnnAST, Info}}},
		    ast_server(Env, SearchPaths);
		false -> case refac_util:parse_annotate_file(FileName, true, SearchPaths) of 
			     {ok, {AnnAST, Info}} ->
				 From ! {ast_server, {ok, {AnnAST, Info}}},
				 ast_server([{FileName, {AnnAST, Info}}|Env], SearchPaths);
			     {error, Reason} ->
				 From ! {ast_server, {error, Reason}},
				 ast_server(Env, SearchPaths)
			 end
	    end;
	{update, {FileName, {AnnAST, Info}}} ->
	    Env1 =case lists:keysearch(FileName, 1, Env) of
		       {value, {FileName,  {_AnnAST1, _Info1}}} ->
			   lists:keyreplace(FileName, 1, Env, {FileName, {AnnAST, Info}});
		       false ->
			   [{FileName, {AnnAST, Info}} |Env]
		   end,
	    ast_server(Env1, SearchPaths);
	stop ->
	    ok
    end.



start_call_graph_server() ->
    Pid = spawn_link(refac_register_pid, call_graph_server, [[]]),
    register(call_graph_server, Pid).

stop_call_graph_server() ->
    call_graph_server!stop.


get_call_graph(SearchPaths) ->
    call_graph_server ! {self(), get, SearchPaths},
    receive
	{call_graph_server, CallGraph} ->
	    CallGraph
    end.
    
call_graph_server(State) ->    
    receive
	{From, get, SearchPaths} ->
	   case lists:keysearch(SearchPaths, 1, State) of 
	       {value, {SearchPaths, CallGraph}} ->
		   From ! {call_graph_server, CallGraph},
		   call_graph_server(State);
	       false ->
		   {CallerCallee, Sccs, E} = build_call_graph(SearchPaths),
		   From ! {call_graph_server,  {CallerCallee, Sccs, E}},
		   call_graph_server([{SearchPaths, {CallerCallee, Sccs, E}}|State])
	   end;
	stop -> 
		ok
    end.
    
build_call_graph(DirList) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    CallGraph = build_call_graph(Files, []),
    CallerCallee = lists:map(fun ({{Caller, _CallerDef}, Callee}) -> {Caller, Callee} end, CallGraph),
    #callgraph{scc_order = Sccs, external_calls = E} = refac_callgraph:construct(CallGraph),
    {CallerCallee, Sccs, E}.
     
   
%%@private
%%@spec build_call_graph(DirList::[dir()], #callgraph{}) -> #callgraph{}
build_call_graph([FileName | Left], Acc) ->
    case get_ast(FileName) of
      {ok, {AnnAST, Info}} ->
	    G1 = refac_util:build_call_graph(AnnAST, Info, FileName),
	    Acc1 = Acc ++ G1,
	    build_call_graph(Left, Acc1);
	{error, Reason} -> erlang:error(Reason)
    end;
build_call_graph([], Acc) -> Acc.			   
			    
				 
				 
