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
%% @doc Register a process id, Pid say, with a name, RegName say, and replace
%% the uses of Pid ! Msg with  RegName ! Msg.
%% @end


%% TODO: 1. should trace back to the define location of the user-selected pid.
%% 

-module(refac_register_pid).

-export([register_pid/5, register_pid_eclipse/5]).

-compile(export_all).

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
	    case refac_util:parse_annotate_file(FName, false, SearchPaths) of
		{ok, {AnnAST, Info}} ->
		    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
		    RegName1 = list_to_atom(RegName), 
		    case refac_util:parse_annotate_file(FName, true, SearchPaths) of
			{ok, {AnnAST2, _Info1}} ->
			    AnnAST3 =  refac_process_info:ann_process_in_a_file(FName),   %% TODO: too many passes. should be  improved.
			    case pos_to_pid(AnnAST3, Start) of
				{ok, Pid} ->
				    case pre_cond_check(FName, ModName,AnnAST3, Pid, RegName1, Start) of 
					ok -> AnnAST4 = do_register(AnnAST3, Pid, RegName1), %% Should be AnnAST3 here.
					      case Editor of 
						  emacs ->
						      refac_util:write_refactored_files([{{FName, FName}, AnnAST4}]),
						      {ok, [FName]};
						  eclipse ->
						      {ok, [{FName, FName, refac_prettypr:print_ast(AnnAST4)}]}
					      end;
					{error, Reason} -> {error, Reason}
				    end;
				{error, _Reason} -> {error, "You have not selected a variable representing a Pid!"}
			    end;
			{error, Reason} -> {error, Reason}
		    end;
		{error, Reason} -> {error, Reason}
	    end;
	false -> {error, "Invalid process name."}
    end.


%% {regname,[{pname, {inital_fun, none}, {spawn, none}}] 
%% {pid, [{{initial_fun, unknown}, {spawn, unknown}}]

pre_cond_check(FileName, ModName, AnnAST, Pid={PidName, DefPos, ProcessInfo}, RegName, Pos) ->
     RegInfo = collect_reg_info(AnnAST),
     RegNames = lists:map(fun({PName, _InitialFun, _Spawn}) -> PName end, RegInfo),
     RegPids = lists:map(fun ({_PName, InitialFun, Spawn}) ->
 				{InitialFun, Spawn} end, RegInfo),
     case lists:member(RegName, RegNames) of 
 	true ->
 	    {error, "This process name has been used, please select another one."};
 	_ -> case lists:member(ProcessInfo, RegPids) of 
 		 true -> {error, "This process is a registered process."};
		 _ -> 
		     {ok, FunDef} = refac_util:pos_to_fun_def(AnnAST, Pos),
		     FunName= refac_syntax:atom_value(refac_syntax:function_name(FunDef)),
		     Arity = refac_syntax:function_arity(FunDef),
		     case is_recursive_fun([FileName], {ModName, FunName, Arity, FunDef}) of 
			 true -> {error, "The seleccted process could represent multiple process instances."};
			 false -> ok
 		      end
 	     end
     end.			       
   

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
	
   
collect_reg_info(AnnAST) ->
    Acc =refac_syntax_lib:fold(fun do_collect_reg_info/2, [], AnnAST),
    lists:usort(Acc).

do_collect_reg_info(Node, Acc) ->
    case refac_syntax:type(Node) of 
	application ->
	    case refac_process_info:is_register_app(Node) of 
		true -> 
		    Operator = refac_syntax:application_operator(Node),
		    Ann = refac_syntax:get_ann(Operator),
		    {value, {regname,RegInfo}} = lists:keysearch(regname,1, Ann),
		    Acc++RegInfo;
		_ -> Acc
	    end;
	_ ->  Acc
    end.

do_register(AnnAST, Pid, RegName) ->
   {AnnAST1, Modified} = add_register_expr(AnnAST, Pid, RegName),
   case Modified of 
       true -> Res = refactor_send_exprs(AnnAST1, Pid, RegName),
	       Res;
	   
       _ -> {error, "Refactoring failed"}
   end.
 


refactor_send_exprs(AnnAST, Pid, RegName) ->
    refac_util:full_buTP(fun do_refactor_send_exprs/2, AnnAST, {Pid, RegName}).

do_refactor_send_exprs(Node, {Pid={PidName, DefPos, ProcessInfo}, RegName}) ->
    case refac_syntax:type(Node) of 
	infix_expr -> Dest = refac_syntax:infix_expr_left(Node),
		      Op = refac_syntax:infix_expr_operator(Node),
		      Msg = refac_syntax:infix_expr_right(Node),
		      Ann = refac_syntax:get_ann(Dest),
		      io:format("processInfo:\n~p\n",[ProcessInfo]),
		      io:format("PINFO:\n~p\n", [lists:keysearch(process, 1, Ann)]),
		      case lists:keysearch(process, 1, Ann) of 
			  {value, {process,ProcessInfo}} ->
			      Node1 = refac_syntax:infix_expr(refac_syntax:atom(RegName), Op, Msg),
			      refac_syntax:copy_attrs(Node, Node1);
			  _ -> Node
		      end;
	_  -> Node %% TODO: Need to handle uses of send/2.
    end.
			  
	    
    

add_register_expr(AnnAST, Pid={PidName, DefPos, ProcessInfo}, RegName) ->
    RegExpr = refac_syntax:application(refac_syntax:atom(register),
				       [refac_syntax:atom(RegName), refac_syntax:variable(PidName)]),
    
    refac_util:stop_tdTP(fun do_add_register_expr/2, AnnAST, {hd(DefPos), RegExpr}).
    


do_add_register_expr(Node, {DefPos, RegExpr}) ->
    case refac_syntax:type(Node) of 
	clause -> 
	    {Start, End} = refac_util:get_range(Node),
	    case (Start =< DefPos) andalso (DefPos =< End) of 
		true ->
		    P = refac_syntax:clause_patterns(Node),
		    B = refac_syntax:clause_body(Node),
		    G = refac_syntax:clause_guard(Node),
		    {PStart, PEnd} = refac_util:get_range(P),
		    case (PStart =< DefPos) andalso (DefPos =< PEnd) of 
			true ->
			    B1 = [RegExpr|B],
			    Node1 = refac_syntax:clause(P, G, B1),
			    {Node1, true};
			false ->
			    {Exprs1, Exprs2} = lists:splitwith(fun(E) ->
								       {EStart, EEnd} = refac_util:get_range(E), 
								       EEnd < DefPos
							       end, B),
			    Expr = hd(Exprs2),
			    case refac_syntax:type(Expr) of 
				match_expr ->
				    B1 = Exprs1 ++ [Expr, RegExpr]++tl(Exprs2),
				    Node1 = refac_syntax:clause(P, G, B1),
				    {Node1, true};
				_ -> {NewExpr, Modified} = do_add_register_expr(Expr, {DefPos, RegExpr}),
				     case Modified of 
					 true -> B1 = Exprs1++[NewExpr]++tl(Exprs2),
						 Node1 = refac_syntax:clause(P, G, B1),
						 {Node1, true};
					 false -> {Node, false}
				     end
			    end
		    end;
		false -> {Node, false}
	    end;
	_  -> {Node, false}
    end.	  
	    

    
pos_to_pid(AST, Pos) ->
    case refac_util:once_tdTU(fun pos_to_pid_1/2, AST, Pos) of
	{_, false} -> {error, none};
	{R, true} -> {ok, R}
    end.

pos_to_pid_1(Node, _Pos = {Ln, Col}) ->
    case refac_syntax:type(Node) of
      variable ->
	  {Ln1, Col1} = refac_syntax:get_pos(Node),
	  case (Ln == Ln1) and (Col1 =< Col) and
		 (Col =< Col1 + length(atom_to_list(refac_syntax:variable_name(Node))) - 1)
	      of
	    true ->
		Ann = refac_syntax:get_ann(Node),
		case lists:keysearch(process, 1, Ann) of
		  {value, {process, ProcessInfo}} ->
			case lists:keysearch(def, 1, Ann) of 
			    {value, {def, DefinePos}} ->
				{{refac_syntax:variable_name(Node), DefinePos, ProcessInfo}, true};
			    _ -> {[], false}
			end;
		  false ->
		      {[], false}
		end;
	    false -> {[], false}
	  end;
      _ -> {[], false}
    end.
