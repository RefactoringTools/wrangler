
%% ============================================================================================
%% Refactoring: Add a tag to all the messages received by a process.
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
%% =============================================================================================
%% Notes: 
%% 1) The current implementation of this refactoring solely depends on the information inferred
%% by static analysis, and no trace information is used yet.
%% 2) To perform this refactoring, selected  a function containing the receive expression, then Wrangler
%% will prompt the tag name.
%% 3) limitations:
%%    a). the current implementation only handles processes spawned using spawn or spawn_link;
%% =============================================================================================
-module(refac_add_a_tag).

-export([add_a_tag/5]).

-include("../hrl/wrangler.hrl").

%% =============================================================================================
-spec(add_a_tag/5::(filename(), integer(), integer(), string(), [dir()]) ->{ok, [filename()]} | {error, string()}).	     
add_a_tag(FileName, Line, Col, Tag, SearchPaths) ->
    io:format("\n[CMD: add_a_tag, ~p, ~p, ~p, ~p,~p]\n", [FileName, Line, Col, Tag, SearchPaths]),
    case refac_util:parse_annotate_file(FileName, true, SearchPaths) of 
	{ok, {AnnAST1, _Info1}} ->
	    case pos_to_receive_fun(AnnAST1, {Line, Col}) of 
		{ok, _FunDef} ->
		    _Res=refac_annotate_pid:ann_pid_info(SearchPaths),
		    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths),
		    {ok, FunDef} = pos_to_receive_fun(AnnAST, {Line, Col}),
		    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
		    case pre_cond_check(AnnAST,  ModName, FunDef, SearchPaths) of 
			{ok, AffectedInitialFuns} ->
			    Results = do_add_a_tag(FileName, {AnnAST, Info}, list_to_atom(Tag), AffectedInitialFuns, SearchPaths),
			    refac_util:write_refactored_files(Results),
			    ChangedFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
			    io:format("The following files have been changed by this refactoring:\n~p\n",
				      [ChangedFiles]),
			    {ok, ChangedFiles};
			{error,  Reason} -> {error, Reason}
		    end;
		{error, Reason} -> {error, Reason}
	    end;
	{error, Reason} -> {error, Reason}
    end.
	


pre_cond_check(_AnnAST, ModName, CurrentFunDef,  SearchPaths) ->
    FunName = refac_syntax:data(refac_syntax:function_name(CurrentFunDef)),
    Arity = refac_syntax:function_arity(CurrentFunDef),
    InitialFuns = collect_process_initial_funs(SearchPaths),
    AffectedInitialFuns = get_affected_initial_funs(InitialFuns, {ModName, FunName, Arity}, SearchPaths),
    io:format("InitialFuns:\n~p\n", [AffectedInitialFuns]),
    case AffectedInitialFuns of 
	[] -> {error, "Sorry, but Wrangler could not figure out where the process is spawned."};
	[_H|T]-> case T of 
		    [] -> {ok, AffectedInitialFuns};
		    _ ->  case length(lists:usort(lists:map(fun({_InitialFun, ReceiveFuns}) -> ReceiveFuns end, AffectedInitialFuns))) of
			      1 -> {ok, AffectedInitialFuns};
			      _ -> {error, "The selected receive function is shared by processes with different set of receive expressions."}
			  end
		end
    end.
   

get_affected_initial_funs(InitialFuns, {ModName, FunName, Arity}, SearchPaths) ->
    ReachedReceiveFuns = reached_receive_funs(InitialFuns, SearchPaths),
    io:format("ReachedReceiveFuns:\n~p\n", [ReachedReceiveFuns]),
    lists:filter(fun({_InitialFun, Fs}) ->
					    lists:member({ModName, FunName, Arity}, Fs) end, ReachedReceiveFuns).


collect_process_initial_funs_1({AnnAST, Info}, _SearchPaths) ->  
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    HandleSpecialFuns = fun(Node) ->
				{{Ln, _}, _} = refac_util:get_range(Node),
				Op = refac_syntax:application_operator(Node),
				Arguments = refac_syntax:application_arguments(Node),
				Ann = refac_syntax:get_ann(Op),
				{value, {fun_def, {erlang, FunName, _Arity, _, _}}}=lists:keysearch(fun_def, 1, Ann),
				case lists:member(FunName, [spawn, spawn_link]) of 
				    true ->
					{value, {pid, PidInfo}} = lists:keysearch(pid, 1, refac_syntax:get_ann(Node)),
					case Arguments of 
					    [FunExpr] -> 
						{PidInfo, {FunExpr, {ModName, Ln}}};   %%Question: what about if the FunExpr contains free variables?
					    [_N, FunExpr] -> {PidInfo, FunExpr};
					    [M, F, A] -> 
						case {refac_syntax:type(M), refac_syntax:type(F), refac_syntax:type(A)} of
						    {atom, atom, list} ->
							{PidInfo, {refac_syntax:atom_value(M),   %%TODO: need to use backward slice
								   refac_syntax:atom_value(F), 
								   refac_syntax:list_length(A)}};    
						    _ -> io:format("\n*************************************Warning****************************************\n"),
							 io:format("Wrangler could not handle the spawn expression used in module ~p at line ~p\n", [ModName,Ln])
						end;
					    [_N, M, F, A] ->
						case {refac_syntax:type(M), refac_syntax:type(F), refac_syntax:type(A)} of
						    {atom, atom, list} ->
							{PidInfo, {refac_syntax:atom_value(M),   %%TODO: need to use backward slice
								   refac_syntax:atom_value(F), 
								   refac_syntax:list_length(A)}};    
						    _ -> io:format("\n*************************************Warning****************************************\n"),
							 io:format("Wrangler could not handle the spawn expression used in module ~p at line ~p\n", [ModName,Ln])
						end
					    end;
				    _ ->
					io:format("\n*************************************Warning****************************************\n"),
					io:format("Wrangler could not handle the spawn expression used in module ~p at line ~p\n", [ModName,Ln]),
					[]   
				end
			end, 
    Fun= fun(Node, Acc) ->			       
		 case refac_syntax:type(Node) of 
		     function ->
			 F = fun(T, S) ->
				     case is_spawn_expr(T) of 
					 true -> [HandleSpecialFuns(T)|S];
					 _ -> S
				     end
			     end,
			 Res = refac_syntax_lib:fold(F, [],  Node),
			 lists:usort(Res ++ Acc);
		     _ -> Acc
		 end
	 end,
    refac_syntax_lib:fold(Fun, [], AnnAST).
		 
do_add_a_tag(FileName, {AnnAST, Info}, Tag, AffectedInitialFuns, SearchPaths) ->
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    {InitialFuns1, ReceiveFuns1} = lists:unzip(AffectedInitialFuns),
    InitialFuns = lists:usort(lists:flatmap(fun({Init, _}) -> Init end, InitialFuns1)),
    ReceiveFuns = lists:usort(lists:append(ReceiveFuns1)),
    io:format("The current file under refactoring is:\n~p\n", [FileName]),
    {AnnAST1, _Changed} =refac_util:stop_tdTP(fun do_add_a_tag_1/2, AnnAST, {ModName, Tag, InitialFuns, ReceiveFuns}),
    OtherFiles = refac_util:expand_files(SearchPaths, ".erl") -- [FileName],
    Results = do_add_a_tag_in_other_modules(OtherFiles, Tag, InitialFuns, ReceiveFuns, SearchPaths),    
    [{{FileName, FileName}, AnnAST1} | Results].

do_add_a_tag_in_other_modules(Files, Tag, InitialFuns, ReceiveFuns, SearchPaths) ->
    case Files of
	[] ->
	    [];
	[F |Fs] ->
	    io:format("The current file under refactoring is:\n~p\n", [F]),
	    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(F, true, SearchPaths),
	    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
	    {AnnAST1, Changed} = refac_util:stop_tdTP(fun do_add_a_tag_1/2, AnnAST, {ModName, Tag, InitialFuns, ReceiveFuns}),
	    if Changed ->
		    [{{F, F}, AnnAST1} | do_add_a_tag_in_other_modules(Fs, Tag, InitialFuns, ReceiveFuns, SearchPaths)];
		true -> do_add_a_tag_in_other_modules(Fs, Tag, InitialFuns, ReceiveFuns, SearchPaths)
	    end
    end.
do_add_a_tag_1(Node, {ModName,Tag, InitialFuns, ReceiveFuns}) ->
    case refac_syntax:type(Node) of 
	function ->
	    {value, {fun_def, {M, F, A, _, _}}} = lists:keysearch(fun_def,1, refac_syntax:get_ann(Node)),
	    case lists:member({M, F, A}, ReceiveFuns) of 
		true ->
		    Node1 = refac_util:full_buTP(fun do_add_tag_to_receive_exprs/2, Node,  Tag),
		    {Node2, _} = refac_util:stop_tdTP(fun do_add_tag_to_send_exprs/2, Node1, {ModName,Tag, InitialFuns}),
		    {Node2, true};
		false ->
		   refac_util:stop_tdTP(fun do_add_tag_to_send_exprs/2, Node, {ModName, Tag, InitialFuns})
	    end;
	_ ->
	    {Node, false}
    end.

do_add_tag_to_receive_exprs(Node, Tag) ->
    F = fun(C) ->
		P = refac_util:ghead("do_add_tag_to_receive_exprs", refac_syntax:clause_patterns(C)), %%
		G = refac_syntax:clause_guard(C),
		B = refac_syntax:clause_body(C),
		P1 =case refac_syntax:type(P) of 
			tuple ->
			    Es = [refac_syntax:atom(Tag)| refac_syntax:tuple_elements(P)],
			    refac_syntax:tuple(Es);
			_-> refac_syntax:tuple([refac_syntax:atom(Tag), P])
		    end,		
		refac_syntax:copy_attrs(C, refac_syntax:clause([P1], G, B))
	end,	
    case refac_syntax:type(Node) of 
	receive_expr ->
	    Cs = refac_syntax:receive_expr_clauses(Node),
	    Cs1 = lists:map(F, Cs),
	    refac_syntax:copy_attrs(Node, refac_syntax:receive_expr(Cs1));
	_ -> Node
    end.

do_add_tag_to_send_exprs(Node, {ModName, Tag, AffectedInitialFuns}) ->
     case refac_syntax:type(Node) of
	infix_expr ->
	    case is_send_expr(Node) of 
		true ->{{Ln, _},_} = refac_util:get_range(Node), 
		        Dest = refac_syntax:infix_expr_left(Node),
			Msg = refac_syntax:infix_expr_right(Node),
			Ann = refac_syntax:get_ann(Dest),
			Op  = refac_syntax:infix_expr_operator(Node),
			InitialFuns = case lists:keysearch(pid,1, Ann) of 
					  {value, {pid, InitialFuns1}} ->
					      InitialFuns1;
					  _ -> case lists:keysearch(pname, 1, Ann) of 
						   {value, {pname, InitialFuns2}} ->
						       InitialFuns2;
						   _ -> []
					       end
				      end,						   
			case InitialFuns of 
			    [] -> io:format("\n*************************************Warning****************************************\n"),
				  io:format("Wrangler could not identify the recipent process of the send expression in module ~p at line ~p\n", [ModName,Ln]),
				{Node, false};
			    _ -> case InitialFuns -- AffectedInitialFuns of 
				     [] ->
					 Msg1 = case refac_syntax:type(Msg) of 
						    tuple ->
							refac_syntax:tuple([refac_syntax:atom(Tag) | refac_syntax:tuple_elements(Msg)]);
						    _ -> refac_syntax:tuple([refac_syntax:atom(Tag), Msg])
						end,
					 Node1 = refac_syntax:copy_attrs(Node, refac_syntax:infix_expr(Dest, Op, Msg1)), 
					 {Node1, true};
				     InitialFuns -> 
					 {Node, false};  
				     _ -> %% io:format("\n*************************************Warning****************************************\n"),
%% 					  io:format("The recipent process of the send expression in module ~p at line ~p could refer to multiple processes. \n", [ModName,Ln]),
					  {Node, false}
				 end
			end;
		_ -> {Node, false}
	    end;
	application ->
	    case is_send_expr(Node) of 
		true -> [ReceiverPid, Msg] = refac_syntax:application_arguments(Node),
			Operator = refac_syntax:application_operator(Node),
			Ann = refac_syntax:get_ann(ReceiverPid),
			case lists:keysearch(pid,1, Ann) of 
			    {value, {pid, InitialFuns}} ->
				io:format("IntialFuns:\n~p\n", [InitialFuns]),
				case InitialFuns of 
				    [] -> {Node, false};
				    _ -> case InitialFuns -- AffectedInitialFuns of 
					     [] ->
						 Msg1 = case refac_syntax:type(Msg) of 
							    tuple ->
								refac_syntax:tuple([Tag | refac_syntax:tuple_elements(Msg)]);
							    _ -> refac_syntax:tuple([Tag, Msg])
							end,
						 Node1 = refac_syntax:copy_attrs(Node, refac_syntax:application(Operator, [ReceiverPid, Msg1])), 
						 {Node1, true};
					     InitialFuns -> 
						 {Node, false};
					     _ -> io:format("Node:\n~p\n", [Node]),%% TODO: Undicidables;
						  {Node, false}
					 end
				end;
			    _ -> {Node, false}  %%TODO: undicicabels.
			end;	
		false -> {Node, false}
	    end;	
	_ -> {Node, false}
    end.
				
		    
pos_to_receive_fun(AnnAST, Pos) ->
    case refac_util:pos_to_fun_def(AnnAST, Pos) of 
	{ok, FunDef} ->
	    case has_receive_expr(FunDef) of 
		true -> {ok, FunDef};
		_ -> {error, "You have not selected a receive expression!"}
	    end;	    
	_  -> {error, "You have not selected a receive expression!"}
    end.

has_receive_expr(Node) ->
    case refac_util:once_tdTU(fun has_receive_expr/2, Node, []) of 
	{_, false} ->
	     false;
	{_R, true} -> true
    end.

has_receive_expr(Node, []) ->
    case refac_syntax:type(Node) of 
	receive_expr ->
	    {Node, true};
	_  ->{[], false}
    end.



collect_process_initial_funs(SearchPaths) ->
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    lists:flatmap(fun(F)-> {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(F, true, SearchPaths),
			   collect_process_initial_funs_1({AnnAST, Info}, SearchPaths)
		  end, Files).
			  
collect_fun_apps(Expr, {ModName, Ln}) ->
    Fun = fun (T, S) ->
		 case refac_syntax:type(T) of
		     application ->
			 Operator = refac_syntax:application_operator(T),
			 case lists:keysearch(fun_def,1,refac_syntax:get_ann(Operator)) of
			     {value, {fun_def, {M, F, A, _, _}}}-> lists:keysearch(fun_def,1, refac_syntax:get_ann(Operator)),
								   ordsets:add_element({M,F, A},S);
			     
			     _ -> io:format("\n*************************************Warning****************************************\n"),
				  io:format("Wrangler could not handle the spawn expression used in module ~p at line ~p\n", [ModName,Ln])
			 end;
		     arity_qualifier -> 
			 {value, {fun_def, {M, F, A, _, _}}} = lists:keysearch(fun_def,1, refac_syntax:get_ann(T)),
			 ordsets:add_element({M,F,A}, S);
		     _ -> S
		 end
	  end,
    lists:usort(refac_syntax_lib:fold(Fun, [], Expr)). 
    

get_fun_def({M, F, A}, SearchPaths) ->  
    Fun=fun(Node, {M1, F1, A1}) ->
	      case refac_syntax:type(Node) of
		  function ->
		      Ann  = refac_syntax:get_ann(Node),
		      case lists:keysearch(fun_def,1, Ann) of 
			  {value, {fun_def, {M1,F1, A1, _, _}}} ->
			      {Node, true};
			  _ -> {[], false}
		      end;
		  _ -> {[], false}
	      end
      end,		      
    Files =refac_util:expand_files(SearchPaths, ".erl"),
    FileNames =lists:filter(fun(F1) -> list_to_atom(filename:basename(F1, ".erl"))==M end, Files),
    case FileNames of 
	[] ->
	     {error, no_source_file};
	_ ->
	    FileName = hd(FileNames),
	    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths),
	    case refac_util:once_tdTU(Fun, AnnAST, {M, F, A}) of 
		{_, false} ->
		    {error, "Wrangler could not find the definition of "++ atom_to_list(M)++":"++atom_to_list(F)++"/"++integer_to_list(A)};
		{R, true} -> {ok, R}
	    end
    end.     


reached_receive_funs([], _SearchPaths) -> [];
reached_receive_funs(InitialFuns, SearchPaths) ->
    F  = fun(InitialFun) ->
 		 case InitialFun of 
 		     {Mod, Fun, Arity} -> [{Mod,Fun, Arity}];
 		     {FunExpr, {ModName, Ln}} -> collect_fun_apps(FunExpr, {ModName, Ln})
		 end
	 end,	
    {CallerCallee, _, _} = refac_callgraph_server:get_callgraph(SearchPaths),  %% should remove spawn pars before calcuate callgraph.
    ReachedFuns = lists:zip(InitialFuns, lists:map(fun({_SpawnExpr, InitialF}) -> 
							   reached_receive_funs_1(CallerCallee, F(InitialF), SearchPaths) end, 
						   InitialFuns)),
    ReachedFuns.
   

reached_receive_funs_1(CallerCallee, InitialAcc, SearchPaths) ->
    io:format("InitialAcc:\n~p\n", [InitialAcc]),
    Funs =reached_funs_1(CallerCallee, InitialAcc),
    io:format("Funs:\n~p\n", [Funs]),
    ReceiveFuns = lists:filter(fun(MFA) ->
				      case get_fun_def(MFA, SearchPaths) of 
					  {error, no_source_file} -> false;
					  {error, Reason} ->erlang:exit(Reason);
					  {ok, FunDef} -> has_receive_expr(FunDef)
				      end
			       end, Funs),
    ReceiveFuns.
			       

reached_funs_1(CallerCallee, Acc) ->
    Res = lists:usort(lists:concat(lists:map(fun({Mod, Fun, Args}) ->
					 case lists:keysearch({Mod, Fun, Args}, 1, CallerCallee) of 
					     {value, {{Mod, Fun, Args}, CalledFuns}} ->
						 CalledFuns;
					     _ ->[]
					 end
				 end, Acc))),
     case lists:usort(Res++Acc) == Acc of 
	true -> Res;
	_ -> reached_funs_1(CallerCallee, lists:usort(Res++Acc)) 
    end.
		       
   
is_send_expr(Tree) ->
    case refac_syntax:type(Tree) of 
	infix_expr ->
	    Op = refac_syntax:infix_expr_operator(Tree),
	    case refac_syntax:type(Op) of 
		operator ->   %% TODO: should also check the uses of erlang:send/2, erlang:send/3 and other variants of send.
		    refac_syntax:operator_name(Op) == '!';
		_ -> false
	    end;
	application -> Op = refac_syntax:application_operator(Tree),
		       Ann = refac_syntax:get_ann(Op),
		       case lists:keysearch(fun_def,1, Ann) of 
			   {value, {fun_def, {erlang, send, 2, _Pos1, _Pos2}}} -> true;
			   _ -> false
		       end;
 	_ ->false
    
    end.


is_spawn_expr(Tree) ->
    SpawnFuns1 = [{erlang, spawn, 1}, {erlang, spawn, 2}, {erlang, spawn, 3}, {erlang, spawn, 4},
		  {erlang, spawn_link, 1}, {erlang, spawn_link, 2}, {erlang, spawn_link, 3}, {erlang, spawn_link, 4},
		  {erlang, spawn_opt, 3}, {erlang, spawn_opt, 5}],
    SpawnFuns2 = [{erlang, spawn_monitor, 1}, {erlang, spawn_monitor, 3}, {erlang, spawn_opt, 2},
 		  {erlang, spawn_opt, 4}],  %% These funs return more than a single Pid.
    case refac_syntax:type(Tree) of
      application ->
	  Operator = refac_syntax:application_operator(Tree),
	  Ann = refac_syntax:get_ann(Operator),
	  case lists:keysearch(fun_def, 1, Ann) of
	    {value, {fun_def, {Mod, Fun, Arity, _, _}}} 
	      -> lists:member({Mod, Fun, Arity}, SpawnFuns1) orelse 
		     lists:member({Mod, Fun, Arity}, SpawnFuns2);
	      _ -> false
	  end;
      _ -> false
    end.



%% Qn: is it possible to get more static infomration? How to balance the usages of staic and dynamic info?
%% How do we know the existing trace info is uptodate?
%% what about the spawn expression has receive expressions?

%% Is theory, it is possible that a  receiver function is shared by different processes (processse with different 
%% entries); it practice, this rarely happen. We ignore this case for now.

