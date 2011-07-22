
%% Copyright (c) 2010, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% ============================================================================================
%% Refactoring: Add a tag to all the messages received by a process.

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
%%@private
-module(refac_add_a_tag).

-export([add_a_tag/6]).

-include("../include/wrangler_internal.hrl").

%%TODO: THIS MODULE IS IN NEED OF UPDATED.


%% =============================================================================================
%%-spec(add_a_tag/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->{ok, [filename()]} | {error, string()}).      
add_a_tag(FileName, Line, Col, Tag, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:add_a_tag(~p, ~p, ~p, ~p,~p, ~p).\n", [?MODULE, FileName, Line, Col, Tag, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":add_a_tag(" ++ "\"" ++ 
        FileName ++ "\", " ++ integer_to_list(Line) ++ 
        ", " ++ integer_to_list(Col) ++ ", " ++ "\"" ++ 
        Tag ++ "\"," ++ "[" ++ wrangler_misc:format_search_paths(SearchPaths) ++ "],"
         ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST1, _Info1}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    case pos_to_receive_fun(AnnAST1, {Line, Col}) of
	{ok, _FunDef} ->
	    _Res = wrangler_annotate_pid:ann_pid_info(SearchPaths, TabWidth),
	    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
	    {ok, FunDef} = pos_to_receive_fun(AnnAST, {Line, Col}),
	    ModName = get_module_name(FileName, Info),
	    case pre_cond_check(AnnAST, ModName, FunDef, SearchPaths, TabWidth) of
		{ok, AffectedInitialFuns} ->
		    Results = do_add_a_tag(FileName, {AnnAST, Info}, list_to_atom(Tag), AffectedInitialFuns, SearchPaths, TabWidth),
		    wrangler_write_file:write_refactored_files_for_preview(Results, TabWidth, Cmd),
		    ChangedFiles = [F || {{F, _F}, _AST} <- Results],
		    ?wrangler_io("The following files are to be changed by this refactoring:\n~p\n",
				 [ChangedFiles]),
		    {ok, ChangedFiles};
		{error, Reason} -> {error, Reason}
	    end;
	{error, Reason} -> {error, Reason}
    end.
	


pre_cond_check(_AnnAST, ModName, CurrentFunDef,  SearchPaths, TabWidth) ->
    FunName = wrangler_syntax:data(wrangler_syntax:function_name(CurrentFunDef)),
    Arity = wrangler_syntax:function_arity(CurrentFunDef),
    InitialFuns = collect_process_initial_funs(SearchPaths, TabWidth),
    AffectedInitialFuns = get_affected_initial_funs(InitialFuns, {ModName, FunName, Arity}, SearchPaths, TabWidth),
    case AffectedInitialFuns of 
	[] -> {error, "Wrangler could not figure out where the process is spawned."};
	[_H|T]-> case T of 
		    [] -> {ok, AffectedInitialFuns};
		    _ ->  case length(lists:usort(lists:map(fun({_InitialFun, ReceiveFuns, _}) -> ReceiveFuns end, AffectedInitialFuns))) of
			      1 -> {ok, AffectedInitialFuns};
			      _ -> {error, "The selected receive function is shared by processes with different set of receive expressions."}
			  end
		end
    end.
   

get_affected_initial_funs(InitialFuns, {ModName, FunName, Arity}, SearchPaths, TabWidth) ->
    ReachedReceiveFuns = reached_receive_funs(InitialFuns, SearchPaths, TabWidth),
    lists:filter(fun({_InitialFun, Fs}) ->
			 lists:member({ModName, FunName, Arity}, Fs) end, ReachedReceiveFuns).

collect_process_initial_funs(SearchPaths, TabWidth) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    lists:flatmap(fun
		      (F) -> {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(F, true, SearchPaths, TabWidth),
			     collect_process_initial_funs_1({F, AnnAST, Info}, SearchPaths)
		  end, Files).

collect_process_initial_funs_1({FileName, AnnAST, Info}, _SearchPaths) ->
    ModName = get_module_name(FileName, Info),
    HandleSpecialFuns = fun (Node) ->
				{{Ln, _}, _} = wrangler_misc:start_end_loc(Node),
				Op = wrangler_syntax:application_operator(Node),
				Arguments = wrangler_syntax:application_arguments(Node),
				Ann = wrangler_syntax:get_ann(Op),
				{value, {fun_def, {erlang, FunName, _Arity, _, _}}} = lists:keysearch(fun_def, 1, Ann),
				case lists:member(FunName, [spawn, spawn_link]) of
				    true ->
					{value, {pid, PidInfo}} = lists:keysearch(pid, 1, wrangler_syntax:get_ann(Node)),
					case Arguments of
					    [FunExpr] ->
						[{PidInfo, {FunExpr, {ModName, Ln}}, {FileName, Node}}];   %%Question: what about if the FunExpr contains free variables?
					    [_N, FunExpr] -> [{PidInfo, FunExpr, {FileName, Node}}];
					    [M, F, A] ->
						case {wrangler_syntax:type(M), wrangler_syntax:type(F), wrangler_syntax:type(A)} of
						    {atom, atom, list} ->
							[{PidInfo, {wrangler_syntax:atom_value(M),      %%TODO: need to use backward slice
								    wrangler_syntax:atom_value(F),
								    wrangler_syntax:list_length(A)}, {FileName, Node}}];
						    _ -> ?wrangler_io("\n*************************************Warning****************************************\n", []),
							 ?wrangler_io("Wrangler could not handle the spawn expression used in module ~p at line ~p\n", [ModName, Ln]),
							 []
						end;
					    [_N, M, F, A] ->
						case {wrangler_syntax:type(M), wrangler_syntax:type(F), wrangler_syntax:type(A)} of
						    {atom, atom, list} ->
							[{PidInfo, {wrangler_syntax:atom_value(M),      %%TODO: need to use backward slice
								    wrangler_syntax:atom_value(F),
								    wrangler_syntax:list_length(A)}, {FileName, Node}}];
						    _ -> ?wrangler_io("\n*************************************Warning****************************************\n", []),
							 ?wrangler_io("Wrangler could not handle the spawn expression used in module ~p at line ~p\n", [ModName, Ln]),
							 []
						end
					end;
				    _ ->
					?wrangler_io("\n*************************************Warning****************************************\n", []),
					?wrangler_io("Wrangler could not handle the spawn expression used in module ~p at line ~p\n", [ModName, Ln]),
					[]
				end
			end,
    Fun = fun (Node, Acc) ->
		  case wrangler_syntax:type(Node) of
		      function ->
			  F = fun (T, S) ->
				      case is_spawn_expr(T) of
					  true -> HandleSpecialFuns(T) ++ S;
					  _ -> S
				      end
			      end,
			  Res = api_ast_traverse:fold(F, [], Node),
			  lists:usort(Res ++ Acc);
		      _ -> Acc
		  end
	  end,
    api_ast_traverse:fold(Fun, [], AnnAST).

do_add_a_tag(FileName, {AnnAST, Info}, Tag, AffectedInitialFuns, SearchPaths, TabWidth) ->
    ModName = get_module_name(FileName, Info),
    {InitialFuns1, ReceiveFuns1} = lists:unzip(AffectedInitialFuns),
    InitialFuns = lists:usort(lists:flatmap(fun ({Init, _, _}) -> Init end, InitialFuns1)),
    InitialSpawnExprs = lists:usort(lists:map(fun ({_, _, SpawnExpr}) -> SpawnExpr end, InitialFuns1)),
    AffectedModsFuns = get_affected_mods_and_funs(InitialSpawnExprs, SearchPaths, TabWidth),
    ReceiveFuns = lists:usort(lists:append(ReceiveFuns1)),
    ?wrangler_io("The current file under refactoring is:\n~p\n", [FileName]),
    {AnnAST1, _Changed} = api_ast_traverse:stop_tdTP(fun do_add_a_tag_1/2, AnnAST, {ModName, Tag, InitialFuns, ReceiveFuns, AffectedModsFuns}),
    OtherFiles = wrangler_misc:expand_files(SearchPaths, ".erl") -- [FileName],
    Results = do_add_a_tag_in_other_modules(OtherFiles, Tag, InitialFuns, ReceiveFuns, AffectedModsFuns, SearchPaths, TabWidth),
    [{{FileName, FileName}, AnnAST1}| Results].

do_add_a_tag_in_other_modules(Files, Tag, InitialFuns, ReceiveFuns, AffectedModsFuns = {Mods, _Funs}, SearchPaths, TabWidth) ->
    case Files of
	[] ->
	    [];
	[F| Fs] ->
	    BaseName = list_to_atom(filename:basename(F, ".erl")),
	    case lists:member(BaseName, Mods) of
		true ->
		    ?wrangler_io("The current file under refactoring is:\n~p\n", [F]),
		    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(F, true, SearchPaths, TabWidth),
		    ModName = get_module_name(F, Info),
		    {AnnAST1, Changed} = api_ast_traverse:stop_tdTP(fun do_add_a_tag_1/2, AnnAST, {ModName, Tag, InitialFuns, ReceiveFuns, AffectedModsFuns}),
		    if Changed ->
			   [{{F, F}, AnnAST1}| do_add_a_tag_in_other_modules(Fs, Tag, InitialFuns, ReceiveFuns, AffectedModsFuns, SearchPaths, TabWidth)];
		       true -> do_add_a_tag_in_other_modules(Fs, Tag, InitialFuns, ReceiveFuns, AffectedModsFuns, SearchPaths, TabWidth)
		    end;
		_ -> do_add_a_tag_in_other_modules(Fs, Tag, InitialFuns, ReceiveFuns, AffectedModsFuns, SearchPaths, TabWidth)
	    end
    end.
do_add_a_tag_1(Node, {ModName, Tag, InitialFuns, ReceiveFuns, {_Mods, Funs}}) ->
    case wrangler_syntax:type(Node) of
      function ->
	  {value, {fun_def, {M, F, A, _, _}}} = lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(Node)),
	  case lists:member({M, F, A}, ReceiveFuns) of
	    true ->
		Node1 = api_ast_traverse:full_buTP(fun do_add_tag_to_receive_exprs/2, Node, Tag),
		%% Can a process send an expression to itself?
		%% {Node2, _} = refac_util:stop_tdTP(fun do_add_tag_to_send_exprs/2, Node1, {ModName,Tag, InitialFuns}), 
		{Node1, true};
	    false ->
		case lists:member({M, F, A}, Funs) of
		  true ->
		      api_ast_traverse:stop_tdTP(fun do_add_tag_to_send_exprs/2, Node, {ModName, Tag, InitialFuns});
		  _ ->
		      {Node, false}
		end
	  end;
      _ ->
	  {Node, false}
    end.

do_add_tag_to_receive_exprs(Node, Tag) ->
    F = fun (C) ->
		P = wrangler_misc:ghead("do_add_tag_to_receive_exprs", wrangler_syntax:clause_patterns(C)), %%
		G = wrangler_syntax:clause_guard(C),
		B = wrangler_syntax:clause_body(C),
		P1 = case wrangler_syntax:type(P) of
			 tuple ->
			     Es = [wrangler_syntax:atom(Tag)| wrangler_syntax:tuple_elements(P)],
			     wrangler_syntax:tuple(Es);
			 _ -> wrangler_syntax:tuple([wrangler_syntax:atom(Tag), P])
		     end,
		wrangler_syntax:copy_attrs(C, wrangler_syntax:clause([P1], G, B))
	end,
    case wrangler_syntax:type(Node) of
	receive_expr ->
	    Cs = wrangler_syntax:receive_expr_clauses(Node),
	    Cs1 = lists:map(F, Cs),
	    wrangler_syntax:copy_attrs(Node, wrangler_syntax:receive_expr(Cs1));
	_ -> Node
    end.

do_add_tag_to_send_exprs(Node, {_ModName, Tag, AffectedInitialFuns}) ->
    case wrangler_syntax:type(Node) of
	infix_expr ->
	    case is_send_expr(Node) of
		true -> {{_Ln, _}, _} = wrangler_misc:start_end_loc(Node),
			Dest = wrangler_syntax:infix_expr_left(Node),
			Msg = wrangler_syntax:infix_expr_right(Node),
			Ann = wrangler_syntax:get_ann(Dest),
			Op = wrangler_syntax:infix_expr_operator(Node),
			InitialFuns = case lists:keysearch(pid, 1, Ann) of
					  {value, {pid, InitialFuns1}} ->
					      InitialFuns1;
					  _ -> case lists:keysearch(pname, 1, Ann) of
						   {value, {pname, InitialFuns2}} ->
						       InitialFuns2;
						   _ -> []
					       end
				      end,
			case InitialFuns of
			    [] -> ?wrangler_io("\n*************************************Warning****************************************\n", []),
				  ?wrangler_io("Wrangler could not identify the recipent process of the send expression in module ~p at line ~p\n", [_ModName, _Ln]),
				  {Node, false};
			    _ -> case InitialFuns -- AffectedInitialFuns of
				     [] ->
					 Msg1 = case wrangler_syntax:type(Msg) of
						    tuple ->
							wrangler_syntax:tuple([wrangler_syntax:atom(Tag)| wrangler_syntax:tuple_elements(Msg)]);
						    _ -> wrangler_syntax:tuple([wrangler_syntax:atom(Tag), Msg])
						end,
					 Node1 = wrangler_syntax:copy_attrs(Node, wrangler_syntax:infix_expr(Dest, Op, Msg1)),
					 {Node1, true};
				     InitialFuns ->
					 {Node, false};
				     _ -> %% ?wrangler_io("\n*************************************Warning****************************************\n"),
					 %% 					  ?wrangler_io("The recipent process of the send expression in module ~p at line ~p could refer to multiple processes. \n", [ModName,Ln]),
					 {Node, false}
				 end
			end;
		_ -> {Node, false}
	    end;
	application ->
	    case is_send_expr(Node) of
		true -> [ReceiverPid, Msg] = wrangler_syntax:application_arguments(Node),
			Operator = wrangler_syntax:application_operator(Node),
			Ann = wrangler_syntax:get_ann(ReceiverPid),
			case lists:keysearch(pid, 1, Ann) of
			    {value, {pid, InitialFuns}} ->
				case InitialFuns of
				    [] -> {Node, false};
				    _ -> case InitialFuns -- AffectedInitialFuns of
					     [] ->
						 Msg1 = case wrangler_syntax:type(Msg) of
							    tuple ->
								wrangler_syntax:tuple([Tag| wrangler_syntax:tuple_elements(Msg)]);
							    _ -> wrangler_syntax:tuple([Tag, Msg])
							end,
						 Node1 = wrangler_syntax:copy_attrs(Node, wrangler_syntax:application(Operator, [ReceiverPid, Msg1])),
						 {Node1, true};
					     InitialFuns ->
						 {Node, false};
					     _ -> {Node, false}
					 end
				end;
			    _ -> {Node, false}  %%TODO: undicicabels.
			end;
		false -> {Node, false}
	    end;
	_ -> {Node, false}
    end.
pos_to_receive_fun(AnnAST, Pos) ->
    case api_interface:pos_to_fun_def(AnnAST, Pos) of
      {ok, FunDef} ->
	  case has_receive_expr(FunDef) of
	    true -> {ok, FunDef};
	    _ -> {error, "You have not selected a receive expression!"}
	  end;
      _ -> {error, "You have not selected a receive expression!"}
    end.

has_receive_expr(Node) ->
    case
      api_ast_traverse:once_tdTU(fun has_receive_expr/2, Node, [])
	of
      {_, false} ->
	  false;
      {_R, true} -> true
    end.

has_receive_expr(Node, []) ->
    case wrangler_syntax:type(Node) of
	receive_expr ->
	    {Node, true};
	_  ->{[], false}
    end.

collect_fun_apps(Expr, {_ModName, _Ln}) ->
    Fun = fun (T, S) ->
		  case wrangler_syntax:type(T) of
		      application ->
			  Operator = wrangler_syntax:application_operator(T),
			  case lists:keysearch(fun_def,1,wrangler_syntax:get_ann(Operator)) of
			      {value, {fun_def, {M, F, A, _, _}}} ->
				  ordsets:add_element({M, F, A},S);
			      _ -> ?wrangler_io("\n*************************************Warning****************************************\n",[]),
				   ?wrangler_io("Wrangler could not handle the spawn expression used in module ~p at line ~p\n", [_ModName,_Ln])
			  end;
		      arity_qualifier ->
			  {value, {fun_def, {M, F, A, _, _}}} = lists:keysearch(fun_def, 1, wrangler_syntax:get_ann(T)),
			  ordsets:add_element({M,F,A}, S);
		      _ -> S
		  end
	  end,
    lists:usort(api_ast_traverse:fold(Fun, [], Expr)).

get_fun_def({M, F, A}, SearchPaths, TabWidth) ->
    Fun = fun (Node, {M1, F1, A1}) ->
		  case wrangler_syntax:type(Node) of
		      function ->
			  Ann = wrangler_syntax:get_ann(Node),
			  case lists:keysearch(fun_def, 1, Ann) of
			      {value, {fun_def, {M1, F1, A1, _, _}}} ->
				  {Node, true};
			      _ -> {[], false}
			  end;
		      _ -> {[], false}
		  end
	  end,
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    FileNames = lists:filter(fun (F1) -> list_to_atom(filename:basename(F1, ".erl")) == M end, Files),
    case FileNames of
	[] ->
	    {error, no_source_file};
	_ ->
	    FileName = hd(FileNames),
	    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
	    case api_ast_traverse:once_tdTU(Fun, AnnAST, {M, F, A}) of
		{_, false} ->
		    {error, "Wrangler could not find the definition of " ++ atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A)};
		{R, true} -> {ok, R}
	    end
    end.


reached_receive_funs([], _SearchPaths, _TabWidth) -> [];
reached_receive_funs(InitialFuns, SearchPaths, TabWidth) ->
    F  = fun(InitialFun) ->
 		 case InitialFun of 
 		     {Mod, Fun, Arity} -> [{Mod,Fun, Arity}];
 		     {FunExpr, {ModName, Ln}} -> collect_fun_apps(FunExpr, {ModName, Ln})
		 end
	 end,	
    CallGraph = wrangler_callgraph_server:get_callgraph(SearchPaths),  
    ReachedFuns = lists:zip(InitialFuns, lists:map(fun({_PidInfo, InitialF, _SpawnExpr}) -> 
							   reached_receive_funs_1(CallGraph#callgraph.callercallee, F(InitialF), SearchPaths, TabWidth) end, 
						   InitialFuns)),
    ReachedFuns.
   

reached_receive_funs_1(CallerCallee, InitialAcc, SearchPaths, TabWidth) ->
    Funs =reached_funs_1(CallerCallee, InitialAcc),
    ReceiveFuns = lists:filter(fun(MFA) ->
				      case get_fun_def(MFA, SearchPaths, TabWidth) of 
					  {error, no_source_file} -> false;
					  {error, Reason} ->throw({error, Reason});
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
    case wrangler_syntax:type(Tree) of
	infix_expr ->
	    Op = wrangler_syntax:infix_expr_operator(Tree),
	    case wrangler_syntax:type(Op) of
		operator   %% TODO: should also check the uses of erlang:send/2, erlang:send/3 and other variants of send.
		         ->
                    wrangler_syntax:operator_name(Op) == '!';
		_ -> false
	    end;
	application -> Op = wrangler_syntax:application_operator(Tree),
		       Ann = wrangler_syntax:get_ann(Op),
		       case lists:keysearch(fun_def,1, Ann) of 
			   {value, {fun_def, {erlang, send, 2, _Pos1, _Pos2}}} -> true;
			   _ -> false
		       end;
 	_ ->false
    end.

is_spawn_expr(Tree) ->
    SpawnFuns1 = wrangler_misc:spawn_funs(),
    SpawnFuns2 = [{erlang, spawn_monitor, 1}, {erlang, spawn_monitor, 3}, {erlang, spawn_opt, 2},
		  {erlang, spawn_opt, 4}],  %% These funs return more than a single Pid.
    case wrangler_syntax:type(Tree) of
	application ->
	    Operator = wrangler_syntax:application_operator(Tree),
	    Ann = wrangler_syntax:get_ann(Operator),
	    case lists:keysearch(fun_def, 1, Ann) of
		{value, {fun_def, {Mod, Fun, Arity, _, _}}} ->
		    lists:member({Mod, Fun, Arity}, SpawnFuns1) orelse 
		      lists:member({Mod, Fun, Arity}, SpawnFuns2);
		_ -> false
	    end;
	_ -> false
    end.

get_affected_mods_and_funs(SpawnExprs, SearchPaths, TabWidth) ->
    SliceRes =forward_slice(SpawnExprs, SearchPaths, [], TabWidth),
    AffectedFuns = lists:usort(lists:map(fun({{Mod, Fun, Arity, _}, _Value}) -> 
						 {Mod, Fun, Arity} end, SliceRes)),
    {Mods, _, _} = lists:unzip3(AffectedFuns),
    AffectedMods = lists:usort(Mods),
    {AffectedMods, AffectedFuns}.
    
forward_slice([], _SearchPaths, Acc, _TabWidth) -> Acc;
forward_slice([{FileName, Expr}|T], SearchPaths, Acc, TabWidth) ->
   case forward_slice_1(FileName, Expr, SearchPaths, TabWidth) of 
       {ok, Res} -> 
	   forward_slice(T, SearchPaths, Res ++ Acc, TabWidth);
       {error, Msg} ->
	   {error, Msg}
   end.

forward_slice_1(FileName, Expr, SearchPaths, TabWidth) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
    {StartPos, EndPos} = wrangler_misc:start_end_loc(Expr),
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FileName, Info),
    case api_interface:pos_to_expr(AnnAST, StartPos, EndPos) of
	{ok, Expr} ->
	    case api_interface:pos_to_fun_def(AnnAST, StartPos) of
		{ok, FunDef} ->
		    {ok, wrangler_slice:forward_slice(Files, AnnAST, ModName, FunDef, Expr)};
		_ ->
		    {error, "Forward slicing failed"}
	    end;
	_ -> {error, "Foward slicing failed"}
    end.

%% Qn: is it possible to get more static infomration? How to balance the usages of staic and dynamic info?
%% How do we know the existing trace info is uptodate?
%% what about the spawn expression has receive expressions?

%% Is theory, it is possible that a  receiver function is shared by different processes (processse with different 
%% entries); it practice, this rarely happen. We ignore this case for now.

get_module_name(FName, Info) ->
    ModName = case lists:keysearch(module, 1, Info) of
		{value, {module, Mod}} -> Mod;
		_ -> list_to_atom(filename:basename(FName, ".erl"))
	      end,
    ModName.
