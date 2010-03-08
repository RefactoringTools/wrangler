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
%% ===========================================================================================
%% Refactoring: Search an user-selected expression/expression sequence from the current buffer.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
-module(refac_sim_expr_search).

-export([sim_expr_search_in_buffer/6,
	 sim_expr_search_in_dirs/6, normalise_record_expr/5]).

-export([find_anti_unifier/5, generalise_expr/3]).

-include("../include/wrangler.hrl").

-define(DefaultSimiScore, 0.8).


%% ================================================================================================
%% @doc Search for expressions that are 'similar' to an expression/expression sequence selected by
%%      the user in the current Erlang buffer.
%% <p> Given expression selected by the user, A say, expression B is similar to A if there exist a least 
%% general common abstation, C, such that, substitution C(Args1) = A, and C(Arg2) == B, and 
%% Size(C) /Size(A) > the threshold specified (0.8 by default).
%% </p>
%% <p> In the case that code selected contains multiple, but non-continuous, sequence of expressions, the first
%% continuous sequence of expressions is taken as the expression selected by the user. A continuous sequence of
%% expressions is a sequence of expressions separated by ','.
%% <p>

-spec(sim_expr_search_in_buffer/6::(filename(), pos(), pos(), string(),[dir()],integer())
      -> {ok, [{integer(), integer(), integer(), integer()}]}).    
sim_expr_search_in_buffer(FName, Start = {Line, Col}, End = {Line1, Col1}, SimiScore0, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_expr_search(~p, {~p,~p},{~p,~p},~p, ~p, ~p).\n",
		 [?MODULE, FName, Line, Col, Line1, Col1, SimiScore0, SearchPaths, TabWidth]),
    SimiScore = get_simi_score(SimiScore0),
    {FunDef, Exprs, SE} = get_fundef_and_expr(FName, Start, End, SearchPaths, TabWidth),
    {Ranges, AntiUnifier} = search_and_gen_anti_unifier(FName, {FName, FunDef, Exprs, SE}, SimiScore, SearchPaths, TabWidth),
    refac_code_search_utils:display_search_results(Ranges, AntiUnifier, "similar").


-spec(sim_expr_search_in_dirs/6::(filename(), pos(), pos(), string(),[dir()],integer())
      -> {ok, [{integer(), integer(), integer(), integer()}]}).    
sim_expr_search_in_dirs(FileName, Start = {Line, Col}, End = {Line1, Col1}, SimiScore0, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_expr_search_in_dirs(~p, {~p,~p},{~p,~p},~p, ~p, ~p).\n",
		 [?MODULE, FileName, Line, Col, Line1, Col1, SearchPaths, SearchPaths, TabWidth]),
    Files = [FileName| refac_util:expand_files(SearchPaths, ".erl") -- [FileName]],
    SimiScore = get_simi_score(SimiScore0),
    {FunDef, Exprs, SE} = get_fundef_and_expr(FileName, Start, End, SearchPaths, TabWidth),
    {Ranges, AntiUnifier} = search_and_gen_anti_unifier(Files, {FileName, FunDef, Exprs, SE}, SimiScore, SearchPaths, TabWidth),
    refac_code_search_utils:display_search_results(Ranges, AntiUnifier, "similar").

search_and_gen_anti_unifier(Files, {FName,FunDef, Exprs, SE}, SimiScore, SearchPaths, TabWidth) ->
    {_Start, End} = SE,
    Res =lists:append([search_similar_expr_1(F, Exprs, SimiScore, SearchPaths, TabWidth) ||F<-Files]),
    {Ranges, ExportVars, SubSt} = lists:unzip3(Res),
    ExportVars1 = {element(1, lists:unzip(vars_to_export(FunDef, End, Exprs))),
		   lists:usort(lists:append(ExportVars))},
    AntiUnifier = generalise_expr(Exprs, SubSt, ExportVars1),
    {[{FName, SE}| Ranges -- [{FName, SE}]], AntiUnifier}.

search_similar_expr_1(FName, Exprs, SimiScore, SearchPaths, TabWidth) ->
    try refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth) of
	{ok, {AnnAST, _}} ->
	    RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
	    do_search_similar_expr(FName, AnnAST, RecordInfo, Exprs, SimiScore)
    catch 
	_E1:_E2 ->
	    []
    end.


do_search_similar_expr(FileName, AnnAST, RecordInfo, Exprs, SimiScore) when is_list(Exprs) ->
    F0 = fun (FunNode, Acc) ->
		 F = fun (T, Acc1) ->
			     Exprs1 = get_expr_seqs(T),
			     do_search_similar_expr_1(FileName, Exprs, Exprs1, RecordInfo, SimiScore, FunNode) ++ Acc1
		     end,
		 refac_syntax_lib:fold(F, Acc, FunNode)
	 end,
    do_search_similar_expr_1(AnnAST, F0).


do_search_similar_expr_1(AnnAST, Fun) ->
    F1 = fun (Node, Acc) ->
		 case refac_syntax:type(Node) of
		   function -> Fun(Node, Acc);
		   _ -> Acc
		 end
	 end,
    lists:reverse(refac_syntax_lib:fold(F1, [], AnnAST)).


get_expr_seqs(T) ->
    case refac_syntax:type(T) of
	clause ->
	    refac_syntax:clause_body(T);
	block_expr ->
	    refac_syntax:block_expr_body(T);
	try_expr ->
	    refac_syntax:try_expr_body(T);
	_ -> []
    end.

overlapped_locs({Start1, End1}, {Start2, End2}) ->
    Start1 =< Start2 andalso End2 =< End1 orelse
      Start2 =< Start1 andalso End1 =< End2.


do_search_similar_expr_1(FileName, Exprs1, Exprs2, RecordInfo, SimiScore, FunNode) ->
    Len1 = length(Exprs1),
    Len2 = length(Exprs2),
    case Len1 =< Len2 of
      true -> Exprs21 = lists:sublist(Exprs2, Len1),
	      {S1, E1} = refac_misc:get_start_end_loc(Exprs1),
	      {S2, E2} = refac_misc:get_start_end_loc(Exprs21),
	      case overlapped_locs({S1, E1}, {S2, E2}) of
		true -> [];
		_ ->
		    ExportVars = vars_to_export(FunNode, E2, Exprs21),
		    find_anti_unifier(FileName, Exprs1, normalise_expr(Exprs21, RecordInfo), SimiScore, ExportVars)
		      ++ do_search_similar_expr_1(FileName, Exprs1, tl(Exprs2), RecordInfo, SimiScore, FunNode)
	      end;
      _ -> []
    end.


find_anti_unifier(FileName, Expr1, Expr2, SimiScore, Expr2ExportVars) ->
    try
      do_find_anti_unifier(Expr1, Expr2)
    of
      SubSt ->
	  {SubExprs1, SubExprs2} = lists:unzip(SubSt),
	  Score1 = simi_score(Expr1, SubExprs1),
	  Score2 = simi_score(Expr2, SubExprs2),
	  case Score1 >= SimiScore andalso Score2 >= SimiScore of
	    true ->
		case subst_check(Expr1, SubSt) of
		  false ->
		      [];
		  _ ->
		      {SLoc, ELoc} = refac_misc:get_start_end_loc(Expr2),
		      EVs1 = [E1 || {E1, E2} <- SubSt, refac_syntax:type(E2) == variable,
				    lists:member({refac_syntax:variable_name(E2), get_var_define_pos(E2)}, Expr2ExportVars)],
		      [{{FileName, {SLoc, ELoc}}, EVs1, SubSt}]
		end;
	    _ -> []
	  end
    catch
      _ -> []
    end.

simi_score(Expr, SubExprs) ->
    case no_of_nodes(Expr) of
      0 -> 0;
      ExprSize ->
	  NonVarExprs = [E || E <- SubExprs, refac_syntax:type(E) =/= variable],
	  NoOfNewVars = length(NonVarExprs),
	  Res = 1 - (no_of_nodes(SubExprs) - length(SubExprs) + NoOfNewVars * (NoOfNewVars + 1) / 2) / ExprSize,
	  %% Res =1 -((no_of_nodes(SubExprs)-length(SubExprs))/ExprSize),
	  Res
    end.

subst_check(Expr1, SubSt) ->
    BVs = refac_util:get_bound_vars(Expr1),
    F = fun ({E1, E2}) ->
		case refac_syntax:type(E1) of
		    variable ->
			case is_macro_name(E1) of
			    true ->
				false;
			    _ -> 
				{value, {def, DefPos}} = lists:keysearch(def, 1, refac_syntax:get_ann(E1)),
				%% local vars should have the same substitute.
				not lists:any(fun ({E11, E21}) ->
						      refac_syntax:type(E11) == variable 
							  andalso
							    {value, {def, DefPos}} == lists:keysearch(def, 1, refac_syntax:get_ann(E11))
							  andalso
							  refac_prettypr:format(reset_attrs(E2))
							  =/= refac_prettypr:format(reset_attrs(E21))
					      end, SubSt)
			end;
		    _ ->
			%% the expression to be replaced should not contain local variables.
			BVs -- refac_util:get_free_vars(E1) == BVs
		end
	end,
    case BVs of
      [] ->
	  true;
      _ -> lists:all(F,SubSt)
    end.
    
reset_attrs(Node) ->
    refac_util:full_buTP(fun (T, _Others) ->
				 T1 =refac_syntax:set_ann(T, []),
				 refac_syntax:remove_comments(T1)
			 end, 
			 Node, {}).

   
do_find_anti_unifier(Exprs1, Exprs2) when is_list(Exprs1) andalso is_list(Exprs2) ->
    case length(Exprs1) == length(Exprs2) of
      true ->
	  case Exprs1 of
	    [] -> [];
	    _ ->
		lists:append([do_find_anti_unifier(E1, E2) || {E1, E2} <- lists:zip(Exprs1, Exprs2)])
	  end;
      false ->
	  ?debug("Does not unify 1:\n~p\n", [{Exprs1, Exprs2}]),
	  throw(non_unifiable)
    end;
do_find_anti_unifier(Expr1, _Expr2) when is_list(Expr1) ->
    ?debug("Does not unify 2:n~p\n", [{Expr1, _Expr2}]),
    throw(non_unifiable);
do_find_anti_unifier(_Expr1, Expr2) when is_list(Expr2) ->
    ?debug("Does not unify 3:\n~p\n", [{_Expr1, Expr2}]),
    throw(non_unifiable);
do_find_anti_unifier(Expr1, Expr2) ->
    F = fun (E1, E2) ->
		SubExprs1 = refac_syntax:subtrees(E1),
		SubExprs2 = refac_syntax:subtrees(E2),
		try
		  do_find_anti_unifier(SubExprs1, SubExprs2)
		of
		  Subst -> Subst
		catch
		  _ -> case refac_misc:variable_replaceable(E1) andalso refac_misc:variable_replaceable(E2) of
			 true ->
			     [{E1, E2}];
			 _ -> throw(non_unificable)
		       end
		end
	end,
    T1 = refac_syntax:type(Expr1),
    T2 = refac_syntax:type(Expr2),
    case T1 == T2 of
      false ->
	  case refac_misc:variable_replaceable(Expr1) andalso refac_misc:variable_replaceable(Expr2) of
	    true -> [{Expr1, Expr2}];
	    false ->
		?debug("Does not unify 4:\n~p\n", [{Expr1, Expr2}]),
		throw(non_unifiable)
	  end;
      true -> case refac_syntax:is_literal(Expr1) andalso refac_syntax:is_literal(Expr2) of
		true ->
		    case refac_syntax:concrete(Expr1) == refac_syntax:concrete(Expr2) of
		      true ->
			  [];
		      _ ->
			  case refac_misc:variable_replaceable(Expr1) andalso refac_misc:variable_replaceable(Expr2) of
			    true ->
				case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Expr1)) of
				  {value, {fun_def, {M, _, A, _, _}}} ->
				      case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Expr2)) of
					{value, {fun_def, {M, _, A, _, _}}} ->
					    [];
					_ -> [{Expr1, Expr2}]
				      end;
				  _ ->
				      [{Expr1, Expr2}]
				end;
			    false -> throw(non_unificable)
			  end
		    end;
		_ -> case T1 of
		       variable ->
			   case {is_macro_name(Expr1), is_macro_name(Expr2)} of
			     {true, true} ->
				 case refac_code_search_utils:identifier_name(Expr1) ==
					refac_code_search_utils:identifier_name(Expr2)
				     of
				   true -> [];
				   false -> throw(non_unifiable)
				 end;
			     {false, false} -> [{Expr1, Expr2}];
			     _ ->
				 ?debug("Does not unify 5:\n~p\n", [{Expr1, Expr2}]),
				 throw(non_unifiable)
			   end;
		       operator -> case refac_syntax:operator_name(Expr1) == refac_syntax:operator_name(Expr2) of
				     true -> [];
				     false ->
					 ?debug("Does not unify 6:\n~p\n", [{Expr1, Expr2}]),
					 throw(non_unifiable)
				   end;
		       underscore -> [];
		       macro -> MacroName1 = refac_syntax:macro_name(Expr1),
				MacroName2 = refac_syntax:macro_name(Expr2),
				case refac_code_search_utils:identifier_name(MacroName1) =/=
				       refac_code_search_utils:identifier_name(MacroName2)
				    of
				  true -> [{Expr1, Expr2}];
				  false -> F(Expr1, Expr2)
				end;
		       _ -> case refac_syntax:is_leaf(Expr1) of
			      true -> case refac_misc:variable_replaceable(Expr1) andalso
					     refac_misc:variable_replaceable(Expr2)
					  of
					true ->
					    [{Expr1, Expr2}];
					_ ->
					    ?debug("Does not unify 7:\n~p\n", [{Expr1, Expr2}]),
					    throw(non_unifiable)
				      end;
			      false -> F(Expr1, Expr2)
			    end
		     end
	      end
    end.
    
is_macro_name(Exp) ->
    {value, {category, macro_name}} == 
	lists:keysearch(category, 1, refac_syntax:get_ann(Exp)).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generalise_expr(Exprs, SearchRes, ExportVars) ->
    FunName = refac_syntax:atom(new_fun),
    BVs = refac_util:get_bound_vars(Exprs),
    FVs = lists:ukeysort(2, refac_util:get_free_vars(Exprs)),
    {NewExprs, NewExportVars} = generalise_expr_1(Exprs, SearchRes, ExportVars),
    NewExprs1 = case NewExportVars of
		  [] -> NewExprs;
		  [V] -> NewExprs ++ [refac_syntax:variable(V)];
		  _ -> E = refac_syntax:tuple([refac_syntax:variable(V) || V <- NewExportVars]),
		       NewExprs ++ [E]
		end,
    Pars = refac_misc:collect_var_names(NewExprs) -- element(1, lists:unzip(BVs)),
    FVPars = [V || {V, _} <- FVs, lists:member(V, Pars)],
    NewVarPars = Pars -- FVPars,
    Pars1 = [refac_syntax:variable(V) || V <- FVPars] ++
	      [refac_syntax:variable(V) || V <- NewVarPars],
    C = refac_syntax:clause(refac_util:remove_duplicates(Pars1), none, NewExprs1),
    refac_syntax:function(FunName, [C]).
   
    
generalise_expr_1(Exprs, Subst, ExportVars) when is_list(Exprs)->
    BlockExpr =refac_syntax:block_expr(Exprs),
    FVs = refac_util:get_free_vars(Exprs),
    {E, NewExportVars} = generalise_expr_2(BlockExpr, Subst, FVs, ExportVars),
    {refac_syntax:block_expr_body(E), NewExportVars};
generalise_expr_1(Expr, Subst, ExportVars) ->
    FVs = refac_util:get_free_vars(Expr),
    {E, NewExportVars}=generalise_expr_2(Expr, Subst, FVs, ExportVars),
    {[E], NewExportVars}.

generalise_expr_2(Expr, Subst, ExprFreeVars, {ExportVars1, ExportVars2}) ->
    case lists:all(fun (S) -> S == [] end, Subst) of
      true -> {Expr, ExportVars1};
      _ ->
	  UsedVarNames = sets:from_list(refac_misc:collect_var_names(Expr)),
	  Pid = refac_code_search_utils:start_counter_process(UsedVarNames),
	  ExportVars3 = [E || E <- ExportVars2, refac_syntax:type(E) =/= variable],
	  {Expr1, _} = refac_util:stop_tdTP(fun do_replace_expr_with_var_1/2, Expr,
					    {Subst, ExprFreeVars, Pid, ExportVars3}),
	  NewVarsToExport = refac_code_search_utils:get_new_export_vars(Pid),
	  refac_code_search_utils:stop_counter_process(Pid),
	  VarsToExport1 = ExportVars1 ++
			    [refac_syntax:variable_name(E)
			     || E <- ExportVars2, refac_syntax:type(E) == variable] ++
			      NewVarsToExport,
	  VarsToExport = refac_util:remove_duplicates(VarsToExport1),
	  {Expr1, VarsToExport}
    end.

do_replace_expr_with_var_1(Node, {SubSt, ExprFreeVars, Pid, ExportExprs}) ->
    F = fun (S, Name) -> Es = [refac_prettypr:format(E2)
			       || {E1, E2} <- S,
				  refac_syntax:type(E1) == variable,
				  refac_syntax:variable_name(E1) == Name],
			 length(sets:to_list(sets:from_list(Es))) == 1
	end,
    ExprsToReplace = sets:from_list([E1 || S <- SubSt, {E1, _E2} <- S]),
    case sets:is_element(Node, ExprsToReplace) of
      true ->
	  FVs = refac_util:get_free_vars(Node),
	  case refac_syntax:type(Node) of
	    variable ->
		case FVs of
		  [] -> {Node, false};
		  _ -> case FVs -- ExprFreeVars of
			 [] ->
			     Name = refac_syntax:variable_name(Node),
			     case lists:all(fun (S) -> F(S, Name) end, SubSt) of
			       true -> {Node, false};
			       _ -> NewVar = refac_code_search_utils:gen_new_var_name(Pid),
				    case lists:member(Node, ExportExprs) of
				      true ->
					  refac_code_search_utils:add_new_export_var(Pid, NewVar);
				      _ -> ok
				    end,
				    {refac_util:rewrite(Node, refac_syntax:variable(NewVar)), true}
			     end;
			 _ -> {Node, false}
		       end
		end;
	    _ ->
		NewVar = refac_code_search_utils:gen_new_var_name(Pid),
		case lists:member(Node, ExportExprs) of
		  true ->
		      refac_code_search_utils:add_new_export_var(Pid, NewVar);
		  _ -> ok
		end,
		{refac_util:rewrite(Node, refac_syntax:variable(NewVar)), true}
	  end;
      _ -> {Node, false}
    end.

normalise_expr(Exprs, RecordInfo) ->
    try normalise_record_expr(Exprs, RecordInfo) of
	Exprs1->
	    Exprs1
    catch 
	_E1:_E2 ->
	    Exprs
    end.
	    
   
normalise_record_expr(Exprs, RecordInfo)->
     [refac_util:full_buTP(fun do_normalise_record_expr_1/2, E, {RecordInfo, true})|| E<-Exprs].
    
get_simi_score(SimiScore0) ->
    try  case SimiScore0 of
	     [] -> ?DefaultSimiScore;
	     _ -> list_to_float(SimiScore0)
	 end
    catch
	V -> case V>=0.1 andalso V=<1.0 of 
		 true -> V;
		 _ ->?DefaultSimiScore
	     end;			      
	_:_ -> throw({error, "Parameter input is invalid."})
    end.
    

get_fundef_and_expr(FName, Start, End, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    case refac_util:pos_to_fun_def(AnnAST, Start) of
      {ok, FunDef} ->
	  Exprs = refac_util:pos_to_expr_list(FunDef, Start, End),
	  case Exprs of
	    [] -> throw({error, "You have not selected an expression!"});
	    _ ->
		SE = refac_misc:get_start_end_loc(Exprs),
		RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
		Exprs1 = normalise_expr(Exprs, RecordInfo),
		{FunDef, Exprs1, SE}
	  end;
      {error, _} -> throw({error, "You have not selected an expression!"})
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refactoring: Normalise record expression.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%-spec(normalise_record_expr/5::(filename(), pos(), bool(), [dir()], integer()) -> {error, string()} | {ok, [filename()]}).
normalise_record_expr(FName, Pos={Line, Col}, ShowDefault, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:normalise_record_expr(~p, {~p,~p},~p, ~p, ~p).\n", 
		 [?MODULE, FName, Line, Col, ShowDefault, SearchPaths,TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":normalise_record_expr(" ++ "\"" ++
	FName ++ "\", {" ++ integer_to_list(Line) ++", " ++ integer_to_list(Col) ++ "},"
	 ++ atom_to_list(ShowDefault)++ " [" ++ refac_util:format_search_paths(SearchPaths)
	++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, _Info}} =refac_util:parse_annotate_file(FName,true, [], TabWidth),
    RecordExpr =pos_to_record_expr(AnnAST, Pos),
    case refac_syntax:type(refac_syntax:record_expr_type(RecordExpr)) of
	atom -> ok;
	_ -> throw({error, "Wrangler can only normalise a record expression with an atom as the record name."})
    end,
    {AnnAST1, _Changed} = normalise_record_expr_1(FName, AnnAST,Pos,ShowDefault, SearchPaths, TabWidth),
    refac_util:write_refactored_files_for_preview([{{FName, FName}, AnnAST1}], Cmd),
    {ok, [FName]}.


normalise_record_expr_1(FName, AnnAST,Pos, ShowDefault, SearchPaths, TabWidth) ->
    RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
    refac_util:stop_tdTP(fun do_normalise_record_expr/2, AnnAST, {Pos,RecordInfo, ShowDefault}).
    

do_normalise_record_expr(Node, {Pos, RecordInfo, ShowDefault}) ->
    case refac_syntax:type(Node) of
      record_expr ->
	  {S, E} = refac_util:get_range(Node),
	  case S =< Pos andalso Pos =< E of
	    true ->
		{refac_util:full_buTP(fun do_normalise_record_expr_1/2,
				      Node, {RecordInfo, ShowDefault}), true};
	    _ -> {Node, false}
	  end;
      _ -> {Node, false}
    end.

do_normalise_record_expr_1(Node, {RecordInfo, ShowDefault}) ->
    Fun = fun ({FName, FVal}, Fields) ->
		  R = [F || F <- Fields, refac_syntax:type(refac_syntax:record_field_name(F)) == atom,
			    refac_syntax:concrete(refac_syntax:record_field_name(F)) == FName],
		  case R of
		    [F] when ShowDefault -> [F];
		    [F] -> V = refac_syntax:record_field_value(F),
			   Cond = refac_syntax:type(V) == atom andalso refac_syntax:concrete(V) == undefined orelse
				    FVal =/= none andalso refac_prettypr:format(V) == refac_prettypr:format(FVal),
			   case Cond of
			     true -> [];
			     false -> [F]
			   end;
		    [] ->
			Fs = [F || F <- Fields, refac_syntax:type(refac_syntax:record_field_name(F)) == underscore],
			case Fs of
			  [F] ->
			      [refac_syntax:record_field(refac_syntax:atom(FName), refac_syntax:record_field_value(F))];
			  [] when ShowDefault ->
			      case FVal of
				none -> [refac_syntax:record_field(
					   refac_syntax:atom(FName), set_random_pos(refac_syntax:atom(undefined)))];
				_ -> [refac_syntax:record_field(refac_syntax:atom(FName), set_random_pos(FVal))]
			      end;
			  _ -> []
			end
		  end
	  end,
    case refac_syntax:type(Node) of
      record_expr ->
	  Arg = refac_syntax:record_expr_argument(Node),
	  Type = refac_syntax:record_expr_type(Node),
	  Fields = refac_syntax:record_expr_fields(Node),
	  case refac_syntax:type(Type) of
	    atom ->
		case lists:keysearch(refac_syntax:concrete(Type), 1, RecordInfo) of
		  {value, {_, Fields1}} ->
		      Fields2 = lists:append([Fun(F, Fields) || F <- Fields1]),
		      refac_util:rewrite(Node, refac_syntax:record_expr(Arg, Type, Fields2));
		  _ ->
		      Node
		end;
	    _ -> Node
	  end;
      _ -> Node
    end.

set_random_pos(Node) ->
    refac_syntax:set_pos(Node, {-random:uniform(200), -random:uniform(200)}).
 
pos_to_record_expr(Tree, Pos) ->
    case refac_util:once_tdTU(fun pos_to_record_expr_1/2, Tree, Pos) of 
	{_, false} ->
	     throw({error, "You have not selected a record expression, "
		   "or the function containing the record expression selected does not parse."});
	{R, true} -> 
	    R
    end.

pos_to_record_expr_1(Node, Pos) ->
    case refac_syntax:type(Node) of
      record_expr ->
	  {S, E} = refac_util:get_range(Node),
	  case S =< Pos andalso Pos =< E of
	    true -> {Node, true};
	    _ -> {[], false}
	  end;
      _ -> {[], false}
    end.


get_module_record_info(FName, SearchPaths, TabWidth) ->
    Dir = filename:dirname(FName),
    DefaultIncl = [filename:join(Dir, X) || X <-refac_util:default_incls()],
    Includes = SearchPaths++DefaultIncl,
    case refac_epp:parse_file(FName, Includes,[], TabWidth, refac_util:file_format(FName)) of 
	{ok, Forms, _} -> Forms1 =[F || F <-Forms, case F of 
						       {attribute, _, file, _} -> false;
						       {attribute, _, type, {{record, _}, _, _}} -> false;
						       _ -> true
						   end],
			  SyntaxTree = refac_recomment:recomment_forms(Forms1,[]),
			  Info = refac_syntax_lib:analyze_forms(SyntaxTree),
			  case lists:keysearch(records,1, Info) of 
			      {value, {records, Records}} -> Records;
			      _ ->[]
			  end;
	{error, _Reason} -> []
    end.


no_of_nodes(Nodes) when is_list(Nodes) ->
    lists:sum([no_of_nodes(N)||N<-Nodes]);
no_of_nodes(Node) ->
    case refac_syntax:is_leaf(Node) of
	true -> 1;
	_ ->
	    lists:sum([no_of_nodes(T)||
			  T<-refac_syntax:subtrees(Node)])
    end.


vars_to_export(Fun, ExprEndPos, Expr) ->
    AllVars = refac_misc:collect_var_source_def_pos_info(Fun),
    ExprBdVarsPos = [Pos || {_Var, Pos} <- refac_util:get_bound_vars(Expr)],
    [{V, DefPos} || {V, SourcePos, DefPos} <- AllVars,
		    SourcePos > ExprEndPos,
		    lists:subtract(DefPos, ExprBdVarsPos) == []].
  

get_var_define_pos(V) ->
    {value, {def, DefinePos}} = lists:keysearch(def,1, refac_syntax:get_ann(V)),
    DefinePos.




