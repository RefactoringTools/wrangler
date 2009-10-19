%% Copyright (c) 2009, Huiqing Li, Simon Thompson
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
%%       names of its contributors may be used to endoorse or promote products
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
%% Refactoring: Unfold a function application.
%% This refactoring replaces a function application with the function definition whose formal 
%% parameters being replaced by actual parameters. Some remarks about the implementation:
%% 1) This refactoring does automatic variable renaming of variables decared in the function 
%% to be inlined if there would be a name capture/conflict without doing so;
%% 2) In the case that the function definition has multiple clause, Wrangler tries to work out 
%% which function clause to inline by matching the actual and formal parameters; but only do 
%% the unfolding if Wrangler is certain which clause to unfold;
%% 3) Because Erlang is a strict language, Wrangler needs to make sure that actual parameters 
%% are evaluated only once.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% =============================================================================================

%% =============================================================================================
-module(refac_unfold_fun_app).

-export([unfold_fun_app/4, unfold_fun_app_eclipse/4]).

-include("../include/wrangler.hrl").

%% =============================================================================================
-spec(unfold_fun_app/4::(FileName::filename(), Pos::pos(), SearchPaths::[dir()], TabWidth::integer)
      ->{'ok', [string()]} | {error, string()}).
unfold_fun_app(FileName, Pos, SearchPaths, TabWidth) ->
    unfold_fun_app(FileName, Pos, SearchPaths, TabWidth, emacs).

-spec(unfold_fun_app_eclipse/4::(FileName::filename(), Pos::pos(), SearchPaths::[dir()], TabWidth::integer)
      ->{ok, [{filename(), filename(), string()}]} | {error, string()}).
unfold_fun_app_eclipse(FileName,Pos,SearchPaths, TabWidth) ->
    unfold_fun_app(FileName, Pos, SearchPaths, TabWidth, eclipse).


unfold_fun_app(FName, Pos = {Line, Col}, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:unfold_fun_app(~p, {~p,~p}, ~p, ~p).\n",
		 [?MODULE, FName, Line, Col, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":unfold_fun_app(" ++ "\"" ++
	FName ++ "\", {" ++ integer_to_list(Line) ++	", " ++ integer_to_list(Col) ++ "},"++
	"[" ++ refac_util:format_search_paths(SearchPaths) ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    case pos_to_fun_clause_app(AnnAST, Pos) of
	{ok, {Clause, App}} -> 
	    {ok, {FunClause, {Subst, MatchExprs}}} = side_cond_analysis(ModName, AnnAST, App),
	    {FunClause1, MatchExprs1} = auto_rename_vars({FunClause, MatchExprs}, {Clause, App}),
	    fun_inline_1(FName, AnnAST, Pos, {FunClause1, Subst, MatchExprs1}, {Clause, App}, Editor, Cmd);
	{error, Reason} -> throw({error, Reason})
    end.


side_cond_analysis(ModName, AnnAST, App) ->
    Op = refac_syntax:application_operator(App),
    Args = refac_syntax:application_arguments(App),
    Arity = length(Args),
    case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of
	{value, {fun_def, {ModName, FunName, Arity, _, _}}} ->
	    Fs = refac_syntax:form_list_elements(AnnAST),
	    Res = [F || F <- Fs, refac_syntax:type(F) == function,
			case lists:keysearch(fun_def, 1, refac_syntax:get_ann(F)) of
			    {value, {fun_def, {ModName, FunName, Arity, _, _}}} -> true;
			    _ -> false
			end],
	    case Res of
		[FunDef] ->
		    Cs = refac_syntax:function_clauses(FunDef),
		    try
			find_matching_clause(Cs, Args) of
			none -> throw({error, "The function to be inlined has multiple clauses, "
				       "and Wrangler could not figure out which function clause to inline."});
			{C, {Subst, MatchExprs}} ->
			    {ok, {C,{Subst, MatchExprs}}}
		    catch 
 			throw:E2 -> 
			    throw(E2)
 		    end;
		[] ->
		    throw({error, "The function to be inlined is not defined."});
		[_| _] ->
		    throw({error, "The function to be inlined has been defined more than once."})
	    end;
	{value, {fun_def, {'_', _,_, _, _}}} ->
	    throw({error, "The function to be inlined is not defined."});
	{value, {fun_def, {_M, _F, _A, _, _}}} ->
	    throw({error, "Inlining a function defined in another module is not supported yet."});
	_ -> throw({error, "Sorry, Wrangler could not figure out where the function to be inlined is defined."})
    end.


%% Find the function clause to be inlined by matching actual and formal parameters.
find_matching_clause([], _Ps) -> none;
find_matching_clause([C], Ps) ->
    case find_matching_clause_1(C, Ps, true) of 
	no_match -> 
	    none;
	no_more_match -> none;
	Res ->
	    MatchExprs = [ M|| M<-Res, case M of 
					   {tree, match_expr, _, _} -> true;
					   _ -> false
				       end],
	    SubSt = [ M|| M<-Res, case M of 
				      {tree, match_expr, _, _} -> false;
				      _ -> true
				  end],
	    {C, {SubSt, MatchExprs}}
    end;
find_matching_clause([C|Cs], Ps) ->
    case find_matching_clause_1(C, Ps, false) of
	no_match ->
	    find_matching_clause(Cs, Ps);
	no_more_match ->
	    none;
	Res ->
	    SubSt = [{P1, P2} || {P1, P2} <-Res],
	    MatchExprs = [ M|| M<-Res, not (is_tuple(M))],
	    {C, {SubSt, MatchExprs}}
    end.

find_matching_clause_1(C, AppPs, LastClause) ->
    DefPs = refac_syntax:clause_patterns(C),
    G = refac_syntax:clause_guard(C),
    case G of
	none -> match_patterns(DefPs, AppPs, LastClause);
	_ when LastClause ->
	    throw({error, "Inlining of a function clause with guard expression(s) is not supported yet."});
	_ -> no_more_match
    end.

match_patterns(DefPs, AppPs, LastClause) ->
    try do_match_patterns(DefPs, AppPs, LastClause) of 
	Subst -> Subst
    catch 
	throw:E2 -> E2
    end.  

do_match_patterns(DefP, AppP, LastClause) when is_list(DefP) andalso is_list(AppP) ->
    case length(DefP)== length(AppP) of 
	false ->
	    no_match;
	true-> case DefP of 
		   [] ->
		       [];
		   _ ->
		       lists:append([do_match_patterns(P1, P2, LastClause) || 
					{P1, P2} <-lists:zip(DefP, AppP)])
	       end
    end;

do_match_patterns(DefP, _AppP, _) when is_list(DefP) ->
    throw(no_more_match);
do_match_patterns(_DefP, AppP, _) when is_list(AppP) ->
    throw(no_more_match);
do_match_patterns(DefP, AppP, LastClause) ->
    F = fun(P1, P2) ->
		SubPats1 = refac_syntax:subtrees(P1),
		SubPats2 = refac_syntax:subtrees(P2),
		try do_match_patterns(SubPats1, SubPats2, LastClause) of
		    Subst -> Subst
		catch
		   throw:E -> E
		end
	end,
    T1 = refac_syntax:type(DefP),
    T2 = refac_syntax:type(AppP),
    case T1==T2 of 
	false ->
	    case T1 of 
		variable ->
		    case is_non_reducible_term(AppP) of 
			true ->
			    Ann = refac_syntax:get_ann(DefP),
			    case lists:keysearch(def,1,Ann) of 
				{value, {def, DefinePos}} ->
				    [{DefinePos, AppP}];
				_ -> [refac_syntax:match_expr(DefP, AppP)]
			    end;
			false ->
			    [refac_syntax:match_expr(DefP, AppP)]
		    end;
		underscore ->
		    [];
		_ when LastClause ->   %% The last function clause; do an enforced match;
		    [refac_syntax:match_expr(DefP, AppP)];
		_ ->case refac_syntax:is_literal(AppP) of 
			true -> throw(no_match);  
			_ -> throw(no_more_match) %% stop here; no more further match needed.
		    end
	    end;
	true -> case refac_syntax:is_literal(DefP) andalso refac_syntax:is_literal(AppP) of
		    true ->
			case refac_syntax:concrete(DefP)==refac_syntax:concrete(AppP) of 
			    true ->
				[];
			    _ -> throw(no_match)   %% should continue matching the next clause;
			end;
		    false ->
			case T1 of 
			    variable ->
				case {is_macro_name(DefP), is_macro_name(AppP)} of 
				    {true, true} ->
					case macro_name_value(DefP) ==
					    macro_name_value(AppP) of 
					    true ->
						[];
					    false -> throw(no_match)  %% should continure matching the next clause;
					end;
				    {false, false} ->  %% both are variables;
					Ann = refac_syntax:get_ann(DefP),
					case lists:keysearch(def,1,Ann) of 
					    {value, {def, DefinePos}} ->
						[{DefinePos, AppP}];
					         %% this should not happen;
					    _ -> [refac_syntax:match_expr(DefP, AppP)]
					end
				end;
			    underscore -> [];
			    _ -> case refac_syntax:is_leaf(DefP) of
				     true ->
					 throw(no_match);  %% should continue matching the next clause;
				     _ -> F(DefP, AppP)
				 end
			end
		end
    end.


is_non_reducible_term(T) ->
      case refac_syntax:type(T) of
	  variable -> true;
	  atom -> true;
	  integer -> true;
	  float -> true;
	  char -> true;
	  string -> true;
	  nil -> true;
	  list ->
	      case is_non_reducible_term(refac_syntax:list_head(T)) of
		  true -> is_non_reducible_term(refac_syntax:list_tail(T));
		  false -> false
	      end;
	  tuple -> lists:all(fun is_non_reducible_term/1, refac_syntax:tuple_elements(T));
	  _ -> false
      end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun_inline_1(FName, AnnAST, Pos, {FunClauseToInline, Subst, MatchExprsToAdd}, {Clause, App}, Editor, Cmd) ->
    B = refac_syntax:clause_body(FunClauseToInline),
    {SubstedBody1, _} = lists:unzip([refac_util:stop_tdTP(fun do_subst/2, E, Subst) || E <- B]),
    SubstedBody = MatchExprsToAdd ++ SubstedBody1,
    Fs = refac_syntax:form_list_elements(AnnAST),
    Fs1 = [do_inline(F, Pos, Clause, App, SubstedBody) || F <- Fs],
    AnnAST1 = refac_util:rewrite(AnnAST, refac_syntax:form_list(Fs1)),
    case Editor of
      emacs ->
	    refac_util:write_refactored_files_for_preview([{{FName, FName}, AnnAST1}], Cmd),
	    {ok, [FName]};
	eclipse ->
	    Content = refac_prettypr:print_ast(refac_util:file_format(FName), AnnAST1),
	    {ok, [{FName, FName, Content}]}
    end.


do_inline(Form, Pos, _Clause, App, SubstedBody) ->
    {S, E} = refac_util:get_range(Form),
    if (S =< Pos) and (Pos =< E) ->
	   {NewForm, _} = refac_util:stop_tdTP(fun do_inline_1/2, Form, {App, SubstedBody}),
	    case length(SubstedBody) > 1 of
		true ->
		    {NewForm1, _} = refac_util:stop_tdTP(fun remove_begin_end/2, NewForm, SubstedBody),
		    NewForm1;
		_ -> NewForm
	    end;
       true ->
	    Form
    end.

do_inline_1(Node, {App, SubstedBody}) ->
    case Node of
	App ->
	    case SubstedBody of 
		[B] ->
		    {B, true};
		[_|_] ->
		    {refac_syntax:block_expr(SubstedBody), true}
	    end;
	_ -> {Node, false}
    end.
    
remove_begin_end(Node, BlockBody) ->
    Fun = fun(E) ->
		  case refac_syntax:type(E) of
		      block_expr ->
			  case refac_syntax:block_expr_body(E) of
			      BlockBody ->
				  BlockBody;
			      _ -> [E]
			  end;
		      match_expr ->
			  Ps = match_expr_patterns(E),
			  B =  match_expr_body(E),
			  case refac_syntax:type(B) of 
			      block_expr ->
				  case refac_syntax:block_expr_body(B) of 
				      BlockBody ->
					  Last = lists:last(BlockBody),
					  NewLast = make_match_expr([refac_util:reset_attrs(P)||P<-Ps], Last),
					  lists:sublist(BlockBody, length(BlockBody)-1)++[NewLast];
				      _ -> [E]
				  end;
			      _ -> [E]
			  end;
		      _ -> [E]
		  end
	  end,
    case refac_syntax:type(Node) of
	clause ->
	    P = refac_syntax:clause_patterns(Node),
	    G = refac_syntax:clause_guard(Node),
	    B= refac_syntax:clause_body(Node),
	    B1=lists:append([Fun(E)||E <-B]),
	    {refac_util:rewrite(Node, refac_syntax:clause(P, G, B1)), 
	     length(B) =/= length(B1)};
	block_expr ->
	    Es = refac_syntax:block_expr_body(Node),
	    case Es of 
		BlockBody ->
		    {Node, false};
		_ ->
		    Es1 = lists:append([Fun(E)||E <-Es]),
		    {refac_util:rewrite(Node, refac_syntax:block_expr(Es1)), 
		     length(Es)=/=length(Es1)}
	    end;
	try_expr->
	    B = refac_syntax:try_expr_body(Node),
	    B1 = lists:append([Fun(E)||E <-B]),
	    Cs = refac_syntax:try_expr_clauses(Node),
	    Handlers = refac_syntax:try_expr_handlers(Node),
	    After = refac_syntax:try_expr_after(Node),
	    {refac_util:rewrite(Node, refac_syntax:try_expr(B, Cs, Handlers, After)),
	     length(B)=/=length(B1)};
	_ ->
	    {Node, false}
    end.
    
   
do_subst(Node, Subst) ->
    case refac_syntax:type(Node) of
	variable ->
	    As = refac_syntax:get_ann(Node),
	    case lists:keysearch(def,1, As) of 
		{value, {def, DefinePos}} ->
		    case lists:keysearch(DefinePos,1, Subst) of
			{value, {DefinePos, Expr}} ->
			    {refac_util:rewrite(Node,Expr), true};
			_ ->{Node, false}
		    end;
		_ -> {Node, false}
	    end;
	_ ->{Node,false}
    end.
	
%% From source postion to the function name part in a function application.
pos_to_fun_clause_app(Node, Pos) ->
    case refac_util:once_tdTU(fun pos_to_fun_clause_app_1/2, Node, Pos) of
	{_, false} -> {error, none};
	{{C, App}, true} -> {ok, {C, App}}
    end.

pos_to_fun_clause_app_1(Node, Pos) ->
    case refac_syntax:type(Node) of 
	function ->
	    {S, E} = refac_util:get_range(Node),
	    if (S=<Pos) and (Pos =< E) ->
		    Cs  = refac_syntax:function_clauses(Node),
		    [C] = [C1|| C1 <- Cs,  
				{S1, E1} <- [refac_util:get_range(C1)],
				S1=<Pos,Pos=<E1],
		    case pos_to_fun_app(C, Pos) of
			{_, false} ->throw({error, "You have not selected a function application, "
				      "or the function containing the function application selected does not parse."});
			{App, true} ->
			    {{C, App}, true}
		    end;
	       true -> {[], false}
	    end;
	_ ->
	    {[], false}
    end. 
    

pos_to_fun_app(Node, Pos) ->
    refac_util:once_tdTU(fun pos_to_fun_app_1/2, Node, Pos).

pos_to_fun_app_1(Node, Pos) ->
    case refac_syntax:type(Node) of
	application ->
	    Op = refac_syntax:application_operator(Node),
	    {S, E} = refac_util:get_range(Op),
	    if (S =< Pos) and (Pos =< E) ->
		    {Node, true};
	       true -> {[], false}
	    end;
	_ -> {[], false} 
    end.



auto_rename_vars({ClauseToInline, MatchExprs},{Clause, App}) ->
    VarName = 'WRANGLER_TEMP_VAR',
    {Clause1, _} = refac_util:stop_tdTP(fun do_replace_app_with_match/2, Clause,
					{App, refac_syntax:variable(VarName)}),
    {Clause2,_,_} = refac_syntax_lib:vann_clause(Clause1,[],[],[]),
    BdsInFunToInline = get_bound_vars(ClauseToInline),
    Fun = fun (Node, S) ->
		  S ++ get_vars_to_rename(Node, {VarName, BdsInFunToInline})
	  end,
    VarsToRename = [{Name, DefinePos}|| 
		       {_Name, Pos} <- lists:keysort(2, refac_syntax_lib:fold(Fun, [], Clause2)),
		       {ok, {Name,DefinePos, _}} <-[refac_util:pos_to_var_name(ClauseToInline, Pos)]],
    do_rename_var({ClauseToInline, MatchExprs}, lists:usort(VarsToRename), collect_vars(Clause)).


do_replace_app_with_match(Node, {App, Var}) ->
    case Node of
	App ->
	  {refac_syntax:match_expr(Var, App), true};
	_-> {Node, false}
    end.

get_vars_to_rename(Node, {VarName, BdsInFunToInline}) ->
    Bds = lists:usort([Name || {Name, _Loc} <- refac_util:get_bound_vars(Node)]),
    Envs = lists:usort([Name || {Name, _Loc} <- refac_util:get_env_vars(Node)]),
    VarsToRename1 = case lists:member(VarName, Bds) of
		      true ->
			    [{Name, Loc} || {Name, Loc} <- BdsInFunToInline,
					  lists:member(Name, Envs)];
		      _ -> []
		    end,
    VarsToRename2 = case lists:member(VarName, Envs) of
		      true ->
			  [{Name, Loc} || {Name, Loc} <- BdsInFunToInline,
					  lists:member(Name, Bds)];
		      _ -> []
		    end,
    VarsToRename1 ++ VarsToRename2.

do_rename_var({Node, MatchExprs}, [], _UsedVarNames) ->  
    {Node, [refac_util:reset_attrs(M)||M<-MatchExprs]};
do_rename_var({Node, MatchExprs}, [V|Vs], UsedVarNames) ->
    {Node1, MatchExprs1} =do_rename_var_1({Node, MatchExprs}, V, UsedVarNames),
    do_rename_var({Node1, MatchExprs1}, Vs, UsedVarNames).

do_rename_var_1({Node, MatchExprs}, {VarName, DefLoc}, UsedVarNames) ->
    NewVarName =make_new_name(VarName, ordsets:union(collect_vars(Node), UsedVarNames)),
    {Node1, _}=refac_rename_var:rename(Node, DefLoc, NewVarName),
    MatchExprs1 = do_rename_in_match_exprs(MatchExprs, DefLoc, NewVarName),
    {Node1, MatchExprs1}.

do_rename_in_match_exprs(MatchExprs, DefLoc, NewVarName) ->
    [do_rename_in_match_expr_1(M, DefLoc, NewVarName) || M <-MatchExprs].

do_rename_in_match_expr_1(MatchExpr, DefLoc, NewVarName) ->
    P = refac_syntax:match_expr_pattern(MatchExpr),
    B = refac_syntax:match_expr_body(MatchExpr),
    refac_io:format("P:\n~p\n", [P]),
    refac_io:format("DefLoc:\n~p\n", [P]),
    {P1, _} = refac_rename_var:rename(P, DefLoc, NewVarName),
    refac_syntax:match_expr(P1, B).
    



make_new_name(VarName, UsedVarNames) ->
    NewVarName = list_to_atom(atom_to_list(VarName)++"_1"),
    case ordsets:is_element(NewVarName, UsedVarNames) of
	true ->
	    make_new_name(NewVarName, UsedVarNames);
	_ -> 
	    NewVarName
    end.
    
    
%%-spec(get_bound_vars(Node::[syntaxTree()]|syntaxTree())-> [{atom(),pos()}]).
get_bound_vars(Nodes) when is_list(Nodes)->
    lists:flatmap(fun(Node) ->get_bound_vars(Node) end, Nodes);			   
get_bound_vars(Node) ->
    get_bound_vars_1(refac_syntax:get_ann(Node)).
					       
get_bound_vars_1([{bound, B} | _Bs]) -> B;
get_bound_vars_1([_ | Bs]) -> get_bound_vars_1(Bs);
get_bound_vars_1([]) -> [].


is_macro_name(Exp1) ->
    {value, {category, macro_name}} ==
      lists:keysearch(category, 1, refac_syntax:get_ann(Exp1)).

macro_name_value(Exp) ->
    case refac_syntax:type(Exp) of 
	    atom ->
		refac_syntax:atom_value(Exp);
	    variable ->
		refac_syntax:variable_name(Exp)
    end.


%% Note that In Erlang a match expression in the form of  'P1 = P2 ... = Pn = E' is allowed.
match_expr_patterns(E) ->
    case refac_syntax:type(E) of 
	match_expr ->
	    P = refac_syntax:match_expr_pattern(E),
	    B = refac_syntax:match_expr_body(E),
	    [P|match_expr_patterns(B)];
	_ ->[]
    end.
match_expr_body(E) ->
    case refac_syntax:type(E) of
	match_expr ->
	    B = refac_syntax:match_expr_body(E),
	    match_expr_body(B);
	_ -> E
    end.

make_match_expr(Ps, Body) ->	
    make_match_expr_1(lists:reverse(Ps), Body).

make_match_expr_1([], Body) -> Body;
make_match_expr_1([P], Body) ->
    refac_syntax:match_expr(P, Body);
make_match_expr_1([P|Ps], Body) ->
    make_match_expr_1(Ps, refac_syntax:match_expr(P, Body)).



collect_vars(Node) ->
    Fun= fun(T, S) ->
		 case refac_syntax:type(T) of 
		     variable ->
			 VarName = refac_syntax:variable_name(T),
			 ordsets:add_element(VarName, S);
		     _ -> S
		 end
	 end,
    refac_syntax_lib:fold(Fun, ordsets:new(), Node).
