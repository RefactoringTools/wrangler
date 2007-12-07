%% ============================================================================================
%% Refactoring: Fold expression(s) against a function definition.
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
 
%% =============================================================================================
-module(refac_fold_expression).

-export([fold_expression/3]).

-compile(export_all).


%% ToThink: 1. what about unused parameters.
%% =============================================================================================
%% @spec fold_expression(FileName::filename(), Line::integer(), Col::integer())-> term()
%%         
fold_expression(FileName, Line, Col) ->
    io:format("\n[CMD: fold_expression(~p, ~p,~p)]\n", [FileName, Line, Col]),
    case refac_util:parse_annotate_file(FileName,2) of 
	{ok, {AnnAST, Info}} ->
	    case refac_util:pos_to_fun_def(AnnAST, {Line, Col}) of 
		{ok, {Mod, _FunName, _Arity, FunDef}} ->
		    case side_condition_analysis(FunDef) of 
			ok ->  Candidates = search_candidate_exprs(AnnAST, FunDef),
			       %% io:format("Candidates:\n~p\n", [Candidates]),
			       case Candidates of 
				   [] -> io:format("No expressions that are suitable for folding against the selected function have been found!");
				   _ -> %% io:format("~p canidate expressions which can be folded against the selected function have been found!", [length(Candidates)]),
					Regions = lists:map(fun({{{StartLine, StartCol}, {EndLine, EndCol}},FunCall}) ->
								    {StartLine, StartCol, EndLine,EndCol, FunCall} end, Candidates),
					%% io:format("Regions:\n~p\n", [Regions]),
					{ok, Regions}
			       end;				 
			{error, Reason} -> {error, Reason}
		    end;
		{error, _} ->
		    {error, "You have not selected a function definition."}
	    end;
	{error, Reason} -> {error, Reason}
    end.


fold_expression_1(FileName, StartLine, StartCol, EndLine, EndCol, FunCall) -> 
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, 2),
    FunName = refac_syntax:atom_value(refac_syntax:application_operator(FunCall)),
    Arity = length(refac_syntax:application_arguments(FunCall)),		   
    AnnAST1 = refac_util:stop_tdTP(fun do_replace_expr_with_fun_call/2, AnnAST, {FunCall, {{StartLine, StartCol}, {EndLine, EndCol}}}),
    refac_util:write_refactored_files([{{FileName, FileName}, AnnAST1}]),
    {ok, {AnnAST2, _Info1}} = refac_util:parse_annotate_file(FileName,2),
    %% io:format("AnnAST2:\n~p\n", [AnnAST2]),
    case get_fun_def(AnnAST2, FunName, Arity) of 
	{ok, {Mod, _FunName, _Arity, FunDef}} ->
	    Candidates = search_candidate_exprs(AnnAST2, FunDef),
	    Regions = [{StartLine1, StartCol1, EndLine1, EndCol1, FunCall1} || {{{StartLine1, StartCol1}, {EndLine1, EndCol1}}, FunCall1}<-Candidates,
									      StartLine1 >= StartLine],
	    {ok, Regions};
	{error, reason} ->
	     {error, "You have not selected a function definition."}  %% THIS SHOULD NOT HAPPEN.
    end.
	    


    
    
get_fun_def(Node, FunName, Arity) ->
    case refac_util:once_tdTU(fun get_fun_def_1/2, Node, {FunName, Arity}) of
	{_, false} -> {error, none};
	{R, true} -> {ok, R}
    end.

get_fun_def_1(Node, {FunName, Arity}) ->
    case refac_syntax:type(Node) of 
	function ->
	    As = refac_syntax:get_ann(Node),
	    case lists:keysearch(fun_def, 1, As) of 
		{value, {fun_def, {Mod, FunName, Arity, _Pos1, _Pos2}}} ->
		    {{Mod,FunName, Arity, Node}, true};
		_ -> {[], false}
	    end;
	_ ->
	    {[], false}
    end.
	


do_replace_expr_with_fun_call(Tree, {FunCall, Range}) ->
    case refac_util:get_range(Tree) of 
	Range ->
	    {FunCall, true};
	_  -> {Tree, false}
    end.
    
    
    
    
side_condition_analysis(FunDef) ->
    Cs = refac_syntax:function_clauses(FunDef),
    case length(Cs) of 
	1 -> C = hd(Cs), 
	     G = refac_syntax:clause_guard(C),
	     case G of 
		 none -> 
		     Pats = refac_syntax:clause_patterns(C),
		     AllVars = lists:all(fun(P) -> refac_syntax:type(P) == variable end, Pats), %% TODO: OTHER SIMPLE PARAMETERS SHOULD ALSO Be ALLOWED.
		     case AllVars of 
			 true -> ok;
			 _ -> {error, "Wrangler does not support folding against functions with complex parameters."}
			 end;
		 _  -> {error, "Wrangler does not support folding against functions with guards."}
	     end;
	_  -> {error, "Wrangler does not support folding against functions with multiple clauses."}
    end.
     

search_candidate_exprs(AnnAST, FunDef) ->
    C = hd(refac_syntax:function_clauses(FunDef)),
    Body = refac_syntax:clause_body(C),
    Pats = refac_syntax:clause_patterns(C),
    FunName = refac_syntax:function_name(FunDef),
    Res = do_search_candidate_exprs(AnnAST,Body),
    lists:map(fun({Range, Subst}) -> {Range, make_fun_call(FunName, Pats, Subst)} end, Res).


make_fun_call(FunName, Pats, Subst) -> 
   %% io:format("Subst:\n~p\n", [Subst]),
    Pars = lists:map(fun(P) -> PName = refac_syntax:variable_name(P), 
			      %% io:format("PName:~p\n", [PName]),
			       {value, {PName, Par}} = lists:keysearch(PName, 1, Subst),
			       Par
		     end, Pats),
    FunCall = refac_syntax:application(FunName, Pars),
    FunCall.
    
 


do_search_candidate_exprs(AnnAST, ExpList) ->
    case length(ExpList) of 
	 1 ->
	    do_search_candidate_exprs_1(AnnAST, hd(ExpList));
	_  ->do_search_candidate_exprs_2(AnnAST, ExpList)
    end.


do_search_candidate_exprs_1(AnnAST, Exp) ->
   Fun = fun(T,S) ->
		 As = refac_syntax:get_ann(T),
		 case lists:keysearch(category, 1, As) of 
		     {value, {category, expression}} ->
			 case T=/=Exp of 
			     true ->case expr_unification(Exp, T) of 
					{true, Subst} -> S++[{refac_util:get_range(T), Subst}];
					_ -> S
				    end;			
			     _ -> S
			 end;
		      _  -> S
		 end		     
	 end,
    refac_syntax_lib:fold(Fun, [], AnnAST).


do_search_candidate_exprs_2(AnnAST, ExpList) ->
    Len = length(ExpList),
    Fun = fun(T, S) ->
		  case refac_syntax:type(T) of 
		      clause ->
			  Exprs = refac_syntax:clause_body(AnnAST),
			  SubExprs = sublists(Exprs, Len),
		          CandidateExprs = lists:map(fun(E) -> expr_unification(E, ExpList) end, SubExprs),
			  S ++ CandidateExprs;
		      block_expr ->
			  Exprs = refac_syntax:block_expr_body(T),
			  SubExprs = sublists(Exprs, Len),
			  CandidateExprs = lists:map(fun(E) -> expr_unification(E, ExpList) end, SubExprs),
			  S ++ CandidateExprs;     
		      _  -> S
		  end
	  end,
     refac_syntax_lib:fold(Fun,[], AnnAST).
    

expr_unification(Exp1, Exp2) ->
    case {is_list(Exp1), is_list(Exp2)} of 
	{true, true} ->   %% both are list of expressions
	    case length(Exp1) == length(Exp2) of
		true -> Res = lists:map(fun({E1,E2}) ->			      
						expr_unification(E1,E2) end, lists:zip(Exp1, Exp2)),
			Unifiable = lists:all(fun(E) -> case E of 
							    {true, _} -> true;
							    true -> true;
							    _ -> false
							end
					      end, Res),
		        Substs = lists:usort(lists:concat(lists:map(fun(E) -> case E of 
								      {true,S} -> S;
								      _ -> []
								  end
							end,Res))),
			case Unifiable of 
			    true -> {true, Substs};
			    _ -> false
			end;
		_ -> false
	    end;
	{false, false} ->  %% both are single expression.
	    T1 = refac_syntax:type(Exp1),
	    T2 = refac_syntax:type(Exp2),
	    case T1 == T2 of 
		true -> 
		    case T1 of 
			    variable -> {true, [{refac_syntax:variable_name(Exp1), set_default_ann(Exp2)}]} ;
			    atom -> refac_syntax:atom_value(Exp1) == refac_syntax:atom_value(Exp2);
			    operator -> refac_syntax:atom_value(Exp1) == refac_syntax:atom_value(Exp2);
			    char -> refac_syntax:char_value(Exp1) == refac_syntax:char_value(Exp2);
			    integer -> refac_syntax:integer_value(Exp1) ==refac_syntax:integer_value(Exp2);
			    string -> refac_syntax:string_value(Exp1) == refac_syntax:string_value(Exp2);
			    float -> refac_syntax:float_value(Exp1) == refac_syntax:float_value(Exp2);
			    underscore -> true;
			    nil -> true;
			    _ -> SubTrees1 = erl_syntax:subtrees(Exp1),
				 SubTrees2 = erl_syntax:subtrees(Exp2),
				 case length(SubTrees1) == length(SubTrees2) of 
				     true -> expr_unification(SubTrees1, SubTrees2);
				     _ -> false
				 end 
			    end;
		_ -> case T1 of 
			 variable -> {true, [{refac_syntax:variable_name(Exp1), set_default_ann(Exp2)}]};
			 _ -> false
		     end
	    end;
	{true, false} -> %% Exp1 is a list, but Exp2 is not.
	    false;
	{false, true} ->  %% Exp1 is a single expression, but Exp2 is not.
	   false      %% an actual parameter cannot be a list of expressions.
    end.
	    
  
		
 
sublists(List, Len) ->
    L = length(List),
    case Len > length(List) of
	true ->
	    [];
	_ -> [lists:sublist(List, Index, Len)|| Index<-lists:seq(1, L-Len+1)]
    end.



set_default_ann(Node) ->
    refac_syntax:set_pos(refac_syntax:remove_comments(refac_syntax:set_ann(Node, [])), {0,0}).
