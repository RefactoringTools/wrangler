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

-export([sim_expr_search/6, normalise_record_expr/4]).

-export([get_start_end_loc/1]).

-include("../include/wrangler.hrl").

-define(DefaultSimiScore, 0.8).
%% ================================================================================================
%% @doc Search expressions that are 'similar' to a user-selected expression or a sequence of expressions in the current buffer.
%%
%% <p> Given expression selected by the user, A say, expression B is similar to A if there exist a least general common abstation, C, 
%% such that, substitution C(Args1) = A, and C(Arg2) == B, and Size(C) /Size(A) > the threshold specified (0.6 by default).
%% </p>
%% <p> When the selected code contains multiple, but non-continuous sequence of, expressions, the first
%% continuous sequence of expressions is taken as the user-selected expression. A continuous sequence of
%% expressions is a sequence of expressions separated by ','.
%% <p>

-spec(sim_expr_search/6::(filename(), pos(), pos(), string(),[dir()],integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).    
sim_expr_search(FName, Start = {Line, Col}, End = {Line1, Col1}, SimiScore0, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_expr_search(~p, {~p,~p},{~p,~p},~p, ~p).\n", [?MODULE, FName, Line, Col, Line1, Col1, SearchPaths, TabWidth]),
    SimiScore1 = case SimiScore0 of 
		     [] -> ?DefaultSimiScore;
		     _ -> list_to_float(SimiScore0)
		 end,
    SimiScore = case (SimiScore1>=0.1) andalso (SimiScore1 =<1.0) of 
		    true ->SimiScore1;
		    _ -> ?DefaultSimiScore
		end,    
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, [], TabWidth),
    Exprs= refac_util:pos_to_expr_list(FName, AnnAST, Start, End, TabWidth),
    case Exprs of 
	[] -> throw({error, "You have not selected an expression!"});
	_ -> ok
    end,
    RecordInfo =get_module_record_info(FName, SearchPaths, TabWidth),
    Exprs1 = case length(Exprs) of 1 -> hd(Exprs); _ -> Exprs end,
    ?wrangler_io("The selected expression after normalisation is:\n\n~s\n\n",
 		 [format_exprs(normalise_expr(Exprs1, RecordInfo))]),
    Res =do_search_similar_expr(AnnAST, RecordInfo, Exprs1, SimiScore),
    Res1 = lists:map(fun({Range, _, _})-> Range end, Res),
    Res2 = lists:usort([get_start_end_loc(Exprs) | Res1]),
    AntiUnifier = generalise_expr(normalise_record_expr(Exprs1,RecordInfo),Res),
    case length(Res2) of  
	0 -> ?wrangler_io("No similar expression has been found.\n",[]);  %% This should not happen.
	1 -> ?wrangler_io("No similar expression has been found.\n", []);
	N -> ?wrangler_io("~p expressions (including the expression selected) which are similar to the expression selected have been found. \n", [N]),
	     ?wrangler_io("The generalised expression would be:\n\n~s\n\n", [format_exprs(AntiUnifier)]),
	     ?wrangler_io("Use 'C-c C-e' to remove highlights!",[]),
	     {ok, Res2}
    end.
   

do_search_similar_expr(AnnAST, RecordInfo, Exprs, SimiScore) when is_list(Exprs) ->
      F = fun(T, Acc) ->
		  case refac_syntax:type(T) of
		      clause -> Exprs1 = refac_syntax:clause_body(T),
				do_search_similar_expr_1(Exprs, Exprs1, RecordInfo, SimiScore)++Acc;
		      block_expr -> Exprs1 = refac_syntax:block_expr_body(T),
				    do_search_similar_expr_1(Exprs, Exprs1, RecordInfo, SimiScore)++Acc;
		      _  -> Acc
		  end
	  end,
    lists:reverse(refac_syntax_lib:fold(F, [], AnnAST));

do_search_similar_expr(AnnAST, RecordInfo, Expr, SimiScore) ->
    {EStart, EEnd} = get_start_end_loc(Expr),
    F = fun(Node, Acc) ->
		case refac_util:is_expr(Node) of 
		    true ->
			{NStart, NEnd} = get_start_end_loc(Node),
			case (EStart =< NStart andalso  NEnd =< EEnd) orelse
			    (NStart =< EStart andalso EEnd =< NEnd) of 
			    true -> Acc;
			    _ ->
				find_anti_unifier(Expr, Node, RecordInfo, SimiScore) ++ Acc
			end;
		    _ -> Acc
		end
	end,
    lists:reverse(refac_syntax_lib:fold(F, [], AnnAST)).

 
do_search_similar_expr_1(Exprs1, Exprs2, RecordInfo, SimiScore) ->
    Len1 = length(Exprs1),
    Len2 = length(Exprs2),
    case Len1 =< Len2 of 
	true ->  Exprs21= lists:sublist(Exprs2, Len1),
		 {S1, E1} = get_start_end_loc(Exprs1),
		 {S2, E2} = get_start_end_loc(Exprs21),
		 case (S1 =< S2 andalso E2 =< E1) orelse (S2 =< S1 andalso E1=< E2) of 
		     true -> [];
		     _ ->
			 find_anti_unifier(Exprs1, Exprs21, RecordInfo, SimiScore)
			     ++ do_search_similar_expr_1(Exprs1, tl(Exprs2), RecordInfo, SimiScore)
		 end;
	_ -> []
    end.

find_anti_unifier(Expr1, Expr2, RecordInfo, SimiScore) ->
    Res =do_find_anti_unifier(normalise_expr(Expr1, RecordInfo),
			      normalise_expr(Expr2, RecordInfo)),
    ExprSize = no_of_nodes(Expr1),
    case Res of 
	{true, Subst} -> 
	    {SubExprs1, _SubExprs2} = lists:unzip(Subst),
	    Score = 1 -((no_of_nodes(SubExprs1)-length(SubExprs1))/ExprSize),
	    case Score>=SimiScore of 
		true ->
		    Expr2FreeVars = refac_util:get_free_vars(Expr2),
		    SubstWithFreeVars = lists:map(fun({SubExpr1, SubExpr2}) -> {SubExpr1, {SubExpr2, Expr2FreeVars}} end, Subst),
		    [{get_start_end_loc(Expr2), SubstWithFreeVars, Expr2}];
		_ -> []
	    end;
	_ -> []
    end.
		    
do_find_anti_unifier(Exprs1, Exprs2) when is_list(Exprs1) andalso is_list(Exprs2)->
    case length(Exprs1) == length(Exprs2) of
	true -> 
	    Res = lists:map(fun({E1,E2}) ->
				    do_find_anti_unifier(E1,E2)
			    end, lists:zip(Exprs1,Exprs2)),
	    case lists:any(fun(R) -> R==false end, Res) of 
		true ->false;
		_ ->
		   {true, lists:flatmap(fun({true, S}) -> S end, Res)}
	    end;
	_ -> false
    end;
do_find_anti_unifier(Expr1, _Expr2) when is_list(Expr1) ->
    false;
do_find_anti_unifier(_Expr1, Expr2) when is_list(Expr2) ->
    false;
do_find_anti_unifier(Expr1, Expr2) ->
    T1 = refac_syntax:type(Expr1),
    T2 = refac_syntax:type(Expr2),
    case T1 == T2  of 
	true ->
	    case refac_syntax:is_leaf(Expr1) of 
		true ->
		    case refac_syntax:is_literal(Expr1) of 
			true ->  
			    case refac_syntax:concrete(Expr1) == refac_syntax:concrete(Expr2) of 
				     true ->
					 {true, []};
				     _ -> case  variable_replaceable(Expr1) andalso variable_replaceable(Expr2) of 
					      true -> {true, [{Expr1, Expr2}]};
					      _  -> false
					  end
				 end;
			_ -> case T1 of 
				 variable -> 
				     case lists:keysearch(category, 1, refac_syntax:get_ann(Expr1)) of 
					 {value, {category, macro_name}} ->
					     case refac_syntax:variable_name(Expr1) == refac_syntax:variable_name(Expr2) of 
						 true ->
						     {true, []};
						 _ -> 
						     false   %% macro names are not replaceable by variables.
					     end;
					 _ -> {true, [{Expr1, Expr2}]}  
				     end;
				 operator -> case refac_syntax:operator_name(Expr1) == refac_syntax:operator_name(Expr2) of 
						 true -> {true, []};
						 _ ->
						     false
					     end;
				 underscore ->{true, []};
				 _ -> case variable_replaceable(Expr1) andalso variable_replaceable(Expr2) of 
					  true -> {true, [{Expr1, Expr2}]};
					  _ ->
					      false
				      end
			     end
		    end;
		_ ->
		    SubExprs1 = refac_syntax:subtrees(Expr1),
		    SubExprs2 = refac_syntax:subtrees(Expr2),
		    Res = do_find_anti_unifier(SubExprs1, SubExprs2),
		    case Res of  
			false -> case variable_replaceable(Expr1) andalso variable_replaceable(Expr2) of 
				     true ->
					 {true, [{Expr1, Expr2}]};
				     _ ->
					 false
				 end;
			_ -> Res
		    end
	    end;
	_ -> case T1 of 
		 variable -> 
		     case lists:keysearch(category, 1, refac_syntax:get_ann(Expr1)) of 
			 {value, {category, macro_name}} -> 
			     false;
			 _ ->
			     case variable_replaceable(Expr2) of 
				 true -> {true, [{Expr1, Expr2}]};
				 _ -> false
			     end
		     end;
		 _ -> 
		     case variable_replaceable(Expr1) andalso variable_replaceable(Expr2) of 
			 true ->
			     {true, [{Expr1, Expr2}]};
			 _ -> 
			     false
		      end
	     end
       end.
    

variable_replaceable(Exp) ->
    Res1=case lists:keysearch(category,1, refac_syntax:get_ann(Exp)) of 
	     {value, {category, record_field}} -> false;
	     {value, {category, record_type}} -> false;	 
	     {value, {category, guard_expression}} -> false;
	     {value, {category, application_op}} ->
		 case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Exp)) of 
		     {value, {fun_def, _}} -> true;
		     _ -> false
		 end;
	     _ -> case refac_syntax:type(Exp) of 
		      match_expr -> false;
		      _ -> case refac_util:is_pattern(Exp) of 
			       true -> false;
			       _ -> true
			   end				    
		  end
	 end,
   %% Exp_Free_Vars = refac_util:get_free_vars(Exp),
    Exp_Export_Vars =refac_util:get_var_exports(Exp),
    Res1 andalso (Exp_Export_Vars ==[]). %%andalso (Exp_Free_Vars==[]).  %% here, we don't generalise over expressions containing free vars; or should we?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generalise_expr(Exprs, SearchRes) when is_list(Exprs)->
    generalise_expr_1(refac_syntax:block_expr(Exprs), SearchRes, refac_util:get_free_vars(Exprs));
generalise_expr(Expr, SearchRes) ->
    generalise_expr_1(Expr, SearchRes, refac_util:get_free_vars(Expr)).
    
generalise_expr_1(Expr, SearchRes, ExprFreeVars) ->
    Subst = lists:flatmap(fun({_, Es, _Expr2}) ->Es end, SearchRes),
    GroupedSubst = group_by_expr(Subst),
    case GroupedSubst of 
	[] -> Expr;
	_ ->
	    Pid = start_counter_process(),
	    {Expr1, _}= refac_util:stop_tdTP(fun do_replace_expr_with_var_1/2, Expr, {GroupedSubst, ExprFreeVars, Pid}),
	    stop_counter_process(Pid),
	    Expr1
    end.


do_replace_expr_with_var_1(Node, {GroupedSubSt, ExprFreeVars, Pid}) ->
    Node1 = do_normalise_fun_calls(Node),
    case lists:keysearch(Node1, 1, GroupedSubSt) of
      {value, {Node1, Exprs}} ->
	  case refac_syntax:type(Node) of
	    variable ->
		case refac_util:get_free_vars(Node) of
		  [] -> {Node, false};   %% This is a binding occurrance;
		  FVs ->
		      case FVs -- ExprFreeVars of
			[] -> %% a real free occurence;
			    Args = lists:usort(find_correspond_bound_vars(Exprs, GroupedSubSt)),
			    case Args of
			      [] -> {Node, false};
			      _ -> Args1 = lists:map(fun (V) -> refac_syntax:variable(V) end, Args),
				   NewVar = gen_new_var_name(Pid),
				   {refac_syntax:application(refac_syntax:variable(NewVar), Args1), true}
			    end;
			_ ->
			    BVs = find_correspond_bound_vars(Exprs, GroupedSubSt),
			    case BVs of
			      [] -> {Node, false};
			      _ ->
				  Args = lists:usort(find_correspond_bound_vars(Exprs, GroupedSubSt)),
				  Args1 = lists:map(fun (V) -> refac_syntax:variable(V) end, Args -- [refac_syntax:variable_name(Node)]),
				  case Args1 of
				    [] -> {Node, false};
				    _ -> NewVar = gen_new_var_name(Pid),
					 {refac_syntax:application(refac_syntax:variable(NewVar), [Node| Args1]), true}
				  end
			    end
		      end
		end;
	    _ -> BVs = find_correspond_bound_vars(Exprs, GroupedSubSt),
		 {FreeBVs, _} = lists:unzip(refac_util:get_free_vars(Node) -- ExprFreeVars),
		 case BVs ++ FreeBVs of
		   [] -> {refac_syntax:variable(gen_new_var_name(Pid)), true};
		   _ -> Args = lists:map(fun (V) -> refac_syntax:variable(V) end, lists:usort(BVs ++ FreeBVs)),
			NewVar = gen_new_var_name(Pid),
			{refac_syntax:application(refac_syntax:variable(NewVar), Args), true}
		 end
	  end;
      _ -> {Node, false}
    end.


find_correspond_bound_vars(Exprs, Subst) ->
    BVsInExprs =lists:usort(lists:flatmap(fun({E, FVsInSimExpr}) ->
						  refac_util:get_free_vars(E)--FVsInSimExpr
					  end, Exprs)),
    lists:flatmap(fun({E1, Es}) ->
		      case refac_syntax:type(E1) of 
			  variable -> case refac_util:is_pattern(E1) of 
					  true -> 
					      {Es1,_} = lists:unzip(Es),
					      Bds=lists:flatmap(fun(E) ->
								  refac_util:get_bound_vars(E) end, Es1),
					      case BVsInExprs -- Bds =/= BVsInExprs of 
						  true -> [refac_syntax:variable_name(E1)];
						  _ -> []
					      end;					      
					  _  -> []
				      end;					  
			  _ -> []
		      end
		  end, Subst).

		      
		      
		 

gen_new_var_name(Pid) -> 
    Pid ! {self(), next},
    receive
	{Pid, V} ->
	     V
    end.


start_counter_process() ->               
    spawn_link(fun() -> counter_loop(1) end).
             

stop_counter_process(Pid) ->
    Pid!stop.

counter_loop(SuffixNum) ->
    receive
	{From, next} ->
	    From ! {self(), "NewVar_"++integer_to_list(SuffixNum)},
	    counter_loop(SuffixNum+1);
	stop ->
	    ok
    end.

group_by_expr(Subst) ->
    SortedSubst=lists:sort(fun({Expr1,_}, {Expr2,_}) ->
				   {S1, E1} = get_start_end_loc(Expr1),
				   {S2, E2} = get_start_end_loc(Expr2),
				   (S1 < S2) orelse ((S1==S2) andalso (E2=<E1))
			   end, Subst),
    group_by_expr_1(SortedSubst).
group_by_expr_1([]) ->
     [];
group_by_expr_1(SubstList=[E|Es]) ->
    Expr = element(1,E),
    {SLoc, ELoc} = get_start_end_loc(Expr),
    case {SLoc, ELoc} of 
	{{0,0},{0,0}} ->
	    [E|group_by_expr_1(Es)];
	_ ->
	    {SubstList1, SubstList2} = lists:splitwith(fun(S) ->
							       {SLoc1, ELoc1} = get_start_end_loc(element(1, S)),
							       (SLoc =< SLoc1) andalso (ELoc1=<ELoc)
						       end, SubstList),
	    {_, Exprs} = lists:unzip(SubstList1),
	    [{Expr, Exprs}| group_by_expr_1(SubstList2)]
    end.    
     
format_exprs(Exprs) when is_list(Exprs) andalso (length(Exprs) >1) ->
    refac_prettypr:format(refac_syntax:block_expr(Exprs));
format_exprs(Exprs) when is_list(Exprs) ->
    refac_prettypr:format(hd(Exprs));
format_exprs(E) ->refac_prettypr:format(E).

normalise_expr(Exprs, RecordInfo) ->
    Exprs1=normalise_record_expr(Exprs, RecordInfo),
    do_normalise_fun_calls(Exprs1).
  
normalise_record_expr(Exprs, RecordInfo) when is_list(Exprs)->
    lists:map(fun(E) -> 
		      refac_util:full_buTP(fun do_normalise_record_expr_1/2, E, RecordInfo) 
	      end, Exprs);
normalise_record_expr(Expr, RecordInfo) ->
    refac_util:full_buTP(fun do_normalise_record_expr_1/2, Expr, RecordInfo).

do_normalise_fun_calls(Exprs)  when is_list(Exprs)->
    lists:map(fun(E) ->
		      refac_util:full_buTP(fun do_normalise_fun_calls_1/2, E, [])
	      end, Exprs);
do_normalise_fun_calls(Expr) ->
    refac_util:full_buTP(fun do_normalise_fun_calls_1/2, Expr, []).

do_normalise_fun_calls_1(Node, _Others) ->
    case refac_syntax:type(Node) of
	application ->
	    Op = refac_syntax:application_operator(Node),
	    Args = refac_syntax:application_arguments(Node),
	    case refac_syntax:type(Op) of 
		atom ->
		    As = refac_syntax:get_ann(Op),
		    case lists:keysearch(fun_def, 1, As) of
			{value, {fun_def, {M, _FunName, _Arity, _UsePos, _DefinePos}}} ->
			    Op1 =refac_syntax:copy_attrs(Op, refac_syntax:module_qualifier(refac_syntax:atom(M), Op)),
			    refac_syntax:copy_attrs(Node, refac_syntax:application(Op1,Args));
			_ -> Node
		    end;
		_ -> Node
	    end;
	_ -> Node
    end. 					    

get_start_end_loc(Exprs) when is_list(Exprs) ->
    E1= hd(Exprs),
    En = lists:last(Exprs),
    {S, _E} = refac_util:get_range(E1),
    {_S, E} = refac_util:get_range(En),
    {S, E};
get_start_end_loc(Expr) ->
    refac_util:get_range(Expr).
    


%% generalise_expr_1(Expr, SearchRes) ->
%%     Subst = lists:usort(lists:flatmap(fun({_, Es, Expr2}) ->Es end, SearchRes)),
%%     Expr2 = hd(lists:flatmap(fun({_,Es, Expr2}) ->Expr2 end, SearchRes)), %% This is temporally only.
%%     {SubExprs1, SubExprs2} = lists:unzip(Subst),
%%     %% SubExprs = lists:sort(fun(E1,E2) ->no_of_nodes(E1) >= no_of_nodes(E2) end, SubExprs1),
%%     ExprFreeVars = refac_util:get_free_vars(Expr),
%%     Expr2FreeVars = refac_util:get_free_vars(Expr2),	       
%%     case Subst of 
%% 	[] -> Expr;
%% 	_ ->
%% 	    {Expr1, _}= refac_util:stop_tdTP(fun do_replace_expr_with_var_1/2, Expr, {Subst, ExprFreeVars, Expr2FreeVars}),
%% 	    Expr1
%%     end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refactoring: Normalise record expression.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec(normalise_record_expr/4::(filename(), pos(), [dir()], integer()) -> {error, string()} | {ok, [filename()]}).
normalise_record_expr(FName, Pos={Line, Col}, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:normalise_record_expr(~p, {~p,~p},~p, ~p).\n", [?MODULE, FName, Line, Col, SearchPaths,TabWidth]),
    {ok, {AnnAST, _Info}} =refac_util:parse_annotate_file(FName,true, [], TabWidth),
    RecordExpr =pos_to_record_expr(AnnAST, Pos),
    {AnnAST1, _Changed} = normalise_record_expr_1(FName, AnnAST,Pos, RecordExpr,  SearchPaths, TabWidth),
    refac_util:write_refactored_files_for_preview([{{FName, FName}, AnnAST1}]),
    {ok, [FName]}.


normalise_record_expr_1(FName, AnnAST,Pos, RecordExpr, SearchPaths, TabWidth) ->
    RecordTypes1 = collect_record_types(RecordExpr),
    RecordTypes = lists:flatmap(fun(T) -> case refac_syntax:type(T) of 
					      atom-> [refac_syntax:concrete(T)];
					      _ -> []
					  end
				end, RecordTypes1),
    case length(RecordTypes) < length(RecordTypes1) of 
	true -> ?wrangler_io("Warning: record expressions with a non-atom record type are not normalised by this refactoring.\n",[]);
	_ -> ok
    end,
    case RecordTypes of 
	[] -> throw({error, "No record expression to normalise."});
	_ ->
	    RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
	    refac_util:stop_tdTP(fun do_normalise_record_expr/2, AnnAST, {Pos,RecordInfo})
    end.

do_normalise_record_expr(Node, {Pos, RecordInfo}) ->
    case refac_syntax:type(Node) of 
	record_expr ->
	    {S, E} = refac_util:get_range(Node), 
	    case (S =<Pos) andalso (Pos =< E) of 
		true ->  Node1 =refac_util:full_buTP(fun do_normalise_record_expr_1/2, Node, RecordInfo),
			 {Node1, true};
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.

do_normalise_record_expr_1(Node, RecordInfo) ->
    Fun = fun({{FName, FVal}, Fields}) ->
		  case lists:filter(fun(F1) -> T1 = refac_syntax:record_field_name(F1), 
					       case refac_syntax:type(T1) of 
						   atom -> refac_syntax:concrete(refac_syntax:record_field_name(F1))== FName;
						   _ -> false
					       end
				    end, Fields) of 
		      [F2|_] -> F2;
		      _ ->
			  case lists:filter(fun(F1) ->
					  refac_syntax:type(refac_syntax:record_field_name(F1))== underscore end, Fields) of 
			      [F2|_] ->
				  refac_syntax:record_field(refac_syntax:atom(FName), refac_syntax:record_field_value(F2));
			      _ ->
				  case FVal of 
				      none -> refac_syntax:record_field(refac_syntax:atom(FName),refac_syntax:atom(undefined));
				      _ ->
					  refac_syntax:record_field(refac_syntax:atom(FName), FVal)
				  end
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
		    Type1 = refac_syntax:concrete(Type),
		    case lists:keysearch(Type1, 1, RecordInfo) of 
			{value, {Type1, Fields1}} ->
			    Fields2 = lists:map(fun(F) -> {F, Fields} end, Fields1),
			    NormalisedFields =lists:map(Fun, Fields2),
			    refac_syntax:record_expr(Arg, Type, NormalisedFields);
			_ -> 
			    %% ?wrangler_io("Warning: wrangler could not find the definition of record '~p'.\n",[Type1]),
			    Node
		    end;
		_ -> Node
	    end;
	_ -> Node
    end.
    
	    
collect_record_types(Tree) ->
    F = fun(T,S) ->
		case refac_syntax:type(T) of
		    record_expr ->
			S ++ [refac_syntax:record_expr_type(T)];
		    _ -> S
		end
	end,
    refac_syntax_lib:fold(F, [], Tree).

pos_to_record_expr(Tree, Pos) ->
    case refac_util:once_tdTU(fun pos_to_record_expr_1/2, Tree, Pos) of 
	{_, false} ->
	     throw({error, "You have not selected a record expression"});
	{R, true} -> 
	    R
    end.

pos_to_record_expr_1(Node, Pos) ->
    case refac_syntax:type(Node) of 
	record_expr ->
	    {S, E} = refac_util:get_range(Node), 
	    case (S =<Pos) andalso (Pos =< E) of 
		true -> {Node, true};
		_ -> {[], false}
	    end;
	_ -> {[], false}
    end.


get_module_record_info(FName, SearchPaths, TabWidth) ->
    Dir = filename:dirname(FName),
    DefaultIncl1 = [".","..", "../hrl", "../incl", "../inc", "../include"],
    DefaultIncl2 = [filename:join(Dir, X) || X <-DefaultIncl1],
    Includes = SearchPaths++DefaultIncl2,
    case refac_epp:parse_file(FName, Includes,[], TabWidth, refac_util:file_format(FName)) of 
	{ok, Forms, _} -> Forms1 =  lists:filter(fun(F) ->
							case F of 
							    {attribute, _, file, _} -> false;
							    {attribute, _, type, {{record, _}, _, _}} -> false;
							    _ -> true
							end
						 end, Forms),
			  SyntaxTree = refac_recomment:recomment_forms(Forms1,[]),
			  Info = refac_syntax_lib:analyze_forms(SyntaxTree),
			  case lists:keysearch(records,1, Info) of 
			      {value, {records, Records}} -> Records;
			      _ ->[]
			  end;
	{error, _Reason} -> []
   end.


no_of_nodes(Nodes) when is_list(Nodes) ->
    lists:sum(lists:map(fun(N) -> no_of_nodes(N) end, Nodes));
no_of_nodes(Node) ->
    case refac_syntax:is_leaf(Node) of
	true ->
	     1;
	_ ->
	    SubTrees = refac_syntax:subtrees(Node),
	    lists:sum(lists:map(fun(T) -> no_of_nodes(T) end, SubTrees))
    end.
	     
