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
%% =====================================================================
-module(generalised_unification).

-export([expr_match/2]).

-include("../include/wrangler.hrl").

-spec(expr_match/2::(syntaxTree(), syntaxTree()) ->
                          {true, [{atom(), syntaxTree()}]} | false).
expr_match(Exp1, Exp2) ->
    Res = expr_unification(Exp1, Exp2),
    case Res of
	{true, Subst} ->
            static_semantics_check(Subst);
	_ -> 
	    false
    end.

static_semantics_check(Subst) ->
    Subst1 = [{refac_syntax:variable_name(V), Exp}
        ||{V, Exp}<-Subst],
    GroupedSubst = group_by_index(1, Subst1),
    Res =[static_semantics_check_1(G)||G<-GroupedSubst],
    case lists:member(false, Res) of 
        true -> 
            false;
        false ->
            {true,lists:append([S || {true, S} <- Res])}
    end.

static_semantics_check_1([]) ->
    {true,[]};
static_semantics_check_1(Subst=[{V, S}|_]) ->
    SubstEs = [E2||{_E1, E2} <- Subst],
    EStrs = [format(E)||E<-SubstEs],
    case lists:usort(EStrs) of
        [_] ->
            B=[var_binding_structure(E)||E<-SubstEs],
            case lists:usort(B) of 
                [_] ->
                    {true, [{V,S}]};
                _ -> false
            end;
        _ ->
            false
    end.
        
expr_unification(Exp1, Exp2) ->
    case {is_list(Exp1), is_list(Exp2)} of
        {true, true} ->   
            %%both are list of expressions
            expr_list_unification(Exp1, Exp2);
        {false, false} -> 
            %% both are single expressions.
	    T1 = refac_syntax:type(Exp1),
	    T2 = refac_syntax:type(Exp2),
	    case T1 == T2 of
		true ->
                    same_type_expr_unification(Exp1, Exp2);
                _ -> non_same_type_expr_unification(Exp1, Exp2)
	    end;
        {false, true}-> 
            case is_meta_list_variable(Exp1) of 
                true ->
                    {true, {Exp1, Exp2}};
                false->
                    false
            end;
        _ -> false
    end.

expr_list_unification(Exp1, Exp2)  
  when is_list(Exp1) andalso is_list(Exp2)->
    LEs1 = [E||E<-Exp1,is_list(E)],
    LEs2 = [E||E<-Exp2,is_list(E)],
    case LEs1==[] andalso LEs2==[] of 
        true ->
            expr_list_unification_1(Exp1, Exp2);
        false ->
            expr_list_unification_2(Exp1, Exp2)
    end.


expr_list_unification_1(ExpList1, ExpList2) ->
    case has_meta_list(ExpList1) of
        false ->
            expr_list_unification_2(ExpList1, ExpList2);
        true ->
            expr_list_unification_3(ExpList1, ExpList2)
    end.

expr_list_unification_2(Exp1, Exp2) ->
    case length(Exp1) == length(Exp2) of
        true ->
            Res = [expr_unification(E1, E2) || 
                      {E1, E2} <- lists:zip(Exp1, Exp2)],
            case not lists:member(false, Res) of 
                true ->
                    {true, lists:append([S || {true, S} <- Res])};
                _ -> false
            end;
        _ -> false
    end.


%% Exp1 has meta list variable.
expr_list_unification_3(List1, List2) ->
    L1 = [L||L<-List1, not is_meta_list(L)],
    case length(L1) > length(List2) of 
        true ->
            false;
        _  ->
            expr_list_unification_4(List1, List2)
    end.

expr_list_unification_4(List1, List2) ->
    Res =expr_list_unification_4(List1,List2, []),
    case not lists:member(false, Res) of 
        true ->
            {true, lists:append([S || {true, S} <- Res])};
        false ->
            false
    end.
    
expr_list_unification_4([], [], Acc) ->
    lists:reverse(Acc);
expr_list_unification_4(_, [], Acc) ->
    lists:reverse([false|Acc]);
expr_list_unification_4([], _, Acc) ->
    lists:reverse([false|Acc]);
expr_list_unification_4(_List1=[H1|T1], List2=[H2|T2], Acc) ->
    case is_meta_list(H1) of
        false ->
            Res = expr_unification(H1, H2),
            expr_list_unification_4(T1,T2, [Res|Acc]);
        true ->
            T11= [T||T<-T1, not is_meta_list(T)],
            Len1 = length(T11),
            Len2 = length(List2),
            case try_list_unification(T1, List2, Len2-Len1) of
                {true, Len, Acc1} ->
                    Sub=[{H1,lists:sublist(List2, Len)}],
                    lists:reverse(Acc)++[{true, Sub}|Acc1];
                false ->
                    false
            end
    end.

try_list_unification(_T1, _T2, Len) when Len<0  ->
    false;
try_list_unification(T1, T2, Len) ->
    T21 = lists:nthtail(Len, T2),
    case try_expr_match(T1, T21) of
        {true, Subst} ->
            {true, Len, [{true,Subst}]};
        false ->
            try_list_unification(T1,T2, Len-1)
    end.
 
try_expr_match(Exp1, Exp2) ->
    Res = expr_unification(Exp1, Exp2),
    case Res of
	{true, Subst} ->
            case static_semantics_check(Subst) of
                {true, _} ->
                    {true, Subst};
                false ->
                    false
            end;                    
	_ -> 
	    false
    end.

same_type_expr_unification(Exp1, Exp2) ->
    T1 = refac_syntax:type(Exp1),
    case T1 of
	variable ->
	    Exp1Ann = refac_syntax:get_ann(Exp1),
	    Exp2Ann = refac_syntax:get_ann(Exp2),
	    Exp1Name = refac_syntax:variable_name(Exp1),
	    Exp2Name = refac_syntax:variable_name(Exp2),
            case is_macro_name(Exp1) andalso is_macro_name(Exp2) of
                true ->
                    if Exp1Name==Exp2Name ->
                            {true, []};
                       true -> false
                    end;
                _ -> {true, [{Exp1, Exp2}]}
	    end;
	atom ->
            Exp1Val = refac_syntax:atom_value(Exp1),
            Exp2Val = refac_syntax:atom_value(Exp2),
	    case Exp1Val == Exp2Val of
		true ->
                    Ann1 = refac_syntax:get_ann(Exp1),
                    Ann2 = refac_syntax:get_ann(Exp2),
                    case lists:keysearch(fun_def,1,Ann1) of
                        {value, {fun_def, {M, F, A, _, _}}} ->
                            case lists:keysearch(fun_def,1,Ann2) of
		         	{value, {fun_def, {M1,F1,A1,_,_}}} ->
		         	    case {M, F, A}=={M1, F1, A1} of
		        		true -> {true, []};
		         		false ->
		         		    false
		         	    end;
		         	false ->
		         	    false
		            end;
                        false ->
                            {true, []}
                            %% case lists:keysearch(fun_def, 1, Ann2) of
                            %%      {value, _} ->
                            %%          false;
                            %%      false ->
                            %%          {true, []}
		            %%  end
                             %% This is not accurate!
                    end;
		_ -> 
                    case is_meta_atom(Exp1) of
                        true ->
                            {true, [{Exp1, Exp2}]};
                        false ->
                            false
                    end
	    end;
	operator ->
	    case refac_syntax:operator_name(Exp1) 
                == refac_syntax:operator_name(Exp2) of
		true -> {true, []};
		_ -> false
	    end;
	char ->
	    case refac_syntax:char_value(Exp1) 
                == refac_syntax:char_value(Exp2) of
		true -> {true, []};
		_ -> false
	    end;
	integer ->
	    case refac_syntax:integer_value(Exp1)
                == refac_syntax:integer_value(Exp2) of
		true -> {true, []};
		_ -> false
	    end;
	string ->
	    case refac_syntax:string_value(Exp1)
                == refac_syntax:string_value(Exp2) of
		true -> {true, []};
		_ -> false
	    end;
	float ->
	    case refac_syntax:float_value(Exp1) 
                == refac_syntax:float_value(Exp2) of
		true -> {true, []}
	    end;
	underscore -> {true, []};
	nil -> {true, []};
	%% application  ->
        %%     NewExp2 = normalise_application(Exp2),
        %%     SubTrees1 = refac_syntax:subtrees(Exp1),
        %%     SubTrees2 = refac_syntax:subtrees(NewExp2),
        %%     expr_unification(SubTrees1, SubTrees2);
        _ ->
	    SubTrees1 = refac_syntax:subtrees(Exp1),
            SubTrees2 = refac_syntax:subtrees(Exp2),
            case length(SubTrees1) == length(SubTrees2) of
		true ->
                    expr_unification(SubTrees1, SubTrees2);
		_ -> false
	    end
    end.

%% normalise_application(App) ->
%%     Op = refac_syntax:application_operator(App),
%%     Args = refac_syntax:application_arguments(App),
%%     case refac_syntax:type(Op) of
%%         atom ->
%%             Ann =refac_syntax:get_ann(Op),
%%             case lists:keysearch(fun_def, 1, Ann) of
%%                 {value, {fun_def, {M, _F, _A, _, _}}} when M /= '_' ->
%%                     M1 = refac_syntax:atom(M),
%%                     refac_syntax:application(M1, Op, Args);
%%                 _ ->
%%                     App
%%             end;
%%         _ -> App
%%     end.
   
non_same_type_expr_unification(Exp1, Exp2) ->
    T1 = refac_syntax:type(Exp1),
    case T1 of
        variable ->
            case is_object_variable(Exp1) of
                true -> false;
                _ ->
                    {true, [{Exp1, Exp2}]}
            end;
        _ ->
            false
    end.
            %%  T2 = refac_syntax:type(Exp2),
	    %% case {T1, T2} == {atom, module_qualifier} orelse 
            %%      {T1,T2}  == {module_qualifier, atom} of 
	    %%     true ->
	    %%         Ann1=refac_syntax:get_ann(Exp1),
	    %%         Ann2=refac_syntax:get_ann(Exp2),
	    %%         case lists:keysearch(fun_def,1,Ann1) of
	    %%     	{value, {fun_def, {M,F, A, _, _}}} ->
	    %%     	    case lists:keysearch(fun_def,1,Ann2) of
	    %%     		{value, {fun_def, {M1,F1,A1, _,_}}} ->
	    %%     		    case {M, F,A}=={M1, F1, A1} of
	    %%     			true-> {true, []};
	    %%     			false ->
	    %%     			    false
	    %%     		    end;
	    %%     		false->
	    %%     		    false
	    %%     	    end;
	    %%     	false ->
	    %%     	    false
	    %%         end;
	    %%     _ -> false
	    %% end
    %% end.

var_binding_structure(AST) when not is_list(AST) ->
    var_binding_structure([AST]);
var_binding_structure(ASTList) ->
    VarLocs = lists:keysort(2, refac_util:collect_var_source_def_pos_info(ASTList)),
    case VarLocs of
	[] ->
	    [];
	_ -> 
            [DefLoc|| {_Name, _SrcLoc, DefLoc} <- VarLocs]
    end.



%% TODO:check whether this is OK!!!!
is_meta_list(Node) ->
    is_meta_list_variable(Node) orelse is_meta_list_clause(Node).

has_meta_list(NodeList) ->
    [E ||E<-NodeList, is_meta_list(E)]/=[].

%% has_meta_list_variable(ExprList) ->
%%     [E ||E<-ExprList, is_meta_list_variable(E)]/=[].


%% This is prolematic!!
is_meta_list_clause(C) ->
    false.

    %% case refac_syntax:type(C) of
    %%     clause ->
    %%         Pat = refac_syntax:clause_patterns(C),
    %%         Body = refac_syntax:clause_body(C),
    %%         case Pat of 
    %%             [P] ->
    %%                 case Body of 
    %%                     [B] ->
    %%                         is_meta_list_variable(P) andalso 
    %%                             is_meta_list_variable(B);  
    %%                     _ -> false
    %%                 end;
    %%             _ -> false
    %%         end;
    %%     _ ->
    %%         false
    %% end.
            

            
is_meta_list_variable(Var) ->
    case refac_syntax:type(Var) of
        variable ->
            VarName = refac_syntax:variable_name(Var),
            lists:prefix("@@", lists:reverse(atom_to_list(VarName)));
        _ ->
            false
    end.

is_object_variable(Var) ->
    case refac_syntax:type(Var) of
        variable ->
            VarName = atom_to_list(refac_syntax:variable_name(Var)),
            not (lists:prefix("@", lists:reverse(VarName)));
        _ ->
          false
    end.

is_meta_atom(Node) ->
    case refac_syntax:type(Node) of
        atom ->
            AtomName = atom_to_list(refac_syntax:atom_value(Node)),
            lists:prefix("@", lists:reverse(AtomName));
        _ ->
            false
    end.

group_by_index(N, TupleList) ->
    group_by_1(N, lists:keysort(N, TupleList)).

group_by_1(_N, []) -> [];
group_by_1(N, TupleList=[E|_Es]) ->
    NthEle = element(N, E),
    {E1,E2} = lists:splitwith(fun(T) -> element(N,T) == NthEle end, TupleList),
    [E1 | group_by_1(N, E2)].
    
format(Es)when is_list(Es) ->
    [format(E)||E<-Es];
format(E) ->
    refac_prettypr:format(
      refac_util:reset_ann_and_pos(
        rm_comments(E))).

rm_comments(Node) ->
    refac_syntax:remove_comments(Node).

is_macro_name(Exp) ->
    Ann = refac_syntax:get_ann(Exp),
    {value, {syntax_path, macro_name}} == 
        lists:keysearch(syntax_path, 1, Ann).

