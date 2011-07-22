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
%% Refactoring: Inline a variable definition.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================
%% @doc Inline a variable definition.
%% <p> To unfold a particular use instance of a variable, point the cursor to that instance, and then select
%% <em> Inline Variable </em> from the <em> Refactor </em> menu; to unfold some, or all, use instances of 
%% a variable, point the cursor to the define occurrence of the variable, then select <em> Inline Variable </em> 
%% from the <em> Refactor </em> menu, Wrangler will search for the use occurrences of the variable selected, and 
%% let you choose which instances to unfold. Only variables defined via a match expression of the 
%% format: VarName=Expr can be inlined.
%% </p>
%% @end

%% @private
-module(refac_inline_var).

-export([inline_var/5, inline_var_eclipse/5,
	 inline_var_1/7, inline_var_eclipse_1/6]).

-include("../include/wrangler_internal.hrl").

%%-spec (inline_var/5::(filename(), integer(), integer(),[dir()], integer()) ->
%%			   {ok, string()}|
%%			   {ok,[{pos(), pos()}], string()}).
inline_var(FName, Line, Col, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:inline_var(~p, ~p, ~p, ~p, ~p).\n",
		 [?MODULE, FName, Line, Col, SearchPaths, TabWidth]),
    inline_var(FName, Line, Col, SearchPaths, TabWidth, emacs).

%%-spec (inline_var_eclipse/5::(filename(), integer(), integer(), [dir()], integer()) ->
%%				   {ok, [{filename(), filename(), string()}]} |
%%				   {ok, [{pos(), pos()}]}).
inline_var_eclipse(FName, Line, Col, SearchPaths, TabWidth) ->
    inline_var(FName, Line, Col, SearchPaths, TabWidth, eclipse).

inline_var(FName, Line, Col, SearchPaths, TabWidth, Editor) ->
    Cmd1 = "CMD: " ++ atom_to_list(?MODULE) ++ ":inline_var(" ++ "\"" ++ 
	     FName ++ "\", " ++ integer_to_list(Line) ++ 
	       ", " ++ integer_to_list(Col) ++ ", " ++ "[" ++ wrangler_misc:format_search_paths(SearchPaths)
							       ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    Form = pos_to_form(AnnAST, {Line, Col}),
    case api_interface:pos_to_var(Form, {Line, Col}) of
	{ok, VarNode} ->
	    {ok, MatchExpr} = get_var_define_match_expr(Form, VarNode),
	    cond_check(MatchExpr, VarNode),
	    case is_use_instance(VarNode) of
		true ->
		    AnnAST1 = inline(AnnAST, Form, MatchExpr, VarNode, [wrangler_misc:start_end_loc(VarNode)]),
		    wrangler_write_file:write_refactored_files([{{FName,FName},AnnAST1}], Editor, TabWidth, Cmd1);
		false ->
		    Cands = search_for_unfold_candidates(Form, MatchExpr, VarNode),
		    case Cands of
			[] ->
			    throw({error, "No unfoldable use instances of this variable were found."});
			[C] ->
			    AnnAST1 = inline(AnnAST, Form, MatchExpr, VarNode, [C]),
			    wrangler_write_file:write_refactored_files([{{FName,FName},AnnAST1}], Editor, TabWidth, Cmd1);
			_ ->
			    case Editor of
				emacs ->
				    {ok, Cands, Cmd1};
				_ ->
				    {ok, Cands}
			    end
		    end
	    end;
	_ ->
	    throw({error, "You have not selected a variable name, "
			  "or the variable selected does not belong to "
			  "a syntactically well-formed function."})
    end.


%%-spec (inline_var_1/7::(filename(), integer(), integer(), [{pos(), pos()}], [dir()], integer(), string()) ->
%%				     {ok, [{filename(), filename(), string()}]}).
inline_var_1(FileName, Line, Col, Candidates, SearchPaths, TabWidth, Cmd) ->
    ?wrangler_io("\nCMD: ~p:inline_var_1(~p, ~p, ~p, ~p, ~p, ~p, ~p).\n",
		 [?MODULE, FileName, Line, Col, Candidates, SearchPaths, TabWidth, ""]),
    inline_var_1(FileName, Line, Col, Candidates, SearchPaths, TabWidth, Cmd, emacs).

%%-spec (inline_var_eclipse_1/6::(filename(), integer(), integer(), [{pos(), pos()}], [dir()], integer()) ->
%%				     {ok, [{filename(), filename(), string()}]}).				   
inline_var_eclipse_1(FileName, Line, Col, Candidates, SearchPaths, TabWidth) ->
    inline_var_1(FileName, Line, Col, Candidates, SearchPaths, TabWidth, "", eclipse).

inline_var_1(FileName, Line, Col, Candidates, SearchPaths, TabWidth, Cmd, Editor) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    Form = pos_to_form(AnnAST, {Line, Col}),
    {ok, VarNode} = api_interface:pos_to_var(Form, {Line, Col}),
    {ok, MatchExpr} = get_var_define_match_expr(Form, VarNode),
    AnnAST1 = inline(AnnAST, Form, MatchExpr, VarNode, Candidates),
    wrangler_write_file:write_refactored_files([{{FileName,FileName}, AnnAST1}], Editor, TabWidth, Cmd).

%% inline_var_1(FileName, Line, Cols, Cands, SearchPaths, TabWidth, Cmd)
is_use_instance(VarNode) ->
    {value, {def, DefinePos}} = lists:keysearch(def, 1, wrangler_syntax:get_ann(VarNode)),
    Pos = wrangler_syntax:get_pos(VarNode),
    not lists:member(Pos, DefinePos).
	     
pos_to_form(Node, Pos) ->
    case api_ast_traverse:once_tdTU(fun pos_to_form_1/2, Node, Pos) of
	{_, false} -> throw({error, "You have not selected a variable in a syntactically well-formed function."});
	{R, true} -> R
    end.

pos_to_form_1(Node, Pos) ->
    case wrangler_syntax:type(Node) of
	function ->
	    {S, E} = wrangler_misc:start_end_loc(Node),
	    if (S =< Pos) and (Pos =< E) ->
		   {Node, true};
	       true -> {[], false}
	    end;
	_ -> {[], false}
    end.

cond_check(MatchExpr, VarNode) ->
    {value, {def, DefinePos}} = lists:keysearch(def, 1, wrangler_syntax:get_ann(VarNode)),
    case length(DefinePos)>1 of 
	true ->
	    throw({error, "Wrangler does not support unfolding of "
		   "a variable with multiple binding occurrences yet."});
	false ->
	    ok
    end,
    MatchExprBody = wrangler_syntax:match_expr_body(MatchExpr),
    case cond_check_1(MatchExprBody, VarNode) of 
	ok ->
	    ok;
	{error, Msg} ->
	    throw({error, Msg})
    end.

search_for_unfold_candidates(Form, MatchExprBody, VarNode) ->
    {value, {def, DefinePos}} = lists:keysearch(def, 1, wrangler_syntax:get_ann(VarNode)),
    F = fun (Node, Acc) ->
		case wrangler_syntax:type(Node) of
		    variable when Node/=VarNode ->
			case lists:keysearch(def, 1, wrangler_syntax:get_ann(Node)) of
			    {value, {def, DefinePos}} ->
				case cond_check_1(MatchExprBody, Node) of
				    ok ->
					[wrangler_misc:start_end_loc(Node)| Acc];
				    _ -> Acc
				end;
			    _ -> Acc
			end;
		    _ ->
			Acc
		end
	end,
    lists:sort(api_ast_traverse:fold(F, [], Form)).

collect_all_uses(Form, VarNode) ->
    {value, {def, DefinePos}} = lists:keysearch(def, 1, wrangler_syntax:get_ann(VarNode)),
    F = fun (Node, Acc) ->
		case wrangler_syntax:type(Node) of
		    variable ->
			case lists:keysearch(def, 1, wrangler_syntax:get_ann(Node)) of
			    {value, {def, DefinePos}} ->
				Pos = wrangler_syntax:get_pos(Node),
				case  not  lists:member(Pos, DefinePos) of
				    true ->
					[wrangler_misc:start_end_loc(Node)| Acc];
				    false ->
					[]
				end;
			    _ -> Acc
			end;
		    _ ->
			Acc
		end
	end,
    lists:sort(api_ast_traverse:fold(F, [], Form)).

cond_check_1(MatchExprBody, VarNode) ->
    MatchExprFreeVars = api_refac:free_vars(MatchExprBody),
    MatchExprBoundVars = get_bound_vars(MatchExprBody),
    VarEnvs = api_refac:env_vars(VarNode),
    case MatchExprFreeVars--VarEnvs of
	[] ->
	    VarEnvs1 = VarEnvs -- MatchExprBoundVars,
	    ShadowVs = [{V, Pos} || {V,Pos} <- MatchExprBoundVars,
				    [{V1,P1} || {V1, P1} <- VarEnvs1, V1==V, P1/=Pos]=/=[]],
	    case ShadowVs of
		[] -> ok;
		Vs ->
		    VNames = [V || {V,_} <- Vs],
		    Msg = lists:flatten(io_lib:format("Vairable(s), ~p, used by the definition of the variable "
						      "selected could cause name shadowing, or semantics changes, "
						      "after inlining.", [VNames])),
		    {error, Msg}
	    end;
	[{V,_}] ->
	    Msg = lists:flatten(io_lib:format("Variable, ~p, used by definition of the variable selected is "
					      "not visible to the variable instance to unfold.",[V])),
	    {error, Msg};
	Vs ->
	    VNames = [V || {V,_} <- Vs],
	    Msg = lists:flatten(io_lib:format("Variables, ~p, used by definition of the variable selected are "
					      "not visible to the variable instance to unfold.",[VNames])),
	    {error, Msg}
    end.

get_bound_vars(Tree) ->
    F = fun (T, B) ->
		As = wrangler_syntax:get_ann(T),
		case lists:keysearch(bound, 1, As) of
		    {value, {bound, BdVars1}} -> BdVars1++B;
		    _ -> B
		end
	end,
    lists:usort(api_ast_traverse:fold(F, [], Tree)).
    

get_var_define_match_expr(Form, VarNode)->
    {value, {def, DefinePos}} = lists:keysearch(def, 1, wrangler_syntax:get_ann(VarNode)),
    case api_ast_traverse:once_tdTU(fun pos_to_match_expr_1/2, Form, DefinePos) of
	{MatchExpr, true} ->
	    {ok, MatchExpr};
	{_, false} ->
	    throw({error, "Wrangler can only inline a variable that is "
		   "defined by a simple match expression, i.e. VarName==Expr."})
    end.

pos_to_match_expr_1(Node, DefinePos) ->
    case wrangler_syntax:type(Node) of
	match_expr ->
	    Pattern = wrangler_syntax:match_expr_pattern(Node),
	    case wrangler_syntax:type(Pattern) of
		variable ->
		    case lists:keysearch(def, 1, wrangler_syntax:get_ann(Pattern)) of
			{value, {def, DefinePos}} ->
                            Body = wrangler_syntax:match_expr_body(Node),
                            case wrangler_syntax:type(Body) of
                                match_expr ->
                                    Body1 = get_match_expr_body(Node),
                                    {wrangler_syntax:match_expr(Pattern, Body1), true};
                                _ ->
                                    {Node, true}
                            end;
			_ ->
			   {[], false}
		    end;
		_ ->
		    {[], false}
	    end;
	_ ->
	    {[], false}
    end.

get_match_expr_body(Node) ->
    case wrangler_syntax:type(Node) of
        match_expr ->
            get_match_expr_body(wrangler_syntax:match_expr_body(Node));
        _ ->
            Node
    end.
inline(AnnAST, Form, MatchExpr, VarNode, Ps) ->
    FormPos = wrangler_syntax:get_pos(Form),
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    NewForms = [case wrangler_syntax:get_pos(F) of
		    FormPos ->
                        do_inline_in_form(F, MatchExpr, VarNode, Ps);
		    _ -> F
		end || F <- Forms],
    wrangler_misc:rewrite(AnnAST, wrangler_syntax:form_list(NewForms)).

   
do_inline_in_form(Form, MatchExpr, VarNode, Ps) ->
    MatchExprBody = wrangler_syntax:match_expr_body(MatchExpr),
    AllUseInstances = collect_all_uses(Form, VarNode),
    {Form2, _} = api_ast_traverse:stop_tdTP(fun do_inline/2, Form, {MatchExprBody, Ps}),
    case lists:usort(AllUseInstances) == lists:usort(Ps) of 
        true ->
            remove_match_expr(Form2, MatchExpr);
        false ->
            Form2
    end.
 
remove_match_expr(Form, MatchExpr) ->
    {NewForm, _} = api_ast_traverse:stop_tdTP(
		     fun do_remove_match_expr/2, Form, MatchExpr),
    NewForm.

do_remove_match_expr(Node,MatchExpr) ->
    case wrangler_syntax:type(Node) of
	clause ->
	    Pat = wrangler_syntax:clause_patterns(Node),
	    Guard = wrangler_syntax:clause_guard(Node),
	    Body = wrangler_syntax:clause_body(Node),
	    NewBody = remove_match_expr_from_body(MatchExpr,Body),
	    case NewBody == Body of
		true ->
		    {Node, false};
		false ->
		    Node1 = wrangler_syntax:clause(Pat,Guard,NewBody),
		    {wrangler_misc:rewrite(Node,Node1),true}
	    end;
	block_expr ->
	    Body = wrangler_syntax:block_expr_body(Node),
	    NewBody = remove_match_expr_from_body(MatchExpr,Body),
            case NewBody==Body of
		true ->
		    {Node,false};
		false ->
		    Node1 = wrangler_syntax:block_expr(NewBody),
		    {wrangler_misc:rewrite(Node,Node1),true}
	    end;
	try_expr ->
	    C = wrangler_syntax:try_expr_clauses(Node),
	    H = wrangler_syntax:try_expr_handlers(Node),
	    A = wrangler_syntax:try_expr_after(Node),
	    Body = wrangler_syntax:try_expr_body(Node),
	    NewBody = remove_match_expr_from_body(MatchExpr,Body),
	    case NewBody==Body of
		true ->
		    {Node,false};
		false ->
		    Node1 = wrangler_syntax:try_expr(NewBody,C,H,A),
		    {wrangler_misc:rewrite(Node,Node1),true}
	    end;
	_ -> {Node,false}
    end.

remove_match_expr_from_body(MatchExpr,Body) ->
    {Start,_End} = wrangler_misc:start_end_loc(MatchExpr),
    {B1,B2} = lists:splitwith(fun (B) ->
				      B/=MatchExpr
			      end,Body),
    case B2 of
	[] -> Body;
	[_] -> B1;
	[_| B3] ->
	    B3Hd = hd(B3),
	    B3Tl = tl(B3),
	    {_,End} = wrangler_misc:start_end_loc(B3Hd),
	    B3Hd1 = wrangler_misc:update_ann(B3Hd,{range,{Start,End}}),
	    B1++[B3Hd1| B3Tl]
    end.

do_inline(Node, {MatchExprBody, Ranges}) ->
    case wrangler_syntax:type(Node) of
	variable ->
	    case lists:member(wrangler_misc:start_end_loc(Node), Ranges) of
		true ->
                    {wrangler_misc:rewrite_with_wrapper(Node, MatchExprBody), true};
                false ->
		    {Node, false}
	    end;
	_ -> {Node, false}
    end.



