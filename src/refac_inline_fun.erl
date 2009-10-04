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
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================

%% =============================================================================================
-module(refac_inline_fun).

-export([inline_fun/4, inline_fun_eclipse/4]).

-include("../include/wrangler.hrl").

%% =============================================================================================
-spec(inline_fun/4::(FileName::filename(), Pos::pos(), SearchPaths::[dir()], TabWidth::integer)
      ->{'ok', [string()]}).
inline_fun(FileName, Pos, SearchPaths, TabWidth) ->
    inline_fun(FileName, Pos, SearchPaths, TabWidth, emacs).

-spec(inline_fun_eclipse/4::(FileName::filename(), Pos::pos(), SearchPaths::[dir()], TabWidth::integer)
      ->{ok, [{filename(), filename(), string()}]}).
inline_fun_eclipse(FileName,Pos,SearchPaths, TabWidth) ->
    inline_fun(FileName, Pos, SearchPaths, TabWidth, eclipse).

inline_fun(FName, Pos = {Line, Col}, SearchPaths, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:inline_fun(~p, {~p,~p}, ~p, ~p).\n",
		 [?MODULE, FName, Line, Col, SearchPaths, TabWidth]),
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    case pos_to_fun_clause_app(AnnAST, Pos) of
	{ok, {Clause, App}} -> 
	    {ok, FunDef} = side_cond_analysis(ModName, AnnAST, {Clause, App}),
	    fun_inline_1(FName, AnnAST, Pos, FunDef, {Clause, App}, Editor);
	{error, Reason} -> throw({error, Reason})
    end.
    

side_cond_analysis(ModName, AnnAST, {Clause, App}) ->
    FunName = refac_syntax:atom_value(refac_syntax:application_operator(App)),
    Args = refac_syntax:application_arguments(App),
    Arity = length(Args),
    Fs = refac_syntax:form_list_elements(AnnAST),
    Res = [F || F <- Fs, refac_syntax:type(F) == function,
		case lists:keysearch(fun_def, 1, refac_syntax:get_ann(F)) of
		    {value, {fun_def, {ModName, FunName, Arity, _, _}}} -> true;
		    _ -> false
		end],
    case Res of
	[FunDef] -> 
	    Cs = refac_syntax:function_clauses(FunDef),
	    case length(Cs) >1 of
		true -> throw({error, "Inlining a function with multiple function clauses is not supported yet."});
		false-> C = hd(Cs),
			Body = refac_syntax:clause_body(C),
			Ps = refac_syntax:clause_patterns(C),
			G = refac_syntax:clause_guard(C),
			case G of 
			    none ->
				case lists:all(fun(P) ->
						       refac_syntax:type(P)==variable orelse
							   refac_syntax:is_literal(P) 
					       end, Ps) of
				    true ->
					case static_semantic_checking(Body, {Clause, App}) of
					    {error, Reason} ->
						throw({error, Reason});
					    ok -> 
						{ok, FunDef}
					end;
				    false ->
					throw({error, "Inlining a function with complex parameters is not supported yet."})
				end;
			    _ ->throw({error, "Inlining a function with guard expressions is not supported yet."})
			end
	    end;
	[] -> 
	    throw({error, "The function to be inlined is not defined in this module."});
	[_|_]->
	    throw({error, "The function has been defined more than once."})
    end.

fun_inline_1(FName, AnnAST, Pos, FunDef, {Clause, App}, Editor) ->
    Args = refac_syntax:application_arguments(App),
    C = hd(refac_syntax:function_clauses(FunDef)),
    B = refac_syntax:clause_body(C),
    Ps = refac_syntax:clause_patterns(C),
    %% TOCHANGE: there is not def for literal patterns.
    PsDefPoss = lists:map(fun (P) ->
				  Ann = refac_syntax:get_ann(P),
				  case lists:keysearch(def, 1, Ann) of 
				      {value, {def, DefinePos}}  -> DefinePos;
				      _ -> false
				  end
			  end, Ps),
    Subst = [{P, A}||{P, A}<-lists:zip(PsDefPoss, Args), P=/=false],
    {SubstedBody, _} = lists:unzip([refac_util:stop_tdTP(fun do_subst/2, E, Subst) || E <- B]),
    Fs = refac_syntax:form_list_elements(AnnAST),
    Fs1 = [do_inline(F, Pos, Clause, App, SubstedBody) || F <- Fs],
    AnnAST1 = refac_util:rewrite(AnnAST, refac_syntax:form_list(Fs1)),
    case Editor of
      emacs ->
	    refac_util:write_refactored_files_for_preview([{{FName, FName}, AnnAST1}], ""),
	    {ok, [FName]};
	eclipse ->
	    Content = refac_prettypr:print_ast(refac_util:file_format(FName), AnnAST1),
	    {ok, [{FName, FName, Content}]}
    end.


do_inline(Form, Pos, Clause,App, SubstedBody) ->
    {S, E} = refac_util:get_range(Form),
    if  (S =< Pos) and (Pos =< E) ->
	    {F1, _} = refac_util:stop_tdTP(
			fun do_inline_1/2, Form, {App, SubstedBody}),
	    case length(SubstedBody) >1 of 
		true ->
		    Exprs = collect_exprs_in_expr_sequences(Clause),
		    case lists:member(App, Exprs) of
			true ->
			    {F2, _} = refac_util:stop_tdTP(fun remove_begin_end/2, F1, SubstedBody),
			    F2;
			_ -> F1
		    end;
		_ -> F1
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
    
collect_exprs_in_expr_sequences(Tree) ->  
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		      clause ->
			  S++refac_syntax:clause_body(T);
		      block_expr ->
			  S++ refac_syntax:block_expr_body(T);
		      try_expr ->
			  S++ refac_syntax:try_expr_body(T);
		      _ -> S
		  end
	  end,
    refac_syntax_lib:fold(Fun, [], Tree).

remove_begin_end(Node, BlockBody) ->
    Fun = fun(E) ->
		  case refac_syntax:type(E) of
		      block_expr ->
			  case refac_syntax:block_expr_body(E) of
			      BlockBody ->
				  BlockBody;
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
			{_, false} ->{error, "You have not selected a function application, "
				      "or the function containing the function application selected does not parse."};
			{App, true} ->
			    {{C, App}, true}
		    end;
	       true -> {[], false}
	    end;
	_ ->
	    {[], false}
    end. 
    

pos_to_fun_app(Node, Pos) ->
    case refac_util:once_tdTU(fun pos_to_fun_app_1/2, Node, Pos) of
	{_, false} -> {error, "You have not selected a function application, "
		       "or the function containing the function application does not parse."};
	{App, true} -> {App, true}
    end.

pos_to_fun_app_1(Node, Pos) ->
    case refac_syntax:type(Node) of
	application ->
	    {S, E} = refac_util:get_range(Node),
	    if (S =< Pos) and (Pos =< E) ->
		    {Node, true};
	       true -> {[], false}
	    end;
	_ -> {[], false} 
    end.



static_semantic_checking(BodyToInline, {Clause, App})->
    VarName ='WRANGLER_TEMP_VAR',
    {Clause1,_} = refac_util:stop_tdTP(fun do_replace_app_with_match/2, Clause,
				 {App, refac_syntax:variable(VarName)}),
    Clause2 = refac_syntax_lib:annotate_bindings(refac_util:reset_attrs(Clause1), []),
    BdVars = lists:usort([Name|| {Name,_Loc}<-refac_util:get_bound_vars(BodyToInline)]),
    case refac_util:once_tdTU(fun do_check_static_semantics/2, Clause2, {VarName, BdVars}) of
	{_, false} ->
	    ok;
	{_, true} ->
	    throw({error, "Unfolding this function application could change the semantics of the program!"})
    end.

do_replace_app_with_match(Node, {App, Var}) ->
    case Node of
	App ->
	  {refac_syntax:match_expr(Var, App), true};
	_-> {Node, false}
    end.


do_check_static_semantics(Node, {VarName, BdsInFunToInline}) ->
    Bds = lists:usort([Name || {Name, _Loc} <-refac_util:get_bound_vars(Node)]),
    Envs = lists:usort([Name || {Name, _Loc} <-refac_util:get_env_vars(Node)]),
    case lists:member(VarName, Bds) of 
	true ->
	    case BdsInFunToInline -- Bds of
		[] ->
		    case lists:member(VarName, Envs) of
			true ->
			    case BdsInFunToInline -- Bds of 
				[] ->{[], false};
				_ -> {Node, true}
			    end;
			false ->
			    {[], false}
		    end;
		_ -> {Node, true}
	    end;
	_ -> 
	    case lists:member(VarName, Envs) of
		true ->
		    case BdsInFunToInline -- Bds of 
			[] ->{[], false};
			_ -> {Node, true}
		    end;
		false ->
		    {[], false}
	    end 
    end.
			     
