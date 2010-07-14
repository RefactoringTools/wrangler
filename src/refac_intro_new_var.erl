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
%% Refactoring: Introduce a new function definition to represent a selected expression sequence.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================

%% =============================================================================================
-module(refac_intro_new_var).

-export([intro_new_var/6, intro_new_var_eclipse/6]).


-include("../include/wrangler.hrl").

%% =============================================================================================
-spec(intro_new_var/6::(filename(), pos(), pos(), string(), [dir()],integer()) ->
 	     {'ok', [filename()]}).
intro_new_var(FileName, Start={SLine,SCol}, End={ELine, ECol}, NewVarName, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:intro_new_var(~p, {~p,~p}, {~p,~p}, ~p, ~p,~p).\n",
		 [?MODULE, FileName, SLine, SCol, ELine, ECol, NewVarName, SearchPaths, TabWidth]),
    intro_new_var(FileName, Start, End, NewVarName, SearchPaths, TabWidth, emacs).

-spec(intro_new_var_eclipse/6::(filename(), pos(), pos(), string(), [dir()], integer()) ->
 	      {ok, [{filename(), filename(), string()}]}| {error, string()}).
intro_new_var_eclipse(FileName, Start, End, NewVarName, SearchPaths, TabWidth) ->
    intro_new_var(FileName, Start, End, NewVarName, SearchPaths, TabWidth, eclipse).


intro_new_var(FileName, Start = {Line, Col}, End = {Line1, Col1}, NewVarName0, _SearchPaths, TabWidth, Editor) ->
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":intro_new_var(" ++ "\"" ++
	    FileName ++ "\", {" ++ integer_to_list(Line) ++ ", " ++ integer_to_list(Col) ++ "}," ++
	      "{" ++ integer_to_list(Line1) ++ ", " ++ integer_to_list(Col1) ++ "}," ++ "\"" ++ NewVarName0 ++ "\","
		++ integer_to_list(TabWidth) ++ ").",
    case refac_misc:is_var_name(NewVarName0) of 
	true -> ok;
	false -> throw({error, "Invalid new variable name."})
    end,
    NewVarName = list_to_atom(NewVarName0),
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FileName, true, [], TabWidth),
    case interface_api:pos_to_expr(AnnAST, Start, End) of
	{ok, Exp} ->
	    Exp;
	{error, _} -> throw({error, "You have not selected an expression, "
			     "or the function containing the expression does not parse."}),
		      Exp = none
    end,
    case interface_api:expr_to_fun(AnnAST, Exp) of
	{ok, Fun} ->
	    Fun;
	{error, _} -> throw({error, "You have not selected an expression within a function."}),
		      Fun = none
    end,
    ok=cond_check(Fun, Exp, NewVarName),
    intro_new_var_1(FileName, AnnAST,Fun, Exp, NewVarName, Editor, Cmd).

cond_check(_Form, Expr, NewVarName) ->
    {Envs, _BVs, _FVs} = {refac_misc:get_env_vars(Expr),
			refac_misc:get_bound_vars(Expr),
			refac_misc:get_free_vars(Expr)},
    _ExprFVs =refac_misc:get_free_vars(Expr),
    case lists:member(NewVarName, element(1,lists:unzip(Envs))) of 
	true -> throw({error, "New variable name is already declared in the same scope."});
	false -> ok
    end.
    


intro_new_var_1(FileName, AnnAST, Fun, Expr, NewVarName, Editor, Cmd) ->
    AnnAST1 = do_intro_new_var(AnnAST, Fun, Expr, NewVarName),
    case Editor of
	emacs ->
	    Res = [{{FileName, FileName}, AnnAST1}],
	    refac_util:write_refactored_files_for_preview(Res, Cmd),
	    {ok, [FileName]};
	eclipse ->
	    FileContent = refac_prettypr:print_ast(refac_util:file_format(FileName), AnnAST1),
	    {ok, [{FileName, FileName, FileContent}]}
    end.

do_intro_new_var(AnnAST, FunForm, Expr, NewVarName) ->
    NewFun = do_intro_new_var_in_fun(FunForm, Expr, NewVarName),
    Forms = refac_syntax:form_list_elements(AnnAST),
    Fun = fun (Form) ->
		  case refac_syntax:type(Form) of
		      function ->
			  case refac_syntax:get_pos(Form) == refac_syntax:get_pos(FunForm) of
			      true ->
				  NewFun;
			      false ->
				  Form
			  end;
		      _ ->
			  Form
		  end
	  end,
    refac_syntax:form_list([Fun(Form)|| Form <- Forms]).

do_intro_new_var_in_fun(Fun, Expr, NewVarName) ->
    EnclosingExpr =get_enclosing_body(Fun, Expr),
    {NewFun1,_} = ast_traverse_api:stop_tdTP(
		    fun do_insert_and_replace/2, Fun, {EnclosingExpr, Expr, NewVarName}),
    NewFun1.

do_insert_and_replace(Node, {ExprList, Expr, NewVarName}) ->
    case Node of 
	ExprList ->
	    {insert_new_expr(Node, Expr, NewVarName), true};
	_ -> {Node, false}
    end.


	    
insert_new_expr(Node, Expr,NewVarName) ->
    MatchExpr = make_match_expr(Expr, NewVarName),
    ExprPos = refac_syntax:get_pos(Expr),
    Body = case refac_syntax:type(Node) of 
	       clause ->
		   refac_syntax:clause_body(Node);
	       block_expr ->
		   refac_syntax:block_expr_body(Node);
	       try_expr ->
		   refac_syntax:try_expr_body(Node)
	   end,
    Fun = fun(ExprStatement) ->
		  {Start, End} = refac_misc:get_start_end_loc(ExprStatement),
		  case Start =< ExprPos andalso
		      ExprPos =< End of 
		      true ->
			  {ExprStatement1,_} =ast_traverse_api:stop_tdTP(
					       fun do_replace_expr_with_var/2, 
					       ExprStatement, {Expr, NewVarName}),
			  [MatchExpr, ExprStatement1];
		      false ->
			  [ExprStatement]
		  end
	  end,
    NewBody=[E1||E<-Body, E1<-Fun(E)],
    case refac_syntax:type(Node) of 
	clause ->
	    Pat = refac_syntax:clause_patterns(Node),
	    Guard = refac_syntax:clause_guard(Node),
	    refac_syntax:clause(Pat, Guard, NewBody);
	block_expr ->
	    refac_syntax:block_expr(NewBody);
	try_expr ->
	    C = refac_syntax:try_expr_clauses(Node),
	    H = refac_syntax:try_expr_handlers(Node),
	    A = refac_syntax:try_expr_after(Node),
	    refac_syntax:try_expr(NewBody, C, H, A)	      
    end.

%% get_enclosing_body_and_expr(Form, Expr) ->	 
%%       ExprPos = refac_syntax:get_pos(Expr),
%%     Fun = fun(Node, S) ->
%% 		  case refac_syntax:type(Node) of
%% 		      clause ->
%% 			  Body = refac_syntax:clause_body(Node),
%% 			  {Start,_End}= refac_misc:get_range(hd(Body)),	
%% 			  {_, End} = refac_misc:get_range(lists:last(Body)),
%% 			  case Start =< ExprPos  andalso 
%% 			      ExprPos =< End of
%% 			      true ->
%% 				  [{Node, End}|S];
%% 			      _ -> S
%% 			  end;
%% 		      _ ->
%% 			  S
%% 		  end
%% 	  end,
%%     Res =lists:keysort(2, refac_syntax_lib:fold(Fun, [], Form)),
%%     case Res of
%% 	[{Node,_}|_] ->
%% 	    Node;
%% 	_ -> throw({error, "Wrangler internal error."})
%%     end.

get_enclosing_body(Form, Expr) ->
    ExprPos = refac_syntax:get_pos(Expr),
    Fun = fun(Node, S) ->
		  case refac_syntax:type(Node) of
		      clause ->
			  Body = refac_syntax:clause_body(Node),
			  {Start,_End}= refac_misc:get_range(hd(Body)),	
			  {_, End} = refac_misc:get_range(lists:last(Body)),
			  case Start =< ExprPos  andalso 
			      ExprPos =< End of
			      true ->
				  [{Node, End}|S];
			      _ -> S
			  end;
		      _ ->
			  S
		  end
	  end,
    Res =lists:keysort(2, refac_syntax_lib:fold(Fun, [], Form)),
    case Res of
	[{Node,_}|_] ->
	    Node;
	_ -> throw({error, "Wrangler internal error."})
    end.

do_replace_expr_with_var(Node, {Expr, NewVarName}) ->
    case Node of 
	Expr ->
	    NewVarExpr = refac_misc:rewrite(
			   Expr,refac_syntax:variable(NewVarName)),
	    {NewVarExpr, true};
	_ -> {Node, false}
    end.

make_match_expr(Expr, NewVarName) ->
    Pat = refac_syntax:variable(NewVarName),
    refac_syntax:match_expr(Pat, Expr).

