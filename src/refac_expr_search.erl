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
%%@private
-module(refac_expr_search).

-export([expr_search_in_buffer/5, expr_search_in_dirs/5, expr_search_eclipse/4]).

-export([contained_exprs/2]).

-include("../include/wrangler_internal.hrl").
%% ================================================================================================
%% @doc Search for identical clones of an expression/ expression sequence in the current Erlang buffer.
%%
%% <p> This functionality allows the user to search for identical clones of an expression/expression sequence
%% in the current Erlang buffer. The searching algorithm ignores variables names and 
%% literals, but it takes the binding structure of variables into account. Since atoms have multiple roles 
%% in Erlang, only those atoms that are not function/module/process names are treated at literals (While Wrangler 
%% tries to distinguish not-literal atoms from literal atoms, this functionality is currently still limited.).
%% Therefore the expressions found are the same to the highlighted expression up to variable renaming and 
%% and literal substitution. Layout and comments are ignored during the search process.
%% </p>
%% <p> In the case that the code selected contains multiple, but non-continuous sequence of, expressions, the first
%% continuous sequence of expressions is taken as the user-selected expression. A continuous sequence of
%% expressions is a sequence of expressions separated by ','.
%% </p>
%% @end
%% =================================================================================================
%% @spec expr_search_in_buffer(FileName::filename(), Start::Pos, End::Pos,SearchPaths::[dir()], TabWidth::integer())-> 
%%           {ok, [{filename(), {{integer(), integer()}, {integer(), integer()}}}]}
%% @end
%% =================================================================================================         


%%-spec(expr_search_in_buffer/5::(filename(), pos(), pos(), [dir()], integer()) -> 
%%    {ok, [{filename(),{{integer(), integer()}, {integer(), integer()}}}]}).
expr_search_in_buffer(FileName, Start = {_Line, _Col}, End = {_Line1, _Col1}, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:expr_search_in_buffer(~p, {~p,~p},{~p,~p},~p, ~p).\n",
		 [?MODULE, FileName, _Line, _Col, _Line1, _Col1, SearchPaths, TabWidth]),
    Es = get_expr_selected(FileName, Start, End, SearchPaths, TabWidth),
    Res = do_expr_search(FileName, Es, SearchPaths, TabWidth),
    SE = wrangler_misc:start_end_loc(Es),
    Res1 = [{FileName, SE}| Res -- [{FileName, SE}]],
    wrangler_code_search_utils:display_search_results(Res1, none, "indentical").

%% ================================================================================================
%% @doc Search for identical clones of an expression/ expression sequence across multiple modules.
%%
%% <p> This functionality allows the user to search for identical clones of an expression/expression sequence
%% across an Erlang project, whose boundary is specified by the SearchPaths. The searching algorithm ignores 
%% variables names and literals, but it takes the binding structure of variables into account. Since atoms have 
%% multiple roles in Erlang, only those atoms that are not function/module/process names are treated at literals 
%% (While Wrangler tries to distinguish not-literal atoms from literal atoms, this functionality is currently still limited.).
%% Therefore the expressions found are the same to the highlighted expression up to variable renaming and 
%% and literal substitution. Layout and comments are ignored during the search process.
%% </p>
%% <p> In the case that the code selected contains multiple, but non-continuous sequence of, expressions, the first
%% continuous sequence of expressions is taken as the user-selected expression. A continuous sequence of
%% expressions is a sequence of expressions separated by ','.
%% </p>
%% @end
%% =================================================================================================
%% @spec expr_search_in_dirs(FileName::filename(), Start::Pos, End::Pos,SearchPaths::[dir()], TabWidth::integer())-> 
%%           {ok, [{filename(), {{integer(), integer()}, {integer(), integer()}}}]}
%% @end
%% =================================================================================================         
%%-spec(expr_search_in_dirs/5::(filename(), pos(), pos(), [dir()], integer()) -> 
%%    {ok, [{filename(),{{integer(), integer()}, {integer(), integer()}}}]}).   
expr_search_in_dirs(FileName, Start = {_Line, _Col}, End = {_Line1, _Col1}, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:expr_search_in_dirs(~p, {~p,~p},{~p,~p},~p, ~p, ~p).\n",
		 [?MODULE, FileName, _Line, _Col, _Line1, _Col1, SearchPaths, SearchPaths, TabWidth]),
    Files = [FileName| wrangler_misc:expand_files(SearchPaths, ".erl") -- [FileName]],
    Es = get_expr_selected(FileName, Start, End, SearchPaths, TabWidth),
    Res = lists:append([do_expr_search(F, Es, SearchPaths, TabWidth) || F <- Files]),
    SE = wrangler_misc:start_end_loc(Es),
    Res1 = [{FileName, SE}| Res -- [{FileName, SE}]],
    wrangler_code_search_utils:display_search_results(Res1, none, "indentical").

%%-spec(expr_search_eclipse/4::(filename(), pos(), pos(), integer()) ->
%%   {ok, [{{integer(), integer()}, {integer(), integer()}}]} | {error, string()}).
expr_search_eclipse(FileName, Start, End, TabWidth) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, [], TabWidth),
    case api_interface:pos_to_expr_list(AnnAST, Start, End) of
	[E| Es] ->
	    Res = case Es == [] of
		      true ->
			  search_one_expr(FileName, AnnAST, E);
		      _ ->
			  search_expr_seq(FileName, AnnAST, [E| Es])
		  end,
	    {ok, [SE || {_File, SE} <- Res]};
	_ -> {error, "You have not selected an expression!"}
    end.

get_expr_selected(FileName, Start, End, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    Es = api_interface:pos_to_expr_list(AnnAST, Start, End),
    case Es of
	[] -> throw({error, "You have not selected an expression!"});
	_ -> Es
    end.

do_expr_search(FileName, Es, SearchPaths, TabWidth) ->
    try wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth) of
	{ok, {AnnAST, _}} ->
	    case length(Es) of
		1 -> search_one_expr(FileName, AnnAST, hd(Es));
		_ -> search_expr_seq(FileName, AnnAST, Es)
	    end
    catch
	_E1:_E2 ->
	    []
    end.

%% Search the clones of an expression from Tree.
search_one_expr(FileName, Tree, Exp) ->
    SimplifiedExp = simplify_expr(Exp),
    BdStructExp = wrangler_code_search_utils:var_binding_structure([Exp]),
    F = fun (T, Acc) ->
		case api_refac:is_expr(T) orelse wrangler_syntax:type(T) == match_expr of
		    true -> T1 = simplify_expr(T),
			    case SimplifiedExp == T1 of
				true ->
				    case wrangler_code_search_utils:var_binding_structure([T]) of
					BdStructExp ->
					    StartEndLoc = wrangler_misc:start_end_loc(T),
					    [{FileName, StartEndLoc}| Acc];
					_ -> Acc
				    end;
				_ -> Acc
			    end;
		    _ -> Acc
		end
	end,
    lists:reverse(api_ast_traverse:fold(F, [], Tree)).
    
%% Search for the clones of an expresion sequence.
search_expr_seq(FileName, Tree, ExpList) ->
    AllExpList = contained_exprs(Tree, length(ExpList)),
    lists:flatmap(fun(EL) ->get_clone(FileName, ExpList, EL) end, AllExpList).

get_clone(FileName, ExpList1, ExpList2) ->
    Len1 = length(ExpList1),
    Len2 = length(ExpList2),
    case Len1 =< Len2 of
	true ->
	    SimplifiedExpList1 = simplify_expr(ExpList1),
	    SimplifiedExpList2 = simplify_expr(ExpList2),
	    case lists:prefix(SimplifiedExpList1, SimplifiedExpList2) of
		true ->
		    List22 = lists:sublist(ExpList2, Len1),
		    BdList1 = wrangler_code_search_utils:var_binding_structure(ExpList1),
		    BdList2 = wrangler_code_search_utils:var_binding_structure(List22),
		    case BdList1 == BdList2 of
			true ->
			    E1 = hd(List22),
			    En = lists:last(List22),
			    {StartLoc, _EndLoc} = wrangler_misc:start_end_loc(E1),
			    {_StartLoc1, EndLoc1} = wrangler_misc:start_end_loc(En),
			    [{FileName, {StartLoc, EndLoc1}}] ++ get_clone(FileName, ExpList1, tl(ExpList2));
			_ -> get_clone(FileName, ExpList1, tl(ExpList2))
		    end;
		_ ->
		    get_clone(FileName, ExpList1, tl(ExpList2))
	    end;
	_ -> []
    end.
%% Simplify expressions by masking variable names, literals and locations.
simplify_expr(Exp) when is_list(Exp) ->
    [simplify_expr(E) || E <- Exp];
simplify_expr(Exp) ->
    api_ast_traverse:full_buTP(
      fun (Node, _Others) ->
	      do_simplify_expr(Node)
      end, Exp, {}).


do_simplify_expr(Node) ->
    Node1 = case wrangler_syntax:type(Node) of
	      macro ->
		  case wrangler_syntax:macro_arguments(Node) of
		    none ->
			wrangler_syntax:default_literals_vars(Node, '*');
		    _ -> Node
		  end;
	      variable ->
		  wrangler_syntax:default_literals_vars(Node, '&');
	      integer ->
		  wrangler_syntax:default_literals_vars(Node, 0);
	      float ->
		  wrangler_syntax:default_literals_vars(Node, 0);
	      char ->
		  wrangler_syntax:default_literals_vars(Node, '%');
	      string ->
		  wrangler_syntax:default_literals_vars(Node, '%');
	      atom -> case wrangler_code_search_utils:generalisable(Node) of
			true ->
			    As = wrangler_syntax:get_ann(Node),
			    case lists:keysearch(type, 1, As) of
			      {value, _} ->
				  wrangler_syntax:default_literals_vars(
				       Node, wrangler_syntax:atom_value(Node));
			      false ->
				  wrangler_syntax:default_literals_vars(Node, '%')
			    end;
			_ -> wrangler_syntax:default_literals_vars(
			          Node, wrangler_syntax:atom_value(Node))
		      end;
	      nil -> wrangler_syntax:default_literals_vars(Node, nil);
	      underscore -> wrangler_syntax:default_literals_vars(Node, '&');
	      _ ->
		  Node
	    end,
    set_default_ann(Node1).
			  

set_default_ann(Node) ->
    wrangler_syntax:set_pos(wrangler_syntax:remove_comments(wrangler_syntax:set_ann(Node, [])), {0,0}).

%% get all the expression sequences contained in Tree.	    
contained_exprs(Tree, MinLen) ->
    F = fun (T, Acc) ->
		case wrangler_syntax:type(T) of
		    clause ->
			Exprs = wrangler_syntax:clause_body(T),  %% HOW ABOUT CLAUSE_GUARD?
			Acc ++ [Exprs];
		    application ->
			Exprs = wrangler_syntax:application_arguments(T),
			Acc++ [Exprs];
		    tuple ->
			Exprs = wrangler_syntax:tuple_elements(T),
			Acc++ [Exprs];
		    lists ->
			Exprs = wrangler_syntax:list_prefix(T),
			Acc++ [Exprs];
		    block_expr ->
			Exprs = wrangler_syntax:block_expr_body(T),
			Acc++ [Exprs];
		    try_expr ->
			Exprs = wrangler_syntax:try_expr_body(T),
			Acc ++ [Exprs];
		    _ -> Acc
		end
	end,
    Es = api_ast_traverse:fold(F, [], Tree),
    [E || E <- Es, length(E) >= MinLen].
