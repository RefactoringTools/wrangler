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
%% @private
-module(refac_sim_expr_search).

-export([sim_expr_search_in_buffer/6,sim_expr_search_in_dirs/6, normalise_record_expr/5]).

-export([sim_expr_search_in_buffer_eclipse/6, sim_expr_search_in_dirs_eclipse/6, 
	 normalise_record_expr_eclipse/5]).

-include("../include/wrangler_internal.hrl").

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
%% </p>

%%-spec(sim_expr_search_in_buffer/6::(filename(), pos(), pos(), string(),[dir()],integer())
%%      -> {ok, [{filename(), {{integer(), integer()}, {integer(), integer()}}}]}).    
sim_expr_search_in_buffer(FName, Start = {_Line, _Col}, End = {_Line1, _Col1}, SimiScore0, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_expr_search_in_buffer(~p, {~p,~p},{~p,~p},~p, ~p, ~p).\n",
		 [?MODULE, FName, _Line, _Col, _Line1, _Col1, SimiScore0, SearchPaths, TabWidth]),
    SimiScore = get_simi_score(SimiScore0),
    {FunDef, Exprs, SE} = get_fundef_and_expr(FName, Start, End, SearchPaths, TabWidth),
    {Ranges, AntiUnifier} = search_and_gen_anti_unifier([FName], {FName, FunDef, Exprs, SE}, SimiScore, SearchPaths, TabWidth),
    wrangler_code_search_utils:display_search_results(Ranges, AntiUnifier, "similar").

%%-spec(sim_expr_search_in_dirs/6::(filename(), pos(), pos(), string(),[dir()],integer())
%%      ->{ok, [{filename(), {{integer(), integer()}, {integer(), integer()}}}]}).     
sim_expr_search_in_dirs(FileName, Start = {_Line, _Col}, End = {_Line1, _Col1}, SimiScore0, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_expr_search_in_dirs(~p, {~p,~p},{~p,~p}, ~p, ~p, ~p).\n",
		 [?MODULE, FileName, _Line, _Col, _Line1, _Col1, SimiScore0, SearchPaths, TabWidth]),
    FileName1 = wrangler_misc:expand_files([FileName], ".erl"),
    Files = FileName1 ++ (wrangler_misc:expand_files(SearchPaths, ".erl") -- FileName1),
    SimiScore = get_simi_score(SimiScore0),
    {FunDef, Exprs, SE} = get_fundef_and_expr(FileName, Start, End, SearchPaths, TabWidth),
    {Ranges, AntiUnifier} = search_and_gen_anti_unifier(Files, {FileName, FunDef, Exprs, SE}, SimiScore, SearchPaths, TabWidth),
    wrangler_code_search_utils:display_search_results(lists:usort(Ranges), AntiUnifier, "similar").


%%-spec(sim_expr_search_in_buffer_eclipse/6::(filename(), pos(), pos(), float(),[dir()],integer())
%%      -> {ok, {[{{filename(), integer(), integer()}, {filename(), integer(), integer()}}], string()}}).
sim_expr_search_in_buffer_eclipse(FName, Start, End, SimiScore0, SearchPaths, TabWidth) ->
    sim_expr_search_eclipse(FName, Start, End, [FName], SimiScore0, SearchPaths, TabWidth).

%%-spec(sim_expr_search_in_dirs_eclipse/6::(filename(), pos(), pos(), float(),[dir()],integer())
%%    ->{ok,  {[{{filename(), integer(), integer()}, {filename(), integer(), integer()}}], string()}}).
sim_expr_search_in_dirs_eclipse(FileName, Start, End, SimiScore0, SearchPaths, TabWidth) ->
    Files = [FileName| wrangler_misc:expand_files(SearchPaths, ".erl") -- [FileName]],
    sim_expr_search_eclipse(FileName, Start, End, Files, SimiScore0, SearchPaths, TabWidth).
    

sim_expr_search_eclipse(CurFileName, Start, End, FilesToSearch, SimiScore0, SearchPaths, TabWidth) ->
    SimiScore = get_simi_score_eclipse(SimiScore0),
    {FunDef, Exprs, SE} = get_fundef_and_expr(CurFileName, Start, End, SearchPaths, TabWidth),
    {Ranges, AntiUnifier} = search_and_gen_anti_unifier(FilesToSearch, {CurFileName, FunDef, Exprs, SE},
							SimiScore, SearchPaths, TabWidth),
    Ranges1 = [{{FileName, StartLine, StartCol}, {FileName, EndLine, EndCol}}
	       || {FileName, {{StartLine, StartCol}, {EndLine, EndCol}}} <- Ranges],
    case length(Ranges1) =< 1 of
      true ->
	  {ok, {[], ""}};
      _ ->
	  {ok, {Ranges1, format(AntiUnifier)}}
    end.

   
search_and_gen_anti_unifier(Files, {FName, FunDef, Exprs, SE}, SimiScore, SearchPaths, TabWidth) ->
    {_Start, End} = SE,
    Res = lists:append([search_similar_expr_1(F, Exprs, SimiScore, SearchPaths, TabWidth) || F <- Files]),
    {Ranges, ExportVars, SubSt} = lists:unzip3(Res),
    ExportVars1 = {element(1, lists:unzip(vars_to_export(FunDef, End, Exprs))),
		   lists:usort(lists:append(ExportVars))},
    AntiUnifier = wrangler_anti_unification:generate_anti_unifier(Exprs, SubSt, ExportVars1),
    {[{FName, SE}| Ranges -- [{FName, SE}]], AntiUnifier}.

search_similar_expr_1(FName, Exprs, SimiScore, SearchPaths, TabWidth) ->
    try wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth) of
	{ok, {AnnAST, _}} ->
            RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
	    do_search_similar_expr(FName, AnnAST, RecordInfo, Exprs, SimiScore)
    catch
	_E1:_E2 ->
	    []
    end.

do_search_similar_expr(FileName, AnnAST, RecordInfo, Exprs, SimiScore) when is_list(Exprs) ->
    case length(Exprs)>1 of
	true ->
	    F0 = fun (FunNode, Acc) ->
			 F = fun (T, Acc1) ->
				     Exprs1 = get_expr_seqs(T),
				     do_search_similar_expr_1(FileName, Exprs, Exprs1, RecordInfo, SimiScore, FunNode) ++ Acc1
			     end,
			 api_ast_traverse:fold(F, Acc, FunNode)
		 end,
	    do_search_similar_expr_1(AnnAST, F0);
	false ->
	    Expr = hd(Exprs),
            F0 = fun (FunNode, Acc) ->
			 F = fun (T, Acc1) ->
				     case api_refac:is_expr(T) of
					 true ->
					     do_search_similar_expr_1(FileName, Expr, T, RecordInfo, SimiScore, FunNode) ++ Acc1;
					 _ -> Acc1
				     end
			     end,
			 api_ast_traverse:fold(F, Acc, FunNode)
		 end,
	    do_search_similar_expr_1(AnnAST, F0)
    end.

do_search_similar_expr_1(AnnAST, Fun) ->
    F1 = fun (Node, Acc) ->
		 case wrangler_syntax:type(Node) of
		     function -> Fun(Node, Acc);
		     _ -> Acc
		 end
	 end,
    lists:reverse(api_ast_traverse:fold(F1, [], AnnAST)).


get_expr_seqs(T) ->
    case wrangler_syntax:type(T) of
	clause ->
	    wrangler_syntax:clause_body(T);
	block_expr ->
	    wrangler_syntax:block_expr_body(T);
	try_expr ->
	    wrangler_syntax:try_expr_body(T);
	_ -> []
    end.

overlapped_locs({Start1, End1}, {Start2, End2}) ->
    Start1 =< Start2 andalso End2 =< End1 orelse
      Start2 =< Start1 andalso End1 =< End2.

do_search_similar_expr_1(FileName, Exprs1, Exprs2, RecordInfo, SimiScore, FunNode) when is_list(Exprs1) ->
    Len1 = length(Exprs1),
    Len2 = length(Exprs2),
    case Len1 =< Len2 of
	true ->
	    Exprs21 = lists:sublist(Exprs2, Len1),
	    {S1, E1} = wrangler_misc:start_end_loc(Exprs1),
	    {S2, E2} = wrangler_misc:start_end_loc(Exprs21),
	    case overlapped_locs({S1, E1}, {S2, E2}) of
		true -> [];
		_ ->
		    NormalisedExprs21 = normalise_expr(Exprs21, RecordInfo),
		    ExportedVars = vars_to_export(FunNode, E2, Exprs21),
		    case wrangler_anti_unification:anti_unification_with_score(Exprs1, NormalisedExprs21, SimiScore) of
			none ->
			    do_search_similar_expr_1(FileName, Exprs1, tl(Exprs2), RecordInfo, SimiScore, FunNode);
			SubSt ->
			    EVs = [SE1 || {SE1, SE2} <- SubSt, wrangler_syntax:type(SE2) == variable,
					  lists:member({wrangler_syntax:variable_name(SE2), get_var_define_pos(SE2)}, ExportedVars)],
			    [{{FileName, {S2, E2}}, EVs, SubSt}]++ 
			      do_search_similar_expr_1(FileName, Exprs1, tl(Exprs2), RecordInfo, SimiScore, FunNode)
		    end
	    end;
	_ -> []
    end;
do_search_similar_expr_1(FileName, Expr1, Expr2, RecordInfo, SimiScore, FunNode) ->
    {S1, E1} = wrangler_misc:start_end_loc(Expr1),
    {S2, E2} = wrangler_misc:start_end_loc(Expr2),
    case overlapped_locs({S1, E1}, {S2, E2}) of
	true -> 
            [];
	_ ->
	    NormalisedExpr21 = normalise_expr(Expr2, RecordInfo),
	    ExportedVars = vars_to_export(FunNode, E2, Expr2),
            Res = wrangler_anti_unification:anti_unification_with_score(Expr1, NormalisedExpr21, SimiScore),
	    case Res of
		none ->
		    [];
		SubSt ->
                    EVs = [SE1 || {SE1, SE2} <- SubSt, wrangler_syntax:type(SE2) == variable,
				  lists:member({wrangler_syntax:variable_name(SE2), get_var_define_pos(SE2)}, ExportedVars)],
		    [{{FileName, {S2, E2}}, EVs, SubSt}]
	    end
    end.

    
get_var_define_pos(V) ->
    {value, {def, DefinePos}} = lists:keysearch(def, 1, wrangler_syntax:get_ann(V)),
    DefinePos.

normalise_expr(Exprs, RecordInfo) ->
    try normalise_record_expr(Exprs, RecordInfo) of
	Exprs1->
	    Exprs1
    catch 
	_E1:_E2 ->
	    Exprs
    end.
normalise_record_expr(Exprs, RecordInfo) ->
    [api_ast_traverse:full_buTP(fun do_normalise_record_expr_1/2, E, {RecordInfo, true}) || E <- Exprs].
    
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

get_simi_score_eclipse(SimiScore) ->
    case SimiScore>=0.1 andalso SimiScore=<1.0 of 
	true -> SimiScore;
	_ ->?DefaultSimiScore
    end.

get_fundef_and_expr(FName, Start, End, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    case api_interface:pos_to_fun_def(AnnAST, Start) of
	{ok, FunDef} ->
	    Exprs = api_interface:pos_to_expr_list(FunDef, Start, End),
	    case Exprs of
		[] -> throw({error, "You have not selected an expression!"});
		_ ->
		    SE = wrangler_misc:start_end_loc(Exprs),
		    RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
		    Exprs1 = normalise_expr(Exprs, RecordInfo),
		    {FunDef, Exprs1, SE}
	    end;
	{error, _} -> throw({error, "You have not selected an expression!"})
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refactoring: Normalise record expression.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%-spec(normalise_record_expr/5::(filename(), pos(), boolean(), [dir()], integer()) -> {ok, [filename()]}).
normalise_record_expr(FName, Pos = {Line, Col}, ShowDefault, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:normalise_record_expr(~p, {~p,~p},~p, ~p, ~p).\n",
		 [?MODULE, FName, Line, Col, ShowDefault, SearchPaths, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":normalise_record_expr(" ++ "\"" ++ 
	    FName ++ "\", {" ++ integer_to_list(Line) ++ ", " ++ integer_to_list(Col) ++ "},"
      ++ atom_to_list(ShowDefault) ++ " [" ++ wrangler_misc:format_search_paths(SearchPaths)
    ++ "]," ++ integer_to_list(TabWidth) ++ ").",
    normalise_record_expr_0(FName, Pos, ShowDefault, SearchPaths, TabWidth, emacs, Cmd).
   

%%-spec normalise_record_expr_eclipse/5::(filename(), pos(), boolean(), [dir()], integer()) ->
%%				       {ok, [{filename(), filename(), string()}]}.
normalise_record_expr_eclipse(FName, Pos, ShowDefault, SearchPaths, TabWidth) ->
    normalise_record_expr_0(FName, Pos, ShowDefault, SearchPaths, TabWidth, eclipse, "").

normalise_record_expr_0(FName, Pos, ShowDefault, SearchPaths, TabWidth, Editor, Cmd) ->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FName, true, [], TabWidth),
    RecordExpr = pos_to_record_expr(AnnAST, Pos),
    case wrangler_syntax:type(wrangler_syntax:record_expr_type(RecordExpr)) of
	atom -> ok;
	_ -> throw({error, "Wrangler can only normalise a record expression with an atom as the record name."})
    end,
    {AnnAST1, _Changed} = normalise_record_expr_1(FName, AnnAST, Pos, ShowDefault, SearchPaths, TabWidth),
    wrangler_write_file:write_refactored_files([{{FName,FName}, AnnAST1}], Editor, TabWidth, Cmd).


normalise_record_expr_1(FName, AnnAST, Pos, ShowDefault, SearchPaths, TabWidth) ->
    RecordInfo = get_module_record_info(FName, SearchPaths, TabWidth),
    api_ast_traverse:stop_tdTP(fun do_normalise_record_expr/2, AnnAST, {Pos, RecordInfo, ShowDefault}).

do_normalise_record_expr(Node, {Pos, RecordInfo, ShowDefault}) ->
    case wrangler_syntax:type(Node) of
	record_expr ->
	    {S, E} = wrangler_misc:start_end_loc(Node),
	    case S =< Pos andalso Pos =< E of
		true ->
		    {api_ast_traverse:full_buTP(fun do_normalise_record_expr_1/2,
						Node, {RecordInfo, ShowDefault}), true};
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.

do_normalise_record_expr_1(Node, {RecordInfo, ShowDefault}) ->
    Fun = fun ({FName, FVal}, Fields) ->
		  R = [F || F <- Fields, wrangler_syntax:type(wrangler_syntax:record_field_name(F)) == atom,
			    wrangler_syntax:concrete(wrangler_syntax:record_field_name(F)) == FName],
		  case R of
		      [F] when ShowDefault -> [F];
		      [F] -> V = wrangler_syntax:record_field_value(F),
			     Cond = wrangler_syntax:type(V) == atom andalso wrangler_syntax:concrete(V) == undefined orelse
				      FVal =/= none andalso format(V) == format(FVal),
			     case Cond of
				 true -> [];
				 false -> [F]
			     end;
		      [] ->
			  Fs = [F || F <- Fields, wrangler_syntax:type(wrangler_syntax:record_field_name(F)) == underscore],
			  case Fs of
			      [F] ->
				  [wrangler_syntax:record_field(wrangler_syntax:atom(FName), wrangler_syntax:record_field_value(F))];
			      [] when ShowDefault ->
				  case FVal of
				      none -> [wrangler_syntax:record_field(
						    wrangler_syntax:atom(FName), set_random_pos(wrangler_syntax:atom(undefined)))];
				      _ -> [wrangler_syntax:record_field(wrangler_syntax:atom(FName), set_random_pos(FVal))]
				  end;
			      _ -> []
			  end
		  end
	  end,
    case wrangler_syntax:type(Node) of
	record_expr ->
	    Arg = wrangler_syntax:record_expr_argument(Node),
	    Type = wrangler_syntax:record_expr_type(Node),
	    Fields = wrangler_syntax:record_expr_fields(Node),
	    case wrangler_syntax:type(Type) of
		atom ->
		    case lists:keysearch(wrangler_syntax:concrete(Type), 1, RecordInfo) of
			{value, {_, Fields1}} ->
			    Fields2 = lists:append([Fun(F, Fields) || F <- Fields1]),
			    wrangler_misc:rewrite(Node, wrangler_syntax:record_expr(Arg, Type, Fields2));
			_ ->
			    Node
		    end;
		_ -> Node
	    end;
	_ -> Node
    end.

set_random_pos(Node) ->
    wrangler_syntax:set_pos(Node, {-random:uniform(200), -random:uniform(200)}).
 
pos_to_record_expr(Tree, Pos) ->
    case
      api_ast_traverse:once_tdTU(fun pos_to_record_expr_1/2, Tree, Pos)
	of
      {_, false} ->
	  throw({error, "You have not selected a record expression, "
			"or the function containing the record expression selected does not parse."});
      {R, true} ->
	  R
    end.

pos_to_record_expr_1(Node, Pos) ->
    case wrangler_syntax:type(Node) of
	record_expr ->
	    {S, E} = wrangler_misc:start_end_loc(Node),
	    case S =< Pos andalso Pos =< E of
		true -> {Node, true};
		_ -> {[], false}
	    end;
	_ -> {[], false}
    end.

get_module_record_info(FName, SearchPaths, TabWidth) ->
    Dir = filename:dirname(FName),
    DefaultIncl = [filename:join(Dir, X) || X <- wrangler_misc:default_incls()],
    Includes = SearchPaths ++ DefaultIncl,
    case wrangler_epp:parse_file(FName, Includes, [], TabWidth, wrangler_misc:file_format(FName)) of
	{ok, Forms, _} -> Forms1 = [F || F <- Forms, case F of
							 {attribute, _, file, _} -> false;
							 {attribute, _, type, {{record, _}, _, _}} -> false;
							 _ -> true
						     end],
			  SyntaxTree = wrangler_recomment:recomment_forms(Forms1, []),
			  Info = wrangler_syntax_lib:analyze_forms(SyntaxTree),
			  case lists:keysearch(records, 1, Info) of
			      {value, {records, Records}} -> Records;
			      _ -> []
			  end;
	{error, _Reason} -> []
    end.

vars_to_export(Fun, ExprEndPos, Expr) ->
    AllVars = wrangler_misc:collect_var_source_def_pos_info(Fun),
    ExprBdVarsPos = [Pos || {_Var, Pos} <- api_refac:bound_vars(Expr)],
    [{V, DefPos} || {V, SourcePos, DefPos} <- AllVars,
		    SourcePos > ExprEndPos,
		    lists:subtract(DefPos, ExprBdVarsPos) == []].


format(Node) ->
    wrangler_prettypr:format(wrangler_misc:reset_ann_and_pos(Node)).
