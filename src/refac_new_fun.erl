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
%% @private
-module(refac_new_fun).

-export([fun_extraction/5, fun_extraction_1/5, fun_extraction_eclipse/5, fun_extraction_1_eclipse/5]).


-include("../include/wrangler_internal.hrl").

%% =============================================================================================
%%-spec(fun_extraction/5::(filename(), pos(), pos(), string(), integer()) ->
%% 	     {'ok', [filename()]}).
fun_extraction(FName, Start, End, NewFunName, TabWidth) ->
    fun_extraction(FName, Start, End, NewFunName, TabWidth, emacs).

fun_extraction_1(FName, Start, End, NewFunName, TabWidth) ->
    fun_extraction_1(FName, Start, End, NewFunName, TabWidth, emacs).

%%-spec(fun_extraction_eclipse/5::(filename(), pos(), pos(), string(),integer()) ->
%% 	      {ok, [{filename(), filename(), string()}]}).
fun_extraction_eclipse(FileName, Start, End, NewFunName, TabWidth) ->
    fun_extraction(FileName, Start, End, NewFunName, TabWidth, eclipse).

fun_extraction_1_eclipse(FileName, Start, End, NewFunName, TabWidth) ->
    fun_extraction_1(FileName, Start, End, NewFunName, TabWidth, eclipse).

fun_extraction_1(FileName, Start = {Line, Col}, End = {Line1, Col1}, NewFunName, TabWidth, Editor) ->
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":fun_extraction(" ++ "\"" ++ 
	    FileName ++ "\", {" ++ integer_to_list(Line) ++ ", " ++ integer_to_list(Col) ++ "}," ++ 
	      "{" ++ integer_to_list(Line1) ++ ", " ++ integer_to_list(Col1) ++ "}," ++ "\"" ++ NewFunName ++ "\","
        ++ integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, [], TabWidth),
    ExpList = interface_api:pos_to_expr_list(AnnAST, Start, End),
    {ok, Fun} = interface_api:expr_to_fun(AnnAST, hd(ExpList)),
    fun_extraction_1(FileName, AnnAST, End, Fun, ExpList, NewFunName, Editor, TabWidth, Cmd).

fun_extraction(FileName, Start = {Line, Col}, End = {Line1, Col1}, NewFunName, TabWidth, Editor) ->
    ?wrangler_io("\nCMD: ~p:fun_extraction(~p, {~p,~p}, {~p,~p}, ~p, ~p).\n",
		 [?MODULE, FileName, Line, Col, Line1, Col1, NewFunName, TabWidth]),
    Cmd = "CMD: " ++ atom_to_list(?MODULE) ++ ":fun_extraction(" ++ "\"" ++ 
	    FileName ++ "\", {" ++ integer_to_list(Line) ++ ", " ++ integer_to_list(Col) ++ "}," ++ 
	      "{" ++ integer_to_list(Line1) ++ ", " ++ integer_to_list(Col1) ++ "}," ++ "\"" ++ NewFunName ++ "\","
        ++ integer_to_list(TabWidth) ++ ").",
    case refac_api:is_fun_name(NewFunName) of
	true -> ok;
	false -> throw({error, "Invalid function name!"})
    end,
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, [], TabWidth),
    case interface_api:pos_to_expr_list(AnnAST, Start, End) of
	[] -> ExpList = [],
	      throw({error, "You have not selected an expression or a sequence of expressions, "
			    "or the function containing the expression(s) selected is malformed."});
	ExpList ->
	    ExpList
    end,
    {ok, Fun} = interface_api:expr_to_fun(AnnAST, hd(ExpList)),
    ok = side_cond_analysis(FileName, Info, Fun, ExpList, list_to_atom(NewFunName)),
    fun_extraction_1(FileName, AnnAST, End, Fun, ExpList, NewFunName, Editor, TabWidth, Cmd).

fun_extraction_1(FileName, AnnAST, End, Fun, ExpList, NewFunName, Editor, TabWidth, Cmd) ->
    FunName = refac_syntax:atom_value(refac_syntax:function_name(Fun)),
    FunArity = refac_syntax:function_arity(Fun),
    FunPos = refac_syntax:get_pos(Fun),
    {FrVars, BdVars} = get_free_bd_vars(ExpList),
    VarsToExport = vars_to_export(Fun, End, BdVars),
    AnnAST1 = do_fun_extraction(FileName, AnnAST, ExpList, NewFunName, FrVars, VarsToExport, {FunName, FunArity, FunPos}),
    case Editor of
	emacs ->
	    Res = [{{FileName, FileName}, AnnAST1}],
	    refac_write_file:write_refactored_files_for_preview(Res, TabWidth, Cmd),
	    {ok, [FileName]};
	eclipse ->
	    FileContent = refac_prettypr:print_ast(refac_misc:file_format(FileName), AnnAST1, TabWidth),
	    {ok, [{FileName, FileName, FileContent}]}
    end.

get_free_bd_vars(ExpList) ->
    FrBdVars = [envs_bounds_frees(E) || E <- ExpList],
    BdVars = lists:usort(lists:append([Vars || {{bound, Vars}, _} <- FrBdVars])),
    FrVars1 = lists:usort(lists:append([Vars || {_, {free, Vars}} <- FrBdVars])),
    FrVars = refac_misc:remove_duplicates(
	       [VarName || {VarName, _Pos} <- lists:keysort(2, FrVars1 -- BdVars)]),
    {FrVars, BdVars}.

side_cond_analysis(FileName, Info, Fun, ExpList, NewFunName) ->
    funcall_replaceable(Fun, ExpList),
    {FrVars, _} = get_free_bd_vars(ExpList),
    InScopeFuns = [{F, A} || {_M, F, A} <- refac_api:inscope_funs(Info)],
    case lists:member({NewFunName, length(FrVars)}, InScopeFuns) orelse 
	   erl_internal:bif(erlang, NewFunName, length(FrVars))
	of
	true ->
	    throw({error, "The given function name has been used by this module, "
			  "or is used as an Erlang builtin function name, please choose another name!"});
	_ -> ok
    end,
    TestFrameWorkUsed = refac_misc:test_framework_used(FileName),
    test_framework_aware_name_checking(TestFrameWorkUsed, NewFunName, length(FrVars)).

funcall_replaceable(Fun,[Exp]) ->
    check_expr_category(Fun, Exp),
    {StartPos, EndPos} = refac_api:start_end_loc(Exp),
    Ranges = collect_prime_expr_ranges(Fun),
    Res = lists:any(
	    fun ({StartPos1, EndPos1}) ->
		    StartPos >= StartPos1 andalso EndPos =< EndPos1
	    end, Ranges),
    case Res of
	true -> throw({error, "The expression selected "
			      "cannot be replaced by a function call!"});
	_ ->
	    true
    end;
funcall_replaceable(Fun, ExpList) ->
    ExpList1 = filter_exprs_via_ast(Fun, ExpList),
    case ExpList1 of
	[] -> true;
	_ -> throw({error, "The expression sequence selected "
			   "cannot be replaced by a function call!"})
    end.
  
check_expr_category(Fun, Exp) ->
    case is_macro_arg(Fun, Exp) of
	true ->
	    throw({warning, "The expression selected is part of a macro application, "
		   "Wrangler cannot guarantee the correctness of transformation. Still continue?"});
	false ->
	    check_expr_category_1(Exp)
    end.

check_expr_category_1(Exp) ->
    case lists:keysearch(category, 1, refac_syntax:get_ann(Exp)) of
        {value, {category, guard_expression}} ->
	    throw({error, "Function abstraction whithin a "
                   "guard expression is not supported."});
	_ ->
	    case refac_syntax:type(Exp) of 
		binary_field ->
		    throw({error, "Function abstraction over a binary field is "
			   "not supported."});
		_ ->
		    true
	    end
    end.

is_macro_arg(Fun, Exp) ->
    MacroArgRanges = collect_macro_arg_ranges(Fun),
    {Start, End} = refac_api:start_end_loc(Exp),
    lists:any(fun ({S1, E1}) ->
		      S1 =< Start andalso End =< E1
	      end, MacroArgRanges).

collect_prime_expr_ranges(Tree) ->
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		    application ->
			Operator = refac_syntax:application_operator(T),
			Range = refac_api:start_end_loc(Operator),
			S ++ [Range];
		    _ -> S
		end
	end,
    ast_traverse_api:fold(F, [], Tree).

collect_macro_arg_ranges(Node) ->
    Fun = fun (T, Acc) ->
		  case refac_syntax:type(T) of
		      macro ->
			  Args = refac_syntax:macro_arguments(T),
			  case Args of
			      none -> Acc;
			      _ ->
				  [refac_api:start_end_loc(A) || A <- Args] ++ Acc
			  end;
		      _ -> Acc
		  end
	  end,
    ast_traverse_api:fold(Fun, [], Node).

test_framework_aware_name_checking(UsedFrameWorks, NewFunName, Arity) ->
    eunit_name_checking(UsedFrameWorks, NewFunName, Arity),
    eqc_name_checking(UsedFrameWorks, NewFunName, Arity),
    testserver_name_checking(UsedFrameWorks, NewFunName, Arity),
    commontest_name_checking(UsedFrameWorks, NewFunName, Arity).


eunit_name_checking(UsedFrameWorks, NewFunName, Arity) ->
    case lists:member(eunit,UsedFrameWorks) of 
	true when Arity==0 ->
	    case lists:suffix(?DEFAULT_EUNIT_TEST_SUFFIX, atom_to_list(NewFunName)) of 
		true -> 
		    throw({warning, "The new function name means that "
			   "the new function is an EUnit test function, continue?"});
		false ->
		    case lists:suffix(?DEFAULT_EUNIT_GENERATOR_SUFFIX,atom_to_list(NewFunName)) of 
			true -> 
			    throw({warning, "The new function name means that "
				   "the new function is an Eunit test generator function, continue?"});
			false -> 
			    ok
		    end
	    end;
	_ -> ok
    end.

eqc_name_checking(UsedFrameWorks, NewFunName, Arity) ->
    case lists:member(eqc_statem, UsedFrameWorks) of
	true ->
	    case lists:member({NewFunName, Arity}, refac_misc:eqc_statem_callback_funs()) of
		true ->
		    throw({warning, "The new function name means that "
				    "the new function is a quickcheck callback function, continue?"});
		false -> ok
	    end;
	false -> ok
    end,
    case lists:member(eqc, UsedFrameWorks) of
	true ->
	    case lists:prefix("prop_", atom_to_list(NewFunName)) of
		true ->
		    throw({warning, "The new function name means that "
				    "the new function is a quickcheck property, continue?"});
		false -> ok
	    end;
	false -> ok
    end.

testserver_name_checking(UsedFrameWorks, NewFunName, Arity) ->
    case lists:member(testserver, UsedFrameWorks) of
	true ->
	    case lists:member({NewFunName, Arity}, refac_misc:testserver_callback_funs()) of
		true ->
		    throw({warning, "The new function name means that "
				    "the new function is a Test Server callback function, continue?"});
		false -> ok
	    end;
	false -> ok
    end.

commontest_name_checking(UsedFrameWorks, NewFunName, Arity) ->
    case lists:member(commontest, UsedFrameWorks) of
	true ->
	    case lists:member({NewFunName, Arity}, refac_misc:commontest_callback_funs()) of
		true ->
		    throw({warning, "The new function name means that "
				    "the new function is a Common Test callback function, continue?"});
		false -> ok
	    end;
	false -> ok
    end.

do_fun_extraction(FileName, AnnAST, ExpList, NewFunName, ParNames, VarsToExport, 
                  {EncFunName, EncFunArity, EncFunPos}) ->
    NewFunName1 = refac_syntax:atom(NewFunName),
    Pars = [refac_syntax:variable(P) || P <- ParNames],
    ExportExpr = case VarsToExport of
                     [] -> none;
                     [V] -> refac_syntax:variable(V);
                     [_V| _Vs] ->
                         Elems = [refac_syntax:variable(V1) || V1 <- VarsToExport],
                         refac_syntax:tuple(Elems)
                 end,
    ExpList1 = case VarsToExport of
                   [] ->
                       [E|Es] = lists:reverse(ExpList),
                       case refac_syntax:type(E) of 
                           match_expr ->
                               Pat = refac_syntax:match_expr_pattern(E),
                               case refac_syntax:type(Pat)==variable andalso 
                                   refac_api:free_vars(Pat) == [] of
                                   true ->
                                       lists:reverse([refac_syntax:match_expr_body(E)|Es]);
                                   false ->
                                      ExpList
                               end;
                           _ ->
                               ExpList
                       end;
                   [V1] ->
                       [E|Es] = lists:reverse(ExpList),
                       case refac_syntax:type(E) of 
                           match_expr ->
                               Pat = refac_syntax:match_expr_pattern(E),
                               case refac_syntax:type(Pat)==variable of 
                                   true ->
                                       case refac_syntax:variable_name(Pat)==V1 of 
                                           true ->
                                               lists:reverse([refac_syntax:match_expr_body(E)|Es]);
                                           false ->
                                               ExpList++[ExportExpr]
                                       end;
                                   _ ->
                                       ExpList++[ExportExpr]
                               end;
                           _ ->
                               ExpList++[ExportExpr]
                       end;
                   _ ->
                       ExpList++[ExportExpr]
               end,
    Clause = refac_syntax:clause(Pars, [], ExpList1),
    NewFun = refac_syntax:function(NewFunName1, [Clause]),
    Forms = refac_syntax:form_list_elements(AnnAST),
    Fun = fun (Form) ->
		  case refac_syntax:type(Form) of
		      function ->
			  Name = refac_syntax:atom_value(refac_syntax:function_name(Form)),
			  Arity = refac_syntax:function_arity(Form),
			  Pos = refac_syntax:get_pos(Form),
			  case {Name, Arity, Pos} == {EncFunName, EncFunArity, EncFunPos} of
			      true ->
				  Form1 = replace_expr_with_fun_call(
					    Form, ExpList, NewFunName, ParNames, VarsToExport),
                                  Toks = get_node_toks(FileName, refac_misc:get_toks(Form), ExpList),
                                  NewFun1 = refac_misc:update_ann(NewFun, {toks, Toks}),
				  [Form1, NewFun1];
			      _ -> [Form]
			  end;
		      _ -> [Form]
		  end
	  end,
    refac_syntax:form_list([F || Form <- Forms, F <- Fun(Form)]).


replace_expr_with_fun_call(Form, ExpList, NewFunName, ParNames, VarsToExport) ->
    Op = refac_syntax:atom(NewFunName),
    Pars = [refac_syntax:variable(P) || P <- ParNames],
    FunCall = refac_syntax:copy_pos(hd(ExpList), refac_syntax:application(Op, Pars)),
    NewExpr = case length(VarsToExport) of
		0 ->
		    FunCall;
		1 ->
		    Pats = refac_syntax:variable(hd(VarsToExport)),
		    refac_syntax:match_expr(Pats, FunCall);
		_ ->
		    Pats = refac_syntax:tuple([refac_syntax:variable(V) || V <- VarsToExport]),
		    refac_syntax:match_expr(Pats, FunCall)
	      end,
    case length(ExpList) == 1 andalso refac_syntax:type(hd(ExpList)) =/= match_expr of
      true ->
	  element(1, ast_traverse_api:stop_tdTP(
		       fun do_replace_expr_with_fun_call_1/2, Form, {NewExpr, hd(ExpList)}));
      _ ->
	  element(1, ast_traverse_api:stop_tdTP(
		       fun do_replace_expr_with_fun_call_2/2, Form, {NewExpr, ExpList}))
    end.

do_replace_expr_with_fun_call_1(Tree, {NewExpr, Expr}) ->
    Range = refac_api:start_end_loc(Expr),
    case refac_api:start_end_loc(Tree) of
	Range ->
	    case refac_syntax:type(Tree) of
		binary_field ->
		    {refac_misc:rewrite(Tree, refac_syntax:binary_field(NewExpr)), true};
		_ -> {refac_misc:rewrite(Tree, NewExpr), true}
	    end;
	_ -> {Tree, false}
    end.

do_replace_expr_with_fun_call_2(Tree, {MApp, ExpList}) ->
    Range1 = refac_api:start_end_loc(hd(ExpList)),
    Range2 = refac_api:start_end_loc(lists:last(ExpList)),
    Fun = fun (Exprs) ->
		  {Exprs1, Exprs2} = lists:splitwith(
				       fun (E) ->
                                               refac_api:start_end_loc(E) =/= Range1
				       end, Exprs),
		  case Exprs2 of
		      [] -> {Exprs, false};
		      _ -> {Exprs21, Exprs22} =
			       lists:splitwith(fun (E) ->
                                                       refac_api:start_end_loc(E) =/= Range2
					       end, Exprs2),
			   case Exprs22 of
			       [] -> {Exprs, false}; %% THIS SHOULD NOT HAPPEN.
			       _ -> 
                                   Range3=get_start_end_loc_with_comment(Exprs21++[hd(Exprs22)]),
                                   {Exprs1 ++ [refac_misc:update_ann(MApp, {range, Range3})| tl(Exprs22)], true}
			   end
		  end
	  end,
    case refac_syntax:type(Tree) of
	clause ->
	    Exprs = refac_syntax:clause_body(Tree),
	    {NewBody, Modified} = Fun(Exprs),
	    Pats = refac_syntax:clause_patterns(Tree),
	    G = refac_syntax:clause_guard(Tree),
	    Tree1 = refac_misc:rewrite(Tree, refac_syntax:clause(Pats, G, NewBody)),
	    {Tree1, Modified};
	block_expr ->
	    Exprs = refac_syntax:block_expr_body(Tree),
	    {NewBody, Modified} = Fun(Exprs),
	    Tree1 = refac_misc:rewrite(Tree, refac_syntax:block_expr(NewBody)),
	    {Tree1, Modified};
	try_expr ->
	    Exprs = refac_syntax:try_expr_body(Tree),
	    {NewBody, Modified} = Fun(Exprs),
	    Cs = refac_syntax:try_expr_clauses(Tree),
	    Handlers = refac_syntax:try_expr_handlers(Tree),
	    After = refac_syntax:try_expr_after(Tree),
	    Tree1 = refac_misc:rewrite(Tree, refac_syntax:try_expr(
					       NewBody, Cs, Handlers, After)),
	    {Tree1, Modified};
	_ -> {Tree, false}
    end.
   


envs_bounds_frees(Node) ->
    As = refac_syntax:get_ann(Node),
    BdVars = case lists:keysearch(bound,1,As) of
		 {value, {bound, BdVars1}} ->
		     BdVars1;
		 _ -> []
	     end,
    FrVars = case lists:keysearch(free,1, As) of 
		 {value, {free, FrVars1}} ->
		     FrVars1;
		 _ -> []
	     end,
    {{bound, BdVars},{free, FrVars}}.

vars_to_export(_Fun,_ExpEndPos, []) ->
    [];
vars_to_export(Fun, ExprEndPos, ExprBdVars) ->
    AllVars = refac_misc:collect_var_source_def_pos_info(Fun),
    ExprBdVarsPos = [Pos || {_Var, Pos} <- ExprBdVars],
    VarsToExport = 
        lists:keysort(2, [{V, SourcePos}
                          || {V, SourcePos, DefPos} <- AllVars,
                             SourcePos > ExprEndPos,
                             ExprBdVarsPos --DefPos==[] ]),
    lists:reverse(lists:foldl(fun ({V,_Pos}, Acc) ->
				      case lists:member(V, Acc) of
					  false -> [V| Acc];
					  _ -> Acc
				      end
			      end, [], VarsToExport)).

%% The following functions should be combined with those in 'refac_expr_search.erl'
filter_exprs_via_ast(Tree, ExpList) ->
    F = fun (T, Acc) ->
		case refac_syntax:type(T) of
		    clause -> Exprs = refac_syntax:clause_body(T),
			      Acc ++ [Exprs];
		    block_expr -> Exprs = refac_syntax:block_expr_body(T),
				  Acc++ [Exprs];
		    try_expr -> Exprs = refac_syntax:try_expr_body(T),
				Acc++[Exprs];
		    _ -> Acc
		end
	end,
    AllExprSeqs = lists:flatten(ast_traverse_api:fold(F, [], Tree)),
    case ExpList--AllExprSeqs of
	[] -> [];
	_ -> ExpList
    end.

get_node_toks(FileName,Toks, Node) ->
    {Start, End} = refac_api:start_end_loc(Node),
    case Start =={0,0} orelse End=={0,0} of
	true -> [];
	false ->
	    Toks1 = lists:dropwhile(
		      fun(T) ->
			      element(2, T)<Start
		      end, Toks),
	    Toks2=lists:takewhile(fun(T)->
                                          element(2, T)=<End
                                  end, Toks1),
            get_delimitor(FileName)++Toks2
    end.


get_start_end_loc_with_comment(Node) when is_list(Node) ->
    E1 = hd(Node),
    En = lists:last(Node),
    {S, _E} = get_start_end_loc_with_comment(E1),
    {_S, E} = get_start_end_loc_with_comment(En),
    {S, E};

get_start_end_loc_with_comment(Node) ->
    {Start, End} = refac_api:start_end_loc(Node),
    PreCs = refac_syntax:get_precomments(Node),
    PostCs = refac_syntax:get_postcomments(Node),
    Start1 = case PreCs of
                 [] ->
                     Start;
                 _ ->
                     refac_syntax:get_pos(hd(PreCs))
             end,
    End1 = case PostCs of
               [] ->
                   End;
               _ ->
                   LastC = refac_syntax:comment_text(lists:last(PostCs)),
                   {L, C}=refac_syntax:get_pos(lists:last(PostCs)),
                   {L, C+length(LastC)-1}
           end,
    {Start1, End1}.



get_delimitor(FileName) ->        
    case refac_misc:file_format(FileName) of
        dos ->
            [{whitespace, {0,0}, '\r'},
             {whitespace, {0,0}, '\n'},
             {whitespace, {0,0}, '\r'},
             {whitespace, {0,0}, '\n'}];
        mac -> 
            [{whitespace, {0,0}, '\r'},
            {whitespace, {0,0}, '\r'}];
        _ -> 
            [{whitespace, {0,0}, '\n'},
             {whitespace, {0,0}, '\n'}]
    end.
