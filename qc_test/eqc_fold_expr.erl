-module(eqc_fold_expr).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

% filename generator
gen_filename(Dirs) ->
    AllErlFiles = refac_misc:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).

%% collect function define locations in an AST
collect_fold_candidates(FName, SearchPaths, TabWidth) ->
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		    function ->
			FunName = refac_syntax:data(refac_syntax:function_name(T)),
			Arity1 = refac_syntax:function_arity(T),
			Cs = refac_syntax:function_clauses(T),
			Rs = lists:map(fun (C) -> refac_api:start_end_loc(C) end, Cs),
			Res = lists:flatmap(fun (R) ->
						    {{StartLine, StartCol}, _} = R,
						    Args = [FName, StartLine, StartCol, SearchPaths, TabWidth, emacs],
						    try apply(refac_fold_expression, fold_expression, Args) of
							{ok, Regions} ->
							    Regions1 = lists:map(fun (Reg) ->
											 list_to_tuple([FunName, Arity1| tuple_to_list(Reg)])
										 end,
										 Regions),
							    Regions1;
							_ -> []
						    catch
							E1:E2 -> []
						    end
					    end, Rs),
			Res ++ S;
		    _ -> S
		end
	end,
    Res = lists:usort(ast_traverse_api:fold(F, [], AST)),
    case Res of
	[] ->
	    [{0,0,0,0,0,0,0,{0,0,0,0}}];
	_ -> Res
    end.

prop_fold_expr({FunName, Arity, StartLine, StartCol, EndLine, EndCol, NewExp, ClauseInfo={FName, _Mod, _Def, Index}}, SearchPaths) ->
    ?IMPLIES(StartLine=/=0, 
	     begin
		 Args1 = [FName, StartLine, StartCol, EndLine, EndCol, NewExp, ClauseInfo, SearchPaths, 8],
		 io:format("\nCMDInfo: fold_expression_1:~p\n", [{FName, FunName, Arity, Index}]),
		 try apply(refac_fold_expression, fold_expression_1, Args1) of 
		     {ok, _Res} -> 
			 wrangler_preview_server:commit(),
			 case compile:file(FName, [{i, "c:/cygwin/home/hl/test_codebase"}]) of 
			     {ok, _} ->  wrangler_undo_server:undo(),
					 io:format("\nOk, refactoring succeeded.\n"),
					 true;
			     _ ->wrangler_undo_server:undo(), 
				 io:format("\nResulted file:~p does not compole!\n", [FName]),
				 false
			 end;
		     {error, Msg} -> 
			 io:format("\n~p\n", [{error,Msg}]),
			 true
		 catch
		     throw:Error -> 
			 io:format("Error:\n~\pn", [Error]),
			 error;
		       E1:E2 ->
			 io:format("E1:E2:\n~p\n", [{E1, E2}]),
			 false
		 end
	     end).

 
	     
    

gen_fold_expr_commands(Dirs) ->
    noshrink(?LET(FileName, (gen_filename(Dirs)),
		  oneof(collect_fold_candidates(FileName, Dirs, 8)))).

test_fold_expr(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(numtests(500,?FORALL(C, (gen_fold_expr_commands(Dirs)), prop_fold_expr(C, Dirs)))),
    application:stop(wrangler_app).


test_fold_expr1() ->
    test_fold_expr(["c:/cygwin/home/hl/test_codebase/tableau"]).


test_fold_expr2() ->
    test_fold_expr(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_fold_expr3() ->
    test_fold_expr(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test_fold_expr4() ->
    test_fold_expr(["c:/cygwin/home/hl/test_codebase/suite"]).

test_fold_expr5() ->
    test_fold_expr(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_fold_expr6() ->
    test_fold_expr(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_fold_expr7() ->
    test_fold_expr(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_fold_expr8() ->
    test_fold_expr(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).

test_fold_expr() ->
    test_fold_expr(["c:/cygwin/home/hl/test_codebase"]).


run_test() ->
    test_fold_expr1(),
    test_fold_expr2(),
    %% test_fold_expr3(),
%%     test_fold_expr4(),
%%     test_fold_expr5(),
    test_fold_expr6().
  %%   test_fold_expr7(),
%%     test_fold_expr8().
