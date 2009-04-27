-module(eqc_fold_macro).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

% filename generator
gen_filename(Dirs) ->
    AllErlFiles = refac_util:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).

%% collect function define locations in an AST
collect_fold_candidates(FName, SearchPaths, TabWidth) ->
    {ok, {AST, _Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    F = fun (T, S) ->
		case refac_syntax:type(T) of 
		    attribute ->
			case refac_syntax:atom_value(refac_syntax:attribute_name(T)) of 
			    define ->
				{{Line, Col}, _EndLoc} = refac_util:get_range(T),
				Args = [FName,Line, Col, SearchPaths, TabWidth, emacs],
				case apply(refac_fold_against_macro, fold_against_macro, Args) of 
				    {ok, Regions} ->
					Regions1 = lists:map(fun(Reg) ->
								     list_to_tuple([FName |tuple_to_list(Reg)]) end,
							     Regions),
					Regions1 ++ S;
				    _ -> S
				end;
			    _ -> S
			end;
		    _ -> S
		end
	end,
    Res = lists:usort(refac_syntax_lib:fold(F, [], AST)),
    case Res of 
	[] ->
	    [{0,0,0,0,0,0,0}];
	_ -> Res
    end.

prop_fold_macro({FName, StartLine, StartCol, EndLine, EndCol, MacroApp, MacroDef}, SearchPaths) ->
    ?IMPLIES(StartLine=/=0, 
	     begin
		 Args1 = [FName, StartLine, StartCol, EndLine, EndCol, MacroApp, MacroDef, SearchPaths, 8],
		 try apply(refac_fold_against_macro, fold_against_macro_1, Args1) of 
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
	     
    

gen_fold_macro_commands(Dirs) ->
    noshrink(?LET(FileName, (gen_filename(Dirs)),
		  oneof(collect_fold_candidates(FileName, Dirs, 8)))).

test_fold_macro(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(?FORALL(C, (gen_fold_macro_commands(Dirs)), prop_fold_macro(C, Dirs))),
    application:stop(wrangler_app).

test_fold_macro1() ->
    test_fold_macro(["c:/cygwin/home/hl/test_codebase/tableau"]).


test_fold_macro2() ->
    test_fold_macro(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_fold_macro3() ->
    test_fold_macro(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test_fold_macro4() ->
    test_fold_macro(["c:/cygwin/home/hl/test_codebase/suite"]).

test_fold_macro5() ->
    test_fold_macro(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_fold_macro6() ->
    test_fold_macro(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_fold_macro7() ->
    test_fold_macro(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_fold_macro8() ->
    test_fold_macro(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).

run_test() ->
    test_fold_macro1(),
    test_fold_macro2(),
    test_fold_macro3(),
    test_fold_macro4(),
    test_fold_macro5(),
    test_fold_macro6(),
    test_fold_macro7(),
    test_fold_macro8().
