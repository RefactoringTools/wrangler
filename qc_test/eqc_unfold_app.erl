-module(eqc_unfold_app).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

collect_app_locs(AST, ModName) ->
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		    application ->
			Op = refac_syntax:application_operator(T),
			case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of
			    {value, {fun_def, {ModName, _, _, _, _}}} ->
				{SLoc, _ELoc} = refac_api:start_end_loc(Op),
				[SLoc| S];
			    _ -> S
			end;
		    _ -> S
		end
	end,
    ast_traverse_api:fold(F, [{0,0}], AST).

%% filename newerator
gen_filename(Dirs) ->
    AllErlFiles = refac_misc:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).


%% Properties for 'generalise a function'
prop_new_fun({FName, Loc, SearchPaths, TabWidth}) ->
    Args = [FName,Loc, SearchPaths,  TabWidth],
    try  apply(refac_unfold_fun_app, unfold_fun_app, Args)  of
	 {ok, Res} -> 
	    wrangler_preview_server:commit(),
	    case compile:file(FName,[]) of 
		{ok, _} -> 
		    wrangler_undo_server:undo(),
		    io:format("\n~p\n", [{ok, Res}]),
		    true;
		_ -> wrangler_undo_server:undo(), 
		     io:format("\nResulted file does not compile!\n"),
			       false
	    end;
	 {error, Msg} -> 
	    io:format("\n~p\n", [{error,Msg}]),
	    true	
    catch 
	throw:Error -> 
	    io:format("Error:\n~p\n", [Error]),
	    true;
	  E1:E2 ->
	    io:format("E1:E2:\n~p\n", [{E1, E2}]),
	    false
    end.
	       
gen_new_fun_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_unfold_fun_commands_1(FileName, Dirs)).

%% generate 'gen a function' commands.
gen_unfold_fun_commands_1(FileName, Dirs) ->
    {ok, {AST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Dirs, 8),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    noshrink({FileName, oneof(collect_app_locs(AST, ModName)), Dirs, 8}).

test_unfold_fun_app(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(numtests(500, ?FORALL(C, gen_new_fun_commands(Dirs), prop_new_fun(C)))),
    application:start(wrangler_app).

test1() ->
    test_unfold_fun_app(["c:/cygwin/home/hl/test_codebase/tableau"]).

test2() ->
    test_unfold_fun_app(["c:/cygwin/home/hl/test_codebase/eunit"]).

test3() ->
    test_unfold_fun_app(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test4() ->
    test_unfold_fun_app(["c:/cygwin/home/hl/test_codebase/suite"]).

test5() ->
    test_unfold_fun_app(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test6() ->
    test_unfold_fun_app(["c:/cygwin/home/hl/test_codebase/umbria"]).

test7() ->
    test_unfold_fun_app(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test8() ->
    test_unfold_fun_app(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).


run_test() ->
    test1(),
    test2(),
    test3(),
    test4(),
    test5(),
    test6(),
    test7(),
    test8().
