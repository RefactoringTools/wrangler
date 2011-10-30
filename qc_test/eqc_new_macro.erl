-module(eqc_new_macro).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

collect_expr_locs(AST) ->
    F1 = fun (T,S) ->
		 case api_refac:is_expr(T) of
		     true -> Range = api_refac:start_end_loc(T),
			     [Range| S];
		     _ -> S
		 end
	 end,
    F = fun (T, S) ->
		case wrangler_syntax:type(T) of
		    function ->
			api_ast_traverse:fold(F1, [], T) ++ S;
		    _ -> S
		end
	end,
    Res = lists:usort(api_ast_traverse:fold(F, [], AST)),
    case Res of
	[] ->
	    [{{0,0}, {0,0}}];
	_ -> Res
    end.

%% Default function names.
madeup_macro_names() -> ["aaa", "bbb", "ccc", "DDD", "111"].

existing_macros(FileName, SearchPaths, TabWidth) ->
    Dir = filename:dirname(FileName),
    DefaultIncl1 = [".", "..", "../hrl", "../incl", "../inc", "../include"],
    DefaultIncl2 = [filename:join(Dir, X) || X <- DefaultIncl1],
    NewSearchPaths = SearchPaths++DefaultIncl2,
    case wrangler_epp:parse_file(FileName, NewSearchPaths, [], TabWidth, wrangler_misc:file_format(FileName)) of
	{ok, _, {MDefs, MUses}} ->
	    lists:usort(lists:map(fun ({{_,Name}, _Def}) -> atom_to_list(Name) end, MDefs++MUses));
	_ -> []
    end.

%% Collect atoms in an AST.
collect_vars(AST) ->
    F = fun (T, S) ->
		case wrangler_syntax:type(T) of
		    variable ->
			Name = wrangler_syntax:variable_name(T),
			[atom_to_list(Name)] ++ S;
		    _ -> S
		end
	end,
    lists:usort(api_ast_traverse:fold(F, madeup_macro_names(), AST)).

%% filename newerator
gen_filename(Dirs) ->
    AllErlFiles = wrangler_misc:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).


%% Properties for 'generalise a function'
prop_new_macro({FName, Range, NewName, SearchPaths, TabWidth}) ->
    {Start, End} = Range,
    Args = [FName,Start, End, NewName, SearchPaths, emacs,TabWidth],
    try  apply(refac_new_macro, new_macro, Args)  of
	 {ok, Res} -> 
	    wrangler_preview_server:commit(),
	    case compile:file(FName, []) of 
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
	    io:format("Error:\n~\pn", [Error]),
	    true;
	  E1:E2 ->
	    io:format("E1:E2:\n~p\n", [{E1, E2}]),
	    false
    end.
	       
gen_new_macro_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_new_macro_commands_1(FileName, Dirs)).

%% generate 'gen a macroction' commands.
gen_new_macro_commands_1(FileName, Dirs) ->
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Dirs, 8),
    Ms = existing_macros(FileName, Dirs, 8),
    noshrink({FileName, oneof(collect_expr_locs(AST)), oneof(collect_vars(AST)++Ms), Dirs, 8}).

show_new_macro_commands(Dirs)->
    application:start(wrangler),
    eqc:quickcheck(?FORALL (C, (gen_new_macro_commands(Dirs)), (eqc:collect(C, true)))),
    application:stop(wrangler).
		
	  
test_new_macro(Dirs) ->
    eqc:quickcheck(?FORALL(C, (gen_new_macro_commands(Dirs)), prop_new_macro(C))).

test_new_macro1() ->
    test_new_macro(["c:/cygwin/home/hl/test_codebase/tableau"]).

test_new_macro2() ->
    test_new_macro(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_new_macro3() ->
    test_new_macro(["c:/cygwin/home/hl/test_codebase/refactorerl"]).

test_new_macro4() ->
    test_new_macro(["c:/cygwin/home/hl/test_codebase/suite"]).

test_new_macro5() ->
    test_new_macro(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_new_macro6() ->
    test_new_macro(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_new_macro7() ->
    test_new_macro(["c:/cygwin/home/hl/test_codebase/yaws"]).

test_new_macro8() ->
    test_new_macro(["c:/cygwin/home/hl/test_codebase/dialyzer"]).

test_new_macro9() ->
    test_new_macro(["c:/cygwin/home/hl/test_codebase/syntax_tools"]).

run_test() ->
    test_new_macro1(),
    test_new_macro2(),
   %%  test_new_macro3(),
%%     test_new_macro4(),
%%     test_new_macro5(),
    test_new_macro6().
    %% test_new_macro7(),
%%     test_new_macro8().
