-module(eqc_new_fun).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

collect_expr_locs(AST) ->
    F1 = fun(T,S) ->
		 case refac_util:is_expr(T) of 
		     true ->Range = refac_util:get_range(T),
			    [Range|S];
		     _ -> S
		 end
	 end,
    F = fun (T, S) ->
		case refac_syntax:type(T) of 
		    function ->
			refac_syntax_lib:fold(F1, [],T) ++ S;
		    _ -> S
		end
	end,
    Res = lists:usort(refac_syntax_lib:fold(F, [], AST)),
    case Res of 
	[] ->
	     [{{0,0}, {0,0}}];
	_ -> Res
    end.

%% Default function names.
madeup_fun_names() -> ["aaa", "bbb", "ccc", "DDD"].

%% Collect atoms in an AST.
collect_atoms(AST) ->
     F = fun (T, S) ->
		case refac_syntax:type(T) of
		  atom ->
		      Name = refac_syntax:atom_value(T),
		      [atom_to_list(Name)] ++ S;
		  _ -> S
		end
	end,
    lists:usort(refac_syntax_lib:fold(F, madeup_fun_names(), AST)).

%% filename newerator
gen_filename(Dirs) ->
    AllErlFiles = refac_util:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).


%% Properties for 'generalise a function'
prop_new_fun({FName, Range, NewName, SearchPaths, TabWidth}) ->
    {Start, End} = Range,
    Args = [FName,Start, End, NewName, TabWidth],
    try  apply(refac_new_fun, fun_extraction, Args)  of
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
	 gen_new_fun_commands_1(FileName, Dirs)).

%% generate 'gen a function' commands.
gen_new_fun_commands_1(FileName, Dirs) ->
    {ok, {AST, _Info}} = refac_util:parse_annotate_file(FileName, true, Dirs, 8),
    noshrink( {FileName, oneof(collect_expr_locs(AST)), oneof(collect_atoms(AST)), Dirs, 8}).

show_new_fun_commands(Dirs)->
    eqc:quickcheck(?FORALL (C, (gen_new_fun_commands(Dirs)), (eqc:collect(C, true)))).
		
	  
test_new_fun(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(numtests(500, ?FORALL(C, (gen_new_fun_commands(Dirs)), prop_new_fun(C)))),
    application:start(wrangler_app).
	
    

test_new_fun1() ->
    test_new_fun(["c:/cygwin/home/hl/test_codebase/tableau"]).

test_new_fun2() ->
    test_new_fun(["c:/cygwin/home/hl/test_codebase/inets-5.0.12"]).

test_new_fun3() ->
    test_new_fun(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test_new_fun4() ->
    test_new_fun(["c:/cygwin/home/hl/test_codebase/suite"]).

test_new_fun5() ->
    test_new_fun(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_new_fun6() ->
    test_new_fun(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_new_fun7() ->
    test_new_fun(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_new_fun8() ->
    test_new_fun(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).


run_test() ->
    test_new_fun1(),
    test_new_fun2(),
   %%  test_new_fun3(),
%%     test_new_fun4(),
%%     test_new_fun5(),
    test_new_fun6().
   %%  test_new_fun7(),
%%     test_new_fun8().
