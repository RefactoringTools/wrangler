-module(eqc_partition_export).

-export([test_partition_exports/1, test_partition_exports1/0]).

-compile(export_all).

exported_funs(File) ->
    {ok, {_, Info}} = wrangler_ast_server:parse_annotate_file(File, true),
    ModName = get_module_name(File, Info),
    ImpExports =
	case lists:keysearch(attributes, 1, Info) of
	    {value, {attributes, Attrs}} ->
		case lists:member({compile, export_all}, Attrs) of
		    true ->
			case lists:keysearch(functions, 1, Info) of
			    {value, {functions, Funs}} ->
				[{ModName, F, A} || {F, A} <- Funs];
			    false ->
				[]
			end;
		    false ->
			[]
		end;
	    false -> []
	end,
    case ImpExports of
	[] ->
	    case lists:keysearch(exports, 1, Info) of
		{value, {exports, ExpFuns}} ->
		    [{ModName, F, A} || {F, A} <- ExpFuns];
		false ->
		    []
	    end;
	_ -> ImpExports
    end.

get_module_name(FName, Info) ->
    case lists:keysearch(module, 1, Info) of
	{value, {module, Mod}} -> Mod;
	_ -> list_to_atom(filename:basename(FName, ".erl"))
    end.

prop_partition_exports({FName, Score, Dirs}) ->
    refac_io:format("FName:\n~p\n", [FName]),
    ExpFuns1 = exported_funs(FName),
    Res =try apply(wrangler_modularity_inspection, partition_exports_eclipse, [FName, Score, Dirs, 8])
         catch
             throw:Error ->
                 io:format("Error:\n~\pn", [Error]),
                 
                 error;
             E1:E2 ->
            io:format("E1:E2:\n~p\n", [{E1, E2}]),
                 false
         end,
    case Res of
        false -> false;
        error -> true;
        _ ->
            wrangler_preview_server:commit(),
            ExpFuns2 = exported_funs(FName),
            Res1 =lists:sort(ExpFuns1)==lists:sort(ExpFuns2),
            case Res1 of 
                false ->
                    refac_io:format("Check........\n");
                true -> 
                    ok
            end,
            wrangler_undo_server:undo()
    end.
 
			
test_partition_exports(Dirs) ->
    AllErlFiles = refac_misc:expand_files(Dirs, ".erl"),
    lists:foreach(fun(F) ->prop_partition_exports({F,0.8, Dirs}) end, AllErlFiles).
   
   
test_partition_exports1() ->
    test_partition_exports(["c:/cygwin/home/hl/test_codebase/lampera"]).

test_partition_exports2() ->
    test_partition_exports(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_partition_exports3() ->
    test_partition_exports(["c:/cygwin/home/hl/test_codebase/refactorerl"]).

test_partition_exports4() ->
    test_partition_exports(["c:/cygwin/home/hl/test_codebase/suites"]).

test_partition_exports5() ->
    test_partition_exports(["c:/cygwin/home/hl/test_codebase/wrangler"]).

test_partition_exports6() ->
    test_partition_exports(["c:/cygwin/home/hl/test_codebase/stdlib"]).

test_partition_exports7() ->
    test_partition_exports(["c:/cygwin/home/hl/test_codebase/yxa"]).

test_partition_exports8() ->
    test_partition_exports(["c:/cygwin/home/hl/test_codebase/dialyzer"]).

run_test_partition_exports() ->
    test_partition_exports1(),
    test_partition_exports2(),
    test_partition_exports3(),
    test_partition_exports4(),
    test_partition_exports5(),
    test_partition_exports6(),
    test_partition_exports7(),
    test_partition_exports8().
    
