-module(eqc_swap_args).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

collect_par_locs(FileName, Dirs) ->
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Dirs, 8),
    F = fun (T, S) ->
		case wrangler_syntax:type(T) of
		    function ->
                        {_M, F, A} = api_refac:fun_define_info(T),
                        case A>1 of 
                            true-> [{FileName, {F, A}, integer_to_list(1), 
                                     integer_to_list(A), Dirs, emacs, 8}|S];
                            _ -> S
                        end;
                    _ -> S
                end
        end,
    api_ast_traverse:fold(F, [], AST).
    
%% filename newerator
gen_filename(Dirs) ->
    AllErlFiles = wrangler_misc:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).


%% Properties for 'generalise a function'
prop_swap([]) -> true;
prop_swap({FName, {F, A}, Index1, Index2, SearchPaths, Editor, TabWidth}) ->
    Args = [FName, {F, A}, Index1, Index2, SearchPaths, Editor, TabWidth],
    ?IMPLIES(true,
	     try  apply(refac_swap_function_arguments, swap_args, Args)  of
		  {ok, _Res} ->
		     wrangler_preview_server:commit(),
		     case compile:file(FName, []) of 
			 {ok, _} -> 
			     wrangler_undo_server:undo(),
			     true;
			 _ -> wrangler_undo_server:undo(), 
			      io:format("\nResulted file does not compile!\n"),
                              false
		     end;
		  {error, Msg} -> 
		     io:format("\n~p\n", [{error,Msg}]),
		     true	
	     catch 
		 throw:Error:StackTrace -> 
		     io:format("Error:\n~\pn", [Error]),
		     true;
		   E1:E2 ->
		     io:format("E1:E2:\n~p\n", [{E1, E2, StackTrace}]),
		     false
	     end).
	     
	       
gen_swap_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_swap_commands_1(FileName, Dirs)).

%% generate 'gen a function' commands.
gen_swap_commands_1(FileName, Dirs) ->
    Cs = collect_par_locs(FileName, Dirs),
    case Cs of 
        [] -> [];
        _ -> oneof(Cs)
    end.
	

show_swap_commands(Dirs)->
    eqc:quickcheck(?FORALL (C, (gen_swap_commands(Dirs)), (eqc:collect(C, true)))).
		
	  
test_swap(Dirs) ->
    application:start(wrangler),
    eqc:quickcheck(?FORALL(C, (gen_swap_commands(Dirs)), prop_swap(C))),
    application:stop(wrangler).


test_swap1() ->
    test_swap(["c:/cygwin/home/hl/test_codebase/tableau"]).

test_swap2() ->
    test_swap(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_swap3() ->
    test_swap(["c:/cygwin/home/hl/test_codebase/refactorerl"]).

test_swap4() ->
    test_swap(["c:/cygwin/home/hl/test_codebase/suite"]).

test_swap5() ->
    test_swap(["c:/cygwin/home/hl/test_codebase/wrangler"]).

test_swap6() ->
    test_swap(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_swap7() ->
    test_swap(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_swap8() ->
    test_swap(["c:/cygwin/home/hl/test_codebase/dialyzer"]).

test_swap9() ->
    test_swap(["c:/cygwin/home/hl/test_codebase/syntax_tools"]).

run_test() ->
    test_swap1(),
    test_swap2(),
%%     test_swap3(),
%%     test_swap4(),
  %%  test_swap5(),
    test_swap6(),
    %%  test_swap7(),
    %%     test_swap8(),
    test_swap9().
