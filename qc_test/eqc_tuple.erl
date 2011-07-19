-module(eqc_tuple).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

collect_par_locs(FileName, Dirs) ->
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Dirs, 8),
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		    function ->
			Cs = refac_syntax:function_clauses(T),
			Patterns = refac_syntax:clause_patterns(hd(Cs)),
			case Patterns of
			    [] -> S;
			    _ ->
				IndexedPats = lists:zip(Patterns, lists:reverse(lists:seq(1, length(Patterns)))),
				Pars = lists:map(fun ({P, Index}) -> {Start, End} = refac_api:start_end_loc(P),
								     {{Start, End}, length(Patterns)-Index+1, Index}
						 end, IndexedPats),
				[P || P = {{_S1, _E1}, _Index1, Index2} <- Pars, Index2>1]++S
			end;
		    _ -> S
		end
	end,
    Res = lists:usort(ast_traverse_api:fold(F, [], AST)),
    case Res of
	[] ->
	    [{{{0,0},{0,0}},0,0}];
	_ -> Res
    end.

%% filename newerator
gen_filename(Dirs) ->
    AllErlFiles = refac_misc:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).


%% Properties for 'generalise a function'
prop_tuple({FName, Pos, Index, Number, SearchPaths, TabWidth}) ->
    {Line, Col} = Pos,
    Args = [FName, Line, Col,  Index, Number, SearchPaths, TabWidth],
    ?IMPLIES((Line=/=0),
	     try  apply(refac_tuple, tuple_funpar, Args)  of
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
		 throw:Error -> 
		     io:format("Error:\n~\pn", [Error]),
		     true;
		   E1:E2 ->
		     io:format("E1:E2:\n~p\n", [{E1, E2}]),
		     false
	     end).
	     
	       
gen_tuple_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_tuple_commands_1(FileName, Dirs)).

%% generate 'gen a function' commands.
gen_tuple_commands_1(FileName, Dirs) ->
   noshrink( ?LET(Par, oneof(collect_par_locs(FileName, Dirs)),
		  begin
		      {{{StartLine, StartCol}, {_EndLine, _EndCol}}, Index0, Index} = Par, 
		      {FileName, {StartLine, StartCol}, Index0, choose(2, if Index=<1 -> 2; true ->Index end), Dirs, 8}
		  end)).

show_tuple_commands(Dirs)->
    eqc:quickcheck(?FORALL (C, (gen_tuple_commands(Dirs)), (eqc:collect(C, true)))).
		
	  
test_tuple(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(?FORALL(C, (gen_tuple_commands(Dirs)), prop_tuple(C))),
    application:stop(wrangler_app).


test_tuple1() ->
    test_tuple(["c:/cygwin/home/hl/test_codebase/tableau"]).

test_tuple2() ->
    test_tuple(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_tuple3() ->
    test_tuple(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test_tuple4() ->
    test_tuple(["c:/cygwin/home/hl/test_codebase/suite"]).

test_tuple5() ->
    test_tuple(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_tuple6() ->
    test_tuple(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_tuple7() ->
    test_tuple(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_tuple8() ->
    test_tuple(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).

run_test() ->
    test_tuple1(),
    test_tuple2(),
%%     test_tuple3(),
%%     test_tuple4(),
  %%  test_tuple5(),
    test_tuple6().
   %%  test_tuple7(),
%%     test_tuple8().
