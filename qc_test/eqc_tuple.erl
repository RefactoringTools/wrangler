-module(eqc_tuple).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

collect_par_locs(FileName, Dirs) ->
    {ok, {AST, _Info}} = refac_util:parse_annotate_file(FileName, true, Dirs, 8),
    F = fun (T, S) ->
		case refac_syntax:type(T) of 
		    function ->
			Cs = refac_syntax:function_clauses(T),
			Patterns = refac_syntax:clause_patterns(hd(Cs)),
			case Patterns of 
			    [] -> S;
			    _ ->
				IndexedPats = lists:zip(Patterns, lists:reverse(lists:seq(1, length(Patterns)))),
				Pars =lists:map(fun({P, Index}) -> {Start, End} = refac_util:get_range(P),
								   {{Start, End}, Index}
						end, IndexedPats),
				Pars++S
			end;
		    _ -> S
		end
	end,
    Res = lists:usort(refac_syntax_lib:fold(F, [], AST)),
    case Res of 
	[] ->
	     [{{{0,0},{0,0}},0}];
	_ -> Res
    end.

%% filename newerator
gen_filename(Dirs) ->
    AllErlFiles = refac_util:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).


%% Properties for 'generalise a function'
prop_tuple({FName, Pos, Number, SearchPaths, TabWidth}) ->
    {Line, Col} = Pos,
    Args = [FName, Line, Col,  Number, SearchPaths, TabWidth],
    ?IMPLIES((Line=/=0),
	     try  apply(refac_tuple, tuple_funpar, Args)  of
		  {ok, Res} ->
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
	     {{{StartLine, StartCol}, {EndLine, EndCol}}, Index} = Par, 
	     ?LET(L, ?SUCHTHAT(LOC, {choose(StartLine, EndLine), choose(StartCol, EndCol)}, 
			       LOC>={StartLine, StartCol}andalso LOC=<{EndLine, EndCol}),
		  {FileName, L, choose(1, if Index==0-> 1; true ->Index end), Dirs, 8})
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
