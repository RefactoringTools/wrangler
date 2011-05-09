-module(eqc_fun_to_process).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

% filename generator
gen_filename(Dirs) ->
    AllErlFiles = refac_misc:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).

gen_target_mod(Dirs) ->
    AllErlFiles = refac_misc:expand_files(Dirs, ".erl"),
    AllMods = lists:map(fun (F) -> filename:basename(F, ".erl") end, AllErlFiles),
    oneof(AllMods++madeup_mod_names()).

%% Default function names.
madeup_mod_names() -> ["aaa", "aaa.erl"].

%% collect function define locations in an AST
collect_fun_locs(AST) ->
    F = fun (T, S) ->
		As = refac_syntax:get_ann(T),
		case lists:keysearch(fun_def, 1, As) of
		    {value, {fun_def, {_Mod, _Fun, _Arity, Pos, DefPos}}} ->
			if DefPos == {0, 0} -> S;
			   true -> [Pos] ++ S
			end;
		    _ -> S
		end
	end,
    Res = lists:usort(ast_traverse_api:fold(F, [], AST)),
    case Res of
	[] ->
	    [{0,0}];
	_ -> Res
    end.

%% Default function names.
madeup_process_names() -> ["aaa", "bbb", "ccc", "DDD"].

prop_fun_to_process({FName, Loc, ProcessName, SearchPaths, TabWidth}) ->
    {Line, Col} = Loc,
    ?IMPLIES((Loc =/= {0, 0}),
	     begin
		 Args = [FName, Line, Col, ProcessName,SearchPaths, TabWidth],
 		 try  apply(refac_fun_to_process, fun_to_process, Args)  of 
		      {ok, Res} ->
			 wrangler_preview_server:commit(),
			 case compile:file(FName, []) of 
			     {ok, _} ->
				 wrangler_undo_server:undo(),
				 io:format("\n~p\n", [{ok, Res}]),
				 true;
			     _ ->
				 wrangler_undo_server:undo(), 
				 io:format("\nResulted file:~p does not compole!\n", [FName]),
				 false
			 end;
		      {error, Msg} -> 
			 io:format("\n~p\n", [{error,Msg}]),
			 true;
		      {undecidables, _} ->
			  try apply(refac_fun_to_process, fun_to_process_1, Args) of 
			      {ok, Res} ->
				  wrangler_preview_server:commit(),
				  case compile:file(FName, []) of 
				      {ok, _} ->
					  wrangler_undo_server:undo(),
					  io:format("\n~p\n", [{ok, Res}]),
					  true;
				      _ ->
					  wrangler_undo_server:undo(), 
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
		 catch 
		     throw:Error -> 
			 io:format("Error:\n~\pn", [Error]),
			 error;
		       E1:E2 ->
			 io:format("E1:E2:\n~p\n", [{E1, E2}]),
			 false
		 end
	     end).

gen_fun_to_process_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_fun_to_process_commands_1(FileName, Dirs)).

gen_fun_to_process_commands_1(FileName, Dirs) ->
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Dirs, 8),
    noshrink({FileName, oneof(collect_fun_locs(AST)), oneof(madeup_process_names()), Dirs, 8}).



test_fun_to_process(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(?FORALL(C, (gen_fun_to_process_commands(Dirs)), prop_fun_to_process(C))),
    application:stop(wrangler_app).

test_fun_to_process1() ->
    test_fun_to_process(["c:/cygwin/home/hl/test_codebase/tableau"]).

test_fun_to_process2() ->
    test_fun_to_process(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_fun_to_process3() ->
    test_fun_to_process(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test_fun_to_process4() ->
    test_fun_to_process(["c:/cygwin/home/hl/test_codebase/suite"]).

test_fun_to_process5() ->
    test_fun_to_process(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_fun_to_process6() ->
    test_fun_to_process(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_fun_to_process7() ->
    test_fun_to_process(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_fun_to_process8() ->
    test_fun_to_process(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).

run_test() ->
    test_fun_to_process1(),
    test_fun_to_process2(),
    test_fun_to_process3(),
    test_fun_to_process4(),
    test_fun_to_process5(),
    test_fun_to_process6(),
    test_fun_to_process7(),
    test_fun_to_process8().
    

