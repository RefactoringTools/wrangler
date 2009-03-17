-module(eqc_move_fun).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

% filename generator
gen_filename(Dirs) ->
    AllErlFiles = refac_util:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).

gen_target_mod(Dirs) ->
    AllErlFiles = refac_util:expand_files(Dirs, ".erl"),
    AllMods = lists:map(fun(F) ->filename:basename(F, ".erl") end, AllErlFiles),
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
    Res =lists:usort(refac_syntax_lib:fold(F, [], AST)),
    case Res of 
	[] ->
	     [{0,0}];
	_  -> Res
    end.

is_mod_name(A) ->
    refac_util:is_fun_name(A).


get_target_file_name(CurrentFName, TargetModorFileName) ->
    ErrMsg = {error, "Illegal target module/file name."},
    TargetModName = case filename:extension(TargetModorFileName) of 
			".erl" -> filename:basename(TargetModorFileName, ".erl");
			[] -> TargetModorFileName;
			_ -> "IllegalModName"			
		    end,
    case is_mod_name(TargetModName) of 
	true -> filename:join([filename:dirname(CurrentFName), TargetModName++".erl"]);	    
	_  -> ErrMsg
    end.

%% Properties for 'move a function'
prop_move_fun({FName, Loc, TargetMod, CreateNewFile, SearchPaths, TabWidth}) ->
    {Line, Col} = Loc,
    ?IMPLIES((Loc =/= {0, 0}),
	     begin
		 Args = [FName, Line, Col, TargetMod, CreateNewFile, SearchPaths, TabWidth],
		 TargetFile = get_target_file_name(FName,  TargetMod),
		 try  apply(refac_move_fun, move_fun, Args)  of 
		      {ok, Res} ->
			 case refac_util:parse_annotate_file(FName, false, SearchPaths) of 
			     {ok, _} ->
				 case refac_util:parse_annotate_file(TargetFile, false, SearchPaths) of 
				     {ok, _} ->
					 wrangler_undo_server:undo(),
					 io:format("\n~p\n", [{ok, Res}]),
					 true;
				     _ ->
					 wrangler_undo_server:undo(), 
					 io:format("\nResulted file:~p does not compole!\n", [TargetFile]),
					 false
				 end;
			     _ ->wrangler_undo_server:undo(), 
				 io:format("\nResulted file: ~p  does not compole!\n", [FName]),
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

gen_move_fun_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_move_fun_commands_1(FileName, Dirs)).

gen_move_fun_commands_1(FileName, Dirs) ->
    {ok, {AST, _Info}} = refac_util:parse_annotate_file(FileName, true, Dirs, 8),
    noshrink({FileName, oneof(collect_fun_locs(AST)), gen_target_mod(Dirs), bool(), Dirs, 8}).



test_move_fun(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(?FORALL(C, (gen_move_fun_commands(Dirs)), prop_move_fun(C))),
    application:stop(wrangler_app).

test_move_fun1() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/tableau"]).


test_move_fun2() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_move_fun3() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test_move_fun4() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/suite"]).

test_move_fun5() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_move_fun6() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_move_fun7() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_move_fun8() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).

test_move_fun() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase"]).

run_test() ->
    test_move_fun1(),
    test_move_fun2(),
    test_move_fun3(),
    test_move_fun4(),
    test_move_fun5(),
    test_move_fun6(),
    test_move_fun7(),
    test_move_fun8().
