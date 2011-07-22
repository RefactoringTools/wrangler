-module(eqc_move_fun).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

% filename generator
gen_filename(Dirs) ->
    AllErlFiles = wrangler_misc:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).

gen_target_mod(Dirs) ->
    AllErlFiles = wrangler_misc:expand_files(Dirs, ".erl"),
    AllMods = lists:map(fun (F) -> filename:basename(F, ".erl") end, AllErlFiles),
    oneof(AllErlFiles++madeup_mod_names()).

%% Default function names.
madeup_mod_names() -> ["aaa", "aaa.erl"].

%% collect function define locations in an AST
collect_fun_locs(AST) ->
    F = fun (T, S) ->
		As = wrangler_syntax:get_ann(T),
		case lists:keysearch(fun_def, 1, As) of
		    {value, {fun_def, {_Mod, _Fun, _Arity, Pos, DefPos}}} ->
			if DefPos == {0, 0} -> S;
			   true -> [Pos] ++ S
			end;
		    _ -> S
		end
	end,
    Res = lists:usort(api_ast_traverse:fold(F, [], AST)),
    case Res of
	[] ->
	    [{0,0}];
	_ -> Res
    end.

is_mod_name(A) ->
    api_refac:is_fun_name(A).

default_incls() ->
  [".", "..", "../hrl", "../incl", "../inc", "../include",
   "../../hrl", "../../incl", "../../inc", "../../include",
   "../../../hrl", "../../../incl", "../../../inc", "../../../include"].


get_target_file_name(CurrentFName, TargetModorFileName) ->
    ErrMsg = {error, "Illegal target module/file name."},
    TargetModName = case filename:extension(TargetModorFileName) of 
			".erl" -> filename:basename(TargetModorFileName, ".erl");
			[] -> TargetModorFileName;
			_ -> "IllegalModName"			
		    end,
    case is_mod_name(TargetModName) of 
	true -> filename:join([filename:dirname(CurrentFName), filename:dirname(TargetModorFileName), TargetModName++".erl"]);	    
	_  -> ErrMsg
    end.

%% Properties for 'move a function'
prop_move_fun({FName, Loc, TargetMod,SearchPaths, TabWidth}) ->
    {Line, Col} = Loc,
    ?IMPLIES((Loc =/= {0, 0}),
	     begin
		 Args = [FName, Line, Col, TargetMod,SearchPaths, TabWidth],
		 try  apply(refac_move_fun, move_fun, Args)  of 
		      {ok, Res} ->
			 wrangler_preview_server:commit(),
			 Dir = filename:dirname(FName),
			 DefaultIncl2 = [filename:join(Dir, X) || X <-default_incls()],
			 Includes = SearchPaths++DefaultIncl2,
                         TargetFile = get_target_file_name(FName, TargetMod), 
			 case compile:file(FName, Includes) of 
			     {ok, _} ->
				 case compile:file(TargetFile, Includes) of 
				     {ok, _} ->
					 wrangler_undo_server:undo(),
					 io:format("\n~p\n", [{ok, Res}]),
					 true;
				     _ ->
					 wrangler_undo_server:undo(), 
					 io:format("\nResulted file:~p does not compile!\n", [TargetMod]),
					 false
				 end;
			     _ ->wrangler_undo_server:undo(), 
				 io:format("\nResulted file: ~p  does not compile!\n", [FName]),
				 false
			 end;
		      {question, Msg} ->
			 io:format("\n~p\n", [{question,Msg}]),
			 true;
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
		 end
	     end).

gen_move_fun_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_move_fun_commands_1(FileName, Dirs)).

gen_move_fun_commands_1(FileName, Dirs) ->
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Dirs, 8),
    noshrink({FileName, oneof(collect_fun_locs(AST)), gen_target_mod(Dirs), Dirs, 8}).



test_move_fun(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(numtests(500,?FORALL(C, (gen_move_fun_commands(Dirs)), prop_move_fun(C)))),
    application:stop(wrangler_app).

test_move_fun1() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/lampera"]).


test_move_fun2() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_move_fun3() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/refactorerl"]).

test_move_fun4() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/suite"]).

test_move_fun5() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/wrangler"]).

test_move_fun6() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_move_fun7() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/yaws"]).

test_move_fun8() ->
    test_move_fun(["c:/cygwin/home/hl/test_codebase/dialyzer"]).

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
