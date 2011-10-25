-module(eqc_rename_mod).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

%% Default function names.
madeup_mod_names() -> ["aaa", "aaa_SUITE","aaa.erl","bbb", "ccc", "DDD", "111", "aa-bb", "aa_bb", "a111b_tests", "11ab_tests"].

%% filename generator
gen_filename(Dirs) ->
    AllErlFiles = wrangler_misc:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).

%% collect function define locations in an AST
collect_mod_names(Dirs) ->
    AllErlFiles = wrangler_misc:expand_files(Dirs, ".erl"),
    lists:map(fun (F) -> filename:basename(F, ".erl") end, AllErlFiles).
   
%% Properties for 'rename a function name'
prop_rename_mod({FName, NewName, SearchPaths, TabWidth}) ->
    Args = [FName, NewName, SearchPaths, emacs,  TabWidth],
    try  apply(refac_rename_mod, rename_mod, Args) of
	 {ok, ChangedFiles} -> 
	    wrangler_preview_server:commit(),
	    NewFileName = filename:dirname(FName)++"/"++NewName++".erl",
	    ChangedFiles1= [NewFileName] ++ ChangedFiles -- [FName],
	    Res =lists:all(fun(F) -> case compile:file(F, [{i, "c:/cygwin/home/hl/test_codebase"}]) of 
					 {ok, _} ->
					     true;
				 	 _ -> io:format("\nResulted file does not compile:~p\n", [F]),
					      false
				     end
				  end, ChangedFiles1),
	    wrangler_undo_server:undo(),
	    io:format("\n~p\n", [{ok, ChangedFiles}]),
	    Res;
	 {ok, ChangedFiles,_, _} -> 
	    wrangler_preview_server:commit(),
	    NewFileName = filename:dirname(FName)++"/"++NewName++".erl",
	    ChangedFiles1= [NewFileName] ++ ChangedFiles -- [FName],
	    Res =lists:all(fun(F) -> case compile:file(F, [{i, "c:/cygwin/home/hl/test_codebase"}]) of 
					 {ok, _} ->
					     true;
				 	 _ -> io:format("\nResulted file does not compile:~p\n", [F]),
					      false
				     end
				  end, ChangedFiles1),
	    wrangler_undo_server:undo(),
	    io:format("\n~p\n", [{ok, ChangedFiles}]),
	    Res;
	 {warning, _Msg} ->
	     try apply(refac_rename_mod, rename_mod_1, [FName, NewName, SearchPaths, TabWidth, false]) of
		 {ok, ChangedFiles} ->
		     wrangler_preview_server:commit(),
		     NewFileName = filename:dirname(FName)++"/"++NewName++".erl",
		     ChangedFiles1= [NewFileName] ++ ChangedFiles -- [FName],
		     Res =lists:all(fun(F) -> case compile:file(F, [{i, "c:/cygwin/home/hl/test_codebase"}]) of 
						  {ok, _} ->
						      true;
						  _ -> io:format("\nResulted file does not compole!\n"),
						       false
					      end
				    end, ChangedFiles1),
		     wrangler_undo_server:undo(),
		     io:format("\n~p\n", [{ok, ChangedFiles}]),
		     Res;
		 {ok, ChangedFiles,_, _} -> 
		     wrangler_preview_server:commit(),
		     NewFileName = filename:dirname(FName)++"/"++NewName++".erl",
		     ChangedFiles1= [NewFileName] ++ ChangedFiles -- [FName],
		     Res =lists:all(fun(F) -> case compile:file(F, [{i, "c:/cygwin/home/hl/test_codebase"}]) of 
						  {ok, _} ->
						      true;
						  _ -> io:format("\nResulted file does not compile:~p\n", [F]),
						       false
					      end
				    end, ChangedFiles1),
		     wrangler_undo_server:undo(),
		     io:format("\n~p\n", [{ok, ChangedFiles}]),
		     Res;
		 {error, Reason} ->
		     io:format("\n~p\n", [{error, Reason}]),
		     true    
	     catch
		 throw:Error -> 
		     io:format("Error:\n~\pn", [Error]),
		     true;
		   E1:E2 ->
		     io:format("E1:E2:\n~p\n", [{E1, E2}]),
		     false
	     end;
	 {question, _Msg} ->
	    try apply(refac_rename_mod, rename_mod_1, [FName, NewName, SearchPaths, TabWidth, true]) of
		{ok, ChangedFiles} ->
		    wrangler_preview_server:commit(),
		    NewFileName = filename:dirname(FName)++"/"++NewName++".erl",
		    TestFileName = lists:sublist(FName, 1, length(FName)-4)++"_tests.erl",
		    NewTestFileName = filename:dirname(FName)++"/"++NewName++"_tests.erl",		    
		    ChangedFiles1= [NewFileName, NewTestFileName] ++ ChangedFiles -- [FName, TestFileName],
		    Res =lists:all(fun(F) -> case compile:file(F, [{i, "c:/cygwin/home/hl/test_codebase"}]) of 
						 {ok, _} ->
						     true;
						 _ -> io:format("\nResulted file does not compole!\n"),
						      false
					      end
				   end, ChangedFiles1),
		    wrangler_undo_server:undo(),
		    io:format("\n~p\n", [{ok, ChangedFiles}]),
		    Res;
		{ok, ChangedFiles,_, _} -> 
		    wrangler_preview_server:commit(),
		    NewFileName = filename:dirname(FName)++"/"++NewName++".erl",
		    ChangedFiles1= [NewFileName] ++ ChangedFiles -- [FName],
		    Res =lists:all(fun(F) -> case compile:file(F, [{i, "c:/cygwin/home/hl/test_codebase"}]) of 
						 {ok, _} ->
						     true;
						 _ -> io:format("\nResulted file does not compile:~p\n", [F]),
						      false
					     end
				   end, ChangedFiles1),
		    wrangler_undo_server:undo(),
		    io:format("\n~p\n", [{ok, ChangedFiles}]),
		    Res;
		{error, Reason} ->
		    io:format("\n~p\n", [{error, Reason}]),
		    true    
	    catch
		throw:Error -> 
		    io:format("Error:\n~\pn", [Error]),
		    true;
		  E1:E2 ->
		    io:format("E1:E2:\n~p\n", [{E1, E2}]),
		    false
	    end;
	 {error, Reason} ->
	    io:format("\n~p\n", [{error, Reason}]),
	    true   
    catch 
	throw:Error -> 
	    io:format("Error:\n~\pn", [Error]),
	    true;
	  E1:E2 ->
	    io:format("E1:E2:\n~p\n", [{E1, E2}]),
	    false
    end.

			
gen_rename_mod_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_rename_mod_commands_1(FileName, Dirs)).

gen_rename_mod_commands_1(FileName, Dirs) ->
    noshrink({FileName, oneof(collect_mod_names(Dirs)++madeup_mod_names()), Dirs, 8}).
	
test_rename_mod(Dirs) ->
    application:start(wrangler),
    eqc:quickcheck(numtests(50,?FORALL(C, (gen_rename_mod_commands(Dirs)), prop_rename_mod(C)))),
    application:stop(wrangler).


test_rename_mod1() ->
    test_rename_mod(["c:/cygwin/home/hl/test_codebase/tableau"]).

test_rename_mod2() ->
    test_rename_mod(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_rename_mod3() ->
    test_rename_mod(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test_rename_mod4() ->
    test_rename_mod(["c:/cygwin/home/hl/test_codebase/suite"]).

test_rename_mod5() ->
    test_rename_mod(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_rename_mod6() ->
    test_rename_mod(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_rename_mod7() ->
    test_rename_mod(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_rename_mod8() ->
    test_rename_mod(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).

test_rename_mod9() ->
    test_rename_mod(["c:/cygwin/home/hl/test_codebase/syntax_tools"]).


run_test() ->
    test_rename_mod1(),
    test_rename_mod2(),
    test_rename_mod3(),
    test_rename_mod4(),
    test_rename_mod5(),
    test_rename_mod6(),
    test_rename_mod7(),
    test_rename_mod8(),
    test_rename_mod9().
    
