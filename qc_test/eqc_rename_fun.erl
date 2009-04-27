-module(eqc_rename_fun).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

%% Default function names.
madeup_fun_names() -> ["aaa", "bbb", "ccc", "DDD"].


%% function  generator			
gen_funs(AST) -> oneof(all_funs(AST)).


%% filename generator
gen_filename(Dirs) ->
    AllErlFiles = refac_util:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).

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
    lists:usort(refac_syntax_lib:fold(F, [], AST)) ++ [{0,0}].


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



%% Collect all the function names (in terms of {functon_name, arity, define_position} in an AST.
all_funs(AST) ->
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		    function ->
			ordsets:add_element({refac_syntax:data(refac_syntax:function_name(T)),
					     refac_syntax:function_arity(T), refac_syntax:get_pos(T)},
					    S);
		    _ -> S
		  end
	  end,
    refac_syntax_lib:fold(Fun, ordsets:new(), AST).



%% returns true if a 'rename a variable' command is valid.
valid_rename_fun_command(AST, {_FName, Loc, NewName, _SearchPaths}) ->
    case Loc of
      {0, 0} -> false;
      _ ->
	  case refac_util:pos_to_fun_name(AST, Loc) of
	    {ok, {_Mod, Fun, _Arity, _, DefinePos}} ->
		DefinePos =/= {0, 0} andalso Fun =/= NewName;
	    _ -> false
	  end
    end.

%% Properties for 'rename a function name'
prop_rename_fun({FName, Loc, NewName, SearchPaths, TabWidth}) ->
    {ok, {AST, _Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ?IMPLIES((valid_rename_fun_command(AST, {FName, Loc, NewName, SearchPaths})),
	      begin
		  {Line, Col} = Loc,
		  Args = [FName, Line, Col, NewName, SearchPaths, TabWidth],
		  Res = try  apply(refac_rename_fun, rename_fun, Args) 
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
			  Res1 = (catch compile:file(FName,[{i, SearchPaths}])), 
			   case Res1 of 
			       {ok, _} -> wrangler_undo_server:undo(),true;
			       _ -> io:format("\nResulted file does not Compile!\n"),
				   wrangler_undo_server:undo(),false
			   end
		  end
	      end).
			
gen_rename_fun_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_rename_fun_commands_1(FileName, Dirs)).

gen_rename_fun_commands_1(FileName, Dirs) ->
    {ok, {AST, _Info}} = refac_util:parse_annotate_file(FileName, true, Dirs, 8),
    noshrink({FileName, oneof(collect_fun_locs(AST)), oneof(collect_atoms(AST)), Dirs, 8}).
	
test_rename_fun(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(?FORALL(C, (gen_rename_fun_commands(Dirs)), prop_rename_fun(C))),
    application:stop(wrangler_app).


test_rename_fun1() ->
    test_rename_fun(["c:/cygwin/home/hl/test_codebase/tableau"]).

test_rename_fun2() ->
    test_rename_fun(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_rename_fun3() ->
    test_rename_fun(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test_rename_fun4() ->
    test_rename_fun(["c:/cygwin/home/hl/test_codebase/suite"]).

test_rename_fun5() ->
    test_rename_fun(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_rename_fun6() ->
    test_rename_fun(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_rename_fun7() ->
    test_rename_fun(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_rename_fun8() ->
    test_rename_fun(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).

test_rename_fun() ->
    test_rename_fun(["c:/cygwin/home/hl/test_codebase"]).

run_test() ->
    test_rename_fun1(),
    test_rename_fun2(),
    %% test_rename_fun3(),
%%     test_rename_fun4(),
%%     test_rename_fun5(),
    test_rename_fun6().
   %%  test_rename_fun7(),
%%     test_rename_fun8().
