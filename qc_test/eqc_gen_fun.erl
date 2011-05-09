-module(eqc_gen_fun).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

collect_expr_locs(FileName, Dirs) ->
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Dirs, 8),
    F1 = fun (T,S) ->
		 case refac_api:is_expr(T) of
		     true -> Range = refac_api:start_end_loc(T),
			     [{T, Range}| S];
		     _ -> S
		 end
	 end,
    F = fun (T, S) ->
		case refac_syntax:type(T) of
		    function ->
			ast_traverse_api:fold(F1, [], T) ++ S;
		    _ -> S
		end
	end,
    Res = lists:usort(ast_traverse_api:fold(F, [], AST)),
    case Res of
	[] ->
	    [{none, {{0,0},{0,0}}}];
	_ -> Res
    end.
    
 

%% Default variable names.
madeup_vars() -> frequency([{8,oneof(["AAA", "BBB"])}, {2, oneof(["11", "AA-Vv"])}]).

%% collect all the variables in a function definition in terms of position or name as specified by PosOrName.
vars_within_a_fun(F, PosOrName) ->
    Fun = fun (T, S) ->
		  case refac_syntax:type(T) of
		      variable ->
			  Name = refac_syntax:variable_name(T),
			  Pos = refac_syntax:get_pos(T),
			  case PosOrName of
			      pos -> ordsets:add_element(Pos, S);
			      _ -> ordsets:add_element(atom_to_list(Name), S)
			  end;
		      _ -> S
		  end
	  end,
    case F of
	none -> [];
	_ -> lists:usort(ast_traverse_api:fold(Fun, [], F))
    end.

%% filename generator
gen_filename(Dirs) ->
    AllErlFiles = refac_misc:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).


%% Properties for 'generalise a function'
prop_gen_fun({FName, Range, NewName, SearchPaths, TabWidth}) ->
    Res0= case (catch compile:file(FName, [{i, "c:/cygwin/home/hl/test_codebase"}])) of
	      {ok, _} -> ok;
	      _ -> fail
	   end,
    {Line, Col} = Range,
    Args = [FName, Line, Col, NewName, SearchPaths, TabWidth],
    try  apply(refac_gen, generalise, Args)  of
	 {ok, Res} -> 
	    wrangler_preview_server:commit(),
	    case compile:file(FName,[]) of 
		{ok, _} -> 
		    wrangler_undo_server:undo(),
		    io:format("\n~p\n", [{ok, Res}]),
		    true;
		_ ->
		    case Res0 of 
			ok ->
			    io:format("\nResulted file does not Compile!\n"),
			    wrangler_undo_server:undo(),false;
			fail ->
			    wrangler_undo_server:undo(),true
		    end
	    end;
	 {error, Msg} -> 
	    io:format("\n~p\n", [{error,Msg}]),
	    true;
	 {more_than_one_clause, {ParName, FunName, FunArity, FunDefPos, Exp, SideEffect, _Fun_Dups, Clause_Dups, Cmd}}->
	    try apply(refac_gen, gen_fun_clause, [FName, ParName, FunName, FunArity, FunDefPos, Exp, TabWidth, SideEffect, Clause_Dups, Cmd]) of 
	 	{ok, Res} ->
		    wrangler_preview_server:commit(),
		    case compile:file(FName, []) of 
			{ok, _} -> 
			    wrangler_undo_server:undo(),
			    io:format("\n~p\n", [{ok, Res}]),
			    true;
			_ ->
			    case Res0 of 
				ok ->
				    io:format("\nResulted file does not Compile!\n"),
				    wrangler_undo_server:undo(),false;
				fail ->
			    wrangler_undo_server:undo(),true
			    end
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
	    end;	    
	 {unknown_side_effect,{ParName, FunName, FunArity, FunDefPos,Exp, _NoOfCs, _Fun_Instances, _Clause_Intances, Cmd}} -> 
	    try apply(refac_gen, gen_fun_1, [bool(), FName, ParName, FunName, FunArity, FunDefPos, Exp, SearchPaths, TabWidth,[],Cmd]) of 
	 	{ok, Res} ->
		    wrangler_preview_server:commit(),
		    case compile:file(FName, []) of 
			{ok, _} -> 
			    wrangler_undo_server:undo(),
			    io:format("\n~p\n", [{ok, Res}]),
			    true;
			_ -> 
			    case Res0 of 
				ok ->
				    io:format("\nResulted file does not Compile!\n"),
				    wrangler_undo_server:undo(),false;
				fail ->
				    wrangler_undo_server:undo(),true
			    end
		    end;
		{error, Msg} -> 
		    io:format("\n~p\n", [{error,Msg}]),
		    true
	    catch
		throw:Error -> 
		    io:format("Error:\n~pn", [Error]),
		    true;
		  E1:E2 ->
		    io:format("E1:E2:\n~p\n", [{E1, E2}]),
		    false
	    end;
	 {multiple_instances, {ParName, FunName, FunArity, FunDefPos, Exp, SideEffect, DupsInFun, Cmd}} ->
	     try apply(refac_gen, gen_fun_1, [SideEffect, FName, ParName, FunName, FunArity, FunDefPos, Exp, SearchPaths, TabWidth, DupsInFun, Cmd]) of 
	 	{ok, Res} ->
		    wrangler_preview_server:commit(),
		    case compile:file(FName, []) of 
			{ok, _} -> 
			    wrangler_undo_server:undo(),
			    io:format("\n~p\n", [{ok, Res}]),
			    true;
			_ -> 
			    case Res0 of 
				ok ->
				    io:format("\nResulted file does not Compile!\n"),
				    wrangler_undo_server:undo(),false;
				fail ->
				    wrangler_undo_server:undo(),true
			    end
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
	    end   
    catch 
	throw:Error -> 
	    io:format("Error:\n~\pn", [Error]),
	    true;
	  E1:E2 ->
	    io:format("E1:E2:\n~p\n", [{E1, E2}]),
	    false
    end.
	       
 
%% generate 'gen a function' commands.
gen_gen_fun_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 ?LET(F, oneof(collect_expr_locs(FileName, Dirs)),
	      begin
		  {Fun, Range} = F, 
		  {FileName, Range,  oneof(vars_within_a_fun(Fun, name)++[madeup_vars()]), Dirs, 8}
	      end)).


show_gen_fun_commands(Dirs)->
    application:start(wrangler_app),
    eqc:quickcheck(numtests(500,?FORALL (C, (gen_gen_fun_commands(Dirs)), (eqc:collect(C, true))))),
    application:stop(wrangler_app).
		
	  
test_gen_fun(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(numtests(500,?FORALL(C, (gen_gen_fun_commands(Dirs)), prop_gen_fun(C)))),
    application:stop(wrangler_app).


test_gen_fun1() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/lampera"]).

test_gen_fun2() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_gen_fun3() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/refactorerl"]).

test_gen_fun4() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/suites"]).

test_gen_fun5() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/wrangler"]).

test_gen_fun6() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/stdlib"]).

test_gen_fun7() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/yxa"]).

test_gen_fun8() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/dialyzer"]).

run_test_gen_fun() ->
    test_gen_fun1(),
    test_gen_fun2(),
    test_gen_fun3(),
    test_gen_fun4(),
    test_gen_fun5(),
    test_gen_fun6(),
    test_gen_fun7(),
    test_gen_fun8().
