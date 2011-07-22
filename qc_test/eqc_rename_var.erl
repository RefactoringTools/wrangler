-module(eqc_rename_var).

-export([test_rename_var/1, test_rename_var1/0]).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

%% Default variable names.
madeup_vars() -> ["AAA", "BBB", "CCC"].

%% collect all the variables in a function definition in terms of position or name as specified by PosOrName.
vars_within_a_fun(AST, Function, PosOrName) ->
    Fun1 = fun (T, S) ->
		   case wrangler_syntax:type(T) of
		       variable ->
			   Name = wrangler_syntax:variable_name(T),
			   Pos = wrangler_syntax:get_pos(T),
			   case PosOrName of
			       pos -> ordsets:add_element(Pos, S);
			       _ -> ordsets:add_element(atom_to_list(Name), S)
			   end;
		       _ -> S
		   end
	   end,
    Fun2 = fun (Node, {FunName, Arity, Pos}) ->
		   case wrangler_syntax:type(Node) of
		       function ->
			   case {wrangler_syntax:data(wrangler_syntax:function_name(Node)),
				 wrangler_syntax:function_arity(Node), wrangler_syntax:get_pos(Node)}
			       of
			       {FunName, Arity, Pos} ->
				   {api_ast_traverse:fold(Fun1, ordsets:new(), Node), true};
			       _ -> {[], false}
			   end;
		       _ -> {[], false}
		   end
	   end,
    {R, _} = api_ast_traverse:once_tdTU(Fun2, AST, Function),
    R.

%% function  generator			
gen_funs(AST) -> oneof(all_funs(AST)).

%% filename generator
gen_filename(Dirs) ->
    AllErlFiles = wrangler_misc:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).

%% Collect all the function names (in terms of {functon_name, arity, define_position} in an AST.
all_funs(AST) ->
    Fun = fun (T, S) ->
		  case wrangler_syntax:type(T) of
		      function ->
			  ordsets:add_element({wrangler_syntax:data(wrangler_syntax:function_name(T)),
					       wrangler_syntax:function_arity(T), wrangler_syntax:get_pos(T)},
					      S);
		      _ -> S
		  end
	  end,
    api_ast_traverse:fold(Fun, ordsets:new(), AST) ++ [{none, 0, {0,0}}].



%% returns true if a 'rename a variable' command is valid.
valid_rename_var_command1(AST, {_FName, Loc, NewName, _SearchPaths}) ->
    case Loc of
      {0, 0} -> false;
      _ ->
	  case api_interface:pos_to_var_name(AST, Loc) of
	    {ok, {OldName, DefinePos, _}} ->
		DefinePos =/= {0, 0} andalso OldName =/= NewName;
	    _ -> false
	  end
    end.

%% Properties for 'rename a variable name'
prop_rename_var({FName, Loc, NewName, SearchPaths, TabWidth}) ->
    Res0 = case catch compile:file(FName, [{i, "c:/cygwin/home/hl/test_codebase"}]) of
	       {ok, _} -> ok;
	       _ -> fail
	   end,
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ?IMPLIES(valid_rename_var_command1(AST, {FName, Loc, NewName, SearchPaths}),
	     begin
		 {Line, Col} = Loc,
		 Args = [FName, Line, Col, NewName, SearchPaths, TabWidth],
		 Res = try apply(refac_rename_var, rename_var, Args)
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
			 Res1 = (catch compile:file(FName, [{i, "c:/cygwin/home/hl/test_codebase"}])),
			 case Res1 of
			     {ok, _} -> wrangler_undo_server:undo(),true;
			     _ -> case Res0 of
				      ok ->
					  io:format("\nResulted file does not Compile!\n"),
					  wrangler_undo_server:undo(),false;
				      fail ->
					  wrangler_undo_server:undo(),true
				  end
			 end
		 end
	     end).
			
gen_rename_var_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_rename_var_commands_1(FileName, Dirs)).

gen_rename_var_commands_1(FileName, Dirs) ->
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Dirs),
    ?LET(F, gen_funs(AST),
	 noshrink({FileName,
		   oneof(begin
			     L = vars_within_a_fun(AST, F, pos),
			     if L == [] -> [{0, 0}];
				true -> L
			     end
			 end),
		   oneof(vars_within_a_fun(AST, F, name)++ (madeup_vars())), Dirs, 8})).

test_rename_var(Dirs) ->
    application:start(wrangler_app),
    eqc:quickcheck(numtests(500, ?FORALL(C, (gen_rename_var_commands(Dirs)), prop_rename_var(C)))),
    application:stop(wrangler_app).

   
test_rename_var1() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/lampera"]).

test_rename_var2() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_rename_var3() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/refactorerl"]).

test_rename_var4() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/suites"]).

test_rename_var5() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/wrangler"]).

test_rename_var6() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/stdlib"]).

test_rename_var7() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/yxa"]).

test_rename_var8() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/dialyzer"]).

run_test_rename_var() ->
    test_rename_var1(),
    test_rename_var2(),
    test_rename_var3(),
    test_rename_var4(),
    test_rename_var5(),
    test_rename_var6(),
    test_rename_var7(),
    test_rename_var8().
    
