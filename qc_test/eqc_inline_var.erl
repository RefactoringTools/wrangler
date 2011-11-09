-module(eqc_inline_var).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

vars_within_a_fun(AST, Function) ->
    Fun1 = fun (T, S) ->
		   case wrangler_syntax:type(T) of
		       variable ->
                           Pos = wrangler_syntax:get_pos(T),
                           case  lists:keysearch(def, 1, wrangler_syntax:get_ann(T)) of
                               {value, {def, DefinePos}} -> 
                                   case not lists:member(Pos, DefinePos) of
                                       true -> ordsets:add_element(Pos, S);
                                       _ -> S
                                   end;
                               false -> 
                                   wrangler_io:format("T:\n~p\n", [T]),
                                   S
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
    api_ast_traverse:fold(Fun, ordsets:new(), AST).



%% returns true if a 'inline a variable' command is valid.
valid_inline_var_command1(_AST, {_FName, _Loc, _SearchPaths}) ->
    true.
   

%% Properties for 'inline a variable name'
prop_inline_var({FName, Loc, SearchPaths, TabWidth}) ->
    Res0 = case catch compile:file(FName, [{i, "c:/cygwin/home/hl/test_codebase"}]) of
	       {ok, _} -> ok;
	       _ -> fail
	   end,
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ?IMPLIES(valid_inline_var_command1(AST, {FName, Loc, SearchPaths}),
	     begin
		 {Line, Col} = Loc,
		 Args = [FName, Line, Col, SearchPaths, emacs, TabWidth],
                 Res = try apply(refac_inline_var, inline_var, Args)
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
			
gen_inline_var_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_inline_var_commands_1(FileName, Dirs)).

gen_inline_var_commands_1(FileName, Dirs) ->
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, Dirs),
    ?LET(F, gen_funs(AST),
	 noshrink({FileName,
		   oneof(begin
			     L = vars_within_a_fun(AST, F),
                             if L == [] -> [{0, 0}];
				true -> L
			     end
			 end),
		   Dirs, 8})).

test_inline_var(Dirs) ->
    application:start(wrangler),
    eqc:quickcheck(numtests(500, ?FORALL(C, (gen_inline_var_commands(Dirs)), prop_inline_var(C)))),
    application:stop(wrangler).

   
test_inline_var1() ->
    test_inline_var(["c:/cygwin/home/hl/test_codebase/lampera"]).

test_inline_var2() ->
    test_inline_var(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_inline_var3() ->
    test_inline_var(["c:/cygwin/home/hl/test_codebase/refactorerl"]).

test_inline_var4() ->
    test_inline_var(["c:/cygwin/home/hl/test_codebase/suites"]).

test_inline_var5() ->
    test_inline_var(["c:/cygwin/home/hl/test_codebase/wrangler"]).

test_inline_var6() ->
    test_inline_var(["c:/cygwin/home/hl/test_codebase/stdlib"]).

test_inline_var7() ->
    test_inline_var(["c:/cygwin/home/hl/test_codebase/yxa"]).

test_inline_var8() ->
    test_inline_var(["c:/cygwin/home/hl/test_codebase/dialyzer"]).

test_inline_var9() ->
    test_inline_var(["c:/cygwin/home/hl/test_codebase/syntax_tools"]).



run_test_inline_var() ->
    test_inline_var1(),
    test_inline_var2(),
    test_inline_var3(),
    test_inline_var4(),
    test_inline_var5(),
    test_inline_var6(),
    test_inline_var7(),
    test_inline_var8().
    
