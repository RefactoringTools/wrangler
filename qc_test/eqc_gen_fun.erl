-module(eqc_gen_fun).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

collect_expr_locs(FileName, Dirs) ->
    {ok, {AST, _Info}} = refac_util:parse_annotate_file(FileName, true, Dirs, 8),
    F1 = fun(T,S) ->
		 case refac_util:is_expr(T) of 
		     true ->Range = refac_util:get_range(T),
			    [{T, Range}|S];
		     _ -> S
		 end
	 end,
    F = fun (T, S) ->
		case refac_syntax:type(T) of 
		    function ->
			refac_syntax_lib:fold(F1, [],T) ++ S;
		    _ -> S
		end
	end,
    lists:usort(refac_syntax_lib:fold(F, [], AST)).

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
    lists:usort(refac_syntax_lib:fold(Fun, [], F)).
   
%% filename generator
gen_filename(Dirs) ->
    AllErlFiles = refac_util:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).


%% Properties for 'generalise a function'
prop_gen_fun({FName, Range, NewName, SearchPaths, TabWidth}) ->
    {Line, Col} = Range,
    Args = [FName, Line, Col, NewName, SearchPaths, TabWidth],
    try  apply(refac_gen, generalise, Args)  of
	 {ok, Res} -> case refac_util:parse_annotate_file(FName, false, SearchPaths) of 
			{ok, _} -> 
			      wrangler_undo_server:undo(),
			      io:format("\n~p\n", [{ok, Res}]),
			      true;
			  _ -> wrangler_undo_server:undo(), 
			       io:format("\nResulted file does not compile!\n"),
			       false
		      end;
	 {error, Msg} -> 
	    io:format("\n~p\n", [{error,Msg}]),
	    true;
	 {unknown_side_effect,{ParName, FunName, FunArity, FunDefPos,Exp}} -> 
	    try apply(refac_gen, gen_fun_1, [bool(), FName, ParName, FunName, FunArity, FunDefPos, Exp, TabWidth]) of 
	 	{ok, Res} ->
		    case refac_util:parse_annotate_file(FName, false, SearchPaths) of 
			{ok, _} -> 
			    wrangler_undo_server:undo(),
			    io:format("\n~p\n", [{ok, Res}]),
			    true;
			_ -> wrangler_undo_server:undo(), 
			     io:format("\nResulted file does not compole!\n"),
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
    eqc:quickcheck(?FORALL (C, (gen_gen_fun_commands(Dirs)), (eqc:collect(C, true)))),
    application:stop(wrangler_app).
		
	  
test_gen_fun(Dirs) ->
    eqc:quickcheck(?FORALL(C, (gen_gen_fun_commands(Dirs)), prop_gen_fun(C))).

test_gen_fun1() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/tableau"]).

test_gen_fun2() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_gen_fun3() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test_gen_fun4() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/suite"]).

test_gen_fun5() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_gen_fun6() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_gen_fun7() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_gen_fun8() ->
    test_gen_fun(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).

run_test() ->
    test_gen_fun1(),
    test_gen_fun2(),
    test_gen_fun3(),
    test_gen_fun4(),
    test_gen_fun5(),
    test_gen_fun6(),
    test_gen_fun7(),
    test_gen_fun8().
