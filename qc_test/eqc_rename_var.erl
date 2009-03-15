-module(eqc_rename_var).

-export([test_rename_var/1, test_rename_var1/0]).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

%% Default variable names.
madeup_vars() -> ["AAA", "BBB", "CCC"].

%% collect all the variables in a function definition in terms of position or name as specified by PosOrName.
vars_within_a_fun(AST, Function, PosOrName) ->
    Fun1 = fun (T, S) ->
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
    Fun2 = fun (Node, {FunName, Arity, Pos}) ->
		   case refac_syntax:type(Node) of
		     function ->
			 case {refac_syntax:data(refac_syntax:function_name(Node)),
			       refac_syntax:function_arity(Node), refac_syntax:get_pos(Node)}
			     of
			   {FunName, Arity, Pos} ->
			       {refac_syntax_lib:fold(Fun1, ordsets:new(), Node), true};
			   _ -> {[], false}
			 end;
		     _ -> {[], false}
		   end
	   end,
    {R, true} = refac_util:once_tdTU(Fun2, AST, Function),
    R.

%% function  generator			
gen_funs(AST) -> oneof(all_funs(AST)).


%% filename generator
gen_filename(Dirs) ->
    AllErlFiles = refac_util:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).

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
valid_rename_var_command1(AST, {_FName, Loc, NewName, _SearchPaths}) ->
    case Loc of
      {0, 0} -> false;
      _ ->
	  case refac_util:pos_to_var_name(AST, Loc) of
	    {ok, {OldName, DefinePos, _}} ->
		DefinePos =/= {0, 0} andalso OldName =/= NewName;
	    _ -> false
	  end
    end.

%% Properties for 'rename a variable name'
prop_rename_var({FName, Loc, NewName, SearchPaths, TabWidth}) ->
    {ok, {AST, _Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    ?IMPLIES((valid_rename_var_command1(AST, {FName, Loc, NewName, SearchPaths})),
	      begin
		  {Line, Col} = Loc,
		  Args = [FName, Line, Col, NewName, SearchPaths, TabWidth],
		  Res = try  apply(refac_rename_var, rename_var, Args) 
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
		      _ -> Res1 = (catch refac_util:parse_annotate_file(FName, false, SearchPaths)),
			   case Res1 of 
			       {ok, _} -> wrangler_undo_server:undo(),true;
			       _ -> io:format("\nResulted file does not Compile!\n"),
				   wrangler_undo_server:undo(),false
			   end
		  end
	      end).
			
gen_rename_var_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 gen_rename_var_commands_1(FileName, Dirs)).

gen_rename_var_commands_1(FileName, Dirs) ->
    {ok, {AST, _Info}} = refac_util:parse_annotate_file(FileName, true, Dirs),
    ?LET(F, (gen_funs(AST)),
	noshrink({FileName,
	  oneof(begin
		    L = vars_within_a_fun(AST, F, pos),
		    if L == [] -> [{0, 0}];
		       true -> L
		    end
		end),
	  oneof(vars_within_a_fun(AST, F, name)++ (madeup_vars())), Dirs, 8})).

test_rename_var(Dirs) ->
    eqc:quickcheck(?FORALL(C, (gen_rename_var_commands(Dirs)), prop_rename_var(C))).


test_rename_var1() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/tableau"]).


test_rename_var2() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_rename_var3() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/refactorerl-0.5"]).

test_rename_var4() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/suite"]).

test_rename_var5() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/wrangler-0.7"]).

test_rename_var6() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/umbria"]).

test_rename_var7() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/yaws-1.77"]).

test_rename_var8() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase/dialyzer-1.8.3"]).

test_rename_var() ->
    test_rename_var(["c:/cygwin/home/hl/test_codebase"]).
