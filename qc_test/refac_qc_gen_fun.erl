-module(refac_qc_gen_fun).

-export([test_gen_fun/1]).

-export([show_gen_fun_commands/1]).

-include("c:/erl5.6.3/lib/eqc-1.14/include/eqc.hrl").


collect_expr_locs(FileName, Dirs) ->
    {ok, {AST, _Info}} = refac_util:parse_annotate_file(FileName, true, Dirs),
    F1 = fun(T,S) ->
		 case refac_util:is_expr(T
    F = fun (T, S) ->
		case refac_util:type(T) of 
		    function ->
			refac_syntax_lib:fold(F1, [],T); 
		    true ->
			Range = refac_util:get_range(T),
			[Range|S];
		    false ->
			S
		end
	end,
			;
		    _ -> S
		end
	end,
    lists:usort(refac_syntax_lib:fold(F, [], AST)).

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



%% filename generator
gen_filename(Dirs) ->
    AllErlFiles = refac_util:expand_files(Dirs, ".erl"),
    oneof(AllErlFiles).


%% Properties for 'generalise a function'
prop_gen_fun({FName, Loc, NewName, SearchPaths}) ->
    file:copy(FName, "temp.erl"),
    {ok, {AST, _Info}} = refac_util:annotate_file(FName, true, SearchPaths),
    {Line, Col} = Loc,
    Args = [AST, Line, Col, NewName, SearchPaths],
    Res = try  apply(refac_gen, generalise, Args) 
	  catch 
	      throw:_Error -> true;
		_:_ -> false
	  end,
    case Res of 
	false -> false;
	_ -> try refac_util:parse_annotate_file(FName, false, SearchPaths) 
	     catch
		 _Val -> file:copy("temp.erl", FName), true;
		   _:_ -> file:copy("temp.erl", FName), false
	     end
    end.

	
%% generate 'gen a function' commands.
gen_gen_fun_commands(Dirs) ->
    ?LET(FileName, (gen_filename(Dirs)),
	 {FileName, oneof(collect_expr_locs(FileName, Dirs)),
	  oneof(vars_within_a_fun(AST, F, name) ++ madeup_vars()), Dirs}).
		
show_gen_fun_commands(Dirs)->
    eqc:quickcheck(?FORALL (C, (gen_gen_fun_commands(Dirs)), (eqc:collect(C, true)))).
		  
test_gen_fun(Dirs) ->
    eqc:quickcheck(?FORALL(C, (gen_gen_fun_commands(Dirs)), prop_gen_fun(C))).
