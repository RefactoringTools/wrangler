-module(refac_qc_rename).

-export([prop_rename_var/1, prop_rename_fun/1]).

-compile(export_all).

-include_lib("eqc.hrl").

%% get the AAST represenation of the Erlang File.
parse_file(FName) ->
    get_AST(FName, []).

get_AST(FName, SearchPaths) ->
    case
	wrangler_ast_server:parse_annotate_file(FName, true, SearchPaths)
	of
	{ok, {AST, Info}} -> {ok, {AST, Info}};
	{error, Reason} -> {error, Reason}
    end.

%% Default variable names.
madeup_vars() -> ["AAA", "BBB", "CCC"].

%% Default function names.
madeup_fun_names() -> ["aaa", "bbb", "ccc", "DDD"].

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

%% Collect all the variable names in an AST.
all_vars(AST) ->
    Fun1 = fun (T, S) ->
		   case wrangler_syntax:type(T) of
		       variable ->
			   Name = wrangler_syntax:variable_name(T),
			   ordsets:add_element(atom_to_list(Name), S);
		       _ -> S
		   end
	   end,
    api_ast_traverse:fold(Fun1, ordsets:new(), AST).

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
    {R, true} = api_refac:once_tdTU(Fun2, AST, Function),
    R.

%% collect function define locations in a Erlang file.
collect_fun_locs(FileName) ->
    {ok, {AST, _Info}} = parse_file(FileName),
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
    lists:usort(api_ast_traverse:fold(F, [], AST)).

%% Collect atoms in an Erlang file.
collect_atoms(FileName) ->
    {ok, {AST, _Info}} = parse_file(FileName),
    F = fun (T, S) ->
		case wrangler_syntax:type(T) of
		    atom ->
			Name = wrangler_syntax:atom_value(T),
			[atom_to_list(Name)] ++ S;
		    _ -> S
		end
	end,
    lists:usort(api_ast_traverse:fold(F, madeup_fun_names(), AST)).

%% function  generator			
gen_funs(AST) -> oneof(all_funs(AST)).

%% variable generator
gen_vars(FName, AST) ->
    ?LET(F, (gen_funs(AST)),
	 {FName,
	  oneof(begin
		  L = vars_within_a_fun(AST, F, pos),
		  if L == [] -> [{0, 0}];
		     true -> L
		  end
		end),
	  oneof(all_vars(AST) ++ madeup_vars())}).

%% filename generator
gen_filename(Dir) ->
    {ok, AllFiles} = file:list_dir(Dir),
    AllErlFiles = [F || F <- AllFiles, filename:extension(F) == ".erl"],
    oneof(AllErlFiles).

%% generate 'rename a variable' commands.
commands_rename_var(FName) ->
    {ok, {AST, _Info}} = parse_file(FName),
    ?LET(F, (gen_funs(AST)),
	 {FName,
	  oneof(begin
		  L = vars_within_a_fun(AST, F, pos),
		  if L == [] -> [{0, 0}];
		     true -> L
		  end
		end),
	  oneof(vars_within_a_fun(AST, F, name) ++ madeup_vars()), ["c:/distel-wrangler-0.1/wrangler"]}).

        %% oneof(all_vars(AST) ++ madeup_vars()), ["c:/distel-wrangler-0.1/wrangler"]}).


%% generate 'rename a function' commands.
rename_fun_commands(Dir) ->
    ?LET(FileName, (gen_filename(Dir)),
	 [FileName, oneof(collect_fun_locs(FileName)),
	  oneof(collect_atoms(FileName)), [Dir]]).  %% need to specify a percentage here.


%% get the function name binding structure of an Erlang file.
fun_binding_structure(AST) ->
    Fun1 = fun (T, S) ->
		   case wrangler_syntax:type(T) of
		       atom ->
			   As = wrangler_syntax:get_ann(T),
			   case lists:keysearch(fun_def, 1, As) of
			       {value, {fun_def, {Mod, Fun, Arity, SrcLoc, DefPos}}} ->
				   S ++ [{Mod, Fun, Arity, SrcLoc, DefPos}];
			       _ -> S
			   end;
		       _ -> S
		   end
	   end,
    lists:keysort(4, api_ast_traverse:fold(Fun1, [], AST)).


rename_in_binding_structure(B, {Mod, FunName, Arity}, {Mod, NewFunName, Arity}) ->
    lists:map(fun(T={M, F, A, S, D}) ->
		      case {M, F, A} == {Mod, FunName, Arity} of 
			  true ->
			       {Mod, NewFunName, Arity, S, D};
			  _ -> T
		      end
	      end, B).

%% get the variable binding structure of an Erlang file
var_binding_structure(AST) ->
    Fun1 = fun (T, S) ->
		   case wrangler_syntax:type(T) of
		       variable ->
			   Name = wrangler_syntax:variable_name(T),
			   SrcLoc = wrangler_syntax:get_pos(T),
			   As = wrangler_syntax:get_ann(T),
			   case lists:keysearch(def, 1, As) of
			       {value, {def, DefLoc}} -> ordsets:add_element({atom_to_list(Name), SrcLoc, DefLoc}, S);
			       _ -> S
			   end;
		       _ -> S
		   end
	   end,
    B = api_ast_traverse:fold(Fun1, ordsets:new(), AST),
    DefLocs = lists:usort(lists:map(fun ({_Name, _SrcLoc, DefLoc}) -> DefLoc end, B)),
    SrcLocs = lists:map(fun ({_Name, SrcLoc, _DefLoc}) -> SrcLoc end, B),
    Locs = lists:usort(lists:concat(DefLocs) ++ SrcLocs),
    IndexedLocs = lists:zip(Locs, lists:seq(1, length(Locs))),
    B1 = lists:usort(lists:map(fun ({_Name, SrcLoc, DefLoc}) ->
				       {value, {SrcLoc, Index1}} = lists:keysearch(SrcLoc, 1, IndexedLocs),
				       Index2 = lists:map(fun (L) -> {value, {L, Index}} = lists:keysearch(L, 1, IndexedLocs),Index end,
							  DefLoc),
				       {Index1, Index2}
			       end,
			       B)),
    B1.

%% returns true if a 'rename a variable' command is valid.
valid_rename_var_command1(AST, {_FName, Loc, NewName, _SearchPaths}) ->
    case Loc of
      {0, 0} -> false;
      _ ->
	  case api_refac:pos_to_var_name(AST, Loc) of
	    {ok, {OldName, DefinePos, _}} ->
		DefinePos =/= {0, 0} andalso OldName =/= NewName;
	    _ -> false
	  end
    end.

pos_to_fun_name(AnnAST, {Line, Col}) ->
    {ok, {_Mod, Fun, Arity, _, _DefinePos}} = api_refac:pos_to_fun_name(AnnAST, {Line, Col}),
    {Fun, Arity}.

%% Properties for 'rename a variable name'
prop_rename_var(FName) ->
    file:copy(FName, "temp.erl"),
    {ok, {AST, _Info}} = parse_file(FName),
    B1 = var_binding_structure(AST),
    ?FORALL(C, (commands_rename_var(FName)),
	    (?IMPLIES((valid_rename_var_command1(AST, C)),
		      begin
			{FName, {Line, Col}, NewName1, _SearchPaths} = C,
			NewName = list_to_atom(NewName1),
			P = refac_rename_var:pre_cond_check(AST, Line, Col, NewName),
			{ok, {_, DefinePos, _}} = refac_util:pos_to_var_name(AST, {Line, Col}),
			{NewAST, _} = apply(refac_rename_var, rename, [AST, DefinePos, NewName]),
			file:write_file(FName, list_to_binary(refac_prettypr:print_ast(NewAST))),
			case P of
			  true ->
			      {ok, {AST1, _Info}} = parse_file(FName),
			      B2 = var_binding_structure(AST1),
			      file:copy("temp.erl", FName),
			      B1 == B2;
			  false ->
			      case parse_file(FName) of
				  {error, _Reason} -> file:copy("temp.erl", FName), true
				{ok, {AST1, _}} ->
				      case refac_rename_var:cond_check(AST, DefinePos, NewName) of
				      {false, _} -> B2 = var_binding_structure(AST1), B1 /= B2;
				      _ -> true
				    end
			      end
			end
		      end))).

test_rename_var(File) ->
    eqc:quickcheck(prop_rename_var(File)).



get_callback_funs(ModInfo) ->
    case lists:keysearch(attributes, 1, ModInfo) of
      {value, {attributes, Attrs}} ->
	  case lists:keysearch(behaviour, 1, Attrs) of
	    {value, {behaviour, B}} ->
		wrangler_misc:callback_funs(B);
	    _ -> []
	  end;
      _ -> []
    end.

pretty_print(FileName) -> 
    {ok, {AST, _}} = parse_file(FileName),
    wrangler_prettypr:print_ast(AST).

get_mod_name(FileName) ->  %% assume module name is the samw as the file base name.
    list_to_atom(filename:basename(FileName, ".erl")).

%% properties for 'rename a function name'.
prop_rename_fun(Dir) ->
    F = (?FORALL(C, (rename_fun_commands(Dir)),
		 begin
		     [FileName, SrcLoc={Line,Col} , NewName, SearchPaths] = C,		     
		     %% backup the current version of the program.
		     file:copy(FileName, "temp.erl"),
		     %% get the function name (with arity) to be renamed.
		     {ok, {AST, ModInfo}} = parse_file(FileName),
		     {FunName, Arity} = pos_to_fun_name(AST, SrcLoc),
		     %% get the module name.
		     Mod = get_mod_name(FileName),
		     %% calculate the binding structure of the current program.
		     B1 = fun_binding_structure(AST),
		     %% get the name of the callbacks functions if there is any.
		     CallBacks = get_callback_funs(ModInfo),
		     %% apply the refactoring command to the source.
		     C1 = [FileName, Line, Col, NewName, SearchPaths],
		     Res = apply(refac_rename_fun, rename_fun, C1),
		     io:format("Res:\n~p\n", [Res]),
		     case Res of
			 %% refactoring completed successfully.
			 %% ChangeFiles contains the names of those files
			 %% that have been affected by this refactoring.
			 {ok, ChangedFiles} ->
			     %% get the new binding structure.
			     {ok, {AST1, _}} = parse_file(FileName),
			     B2 = fun_binding_structure(AST1),
			     %% get the name of the callback functions if there is any.
			     CallBacks1 = get_callback_funs(FileName),
			     C2 = [FileName, FunName, Arity, NewName, SearchPaths],
			     %% rename the function back to its original name.
			     %% we can not use location as it might has been changed.
			     {ok, ChangedFiles1} = apply(refac_rename_fun, rename_fun_1, C2),
			     %% property1: renaming in both direction affect
			     %% the same set of files.
			     Prop1 = ChangedFiles == ChangedFiles1,
			     %% property2: rename twice should returns to the original file.
			     %% pretty_print/1 pretty prints an Erlang file.
			     Prop2 = pretty_print(FileName) == pretty_print("temp.erl"),
			     %% property 3: B1 and B2 are isomorphic.
			     %% rename/3 replaces {Mod, FunName, Arity} with
			     %% {Mod, NewName, Arity} in B1
			     Prop3 = B2 == rename_in_binding_structure(B1, {Mod, FunName, Arity}, {Mod, NewName, Arity}),
			     %% property 4: the same set of callback functions.
			     Prop4 = CallBacks == CallBacks1,
			     %% recover the original program for the next refactoring command.
			     file:copy("temp.erl", FileName),
			     %% All there properties should held.
			     Prop1 andalso Prop2 andalso Prop3 andalso Prop4;
			 %% refactoring failed with an error message.
			 {error, _ErrorMsg} ->
			     %% carry out the transformation eventhough the side-conditions
			     %% do not held; do_rename_fun/4 transforms the program.
			     _Res = apply(refac_rename_fun, do_rename_fun, C1),
			     case parse_file(FileName) of 
				 {error, _Reason} -> true;
				 {ok, {AST2, _}} ->
				     B2 = fun_binding_structure(AST2),
				     file:copy("temp.erl", FileName),
				     B2 /= rename_in_binding_structure(B1, {Mod, FunName, Arity}, {Mod, NewName, Arity})
			     end
		     end
		 end)),
    eqc:quickcheck(F).

test_rename_fun(Dir) ->
    eqc:quickcheck(prop_rename_fun(Dir)).

prop_rename_fun_1(Dir) ->
   eqc:quickcheck(?FORALL(F, (rename_fun_commands(Dir)), (eqc:collect(F, true)))).
 
