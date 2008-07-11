-module(refac_clone_removal).

-export([clone_removal/1, env/0]).

clone_removal(DirFileList) ->
    clone_removal(DirFileList, "20", "2").

clone_removal(DirFileList, MinLength, MinClones) ->
   FileNames = refac_util:expand_files(DirFileList, ".erl"),
   Cs = refac_duplicated_code:duplicated_code_1(FileNames, MinLength, MinClones),
   case Cs of 
	[] -> io:format("\nNo code clones have been found.\n");
	_ -> 
	   AffectedFiles = collect_affected_files(Cs),  %% collect all the files which could possibly affected by removal.
	   AffectedFiles1 =lists:map(fun(F) -> {ok, Bin} = file:read_file(F), {{F, F}, Bin} end, AffectedFiles),
	   case erlang:whereis(refactor_undo) of
	       undefined ->
		   io:format("\nWARNING: the UNDO process is not working, "
			     "please restart the refactorer!\n");
	       _ -> refactor_undo ! {add, AffectedFiles1}
	   end, 
	   start_env_process(),
	   do_clone_removal(Cs),
	   {ok, "Clone removal finished!"}
   end.

collect_affected_files(Cs) ->
    collect_affected_files(Cs, []).
collect_affected_files([], Acc) ->
    Acc;
collect_affected_files([{Range, _Len, _F}|Cs], Acc) ->
    Fs = lists:map(fun({{F, _L1, _C1}, _}) -> F end, Range),
    collect_affected_files(Cs, lists:usort(Fs)++Acc).
			      

do_clone_removal(Cs) ->
    Cs1 = process_clones(Cs),
    io:format("Cs1:\n~p\n",[Cs1]),
    NewFuns = do_function_extraction(Cs1),
     stop_env_process().
%%     NewFuns1 = do_generalisation(NewFuns),
%%     do_folding(NewFuns1)
	
    
 
process_clones(Cs) ->
     process_clones(Cs, []).
process_clones([], Acc) -> group_by(lists:keysort(1, Acc));
process_clones([{[{{File, StartLine, StartCol}, {File,EndLine, EndCol}}|_Range], _Len, _F}|Cs], Acc) ->
    process_clones(Cs, [{File, {StartLine, StartCol}, {EndLine, EndCol}}|Acc]).
    
group_by([]) -> [];
group_by(Cs=[{File, StartLoc, EndLoc}|_T]) -> 
    {C1, C2} =lists:splitwith(fun({File1,_S,_E})-> File1 == File end, Cs),
    [C1 | group_by(C2)].


do_function_extraction(Cs) ->
    do_function_extraction(Cs, []).

do_function_extraction([], Acc) ->
    Acc;
do_function_extraction([C|Cs], Acc) ->
    Res1=do_function_extraction_1(C),
    do_function_extraction(Cs, Res1++Acc).

do_function_extraction_1(C) ->
    do_function_extraction_1(C, []).
do_function_extraction_1([], Acc) ->
    Acc;
do_function_extraction_1(Ranges=[{File,Start,End}|T], Acc) ->
    env ! {self(), get, File}, 
    receive
	{env, value, {AnnAST,Info}} ->
	    ExprsList =lists:flatmap(fun({F, {StartLine, StartCol}, {EndLine, EndCol}}) ->
					 Exprs =refac_new_fun:pos_to_expr(F, AnnAST, {{StartLine, StartCol-1}, {EndLine, EndCol+1}}),
					 {ok, Fun} = refac_util:expr_to_fun(AnnAST, hd(Exprs)),
					 NewFunName = new_function_name(),
					 case refac_new_fun:side_cond_analysis(Info, Fun, Exprs, list_to_atom(NewFunName)) of 
					     {ok, {BdVars, FrVars}} ->
						 FunName = refac_syntax:atom_value(refac_syntax:function_name(Fun)),
						 FunArity = refac_syntax:function_arity(Fun),
						 VarsToExport=refac_new_fun:vars_to_export(Fun, End, BdVars, Exprs), 
						 [Exprs, NewFunName, FrVars, VarsToExport, FunName, FunArity];
					     _ -> []

					 end
				 end, Ranges),
	    ExprsList
    end,
    io:format("ExprsList:\n~p\n", [ExprsList]),
    do_function_extraction_1(T, Acc).
  %%   {FinalAnnAST, FinalAcc} =
%% 	lists:foldl(fun(Exprs, {{AnnAST1, Info1}, Acc1}) ->
%% 			    {AnnAST2, NewFun} =refac_new_fun:fun_extraction_1(File,{AnnAST1,Info1}, Exprs, new_function_name()),
%% 			    {{AnnAST2, Info1}, NewFun++Acc1}
%% 		    end, {{AnnAST,Info}, []},ExprsList),
%%     env ! {add, {File, {FinalAnnAST, Info}}},
%%     do_function_extraction_1(T,FinalAcc).
		  

	
%%     lists:map(fun({F, {StartLine, StartCol}, {EndL
%%       receive 
%% 	{env, value, {AnnAST,Info}} ->
	    
%% 	    {AnnAST1, NewFunName, Arity}=refac_new_fun:fun_extraction_1(File, {AnnAST, Info}, {StartLine, StartCol-1}, {EndLine, EndCol+1}, new_function_name()),
%% 	    env ! {add, {File, {AnnAST1, Info}}}
%%     end,
%%     do_function_extraction(Cs, [{File, NewFunName, Arity}|Acc]).

new_function_name() ->
    "newfun" ++ integer_to_list(random:uniform(100)). 
    

do_generalisation(NewFuns) ->
    do_generalisation(NewFuns, []).

do_generalisation([], Acc) ->
    Acc;
do_generalisation([{FileName, NewFunName, Arity}|Funs], Acc) ->
    {AnnAST2, Arity1} = refac_gen:generalise_1(FileName, list_to_atom(NewFunName), Arity),
    file:write_file(FileName, list_to_binary(refac_prettypr:print_ast(AnnAST2))),
    do_generalisation(Funs, [{FileName, NewFunName, Arity1}|Acc]).


do_folding([]) ->
    ok;
do_folding([{FileName, NewFunName, Arity}|Funs]) ->
    {AnnAST3, Info} = refac_fold_expression:fold_expression_1(FileName, list_to_atom(NewFunName), Arity),
    file:write_file(FileName, list_to_binary(refac_prettypr:print_ast(AnnAST3))),
    do_folding(Funs).

	    
start_env_process() ->
    Pid = spawn_link(refac_clone_removal, env, []),
    register(env, Pid).

stop_env_process() ->
    env!{self(), stop},
    receive
	{env, done} ->
	    ok
    end.
   

env() -> env_loop([]).

env_loop(Env) ->
   receive
	{From, get, FileName} ->
	    Env1 = case lists:keysearch(FileName, 1, Env) of
		       {value, {FileName, Value}} -> From ! {env, value, Value},
						     Env;
		       false -> case refac_util:parse_annotate_file(FileName, true, []) of 
				    {ok, {AnnAST, Info}} ->
					From ! {env, value, {AnnAST, Info}},
					[{FileName, {AnnAST, Info}}|Env];
				    {error, Reason} -> erlang:error(Reason)
				end
		   end,		
	    env_loop(Env1);
	{add, {FileName, Value}} ->
	    Env1 =  lists:keyreplace(FileName, 1, Env, {FileName, Value}),
  	    env_loop(Env1);
	{From, stop} ->
	  Res = lists:map(fun({F, {AnnAST, _Info}}) -> {{F, F}, AnnAST} end, Env),
	  write_refactored_files(Res),
	  From ! {env, done}
    end.
 


write_refactored_files(Files) ->
     F = fun ({{File1, File2}, AST}) ->
 		if File1 /= File2 ->
 		       file:delete(File1),
 		       file:write_file(File2,
 				       list_to_binary(refac_prettypr:print_ast(AST)));    
 		   true -> file:write_file(File2, list_to_binary(refac_prettypr:print_ast(AST)))
 		end
 	end,
     lists:map(F, Files).
