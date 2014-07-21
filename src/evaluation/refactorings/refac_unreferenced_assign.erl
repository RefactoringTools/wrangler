%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Roberto S. M. de Barros Filho, Simon  Thompson
-module(refac_unreferenced_assign).

-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1]).
-export([second_transform/4,transform_unref_assign/4]).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prompts for parameter inputs
%%
%% @spec input_par_prompts() -> [string()]
%% @end
%%--------------------------------------------------------------------
input_par_prompts() -> [refac:input_refac_scope_message()].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Select the focus of the refactoring.
%%
%% @spec select_focus(Args::#args{}) ->
%%                {ok, syntaxTree()} |
%%                {ok, none}
%% @end
%%--------------------------------------------------------------------
select_focus(Args=#args{user_inputs=[EntireFileStr]}) ->
    refac:select_focus(Args,EntireFileStr).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec(check_pre_cond(Args::args{}) -> ok | {error, Reason})
%% @end
%%--------------------------------------------------------------------
check_pre_cond(_Args) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Selective transformation or not.
%%
%% @spec selective() -> boolean()
%% @end
%%--------------------------------------------------------------------
selective() ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function does the actual transformation.
%%
%% @spec transform(Args::#args{}) -> 
%%            {ok, [{filename(), filename(), syntaxTree()}]} |
%%            {error, Reason}
%% @end
%%--------------------------------------------------------------------
transform(_Args=#args{current_file_name=File,
		     user_inputs=[EntireFileStr],focus_sel=FunDef,search_paths=SearchPaths}) -> 
    transform_unref_assign(File,EntireFileStr,FunDef,SearchPaths).
	    

transform_unref_assign(File,EntireFileStr, FunDef,SearchPaths) ->
    RefacScope = refac:get_refac_scope(EntireFileStr),
    if
	RefacScope == project orelse RefacScope == file orelse RefacScope == function ->
	    MFA = fun_define_info(RefacScope, FunDef),
	    if
		 MFA /= unknown ->
		    Files = refac:get_files(RefacScope,SearchPaths,File),
		    RefacModuleInfoList = lists:map(fun(CurFile) -> api_refac:module_name(CurFile) end,Files),
		    case lists:filter(fun(Tuple) -> refac:filterError(Tuple) end, RefacModuleInfoList) of
			[_ | _] -> {error, "Refactoring failed!"};
			_ ->
			    CollectResult = collect(Files),
			    Result = ?STOP_TD_TP((rules(CollectResult, {RefacScope, MFA})), Files),
			    second_transform(Result,RefacScope,MFA,false)
		    end;
		 true -> {error, "Please, place the mouse cursor on the desired function!"}
	    end;
	true -> {error, "Please, answer 'y', 'Y' or 'n'!"}
    end.

fun_define_info(function, FunDef) -> api_refac:fun_define_info(FunDef);
fun_define_info(_,_) -> notrelevant.
    
second_transform(Result,RefacScope,MFA,IsFirst) ->
    case Result of
	{ok, ListOfRefacs} ->
	    {ok, lists:map(fun(FileTuple) -> second_transform_file(Result,FileTuple,RefacScope,MFA,IsFirst) end, ListOfRefacs)};	   
	 _ ->
	    Result
   end.

second_transform_file(Result,{{FileName,FileName}, Node},RefacScope,MFA,IsFirst) ->
       FileNode = api_refac:get_ast(FileName),
       case FileNode of
	      {error, _Reason} -> 
		      Result;
		   _ ->
		      try_transform_recursively(IsFirst orelse FileNode /= Node, Node, FileName, {RefacScope, MFA})
       end;
second_transform_file(Result,_,_,_,_) -> Result.

    


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to apply the transformation as much as possible.
%% @end
%%--------------------------------------------------------------------
transform_recursively({{FileName, FileName}, Scope}, FunInfo) ->
    CollectResult = collect_file(Scope,FileName),
    Result = ?STOP_TD_TP(rules(CollectResult, FunInfo), Scope),
    case Result of
	{ok, Node} -> 	       
	    try_transform_recursively(?PP(Node) /= ?PP(Scope), Node, FileName, FunInfo);
	_ ->
	 {{FileName, FileName},Scope}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if transformations can still be done.
%% @end
%%--------------------------------------------------------------------
try_transform_recursively(Changed, Node, FileName, FunInfo) ->
    TransformResult = {{FileName, FileName}, Node},
    if
	Changed -> transform_recursively(TransformResult, FunInfo);
	true -> TransformResult
    end.
	    
rules(CollectResult, FunInfo) ->    
    [variable_assignment_rule(CollectResult, FunInfo)].

variable_assignment_rule(CollectResult, {RefacScope, MFA}) ->
    ?RULE(
       ?T("f@(Args@@) when Guards@@ -> Stmt0@@, Var@ = Expr@, Stmt@@;"),
       case Stmt@@ of	   
	   [] -> ?TO_AST("f@(Args@@) when Guards@@ -> Stmt0@@, Expr@;");
	   _ -> ?TO_AST("f@(Args@@) when Guards@@ -> Stmt0@@, Stmt@@;")
       end,
       begin
	FunMFA = api_refac:fun_define_info(f@),
        (FunMFA /= unknown andalso
	  api_refac:type(Var@) == variable andalso
         (RefacScope /= function orelse
	  MFA == FunMFA)
         andalso
	 begin
	     {M,_,_} = FunMFA,
	     case lists:keyfind(M,1,CollectResult) of
		 {M,Info} -> 
		     lists:filter(fun(Elem) -> Elem == api_refac:bound_vars(Var@) end, Info) == [];
		 _ -> false
	     end
         end)
      end
   ).

collect(Files) ->
    lists:map(fun(Scope) -> 
		      {ok,RefacModule} = api_refac:module_name(Scope),
		      {RefacModule,collect_file_traversal(Scope)} end, Files).

collect_file(Scope,FileName) ->
    {ok,RefacModule} = api_refac:module_name(FileName),
    [{RefacModule, collect_file_traversal(Scope)}].

collect_file_traversal(Scope) ->
    ?FULL_TD_TU(    
		[collect_variables_occurrences()],
		[Scope]
    ).

%%--------------------------------------------------------------------
%% @doc
%% For all Function Clauses in the file collects:
%%   -The function info: Module name, function name and arity
%%   -The AST representation of the pattern clause
%%   -The AST representation of the body
%% @end
%%--------------------------------------------------------------------
collect_variables_occurrences()->
    ?COLLECT(
       ?T("Var@"),
       api_refac:free_vars(Var@),
       api_refac:type(Var@) == variable andalso 
       api_refac:variable_define_pos(Var@) /= [{0,0}] andalso 
       api_refac:bound_vars(Var@) == []
     ).
