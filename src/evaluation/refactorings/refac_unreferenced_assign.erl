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
-export([first_transform/3,second_transform/4,transform_unref_assign/3]).
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
transform(Args=#args{current_file_name=File,
		     user_inputs=[RefacScopeStr],search_paths=SearchPaths}) ->
    Files = refac:get_files(RefacScopeStr,SearchPaths,File),
    transform_unref_assign(Files,RefacScopeStr,Args).
	    
transform_unref_assign(Files,RefacScopeStr,Args) ->
    case refac:validate_refac_scope(RefacScopeStr,Args) of
	{error,Reason} -> {error, Reason};
	_ ->
	    first_transform(Files, RefacScopeStr,Args)
    end.

first_transform(Files,RefacScopeStr,_Args=#args{focus_sel=FunDef}) ->
    RefacScope = refac:get_refac_scope(RefacScopeStr),
    CollectResult = collector(Files),
    MFA = refac:fun_define_info(RefacScope, FunDef),
    Result = ?STOP_TD_TP((rules(CollectResult, {RefacScope, MFA})), Files),
    second_transform(Result,RefacScope,MFA,false).

second_transform(Result,RefacScope,MFA,IsFirst) ->
    case Result of
	{ok, ListOfRefacs} ->
	    {ok, second_transform_fun(Result, RefacScope, MFA,IsFirst,ListOfRefacs)};  
	 _ ->
	    Result
   end.

second_transform_fun(Result, RefacScope, MFA,IsFirst,ListOfRefacs) ->
    lists:map(fun(FileTuple) -> second_transform_file(Result,FileTuple,RefacScope,MFA,IsFirst) end, ListOfRefacs).

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
    CollectResult = collector_file(Scope,FileName),
    Result = ?STOP_TD_TP((rules(CollectResult, FunInfo)), Scope),
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
    [variable_assignment_rule_clause(CollectResult, FunInfo),variable_assignment_rule_outer(CollectResult, FunInfo)].

variable_assignment_rule_outer(CollectResult, {RefacScope, MFA}) ->
    ?RULE(
       ?T("f@(Args@@) when Guards@@ -> Body@@;"),
       begin
	   FunMFA = api_refac:fun_define_info(_This@),
	   {M,_,_} = FunMFA,
	   {_,InfoList} = lists:keyfind(M,1,CollectResult),
	   {{M,_,_}, Info} = lists:keyfind(FunMFA,1,InfoList),
	   NewBody@@ = variable_assignment_refactoring(Info,Body@@),
	   ?TO_AST("f@(Args@@) when Guards@@ -> NewBody@@;")
       end,
       begin
	FunMFA = api_refac:fun_define_info(f@),
        (FunMFA /= unknown andalso
        (RefacScope /= function orelse
	  MFA == FunMFA)
         andalso
	 begin
	     {M,F,A} = FunMFA,
	     case lists:keyfind(M,1,CollectResult) of
		 {M,InfoList} ->
		     case lists:keyfind(FunMFA,1,InfoList) of
			 {{M,F,A}, Info} ->
			   final_cond_variable_assignment(Info,Body@@);
			 _ -> false
		     end;	       
		 _ -> false
	     end
         end)
      end
    ).
final_cond_variable_assignment(Info,Scope) ->
          ?STOP_TD_TU([variable_assignment_collect_inner(Info)],Scope) /= [].

variable_assignment_refactoring(Info,Scope) ->   
    Result = ?STOP_TD_TP([variable_assignment_rule_inner(Info)],Scope),
    case Result of
	{ok, NewNode} -> NewNode;
	_ -> Scope
    end.

variable_assignment_collect_inner(Info) ->
    ?COLLECT(
       ?T("Stmt0@@, Var@ = Expr@, Stmt@@"),
       collected,
       variable_assignment_inner_cond(Var@,Info)
    ).

variable_assignment_rule_inner(Info) ->
    ?RULE(
       ?T("Stmt0@@, Var@ = Expr@, Stmt@@"),
      case Stmt@@ of	   
	   [] ->
	       ?TO_AST("Stmt0@@, Expr@");
	   _ -> 	   
	       ?TO_AST("Stmt0@@, Stmt@@")
       end,
      variable_assignment_inner_cond(Var@,Info)
    ).

variable_assignment_inner_cond(Var@,Info) ->
    api_refac:type(Var@) == variable andalso
    lists:filter(fun(Elem) -> Elem == api_refac:bound_vars(Var@) end, Info) == [].

variable_assignment_rule_clause(CollectResult, {RefacScope, MFA}) ->
    ?RULE(
       ?T("f@(Args@@) when Guards@@ -> Stmt0@@, Var@ = Expr@, Stmt@@;"),
       case Stmt@@ of	   
	   [] ->
	       ?TO_AST("f@(Args@@) when Guards@@ -> Stmt0@@, Expr@;");
	   _ -> 	   
	       ?TO_AST("f@(Args@@) when Guards@@ -> Stmt0@@, Stmt@@;")
       end,
       begin
	FunMFA = api_refac:fun_define_info(f@),
        (FunMFA /= unknown andalso
	  api_refac:type(Var@) == variable andalso
         (RefacScope /= function orelse
	  MFA == FunMFA)
         andalso
	 begin
	     {M,F,A} = FunMFA,
	     case lists:keyfind(M,1,CollectResult) of
		 {M,InfoList} ->
		     case lists:keyfind(FunMFA,1,InfoList) of
			 {{M,F,A}, Info} ->
			     lists:filter(fun(Elem) -> Elem == api_refac:bound_vars(Var@) end, Info) == [];
			 _ -> false
		     end;
		 _ -> false
	     end
         end)
      end
   ).

collector(Files) ->
    lists:map(fun(Scope) -> 
		      {ok,RefacModule} = api_refac:module_name(Scope),
		      {RefacModule,collector_file_traversal([Scope])} end, Files).

collector_file(Scope,FileName) ->
    {ok,RefacModule} = api_refac:module_name(FileName),
    [{RefacModule, collector_file_traversal(Scope)}].

collector_file_traversal(Scope) ->
    ?FULL_TD_TU(    
		[collect()],
		Scope
    ).

collector_variable_occurrences(Scope) ->
    ?FULL_TD_TU(    
		[collect_variables_occurrences()],
		Scope
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

collect() ->
    ?COLLECT(
       ?T("f@(Args@@) when Guards@@ -> Body@@;"),
       {api_refac:fun_define_info(f@), collector_variable_occurrences(Body@@)},
       api_refac:fun_define_info(f@) /= unknown
    ).
