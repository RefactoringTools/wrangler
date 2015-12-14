%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%% Remove Unreferenced Assignments - Removes assignments when the variables are not used afterwards. This refactoring uses Wrangler API and can be found in <em>Wrangler -> Refactor -> gen_refac Refacs -> Symbolic Evaluation -> Remove Unreferenced Assignments</em>.
%%
%% For example, the expression: <br/>
%% <em>
%% begin <br/>
%% <div class="first_align">
%%  A = 10, <br/>
%%  B = 20, <br/>
%%  C = 30, <br/>
%%  E = A + B + C, <br/>
%%  A <br/>
%% </div>
%% end. <br/>
%% </em>
%% can be simplified by this refactoring to: <br/>
%% <em>
%% begin <br/>
%% <div class="first_align">
%%  A = 10, <br/>
%%  A <br/>
%% </div>
%% end. <br/>
%% </em>
%% @end
-module(refac_unreferenced_assign).

-behaviour(gen_refac).

%% Include files
-include_lib("../../../include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1]).
-export([first_transform/3,second_transform/4,transform_unref_assign/3,collector/1,rules/2]).
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

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	    
transform_unref_assign(Files,RefacScopeStr,Args) ->
    case refac:validate_refac_scope(RefacScopeStr,Args) of
	{error,Reason} -> {error, Reason};
	_ ->
	    first_transform(Files, RefacScopeStr,Args)
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
first_transform(Files,RefacScopeStr,_Args=#args{focus_sel=FunDef}) ->
    RefacScope = refac:get_refac_scope(RefacScopeStr),
    CollectResult = collector(Files),
    MFA = refac:fun_define_info(RefacScope, FunDef),
    Result = ?STOP_TD_TP(
             (rules(CollectResult, {RefacScope, MFA})), 
              Files),
    second_transform(Result,RefacScope,MFA,false).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
second_transform(Result,RefacScope,MFA,IsFirst) ->
    case Result of
	{ok, ListOfRefacs} ->
	    {ok, second_transform_fun(Result, RefacScope, MFA,IsFirst,ListOfRefacs)};  
	 _ ->
	    Result
   end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
second_transform_fun(Result, RefacScope, MFA,IsFirst,ListOfRefacs) ->
    lists:map(fun(FileTuple) -> second_transform_file(Result,FileTuple,RefacScope,MFA,IsFirst) end, ListOfRefacs).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
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
	
%%--------------------------------------------------------------------
%%@doc
%%List of rules that remove unreferenced assignments for this refactoring. The following rules exist:
%%<ul>
%%<li>Removes unreferenced assignments in the list of steps of the function clause.</li>
%%<li>Removes unreferenced assignments in more internal structures, like list of steps within if/case expressions.</li>
%%</ul>
%%@spec rules(CollectResult::{modulename(),{mfa() | unknown,[syntaxTree()],[[syntaxTree()]],[[{atom(), pos()}]],[syntaxTree()]}}, FunInfo::{function | project | file, mfa() | unknown}) -> [rule()]
%%@end
%%--------------------------------------------------------------------	  
-spec(rules(CollectResult::{modulename(),{mfa() | unknown,[syntaxTree()],[[syntaxTree()]],[[{atom(), pos()}]],[syntaxTree()]}}, FunInfo::{function | project | file, mfa() | unknown}) -> [{'rule',fun(),list() | tuple()},...]).   
rules(CollectResult, FunInfo) ->    
    [variable_assignment_rule_clause(CollectResult, FunInfo),variable_assignment_rule_outer(CollectResult,FunInfo)].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
variable_assignment_rule_outer(CollectResult,{RefacScope, MFA}) ->
    ?RULE(
       ?T("f@(Args@@) when Guards@@ -> Body@@;"),
       begin
	   NewBody@@ = variable_assignment_refactoring(Body@@, fun core_unreferenced_assign:variable_assignment_rule/1,0),
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
			 {{M,F,A}, ArgsList,GuardsList,BodyInfoList} ->
			   case  get_variable_occurrences(ArgsList,GuardsList,BodyInfoList,Args@@,Guards@@) of
			    noMatch -> false;
			    Info ->
				   final_cond_variable_assignment(Info,Body@@)
			   end;
			 _ -> false
		     end;	       
		 _ -> false
	     end
         end)
      end
    ).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
final_cond_variable_assignment(Info,Scope) ->
          ?STOP_TD_TU([variable_assignment_collect_inner(Info)],Scope) /= [].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
variable_assignment_refactoring(Scope,Fun,LoopInt) ->   
    Info = core_unreferenced_assign:collector_variable_occurrences(Scope),
    Result = ?STOP_TD_TP([Fun(Info)],Scope),
    case Result of
	{ok, NewNode} -> 
	    Changed = ?PP(NewNode) /= ?PP(Scope),
	    if
		Changed -> 
		    variable_assignment_refactoring(NewNode, Fun, LoopInt);
		true -> 
		    case LoopInt of
			0 -> 
			    variable_assignment_refactoring(Scope, fun core_unreferenced_assign:variable_assignment_rule_begin/1, 1);
			_ ->
			    Scope
		    end
	    end;
	_ -> Scope
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
variable_assignment_collect_inner(Info) ->
    ?COLLECT(
       ?T("Stmt0@@, Var@ = Expr@, Stmt@@"),
       collected,
       core_unreferenced_assign:variable_assignment_cond(Var@,Info)
    ).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
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
			 {{M,F,A}, ArgsList,GuardsList,BodyInfoList} ->
			     check_variable_occurrences(ArgsList,GuardsList,BodyInfoList,Args@@,Guards@@,Var@);
			     
			 _ -> false
		     end;
		 _ -> false
	     end
         end)
      end
   ).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
check_variable_occurrences(ArgsList,GuardsList,BodyList,Args@@,Guards@@,Var@) ->
    case get_variable_occurrences(ArgsList,GuardsList,BodyList,Args@@,Guards@@) of
	noMatch -> false;
	Info -> 
	    lists:filter(fun(Elem) -> Elem == api_refac:bound_vars(Var@) end, Info) == []			
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
get_variable_occurrences([],[],[],_,_) -> noMatch;
get_variable_occurrences([Args | ArgsTail],[Guards | GuardsTail],[Body | BodyTail],Args@@,Guards@@) ->
    if 
	Args == Args@@ andalso Guards == Guards@@ -> Body;
	true ->
	    get_variable_occurrences(ArgsTail, GuardsTail,BodyTail,Args@@,Guards@@)
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
collector(Files) ->
    lists:map(fun(Scope) -> 
		      {ok,RefacModule} = api_refac:module_name(Scope),
		      {RefacModule,collector_file_traversal([Scope])} end, Files).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
collector_file(Scope,FileName) ->
    {ok,RefacModule} = api_refac:module_name(FileName),
    [{RefacModule, collector_file_traversal(Scope)}].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
collector_file_traversal(Scope) ->
    ?FULL_TD_TU(    
		[collect()],
		Scope
    ).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
collect() ->
    ?COLLECT(
       ?T("f@(Args@@@) when Guards@@@ -> Body@@@"),
       {api_refac:fun_define_info(f@),Args@@@, Guards@@@, lists:map(fun(Body@@) -> core_unreferenced_assign:collector_variable_occurrences(Body@@) end,Body@@@)},
       api_refac:fun_define_info(f@) /= unknown
    ).
