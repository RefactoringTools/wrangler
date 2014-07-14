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
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prompts for parameter inputs
%%
%% @spec input_par_prompts() -> [string()]
%% @end
%%--------------------------------------------------------------------
input_par_prompts() -> ["Would you like to apply this refactoring to the entire file? (y/n)"].

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
select_focus(_Args=#args{current_file_name=File, 
                         cursor_pos=Pos, user_inputs=[EntireFileStr]}) ->
    RefacBool = refac:str_to_bool(EntireFileStr),
    case RefacBool of
	true -> {ok, none};
	_ -> api_interface:pos_to_fun_def(File, Pos)
    end.

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
		     user_inputs=[EntireFileStr],focus_sel=FunDef}) -> 
	    CheckedFileBool = refac:str_to_bool(EntireFileStr),
	    if
		CheckedFileBool == true orelse CheckedFileBool == false ->
		    MFA = 
			if
			    CheckedFileBool == true -> notrelevant;
			    true -> api_refac:fun_define_info(FunDef)
			 end,		    
		    if
			 MFA /= unknown ->
			    CollectResult = collect(File),
			    Result = ?STOP_TD_TP(rules(CollectResult, {CheckedFileBool, MFA}), [File]),
			    second_transform(Result,File,CheckedFileBool,MFA);
			 true -> {error, "Please, place the mouse cursor on the desired function!"}
		    end;
		true -> {error, "Please, answer 'y' or 'n'!"}
	    end.

second_transform(Result,File,CheckedFileBool,MFA) ->
    case Result of
	{ok, [{{FileName, FileName}, Node}]} -> 
	      FileNode = api_refac:get_ast(File),
	      case FileNode of
		   {error, _Reason} -> 
		      Result;
		   _ ->
		      try_transform_recursively(FileNode /= Node, Node, FileName, {CheckedFileBool, MFA})
	      end;
	 _ ->
	      Result
   end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to apply the transformation as much as possible.
%% @end
%%--------------------------------------------------------------------
transform_recursively([{{FileName, FileName}, Scope}], FunInfo) ->
    CollectResult = collect(Scope),
    Result = ?STOP_TD_TP(rules(CollectResult, FunInfo), Scope),
    case Result of
	{ok, Node} -> 	       
	    try_transform_recursively(?PP(Node) /= ?PP(Scope), Node, FileName, FunInfo);
	_ ->
	 {ok, [{{FileName, FileName},Scope}]}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if transformations can still be done.
%% @end
%%--------------------------------------------------------------------
try_transform_recursively(Changed, Node, FileName, FunInfo) ->
    TransformResult = [{{FileName, FileName}, Node}],
    if
	Changed -> transform_recursively(TransformResult, FunInfo);
	true -> {ok, TransformResult}
    end.
	    
rules(CollectResult, FunInfo) ->    
    [variable_assignment_rule(CollectResult, FunInfo)].

variable_assignment_rule(CollectResult, {RefacWholeFile, MFA}) ->
    ?RULE(
       ?T("f@(Args@@) when Guards@@ -> Stmt0@@, Var@ = Expr@, Stmt@@;"),
       case Stmt@@ of	   
	   [] -> ?TO_AST("f@(Args@@) when Guards@@ -> Stmt0@@, Expr@;");
	   _ -> ?TO_AST("f@(Args@@) when Guards@@ -> Stmt0@@, Stmt@@;")
       end,
        api_refac:type(Var@) == variable andalso
       (RefacWholeFile orelse
	MFA == api_refac:fun_define_info(f@)
       ) andalso
       lists:filter(fun(Elem) -> Elem == api_refac:bound_vars(Var@) end, CollectResult) == []
   ).

collect(Scope) ->
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
