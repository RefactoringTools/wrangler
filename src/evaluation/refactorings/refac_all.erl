%%%-------------------------------------------------------------------
%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%%This module was created with the aim of <b>composing some refactorings and applying all their rules toghether</b>. Thus, this refactoring just calls others refactorings (arithmetics and function applications). 
%%% @end
%%% Created : 18 Oct 2013 by Gabriela Cunha, Roberto Souto <>
%%%-------------------------------------------------------------------
-module(refac_all).

-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1,rules/2]).

%%%===================================================================
%%% gen_refac callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prompts for parameter inputs
%%
%% @spec input_par_prompts() -> [string()]
%% @end
%%--------------------------------------------------------------------
input_par_prompts() -> refac_funApp:input_par_prompts().

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
select_focus(Args) -> refac:select_focus(Args).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec(check_pre_cond(_Args::args{}) -> ok | {error, Reason})
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
		     user_inputs=[TimeOutStr,RefacScopeStr,DefinitionsStr],search_paths=SearchPaths,focus_sel=FunDef}) ->
    case refac:validate_all(TimeOutStr, RefacScopeStr,DefinitionsStr, Args) of
	{error, Reason} -> {error, Reason};
	_ ->
	    RefacScope = refac:get_refac_scope(RefacScopeStr),
	    Files = refac:get_files(RefacScope,SearchPaths,File),
	    case refac_unreferenced_assign:first_transform(Files,RefacScopeStr,Args) of
		{ok, ListOfResults} when is_list(ListOfResults) ->	    
		     case refac_funApp:getDefinitionsInfo(DefinitionsStr, SearchPaths) of
			 {error, Reason} -> {error,Reason};
			 DefinitionsInfo ->
			     Result2 = transform_refacs(ListOfResults,Files,DefinitionsInfo,Args),
			     case Result2 of
				 {ok, ListOfResults2} when is_list(ListOfResults2) ->
					  refac_loop(Result2,RefacScope,refac:fun_define_info(RefacScope,FunDef),refac:checkTimeOut(TimeOutStr),DefinitionsInfo);	 
				 _ -> Result2
			     end
		     end;
		Result -> Result
	    end
    end.

refac_loop(Result,RefacScope,MFA,TimeOut,DefinitionsInfo) ->
     Result2 = refac_unreferenced_assign:second_transform(Result,RefacScope,MFA,true),
     case Result2 of
	 {ok,ListOfResults2} ->
		     Result3 = ?FULL_TD_TP(refac:body_rules(fun refac_all:rules/2, {RefacScope, MFA}, TimeOut, refac_funApp:getInfoList(ListOfResults2,DefinitionsInfo)),ListOfResults2),
		     case Result3 of
			 {ok,_} ->
			     if
				 Result /= Result3 -> 
				     refac_loop(Result3,RefacScope,MFA,TimeOut,DefinitionsInfo);
				 true -> Result3
			     end;
			  _ -> Result3
		     end;
	 _ -> Result2
     end.

concat_results(ListOfResults, NewResult) ->
    case NewResult of
	{ok,ListOfResults2} when is_list(ListOfResults2) ->
		{ok, ListOfResults ++ ListOfResults2};
	_ -> NewResult
    end.

transform_refacs(ListOfResults,Files,DefinitionsInfo,Args=#args{user_inputs=[TimeOutStr,RefacScopeStr,_],focus_sel=FunDef}) ->
    TimeOut = refac:checkTimeOut(TimeOutStr),
    RefacScope = refac:get_refac_scope(RefacScopeStr),
    MFA = refac:fun_define_info(RefacScope,FunDef),
    Result2 = ?FULL_TD_TP(refac:body_rules(fun refac_all:rules/2, {RefacScope, MFA}, TimeOut, refac_funApp:getInfoList(ListOfResults,DefinitionsInfo)),ListOfResults),
    case Result2 of
       {ok,ListOfResults2} when is_list(ListOfResults2) ->
	    FilteredFiles = filter_unused_files(ListOfResults2, Files),
	    Result3 = case FilteredFiles of
		[] -> {ok,[]};
		_ ->		
		      refac:start_transformation(RefacScopeStr,fun refac_all:rules/2,TimeOutStr,refac_funApp:getInfoList(FilteredFiles,DefinitionsInfo),Args,FilteredFiles)   
		     end,
	    concat_results(ListOfResults2, Result3);
       _ -> Result2
    end.

filter_unused_files(ListOfResults,Files) ->
    lists:filter(fun(FileName) -> lists:keyfind({FileName, FileName},1,ListOfResults) == false end,Files).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function calls the rules from the other refactorings.
%%--------------------------------------------------------------------
rules(FunArgs, FunDefInfo) ->
     refac_funApp:rules(FunArgs,FunDefInfo) ++ 
     core_arithmetics:rules([], FunDefInfo) ++ 
     core_boolean_operators:rules(FunArgs, FunDefInfo) ++ 
     core_lists_concat:rules(FunArgs,FunDefInfo) ++
     core_if:rules(FunArgs,FunDefInfo) ++
     core_case:rules(FunArgs, FunDefInfo).




    

