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
	    Files = refac:get_files(RefacScope,SearchPaths,File,DefinitionsStr),
	    CollectResult = refac_unreferenced_assign:collector(Files),
            MFA = refac:fun_define_info(RefacScope, FunDef),
	    DefsTupleList = refac:get_definitions_tuplelist(DefinitionsStr,SearchPaths),
	    case refac_funApp:getDefinitionsInfo(DefsTupleList) of
		{error, Reason} -> {error,Reason};
		DefinitionsInfo ->
		    Pid = spawn(refac, timeout_manager, [[]]),
		    TimeOut = refac:checkTimeOut(TimeOutStr),
		    InternalFiles = refac_funApp:getInternalFiles(Files,DefsTupleList,RefacScope),
		    InfoList = refac_funApp:getInfoList(InternalFiles,DefinitionsInfo),
		    Result = ?STOP_TD_TP(rules_all({fun refac_all:rules/2,TimeOut,InfoList},CollectResult,{RefacScope,MFA},Pid),Files),
		    FinalResult = case Result of
			{ok, ListOfResults} when is_list(ListOfResults) ->
			    refac_loop(Result,RefacScope,MFA,TimeOut,DefinitionsInfo,InternalFiles,Pid);
			_ -> Result
		    end,
		    Pid ! finished,
		    FinalResult	    
	    end
     end.

get_internal_results(project,_,_) -> [];
get_internal_results(_,ListOfResults,InternalFiles) ->
    lists:filter(fun({{FileName,FileName},_}) -> lists:any(fun(FileName2) -> FileName == FileName2 end,InternalFiles) end,ListOfResults).
    							
refac_loop(Result,RefacScope,MFA,TimeOut,DefinitionsInfo,InternalFiles,ManagerPid) ->
    refac_loop(Result,RefacScope,MFA,TimeOut,DefinitionsInfo,InternalFiles,ManagerPid,[]).

refac_loop({ok, ListOfResults},RefacScope,MFA,TimeOut,DefinitionsInfo,InternalFiles,ManagerPid,RemainingRefacs) when is_list(ListOfResults) ->
     Result2 = refac_unreferenced_assign:second_transform({ok, ListOfResults},RefacScope,MFA,true),
     case Result2 of
	 {ok,ListOfResults2} ->
	             FilteredResults = get_internal_results(RefacScope,ListOfResults2,InternalFiles),
		     Result3 = ?FULL_TD_TP(refac:body_rules(fun refac_all:rules/2, {RefacScope, MFA}, TimeOut, refac_funApp:getInfoList(FilteredResults,DefinitionsInfo),ManagerPid),ListOfResults2),
		     case Result3 of
			 {ok,ListOfResults3} ->
			      {ListToRefac,RemainingRefacs2} = lists:partition(fun({FileNameTuple,Node}) -> 										
										{FileNameTuple,Node2} = lists:keyfind(FileNameTuple,1,ListOfResults),
										      
										?PP(Node) /= ?PP(Node2)
								     end,ListOfResults3),
			     if
				 ListToRefac /= [] -> 			    
				     refac_loop({ok,ListToRefac},RefacScope,MFA,TimeOut,DefinitionsInfo,InternalFiles,ManagerPid,RemainingRefacs ++ RemainingRefacs2);
				 true -> {ok, RemainingRefacs ++ ListOfResults3}
			     end;
			  _ -> Result3
		     end;
	 _ -> Result2
     end.

rules_all({RulesFun, TimeOut,Info}, UnrefArgs,FunInfo,ManagerPid) ->
    refac:body_rules(RulesFun,FunInfo,TimeOut,Info,ManagerPid) ++ refac_unreferenced_assign:rules(UnrefArgs,FunInfo).

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




    

