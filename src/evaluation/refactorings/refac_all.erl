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
		     user_inputs=[_,EntireFileStr,_],focus_sel=FunDef})-> 
    Result = refac_funApp:transform_funApp(Args, fun refac_all:rules/2),
    case Result of
	{ok,[]} -> 
	    refac_unreferenced_assign:transform_unref_assign(File,EntireFileStr,FunDef);	    
	_ ->	    
	   CheckedFileBool = refac:str_to_bool(EntireFileStr),
	   refac_unreferenced_assign:second_transform(Result,File,CheckedFileBool,api_refac:fun_define_info(FunDef),true)
     end.

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




    

