%%%-------------------------------------------------------------------
%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%%This module was created with the aim of <b>composing the arithmetic refactorings and applying all their rules toghether</b>. Thus, this refactoring just calls the other arithmetic refactorings (arithmetics simplification and arithmetic calculations). 
%%<p>For instance, the function clause <i>"f(X) -> 0 + X + 1 + 2;"</i> would be transformed into <i>"f(X) -> X + 3;"</i>. This would be done by the refactoring that does arithmetic simplifications (i.e.: "0 + X" is modified to "X") and also by the arithmetic calculations refactoring (this refactoring would substitute <i>"1 + 2"</i> by <i>"3"</i> obtaining <i>"f(X) -> X + 3;"</i>). </p>
%%<p>For the same example, if the arithmetic simplifications refactoring was used separetely the result would be <i>"f(X) -> X + 1 + 2;"</i>. Additionaly, if the arithmetic calculations refactoring was used individually the return would be <i>"f(X) -> 0 + X + 3;"</i>. That's why the composite refactoring is important, to integrate the others two arithmetic refactorings and create a more powerful single refactoring that contains the other two.</p>  
%%%
%%% @end
%%% Created : 31 Jan 2014 Roberto S. M. de Barros Filho <>
%%%-------------------------------------------------------------------
-module(eval_arithmetics).

-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1]).

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
input_par_prompts() -> eval_all:input_par_prompts().

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
select_focus(Args) -> eval_all:select_focus(Args).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec(check_pre_cond(_Args::args{}) -> ok | {error, Reason})
%% @end
%%--------------------------------------------------------------------
check_pre_cond(Args) -> eval_all:check_pre_cond(Args).

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
transform(Args=#args{current_file_name=_File,user_inputs=[E,I], search_paths=_SearchPaths})->
    eval_all:transform(Args,E,I,eval_arithmetics,nil).

  




    

