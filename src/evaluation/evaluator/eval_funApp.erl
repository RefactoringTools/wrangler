%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Simon  Thompson
%%% @doc This module contains refactorings that substitute function calls by its application. 
%%<p>In the present, there is only one refactoring rule on this module: <b>functionCall_rule/1</b></p> 
%%%
%%% @end
%%% Created : 18 Oct 2013 by Gabriela Cunha Sampaio, Roberto Souto Maior de Barros F <>
%%%-------------------------------------------------------------------
-module(eval_funApp).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1,rules/2]).

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
%% Checks the pre-conditions of the refactoring. The user will type a number (the number of steps that will be executed in the refactoring or the letter <i>f</i> to execute all of the steps in one time. This entry needs to be checked, because it can be only a integer or the letter f. If the user types anything different, an error message is given and the refactoring is not executed.
%%
%% @spec(check_pre_cond(Args::args{}) -> ok | {error, Reason})
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
    eval_all:transform(Args,E,I,eval_funApp,nil).
    

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%@private
rules({_,_,BoundVars},Info) ->
    [   
        core_funApp:functionCall_rule(Info,unknown,false,BoundVars),
        core_funApp:anonymousCall_rule()
    ]. 



