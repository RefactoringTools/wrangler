%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Simon  Thompson
%%% @doc 
%%This module covers refactorings with expressions. These expressions can contain <b>variables and/or integers</b>.
%%<p>The code consists basically of applying two rules: <b>arit_rule</b> (manipulates with integers) and <b>variable_rule</b> (does some math with variables).</p>
%%<p> If the expression is valid according to the conditions and  matches with any template of the rules, the expression is then transformed to other expression or a result. </p>
%%<p>
%% Unfortunately, there are some cases that are still not being  covered.  
%%<ul><li>For instance, if an expression contains integers and variables interchanged, as <i>"1 + X + 1"</i>. In this case, this expression is not simplified anymore.</li>
%%<li> Any multiplication or division between variables is also not being covered.</li> </ul></p>
%%<p>However, if the expression contains integers and variables but they are not mixed, as <i>"1 + 2 + 3 + X + 2*X + Y"</i>, the refactoring can be done and the expression <b>becomes</b> <i>"6 + 3 * X + Y"</i></p>

%%%
%%% @end
%%% Created : 18 Oct 2013 by Gabriela Cunha, Roberto Souto <>
%%%-------------------------------------------------------------------
-module(eval_arit_calc).

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
%% Check the pre-conditions of the refactoring. The user will type a number (the number of steps that will be executed in the refactoring or the letter <i>f</i> to execute all of the steps in one time. This entry needs to be checked, because it can be only a integer or the letter f. If the user types anything different, an error message is given and the refactoring is not executed.
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
%% @spec(selective() -> boolean())
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
transform(Args=#args{user_inputs=[E,I], search_paths=_SearchPaths})->
    eval_all:transform(Args,E,I,eval_arit_calc,nil).




