%%%-------------------------------------------------------------------
%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Roberto S. M. de Barros Filho, Simon Thompson
%%% @doc
%% These refactoring does arithmetic simplification modifications.
%% <p>We can include here the following cases:
%% <ul>
%%<li>Sum by zero - Expressions of type "Exp + 0" and "0 + Exp" are transformed into "Exp". For example, <i>X + 0</i> is modified to <i>X</i>.</li>
%%<li>Subtraction by zero - Subtractions with zero as the second operand are modified. For instance, <i>Y - 0</i> becomes <i>Y</i>. </li>
%%<li>Subtraction from zero - Expressions that match "0 - Exp" are altered to "-Exp". To exemplify, <i>0 - Y</i> is changed to <i>-Y</i>. </li>
%%<li>Multiplication by one -  Expressions of the formats "Exp * 1" and "1 * Exp" are simplified to "Exp". To illustrate, <i>Z * 1</i> is modified to <i>Z</i>.</li>
%%<li>Multiplication by zero - Multiplications containing zero as an operand are simplified to zero.</li>
%%<li>Division by one - Divisions by one, using the operator <i>(div)</i>, are replaced by the numerator. For example, <i>3 div 1</i> is transformed to <i>3</i>.</li>
%% </ul>
%%%</p>
%%<p>These refactorings can only by applied to integers, variables and expressions formed by these two types.</p>
%%% @end
%%%-------------------------------------------------------------------
-module(refac_arit_simpl).

-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0,transform/1]).
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
input_par_prompts() -> refac:input_par_prompts().
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
transform(Args)-> 
    refac:try_call_transform(Args, fun core_arit_simpl:rules/2).
