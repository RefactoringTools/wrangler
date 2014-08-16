%%%-------------------------------------------------------------------
%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%% Arithmetic Operators - Covers arithmetic expressions. This refactoring uses Wrangler API and can be found in <em>Wrangler -> Refactor -> gen_refac Refacs -> Symbolic Evaluation -> Arithmetic Operators</em>.
%% <p>
%% The simplifications performed can be divided into two subsections: Arithmetic Simplifications and Arithmetic Calculations.
%% </p>
%% <h4>Arithmetic Simplifications</h4>
%% <p>The following simplifications are covered: </p>
%% <ul>
%%<li><b>Sum by zero</b> - Expressions of type <i>Exp + 0</i> and <i>0 + Exp</i> are transformed into <i>Exp</i>. For example, <i>X + 0</i> is modified to <i>X</i>.</li>
%%<li><b>Subtraction by zero</b> - Subtractions with zero as the second operand are modified. For instance, <i>Y - 0</i> becomes <i>Y</i>. </li>
%%<li><b>Subtraction from zero</b> - Expressions that match <i>0 - Exp</i> are altered to <i>-Exp</i>. To exemplify, <i>0 - Y</i> is changed to <i>-Y</i>. </li>
%%<li><b>Multiplication by one</b> -  Expressions of the formats <i>Exp * 1</i> and <i>1 * Exp</i> are simplified to <i>Exp</i>. To illustrate, <i>Z * 1</i> is modified to <i>Z</i>.</li>
%%<li><b>Multiplication by zero</b> - Multiplications containing zero as an operand are simplified to zero.</li>
%%<li><b>Division by one</b> - Divisions by one, using the operator <i>(div)</i>, are replaced by the numerator. For example, <i>3 div 1</i> is transformed to <i>3</i>.</li>
%% </ul>
%%<p>These simplifications can by applied to any valid Erlang expression.</p>
%% <h4>Arithmetic Calculations</h4>
%% <p>Simplification of basic arithmetic operations involving integer literals or arbitrary expressions. </p>
%% <p> 
%% The four basic arithmetic operators plus the operator <i> rem </i> are covered for integers, while only <i> + </i> and <i> - </i> are simplified for arbitrary expressions.
%% Some examples of simplifications are given below:
%%</p>
%% <ul>
%% <li>
%% <i>(1 + 2 * 3 - 2) div 1</i> becomes <i>5</i>.
%%</li>
%% <li>
%%<i>foo(X) + 2 * foo(X)</i> is simplified to <i>3 * foo(X)</i>.
%%</li>
%% </ul>
%%
%%% @end
%%% Created : 31 Jan 2014 Roberto S. M. de Barros Filho <>
%%%-------------------------------------------------------------------
-module(refac_arithmetics).

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
transform(Args)-> 
    refac:try_call_transform(Args, fun core_arithmetics:rules/2).

  




    

