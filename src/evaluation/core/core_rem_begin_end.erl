%%%-------------------------------------------------------------------
%%% @author Roberto Souto Maior de Barros Filho, Gabriela Cunha Sampaio <>
%%% @copyright (C) 2013, Roberto S. M. de Barros Filho, Gabriela Cunha Sampaio, Simon  Thompson
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
-module(core_rem_begin_end).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).
%%--------------------------------------------------------------------
%% @doc
%% Returns the list of arithmetic simplification rules. This list includes, in order, rules for the following possibilities:
%%<ul>
%%<li>Sub by zero</li>
%%<li>Sub from zero</li>
%%<li>Sum by zero</li>
%%<li>Multiplication by one rule</li>
%%<li>Multiplication by zero </li>
%%<li>Division by one </li>
%%</ul>
%% @end
%% @private
%%--------------------------------------------------------------------
rules(_,_) ->
    [   
     remove_begin_end_rule()
    ]. 

%%%===================================================================
%%% Functions with rules
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function represents a rule that substitutes an expression of type "sub-expression + 0" by "sub-expression". e.g: <i>"Exp + 0"</i> is <b>transformed</b> to <i>"Exp</i>.
%% @end
%%--------------------------------------------------------------------
remove_begin_end_rule() ->
    ?RULE(?T("begin Arg@ end"),
	  Arg@,
	  true
	).







