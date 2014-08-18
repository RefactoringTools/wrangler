%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%% Arithmetic Operators Core - Covers arithmetic expressions. 
%% 
%% The simplifications performed can be divided into two subsections: Arithmetic Simplifications and Arithmetic Calculations.
%% 
%% <h4>Arithmetic Simplifications</h4>
%% The following simplifications are covered: 
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
%% Simplification of basic arithmetic operations involving integer literals or arbitrary expressions. 
%% 
%% The four basic arithmetic operators plus the operator <i> rem </i> are covered for integers, while only <i> + </i> and <i> - </i> are simplified for arbitrary expressions.
%% Some examples of simplifications are given below:
%% <ul>
%% <li>
%% <i>(1 + 2 * 3 - 2) div 1</i> becomes <i>5</i>.
%%</li>
%% <li>
%%<i>foo(X) + 2 * foo(X)</i> is simplified to <i>3 * foo(X)</i>.
%%</li>
%% </ul>
%% @end
%%%-------------------------------------------------------------------
-module(core_arithmetics).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function calls the rules from the other refactorings.
%%--------------------------------------------------------------------
rules(_A, _B) ->
    core_arit_simpl:rules(_A, _B) ++ core_arit_calc:rules(_A, _B).
  




    

