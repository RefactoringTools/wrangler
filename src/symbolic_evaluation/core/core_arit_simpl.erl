%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc
%% Arithmetic Simplifications Core - Basic straightforward arithmetic simplifications. The following simplifications are covered: 
%% <ul>
%%<li><b>Sum by zero</b> - Expressions of type <i>Exp + 0</i> and <i>0 + Exp</i> are transformed into <i>Exp</i>. For example, <i>X + 0</i> is modified to <i>X</i>.</li>
%%<li><b>Subtraction by zero</b> - Subtractions with zero as the second operand are modified. For instance, <i>Y - 0</i> becomes <i>Y</i>. </li>
%%<li><b>Subtraction from zero</b> - Expressions that match <i>0 - Exp</i> are altered to <i>-Exp</i>. To exemplify, <i>0 - Y</i> is changed to <i>-Y</i>. </li>
%%<li><b>Multiplication by one</b> -  Expressions of the formats <i>Exp * 1</i> and <i>1 * Exp</i> are simplified to <i>Exp</i>. To illustrate, <i>Z * 1</i> is modified to <i>Z</i>.</li>
%%<li><b>Multiplication by zero</b> - Multiplications containing zero as an operand are simplified to zero.</li>
%%<li><b>Division by one</b> - Divisions by one, using the operator <i>(div)</i>, are replaced by the numerator. For example, <i>3 div 1</i> is transformed to <i>3</i>.</li>
%% </ul>
%%
%% These simplifications can by applied to any valid Erlang expression.
%%% @end
%%%-------------------------------------------------------------------
-module(core_arit_simpl).

%% Include files
-include_lib("../../../include/wrangler.hrl").
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
%% @spec rules(term(), term()) -> [rule()]
%% @end
%%--------------------------------------------------------------------
-spec(rules(_,_) -> [{'rule',fun(),list() | tuple()},...]).
rules(_,_) ->
    [   
     subZero_rule_1(),
     subZero_rule_2(),         
     sumZero_rule_1(),
     sumZero_rule_2(),   
     multByOne_rule_1(),
     multByOne_rule_2(),
     multByZero_rule_1(),
     multByZero_rule_2(),
     divByOne_rule()
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
sumZero_rule_1() ->
    ?RULE(?T("Expr@ + 0"),
	  Expr@,
	  true
	).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function represents a rule that substitutes an expression of type "0 + sub-expression" by "sub-expression". e.g: <i>"0 + Exp"</i> is <b>transformed</b> to <i>"Exp"</i>.
%% @end
%%--------------------------------------------------------------------
sumZero_rule_2() ->
    ?RULE(?T("0 + Expr@"),
	 Expr@,
	 true).

%%--------------------------------------------------------------------
%%@private
%% @doc
%% This function represents a rule that substitutes an expression of type "sub-expression - 0" by "sub-expression". e.g: <i>"Exp - 0"</i> is <b>transformed</b> to <i>"Exp"</i>.
%% @end
%%--------------------------------------------------------------------
subZero_rule_1() ->
    ?RULE(?T("Expr@ - 0"),
	  Expr@,
	 true).

%%--------------------------------------------------------------------
%%@private
%% @doc
%% This function represents a rule that substitutes an expression of type "0 - sub-expression" by "-sub-expression". e.g: <i>"0 - Exp"</i> is <b>transformed</b> to <i>"-Exp"</i>.
%% @end
%%--------------------------------------------------------------------
subZero_rule_2() ->
    ?RULE(?T("0 - Expr@"),
	  ?TO_AST("-Expr@"),
	  true).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function represents a rule that substitutes an expression of type "sub-expression * 0" by "0". e.g: <i>"Exp * 0"</i> is <b>transformed</b> to <i>"0"</i>.
%% @end
%%--------------------------------------------------------------------
multByZero_rule_1() ->
    ?RULE(?T("Expr@ * 0"),
	 ?TO_AST("0"),
	 true).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function represents a rule that substitutes an expression of type "0 * sub-expression" by "0". e.g: <i>"0 * Exp"</i> is <b>transformed</b> to <i>"0"</i>.
%% @end
%%--------------------------------------------------------------------
multByZero_rule_2() ->
    ?RULE(?T("0 * Expr@"),
	  ?TO_AST("0"),
	  true).

%%--------------------------------------------------------------------
%%@private
%% @doc
%% This function represents a rule that substitutes an expression of type "sub-expression * 1" by "sub-expression". e.g: <i>"Exp * 1"</i> is <b>transformed</b> to <i>"Exp"</i>.
%% @end
%%--------------------------------------------------------------------
multByOne_rule_1() ->
    ?RULE(?T("Expr@ * 1"),
	 Expr@,
	 true).

%%--------------------------------------------------------------------
%%@private
%% @doc
%% This function represents a rule that substitutes an expression of type "1 * sub-expression" by "sub-expression". e.g: <i>"1 * Exp"</i> is <b>transformed</b> to <i>"Exp"</i>.
%% @end
%%--------------------------------------------------------------------
multByOne_rule_2() ->
    ?RULE(?T("1 * Expr@"),
	  Expr@,
	  true).

%%--------------------------------------------------------------------
%%@private
%% @doc
%% This function represents a rule that substitutes an expression of type "sub-expression div 1" by "sub-expression". e.g: <i>"Exp div 1"</i> is <b>transformed</b> to <i>"Exp"</i>.
%% @end

%%--------------------------------------------------------------------
divByOne_rule() ->
    ?RULE(?T("Expr@ div 1"),
	  Expr@,
	  true).





