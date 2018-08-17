%%%-------------------------------------------------------------------
%%% @author  Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Gabriela Cunha Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%% Arithmetic Calculations Core - Simplification of arithmetic operations involving integer literals or arbitrary expressions. 
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
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(core_arit_calc).
%% Include files
-include("wrangler.hrl").

-export([rules/2]).

sum_rule() ->
    ?RULE(
       ?T("Exp@ + Exp2@"),
       do_operation('+', Exp@, Exp2@, false),
       api_refac:type(Exp@) == integer andalso api_refac:type(Exp2@) == integer
    ).

sub_rule() ->
    ?RULE(
       ?T("Exp@ - Exp2@"),
       do_operation('-', Exp@, Exp2@, false),
       api_refac:type(Exp@) == integer andalso api_refac:type(Exp2@) == integer
    ).

mult_rule() ->
    ?RULE(
      ?T("Exp@ * Exp2@"),
       do_operation('*', Exp@, Exp2@, false),
       api_refac:type(Exp@) == integer andalso api_refac:type(Exp2@) == integer
    ).

div_rule() ->
    ?RULE(
       ?T("Exp@ div Exp2@"),
       do_operation('div', Exp@, Exp2@, false),
       api_refac:type(Exp@) == integer andalso api_refac:type(Exp2@) == integer andalso list_to_integer(?PP(Exp2@)) /= 0
    ).

rem_rule() ->
    ?RULE(
      ?T("Exp@ rem Exp2@"),
       do_operation('rem', Exp@, Exp2@, false),
       api_refac:type(Exp@) == integer andalso api_refac:type(Exp2@) == integer
    ).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sums two equal expressions containing variables and returns two times this expression. 
%% <p> E.G: <i>"X + X"</i> is <b>transformed</b> into <i>"2 * X"</i></p>
%% @end
%%--------------------------------------------------------------------
variable_sum_rule_1() ->
    ?RULE(
       ?T("Exp@ + Exp@"),
       ?TO_AST("2 * Exp@"),
       true
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sums two equal expressions containing variables when the expression on the left has a multiplicative factor of N. The result is (N + 1) * Expression.. 
%% <p> E.G: <i>"3 * X + X"</i> is <b>transformed</b> into <i>"4 * X"</i></p>
%% @end
%%--------------------------------------------------------------------
variable_sum_rule_2() ->
    ?RULE(
       ?T("N@ * Exp@ + Exp@"),
       begin
	   N_Plus_1 = integer_to_list(list_to_integer(?PP(N@)) + 1),
	   ?TO_AST(N_Plus_1 ++ " * Exp@")
       end,
       api_refac:type(N@) == integer
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sums two equal expressions containing variables when the expression on the right has a multiplicative factor of N. The result is (N + 1) * Expression.. 
%% <p> E.G: <i>"X + 3 * X"</i> is <b>transformed</b> into <i>"4 * X"</i></p>
%% @end
%%--------------------------------------------------------------------
variable_sum_rule_3() ->
    ?RULE(
       ?T("Exp@ + N@ * Exp@"),
       begin
	   N_Plus_1 = integer_to_list(list_to_integer(?PP(N@)) + 1),
	   ?TO_AST(N_Plus_1 ++ " * Exp@")
       end,
      api_refac:type(N@) == integer
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sums two equal expressions containing variables when both have pottentially different multiplicative factors of N and M respectively. The result is (N + M) * Expression.. 
%% <p> E.G: <i>"2 * X + 3 * X"</i> is <b>transformed</b> into <i>"5 * X"</i></p>
%% @end
%%--------------------------------------------------------------------
variable_sum_rule_4() ->
    ?RULE(
       ?T("N@ * Exp@ + M@ * Exp@"),
      begin
	   N_Plus_M = integer_to_list(list_to_integer(?PP(N@)) + list_to_integer(?PP(M@))),
	   ?TO_AST(N_Plus_M ++ " * Exp@")
       end,
      api_refac:type(N@) == integer andalso api_refac:type(M@) == integer
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Subs two equal expressions containing variables and returns zero. 
%% <p> E.G: <i>"X - X"</i> is <b>transformed</b> into <i>"0"</i></p>
%% @end
%%--------------------------------------------------------------------
variable_sub_rule_1() ->
    ?RULE(
       ?T("Exp@ - Exp@"),
       ?TO_AST("0"),
       true
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Subs two equal expressions containing variables when the first operand has a multiplicative factor of N. 
%% <p> E.G: <i>"2 * X - X"</i> is <b>transformed</b> into <i>"X"</i></p>
%% @end
%%--------------------------------------------------------------------
variable_sub_rule_2() ->
    ?RULE(
       ?T("N@ * Exp@ - Exp@"),
       begin
	   N_Minus_1 = integer_to_list(list_to_integer(?PP(N@)) - 1),
	   ?TO_AST(N_Minus_1 ++ " * Exp@")
       end,
       api_refac:type(N@) == integer
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Subs two equal expressions containing variables when the second operand has a multiplicative factor of N. 
%% <p> E.G: <i>"X - 2 * X"</i> is <b>transformed</b> into <i>"-X"</i></p>
%% @end
%%--------------------------------------------------------------------
variable_sub_rule_3() ->
    ?RULE(
       ?T("Exp@ - N@ * Exp@"),
        begin
	   One_Minus_N = integer_to_list(1 - list_to_integer(?PP(N@))),
	   ?TO_AST(One_Minus_N ++ " * Exp@")
       end,
       api_refac:type(N@) == integer
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Subs two equal expressions containing variables when both operands have multiplicative factors of N and M respectively. 
%% <p> E.G: <i>"3 * X - 2 * X"</i> is <b>transformed</b> into <i>"X"</i></p>
%% @end
%%--------------------------------------------------------------------
variable_sub_rule_4() ->
    ?RULE(
       ?T("N@ * Exp@ - M@ * Exp@"),
       begin
	   N_Minus_M = integer_to_list(list_to_integer(?PP(N@)) - list_to_integer(?PP(M@))),
	   ?TO_AST(N_Minus_M ++ " * Exp@")
       end,
        api_refac:type(N@) == integer andalso api_refac:type(M@) == integer
     ).

%%--------------------------------------------------------------------
%% @doc
%% Returns the list of arithmetics calculations rules. The rules are organised in the following order:
%%<ul>
%%<li>Integer sum</li>
%%<li>Integer subtraction</li>
%%<li>Integer multiplication</li>
%%<li>Integer division</li>
%%<li>Integer rem</li>
%%<li>Arbitrary expressions sum</li>
%%<li>Arbitrary expressions subtraction</li>
%%</ul>
%% @spec rules(term(), term()) -> [rule()]
%% @end
%%--------------------------------------------------------------------
-spec(rules(_,_) -> [{'rule',fun(),list() | tuple()},...]).
rules(_,_) ->
    [   
	sum_rule(),
	sub_rule(),
	mult_rule(),
	div_rule(),
	rem_rule(),
	variable_sum_rule_1(),
	variable_sum_rule_2(),
	variable_sum_rule_3(),
	variable_sum_rule_4(),
	variable_sub_rule_1(),
	variable_sub_rule_2(),
	variable_sub_rule_3(),
	variable_sub_rule_4()
    ]. 

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to do operations with expressions.
%% @spec do_operation(atom(), syntaxTree() | error, syntaxTree()| error,boolean()) -> syntaxTree() | error
%% @end
%%--------------------------------------------------------------------
-spec(do_operation(Op::atom(),Node1::syntaxTree() | error, Node2::syntaxTree()| error,AllowVar::boolean()) -> syntaxTree() | error).
do_operation(_,error,_,_) -> error;
do_operation(_,_,error,_) -> error;
do_operation(Op, Node1, Node2, AllowVar) ->
    case api_refac:type(Node1) of
	integer ->
	    case api_refac:type(Node2) of
		integer ->
		    do_integer_operation(Op,
					 list_to_integer(?PP(Node1)),
					 list_to_integer(?PP(Node2)));
		variable -> createTreeWithVars(Op,Node1,Node2,AllowVar);
		_ -> error
	    end;
	variable ->
		  case api_refac:type(Node2) of
		      integer ->  createTreeWithVars(Op,Node1,Node2,AllowVar);
		      variable -> createTreeWithVars(Op,Node1,Node2,AllowVar);
		      _ -> error
		  end;
	_ -> error
   end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the AST of the expression containing variables if variables are allowed.
%% spec(createTreeWithVars(Op::atom(), Node1::syntaxTree(),Node2::syntaxTree(),AllowVar::boolean()) -> syntaxTree() | error)
%% @end
%%--------------------------------------------------------------------
-spec(createTreeWithVars(Op::atom(), Node1::syntaxTree(),Node2::syntaxTree(),AllowVar::boolean()) -> syntaxTree() | error).
createTreeWithVars(Op, Node1, Node2, true) -> 
    ?TO_AST(?PP(Node1) ++ atom_to_list(Op) ++ ?PP(Node2));
createTreeWithVars(_,_,_,false) -> error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do basic arithmetic between integers. In the case of div by 0, error is returned and no refactoring is done.
%% @spec(do_integer_operation(atom(), integer(), integer()) -> syntaxTree())
%% @end
%%--------------------------------------------------------------------
-spec(do_integer_operation(Op::atom(), Num1::integer(), Num2::integer()) -> syntaxTree()).
do_integer_operation('+',Num1,Num2) -> ?TO_AST(integer_to_list(Num1 + Num2));
do_integer_operation('-',Num1,Num2) -> ?TO_AST(integer_to_list(Num1 - Num2));
do_integer_operation('*',Num1,Num2) -> ?TO_AST(integer_to_list(Num1 * Num2));
do_integer_operation('rem',Num1,Num2) -> ?TO_AST(integer_to_list(Num1 rem Num2));
do_integer_operation('div',_,0) -> error;
do_integer_operation('div',Num1,Num2) -> ?TO_AST(integer_to_list(Num1 div Num2));
do_integer_operation(_,_,_) -> error.
