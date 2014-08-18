%%% @author Gabriela Sampaio, Roberto Souto Maior<>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson

%%--------------------------------------------------------------------
%% @doc
%% This module simplifies variables definitions. When a variable is created and used after, the varible evaluates to its value. Only variables 
%% defined via a match expression of the format: VarName = Expr can be inlined. Consider the function below:<br/>
%% <i> f() -> X = 1,  <br/>
%%            X. </i> <br/>
%% If the user chooses to simplify the function f/0, the Wrangler tool will identify that there is a variable declaration <i>(X=1)</i> and the 
%% variable X in the second clause will be simplified to 1. 
%% @end
%%--------------------------------------------------------------------

-module(eval_inline_variable).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

-export([rules/2]).


%%@private
rules({_,VarsInfo,_},_) ->
    [inline_variable_rule(VarsInfo)].

inline_variable_rule(VarsInfo) ->
    ?RULE(
       ?T("Var@"),
       begin
	  Expr@ = get_assignment_value(Var@,VarsInfo),
	  Expr@
       end,
       inline_variable_cond(Var@,VarsInfo)
    ).

inline_variable_cond(Var@,VarsInfo) ->
       core_unreferenced_assign:is_variable_use(Var@) andalso
       lists:keyfind(api_refac:free_vars(Var@),1,VarsInfo) /= false.

get_assignment_value(Var@,VarsInfo) ->
     {_,Expr@} = lists:keyfind(api_refac:free_vars(Var@),1,VarsInfo),
     Expr@.

