%%% @author Gabriela C. Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
-module(eval_inline_variable).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

-export([rules/2]).

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

