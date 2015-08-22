%%% @author Gabriela Sampaio, Roberto Souto Maior<>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson

%%--------------------------------------------------------------------
%% @doc
%% This modules substitutes variable occurrences by their value. 
%%
%% Only variables defined via a match expression of the format: <em>VarName = Expr</em> can be inlined. Consider the code below:<br/>
%% <em> 
%%begin <br/>
%%<div class="first_align">
%%X = 1,  <br/>
%%Y = X + 2,  <br/>
%%Y + 3  <br/>
%%</div>
%%end. <br/>
%%</em>
%% This module, will simplify the previous expression to:<br/>
%% <em> 
%%begin <br/>
%%<div class="first_align">
%%X = 1,  <br/>
%%Y = 1 + 2,  <br/>
%%1 + 2 + 3  <br/>
%%</div>
%%end. <br/>
%%</em>
%% @end
%%--------------------------------------------------------------------
-module(eval_inline_variable).

%% Include files
-include_lib("../../../include/wrangler.hrl").

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

