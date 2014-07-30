%%% @author Gabriela C. Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
-module(eval_inline_variable).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

-export([rules/2]).

rules({_,VarsInfo,_},_) ->
    [
     inline_variable_rule(VarsInfo)
    %% inline_variable_rule(VarsInfo),
    %% inline_variable_rule_2(VarsInfo),
   %% inline_variable_rule_3(VarsInfo),
   %% inline_variable_rule_4(VarsInfo)
    ].

inline_variable_rule(VarsInfo) ->
    ?RULE(
       ?T("Var@"),
       begin
	  Expr@ = get_assignment_value(Var@,VarsInfo),
	  Expr@
       end,
       inline_variable_cond(Var@,VarsInfo)
    ).

inline_variable_rule_0(VarsInfo) ->
    io:format("Inline variable rule~n"),
    ?RULE(
       ?T("begin Stmt0@@, Var@, Stmt@@ end"),
       begin
	  Expr@ = get_assignment_value(Var@,VarsInfo),
	  ?TO_AST("begin Stmt0@@, Expr@, Stmt@@ end")
       end,
       inline_variable_cond(Var@,VarsInfo)
    ).

inline_variable_rule_2(VarsInfo) ->
    io:format("Inline variable rule 2~n"),
    ?RULE(
       ?T("Stmt0@@, Var@, Stmt@@"),
       begin
	  Expr@ = get_assignment_value(Var@,VarsInfo),
	  ?TO_AST("Stmt0@@, Expr@, Stmt@@")
       end,
       inline_variable_cond(Var@,VarsInfo)
    ).

inline_variable_rule_3(VarsInfo) ->
    io:format("Inline variable rule 3~n"),
    ?RULE(
       ?T("Var@"),
       begin
	  Expr@ = get_assignment_value(Var@,VarsInfo),
	  ?TO_AST("Stmt0@@, Expr@, Stmt@@")
       end,
       inline_variable_cond(Var@,VarsInfo)
    ).

inline_variable_rule_4(VarsInfo) ->
    io:format("Inline variable rule 4~n"),
    ?RULE(
       ?T("begin Stmt0@@, Var@ = Expr@, Stmt@@ end"),
       _This@,
       begin
	   io:format("MAS PQ?~n"),
	   false
       end
    ).

inline_variable_cond(Var@,VarsInfo) ->
       core_unreferenced_assign:is_variable_use(Var@) andalso
       lists:keyfind(api_refac:free_vars(Var@),1,VarsInfo) /= false.

get_assignment_value(Var@,VarsInfo) ->
     {_,Expr@} = lists:keyfind(api_refac:free_vars(Var@),1,VarsInfo),
     Expr@.

