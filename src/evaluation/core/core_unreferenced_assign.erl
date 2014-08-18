%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%% Remove Unreferenced Assignments - Removes assignments when the variables are not used afterwards. 
%%
%% For example, the expression: <br/>
%% <em>
%% begin <br/>
%% <div class="first_align">
%%  A = 10, <br/>
%%  B = 20, <br/>
%%  C = 30, <br/>
%%  E = A + B + C, <br/>
%%  A <br/>
%% </div>
%% end. <br/>
%% </em>
%% can be simplified by this refactoring to: <br/>
%% <em>
%% begin <br/>
%% <div class="first_align">
%%  A = 10, <br/>
%%  A <br/>
%% </div>
%% end. <br/>
%% </em>
%% @end
-module(core_unreferenced_assign).
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2,collector_variable_occurrences/1,variable_assignment_cond/2,variable_assignment_rule/1,variable_assignment_rule_begin/1,collector_var_expr_value/1,is_variable_use/1]).
%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
rules(_,Info) ->
    [variable_assignment_rule(Info),
     variable_assignment_rule_begin(Info),
     variable_assignment_rule_single(Info)].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
variable_assignment_rule(Info) ->
    ?RULE(
       ?T("Stmt0@@, Var@ = Expr@, Stmt@@"),
      api_refac:reset_pos_and_range(case Stmt@@ of	   
	   [] ->
	       ?TO_AST("Stmt0@@, Expr@");
	   _ -> 	   
	       ?TO_AST("Stmt0@@, Stmt@@")
       end),
      variable_assignment_cond(Var@,Info)
    ).    

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
variable_assignment_rule_begin(Info) ->
    ?RULE(
       ?T("begin Stmt0@@, Var@ = Expr@, Stmt@@ end"),
       begin
      api_refac:reset_pos_and_range(case Stmt@@ of	   
	   [] ->
	       ?TO_AST("begin Stmt0@@, Expr@ end");
	   _ -> 	   
	       ?TO_AST("begin Stmt0@@, Stmt@@ end")
       end)
      end,
      variable_assignment_cond(Var@,Info)
    ).  

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
variable_assignment_rule_single(Info) ->
    ?RULE(
       ?T("Var@ = Expr@"),
       Expr@,
       variable_assignment_cond(Var@,Info)
    ).   

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
variable_assignment_cond(Var@,Info) ->
    api_refac:type(Var@) == variable andalso
    lists:filter(fun(Elem) -> Elem == api_refac:bound_vars(Var@) end, Info) == [].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
collector_variable_occurrences(Scope) ->
    ?FULL_TD_TU(    
		[collect_variables_occurrences()],
		Scope
    ).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
collect_variables_occurrences()->
    ?COLLECT(
       ?T("Var@"),
       api_refac:free_vars(Var@),
       is_variable_use(Var@)
     ).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
is_variable_use(Var@) ->
     api_refac:type(Var@) == variable andalso 
     api_refac:variable_define_pos(Var@) /= [{0,0}] andalso 
     api_refac:bound_vars(Var@) == [].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
collector_var_expr_value(Scope) -> 
    ?FULL_TD_TU([collect_variable_assignment()],Scope).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------	 
collect_variable_assignment() ->
    ?COLLECT(
       ?T("Var@ = Exp@"),
       {api_refac:bound_vars(Var@),Exp@},
       api_refac:type(Var@) == variable andalso 
       api_refac:variable_define_pos(Var@) /= [{0,0}]
    ).
