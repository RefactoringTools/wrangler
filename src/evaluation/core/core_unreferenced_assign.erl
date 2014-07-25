-module(core_unreferenced_assign).
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2,collector_variable_occurrences/1,variable_assignment_cond/2,variable_assignment_rule/1,variable_assignment_rule_begin/1]).

rules(_,Info) ->
    [variable_assignment_rule(Info),
     variable_assignment_rule_begin(Info),
     variable_assignment_rule_single(Info)].

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

variable_assignment_rule_single(Info) ->
    ?RULE(
       ?T("Var@ = Expr@"),
       Expr@,
       variable_assignment_cond(Var@,Info)
    ).   

variable_assignment_cond(Var@,Info) ->
    api_refac:type(Var@) == variable andalso
    lists:filter(fun(Elem) -> Elem == api_refac:bound_vars(Var@) end, Info) == [].

collector_variable_occurrences(Scope) ->
    ?FULL_TD_TU(    
		[collect_variables_occurrences()],
		Scope
    ).

collect_variables_occurrences()->
    ?COLLECT(
       ?T("Var@"),
       api_refac:free_vars(Var@),
       api_refac:type(Var@) == variable andalso 
       api_refac:variable_define_pos(Var@) /= [{0,0}] andalso 
       api_refac:bound_vars(Var@) == []
     ).
