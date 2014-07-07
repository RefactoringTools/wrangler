-module(core_if).
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).
rules({_,Scope},_) ->
    [if_rule(),if_rule_2(Scope), if_rule_3(Scope), if_rule_4(Scope), if_rule_5(Scope),if_rule_6(Scope),if_rule_7(Scope)].
	      
%%case_rule() ->
%%    ?RULE(
%%      ?T("case Expr@ of Pats@@@ when Guards@@@-> Body@@@ end"),
%%       _This@,
%%       begin
%%	   io:format("Exp: ~p~n, Patt: ~p~n",[?PP(_This@), Pats@@@]),
%%	   false
%%       end
%%).



if_rule() ->
    io:format("IF RULE~n"),
    ?RULE(
       ?T("if Cond@@@ -> Body@@@ end"),
       _This@,
       begin
	   io:format("WORKING..."),
	   false
       end
    ).

if_rule_2(Scope) ->
    ?RULE(
          ?T("if Cond@ -> Steps1@@; Cond2@ -> Steps2@@ end"), 
          begin      
               {ok,Index} = if_transform([Cond@,Cond2@],1, Scope),
               ?TO_AST("Steps"++Index++"@@")
          end,
	  if_cond([Cond@, Cond2@], Scope)
	). 

if_rule_3(Scope) ->
    ?RULE(
          ?T("if Cond@ -> Steps1@@; Cond2@ -> Steps2@@; Cond3@ -> Steps3@@ end"), 
          begin      
               {ok,Index} = if_transform([Cond@,Cond2@, Cond3@],1, Scope),
               ?TO_AST("Steps"++Index++"@@")
          end,
	  if_cond([Cond@, Cond2@, Cond3@], Scope)
	). 

if_rule_4(Scope) ->
    ?RULE(
          ?T("if Cond@ -> Steps1@@; Cond2@ -> Steps2@@; Cond3@ -> Steps3@@; Cond4@ -> Steps4@@ end"), 
          begin      
              {ok,Index} = if_transform([Cond@,Cond2@, Cond3@, Cond4@],1, Scope),
               ?TO_AST("Steps"++Index++"@@")
          end,
	  if_cond([Cond@, Cond2@, Cond3@, Cond4@], Scope)
	). 

if_rule_5(Scope) ->
    ?RULE(
          ?T("if Cond@ -> Steps1@@; Cond2@ -> Steps2@@; Cond3@ -> Steps3@@; Cond4@ -> Steps4@@; Cond5@ -> Steps5@@ end"), 
          begin      
               {ok,Index} = if_transform([Cond@,Cond2@, Cond3@, Cond4@, Cond5@],1, Scope),
               ?TO_AST("Steps"++Index++"@@")
          end,
	  if_cond([Cond@,Cond2@, Cond3@, Cond4@, Cond5@], Scope)
	). 

if_rule_6(Scope) ->
    ?RULE(
          ?T("if Cond@ -> Steps1@@; Cond2@ -> Steps2@@; Cond3@ -> Steps3@@; Cond4@ -> Steps4@@; Cond5@ -> Steps5@@; Cond6@ -> Steps6@@ end"), 
          begin      
               {ok,Index} = if_transform([Cond@,Cond2@, Cond3@, Cond4@, Cond5@, Cond6@],1, Scope),
               ?TO_AST("Steps"++Index++"@@")
          end,
	  if_cond([Cond@,Cond2@, Cond3@, Cond4@, Cond5@, Cond6@], Scope)
	).

if_rule_7(Scope) ->
    ?RULE(
          ?T("if Cond@ -> Steps1@@; Cond2@ -> Steps2@@; Cond3@ -> Steps3@@; Cond4@ -> Steps4@@; Cond5@ -> Steps5@@; Cond6@ -> Steps6@@; Cond7@ -> Steps7@@ end"), 
          begin      
               {ok,Index} = if_transform([Cond@,Cond2@, Cond3@, Cond4@, Cond5@, Cond6@, Cond7@],1, Scope),
               ?TO_AST("Steps"++Index++"@@")
          end,
	  if_cond([Cond@,Cond2@, Cond3@, Cond4@, Cond5@, Cond6@, Cond7@], Scope)
	).
 
if_cond([], _) -> false; 
if_cond([Node | T], Scope) ->
    Result = utils_guards:evaluateGuardsExpression(Node, Scope),
    atom_value_case_cond(Result, T, Scope).

if_transform([CondNode | T], Index, Scope) ->
   Result = utils_guards:evaluateGuardsExpression(CondNode, Scope),
   atom_value_case_transf(Result, T, Index, Scope);
if_transform(_,_,_) -> {error, 'No transformation can be done!'}.
	    
atom_value_case_cond(NodeValue, T, Scope) ->
    case NodeValue of
          true -> true;
          false -> if_cond(T, Scope);
          _ -> false
   end.

atom_value_case_transf(NodeValue, T, Index, Scope) ->
    case NodeValue of
		true -> {ok, integer_to_list(Index)};
		false -> if_transform(T, Index + 1, Scope);
		_ -> {error, 'No transformation can be done!'}
	    end.
    

 
