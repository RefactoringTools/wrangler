-module(core_if).
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).
rules({_,Scope},_) ->
    [
     if_rule(Scope)
    ].
	      
if_rule(Scope) ->
    ?RULE(
       ?T("if Cond@@@ -> Body@@@ end"),
       api_refac:reset_pos_and_range(if_transform(Cond@@@, Body@@@,Scope)),
       if_cond(Cond@@@, Scope)
    ).

if_cond([], _) -> false; 
if_cond([NodeList | T], Scope) -> 
    [[Node | []] | []] = NodeList,
    Result = utils_guards:evaluateGuardsExpression(Node, Scope),
    atom_value_case_cond(Result, T, Scope).

if_transform([CondNodeList | TCond], [BodyNode | TBody], Scope) ->
   [[CondNode | []] | []] = CondNodeList,
   Result = utils_guards:evaluateGuardsExpression(CondNode, Scope),
   atom_value_case_transf({Result, BodyNode}, {TCond, TBody}, Scope);
if_transform(_,_,_) -> {error, 'No transformation can be done!'}.
	    
atom_value_case_cond(NodeValue, T, Scope) ->
    case NodeValue of
          true -> true;
          false -> if_cond(T, Scope);
          _ -> false
   end.

atom_value_case_transf({NodeValue, BodyNode}, {TCond, TBody}, Scope) ->
    case NodeValue of
		true -> BodyNode;
		false -> if_transform(TCond, TBody, Scope);
		_ -> {error, 'No transformation can be done!'}
	    end.
    

 
