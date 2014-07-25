-module(core_if).
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).
rules({_,VarsInfo,_},_) ->
    [
     if_rule(VarsInfo)
    ].
	      
if_rule(VarsInfo) ->
    ?RULE(
       ?T("if Cond@@@ -> Body@@@ end"),
       (api_refac:reset_pos_and_range(if_transform(Cond@@@, Body@@@, VarsInfo))),
       (if_cond(Cond@@@, VarsInfo))
    ).

if_cond([], _) -> false; 
if_cond([NodeList | T], VarsInfo) ->
    [[Node]] = NodeList,
    Result = utils_guards:evaluateGuardsExpression(Node, VarsInfo),
    atom_value_case_cond(Result, T, VarsInfo).

if_transform([CondNodeList | TCond], [BodyNode | TBody], VarsInfo) ->
   [[CondNode]] = CondNodeList,
   Result = utils_guards:evaluateGuardsExpression(CondNode, VarsInfo),
   atom_value_case_transf({Result, BodyNode}, {TCond, TBody}, VarsInfo);
if_transform(_,_,_) -> {error, 'No transformation can be done!'}.
	    
atom_value_case_cond(NodeValue, T, VarsInfo) ->
    case NodeValue of
          true -> true;
          false -> if_cond(T, VarsInfo);
          _ -> false
   end.

atom_value_case_transf({NodeValue, BodyNode}, {TCond, TBody}, VarsInfo) ->
    case NodeValue of
		true -> BodyNode;
		false -> if_transform(TCond, TBody, VarsInfo);
		_ -> {error, 'No transformation can be done!'}
	    end.
    

 
