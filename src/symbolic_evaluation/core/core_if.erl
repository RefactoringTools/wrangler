%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%% If Core - Where possible, simplifies if expressions by the result of their evaluation.
%%
%% Examples of usage:
%% <ul>
%% <li>
%% <em>
%%if<br/>
%%<div class="first_align">
%%false -> [2];<br/>
%%true -> []<br/>
%%</div>
%%end.<br/>
%% </em>
%% <strong>is simplified to</strong> <em>[]</em>.
%% </li>
%% <li>
%% <em>
%% begin <br/>
%% <div class="first_align">
%%    X = true,<br/>
%%    if<br/>
%% <div class="second_align">
%%	X -> [2];<br/>
%%	true -> []<br/>
%% </div>
%%    end<br/>
%% </div>
%% end.<br/>
%% </em>
%% <strong>becomes</strong> <br/>
%% <em>
%% begin <br/>
%% <div class="first_align">
%%    X = true,<br/>
%%    [2]<br/>
%% </div>
%% end.<br/>
%% </em>
%% </li>
%% </ul>
%%
%%@end
-module(core_if).
-include_lib("../../../include/wrangler.hrl").
-export([rules/2]).
%%--------------------------------------------------------------------
%% @doc
%% Returns a list with a single rule for the if simplification.
%% @spec rules({term(), VarsInfo::[{[{atom(), pos()}], syntaxTree()}],term()},term()) -> [rule()]
%% @end
%%--------------------------------------------------------------------
-spec(rules({_,VarsInfo::[{[{atom(), pos()}], syntaxTree()}],_},_) -> [{'rule',fun(),list() | tuple()},...]).
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
	  maybe -> false;
	  {expr,_} -> false;
	  Other when is_number(Other) orelse is_list(Other) orelse is_atom(Other) orelse is_tuple(Other) -> if_cond(T,VarsInfo);
          _ -> false
   end.

atom_value_case_transf({NodeValue, BodyNode}, {TCond, TBody}, VarsInfo) ->
    case NodeValue of
		true -> BodyNode;
		false -> if_transform(TCond, TBody, VarsInfo);
	        maybe -> false;
	        {expr,_} -> false;
	        Other when is_number(Other) orelse is_list(Other) orelse is_atom(Other) orelse is_tuple(Other) -> if_transform(TCond,TBody,VarsInfo);
		_ -> {error, 'No transformation can be done!'}
	    end.
    

 
