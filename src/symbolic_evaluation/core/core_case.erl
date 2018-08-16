%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%% Case Core - Where possible, simplifies case expressions by the result of their evaluation. 
%%
%% Examples of usage:
%% <ul>
%% <li>
%% <em>
%%case [1,2] of<br/>
%%<div class="first_align">
%%[H | T] -> ok;<br/>
%%_ -> error<br/>
%%</div>
%%end.<br/>
%% </em>
%% <strong>is simplified to</strong> <em>ok</em>.
%% </li>
%% <li>
%% <em>
%% begin <br/>
%% <div class="first_align">
%%    X = true,<br/>
%%    case X of<br/>
%% <div class="second_align">
%%	true -> first;<br/>
%%	_ -> second<br/>
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
%%    first<br/>
%% </div>
%% end.<br/>
%% </em>
%% </li>
%% </ul>
%%
%%@end
-module(core_case).
-include("wrangler.hrl").
-export([rules/2]).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list with a single rule for the case simplification.
%% @spec rules({term(), VarsInfo::[{[{atom(), pos()}], syntaxTree()}],term()},term()) -> [rule()]
%% @end
%%--------------------------------------------------------------------
-spec(rules({_,VarsInfo::[{[{atom(), pos()}], syntaxTree()}],_},_) -> [{'rule',fun(),list() | tuple()},...]).
rules({_,VarsInfo,_},_) ->
    [case_rule(VarsInfo)].

case_rule(VarsInfo) ->
    ?RULE(
      ?T("case Expr@ of Pats@@@ when Guards@@@ -> Body@@@ end"),
      begin
	  Result = case_transf(Expr@,Pats@@@,Guards@@@,Body@@@,VarsInfo),
	  case Result of
	      {ok, {Body,Patt,Args}} -> utils_subst:subst(Body, Patt, Args);
	      _ -> Result
	  end
      end,
      begin
	  Result = case_transf(Expr@,Pats@@@,Guards@@@,Body@@@,VarsInfo),
	  case Result of
	      {ok,_} -> true;
	      _ -> false
	  end
      end
).

case_transf(Expr,Patt,Guards,Body,VarsInfo) ->
    NewExpr = case api_refac:type(Expr) of
	variable ->
		 case utils_guards:get_variable(Expr,VarsInfo) of
		     {expr,Expr} -> Expr;
		     {value,OtherExpr} -> 
			 Convert = utils_convert:convert_elem(OtherExpr),
			 case Convert of
			     error -> Expr;
			     _ -> OtherExpr
			 end;
		     Other -> Other
		 end;
	_ -> Expr
    end,
    case_transf_rec(NewExpr,Patt,Guards,Body,VarsInfo).

case_transf_rec(_,[],_,_,_) -> {error, "No Match!"};
case_transf_rec(Expr,[HPattList|TPatt],[HGuard|TGuard],[HBody|TBody],VarsInfo) ->
    HPatt = utils_subst:getNode(HPattList),
    case utils_match:match(Expr, HPatt) of
	true ->
	    case utils_guards:guardsSuceed(Expr,HPatt,HGuard) of
                 true -> {ok,{HBody,HPatt,Expr}};
                 false -> case_transf_rec(Expr,TPatt,TGuard,TBody,VarsInfo);
                 _ -> {error,"Guards failed!"} 
             end;
	false -> case_transf_rec(Expr,TPatt,TGuard,TBody,VarsInfo);
        _ -> {error, "No Match!"}
      end.



