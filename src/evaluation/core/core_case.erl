-module(core_case).
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).
rules({_,Scope},_) ->
    [case_rule(Scope)].

case_rule(Scope) ->
    ?RULE(
      ?T("case Expr@ of Pats@@@ when Guards@@@ -> Body@@@ end"),
      begin
	  Result = case_transf(Expr@,Pats@@@,Guards@@@,Body@@@,Scope),
	  case Result of
	      {ok, {Body,Patt,Args}} -> utils_subst:subst(Body, Patt, Args);
	      _ -> Result
	  end
      end,
      begin
	  Result = case_transf(Expr@,Pats@@@,Guards@@@,Body@@@,Scope),
	  case Result of
	      {ok,_} -> true;
	      _ -> false
	  end
      end
).

case_transf(Expr,Patt,Guards,Body,Scope) ->
    NewExpr = case api_refac:type(Expr) of
	variable ->
		 case utils_guards:get_variable(Expr,Scope) of
		     {expr,Expr} -> Expr;
		     {value,OtherExpr} -> OtherExpr;
		     Other -> Other
		 end;
	_ -> Expr
    end,
    case_transf_rec(NewExpr,Patt,Guards,Body,Scope).

case_transf_rec(_,[],_,_,_) -> {error, "No Match!"};
case_transf_rec(Expr,[HPattList|TPatt],[HGuard|TGuard],[HBody|TBody],Scope) ->
    HPatt = utils_subst:getNode(HPattList),
    case utils_match:match(Expr, HPatt) of
	true ->
	    case utils_guards:guardsSuceed(Expr,HPatt,HGuard) of
                 true -> {ok,{HBody,HPatt,Expr}};
                 false -> case_transf_rec(Expr,TPatt,TGuard,TBody,Scope);
                 _ -> {error,"Guards failed!"} 
             end;
	false ->  case_transf_rec(Expr,TPatt,TGuard,TBody,Scope);
        _ -> {error, "No Match!"}
      end.



