-module(core_case).
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).
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
		     {value,OtherExpr} -> OtherExpr;
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



