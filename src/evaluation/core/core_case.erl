-module(core_case).
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).
rules({_,Scope},_) ->
    [case_rule()].

case_rule() ->
    ?RULE(
      ?T("case Expr@ of Pats@@@ when Guards@@@ -> Body@@@ end"),
      begin
	  Result = case_transf(Expr@,Pats@@@,Guards@@@,Body@@@),
	  case Result of
	      {ok, AST} -> AST;
	      _ -> Result
	  end
      end,
      begin
	  Result = case_transf(Expr@,Pats@@@,Guards@@@,Body@@@),
	  case Result of
	      {ok,_} -> true;
	      _ -> false
	  end
      end
).

case_transf(_,[],_,_) -> {error, "No Match!"};
case_transf(Expr,[HPattList|TPatt],[HGuard|TGuard],[HBody|TBody]) ->
    HPatt = utils_subst:getNode(HPattList),
    case utils_match:match(Expr, HPatt) of
	true ->
	    io:format("Match~n"),
	    case utils_guards:guardsSuceed(Expr,HPatt,HGuard) of
                 true -> {ok,HBody};
                 false -> case_transf(Expr,TPatt,TGuard,TBody);
                 _ -> {error,"Guards failed!"} 
             end;
	false ->  case_transf(Expr,TPatt,TGuard,TBody);
        _ -> {error, "No Match!"}
      end.



