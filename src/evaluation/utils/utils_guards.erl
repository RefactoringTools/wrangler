%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Roberto S. M. de Barros Filho, Simon  Thompson
-module(utils_guards).
-export([guardsSuceed/3, guardsSuceed/4, rules/2, pos_to_node/2, evaluateGuardsExpression/2,get_variable/2,evaluateIsFun/1]).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

guardsSuceed(Arg1, Arg2, Arg3) ->
    guardsSuceed(Arg1, Arg2, Arg3, []).

guardsSuceed(_,_,[],_) -> true;
guardsSuceed(Arg, ArgPatt, [[Guards | []] | []],Scope) -> 
    Type = api_refac:type(Guards),    
    if
	Type == infix_expr orelse Type == prefix_expr orelse Type == application -> 
	    NewGuards = utils_subst:subst(Guards, ArgPatt, Arg),
	    FinalGuards = utils_transform:transform_body(NewGuards, fun utils_guards:rules/2, Scope),
	    evaluateGuardsExpression(FinalGuards, Scope);	      
	true -> false
   end;
guardsSuceed(_,_,_,_) ->
     false.

rules(Args, FunDefInfo) -> core_arithmetics:rules(Args, FunDefInfo) ++ [core_funApp:length_rule()].
    
evaluateGuardsExpression(Node, Scope)->
    NodeType = api_refac:type(Node),
    if
	NodeType == infix_expr ->
	    Cond = arithmetic_condition(Node),
	    if
		Cond -> {expr, Node};
		true ->
		       Match = findGuardInfixExpr(Node),
		       case Match of
			   {match,Operator, Left, Right} ->
			       LeftEvaluated = evaluateGuardsExpression(Left, Scope),
			       RightEvaluated = evaluateGuardsExpression(Right, Scope),
			       evaluateBooleanBinaryExp(Operator, LeftEvaluated, RightEvaluated);
			   _ -> maybe
		       end
	   end;
	NodeType == prefix_expr orelse NodeType == application ->
	     LengthCall = NodeType == application andalso ?MATCH(?T("length(Exp@)"), Node),
	     if
		 LengthCall -> {expr, Node};
		 true ->
			Match = findGuardApplication(Node, NodeType),
			case Match of
			    negativeNum -> list_to_integer(?PP(Node));
			    {match, "is_function", Args@@} ->
			      evaluateIsFun(Args@@);
			    {match, Operator, Single} ->
				SingleEvaluated = evaluateGuardsExpression(Single, Scope),
				evaluateBooleanApplication(Operator, SingleEvaluated);
			    _ -> 
				maybe
			end
	     end;
	NodeType == variable -> 
	    case get_variable(Node, Scope) of
		{value, Expr@} -> evaluateGuardsExpression(Expr@, Scope);
		Other -> Other
	    end;
	true -> 
	    Convert = utils_convert:convert_elem(Node),
	    case Convert of
		error -> {expr, Node};
		_ -> Convert
	    end
   end.

get_variable(Var, Scope) ->   
    case Scope of
	[] ->
	    {expr, Var};
	_ ->
	    VarDefPos = api_refac:variable_define_pos(Var),
		   case VarDefPos of
			   [{0,0}] -> {expr, Var};
			   [DefPos | _] -> 
			       Result = pos_to_node(Scope, DefPos),
			       case Result of
				   {ok, Node} ->
				       Match = ?MATCH(?T("Var@ = Expr@"), Node) andalso VarDefPos == api_refac:variable_define_pos(Var@),
				       if
					   Match -> {value,Expr@};
					   true -> {expr, Var}
				       end;
				   _ -> {expr, Var}
			       end;
			   _ -> {expr, Var}
		 end
  end.

findGuardApplication(Node, prefix_expr) ->
    Match = ?MATCH(?T("not(Single@)"), Node),
    if
	Match -> {match, "not", Single@};
	true -> 
	    NegativeNum = ?MATCH(?T("-Num@"),Node),
	    if
		NegativeNum -> negativeNum;
		true -> noMatch
	    end	    
    end;
findGuardApplication(Node, application) ->
    Match = ?MATCH(?T("F@(Single@)"), Node),
    if
	Match -> 
	    Name = ?PP(F@),
	    Cond = (Name == "is_list" orelse Name == "is_float" orelse
	    Name == "is_integer" orelse Name == "is_boolean" orelse 
	    Name == "is_atom" orelse Name == "is_tuple" orelse Name == "is_number"),
	    if
		Cond -> {match, Name, Single@};
		true -> noMatch
	    end;
	true -> 
	    IsFun = ?MATCH(?T("is_function(Args@@)"), Node),
	    if
		IsFun ->
		    {match, "is_function", Args@@};
		true ->
		    noMatch
	    end
    end.
    	 			   
findGuardInfixExpr(Node) ->    
    Match = findCompMatch(Node),
    case Match of
	noMatch -> findBooleanOpMatch(Node);
	Result -> Result
    end.
	
findCompMatch(Node) ->
    Match = findSimpleCompMatch(Node),
    case Match of
	noMatch ->
	    GTMatch = findGTMatch(Node),
	    case GTMatch of
		noMatch -> findLTMatch(Node);
		Result -> Result
	    end;
	Result -> Result
    end.

findSimpleCompMatch(Node) ->
    Match = findEqualComp(Node),
    case Match of
	noMatch -> findDiffComp(Node); 
	Result -> Result
    end.
	    

findBooleanOpMatch(Node) ->
    ANDsMatch = findANDsMatch(Node),
    case ANDsMatch of
	noMatch ->
	    ORsMatch = findORsMatch(Node),
	    case ORsMatch of
		noMatch ->
		    MatchXor = ?MATCH(?T("Left@ xor Right@"),Node),
		    if
			MatchXor -> {match, "xor", Left@, Right@};
			true -> noMatch
		    end;
		Result -> Result
	    end;
	Result -> Result
    end.

findEqualComp(Node)->
     MatchEquals = ?MATCH(?T("Left@ == Right@"),Node),
    if
	MatchEquals -> {match, "==", Left@, Right@};
	true -> 
	    MatchExacEquals = ?MATCH(?T("LeftDiff@ =:= RightDiff@"),Node),
	    if
		MatchExacEquals -> {match, "=:=", LeftDiff@, RightDiff@};
		true -> noMatch
	    end
    end.
		    			  
findDiffComp(Node) ->
    MatchDiff = ?MATCH(?T("Left@ /= Right@"),Node),
    if
	MatchDiff -> {match, "/=", Left@, Right@};
	true -> 
	    MatchExacDiff = ?MATCH(?T("LeftDiff@ =/= RightDiff@"),Node),
	    if
		MatchExacDiff -> {match, "=/=", LeftDiff@, RightDiff@};
		true -> noMatch
	    end
    end.

findGTMatch(Node) ->
     MatchGTEquals = ?MATCH(?T("Left@ > Right@"),Node),
    if
	MatchGTEquals -> {match, ">", Left@, Right@};
	true -> 
	    MatchGT2Equals = ?MATCH(?T("Left2@ >= Right2@"),Node),
	    if
		MatchGT2Equals -> {match, ">=", Left2@, Right2@};
		true -> noMatch
	    end
    end.
    

findLTMatch(Node) ->
    MatchLTEquals = ?MATCH(?T("Left@ < Right@"),Node),
    if
	MatchLTEquals -> {match, "<", Left@, Right@};
	true -> 
	    MatchLT2Equals = ?MATCH(?T("Left2@ =< Right2@"),Node),
	    if
		MatchLT2Equals -> {match, "=<", Left2@, Right2@};
		true -> noMatch
	    end
    end.

findANDsMatch(Node) ->
    MatchAndEquals = ?MATCH(?T("Left@ and Right@"),Node),
    if
	MatchAndEquals -> {match, "and", Left@, Right@};
	true -> 
	    MatchAndAlsoEquals = ?MATCH(?T("Left2@ andalso Right2@"),Node),
	    if
		MatchAndAlsoEquals -> {match, "andalso", Left2@, Right2@};
		true -> noMatch
	    end
    end.

findORsMatch(Node) ->
    MatchOrEquals = ?MATCH(?T("Left@ or Right@"),Node),
    if
	MatchOrEquals -> {match, "or", Left@, Right@};
	true -> 
	    MatchOrElseEquals = ?MATCH(?T("Left2@ orelse Right2@"),Node),
	    if
		MatchOrElseEquals -> {match, "orelse", Left2@, Right2@};
		true -> noMatch
	    end
    end.

evaluateBooleanBinaryExp(Operator, {expr, NodeLeft}, {expr, NodeRight}) -> 
    Equal = ?PP(NodeRight) == ?PP(NodeLeft),
    if
	Equal ->
	    evaluateBooleanBinaryExp(Operator, ?PP(NodeLeft), ?PP(NodeRight));
	true ->
	    maybe
    end;
evaluateBooleanBinaryExp(_Operator, {expr, _NodeLeft}, _A) -> maybe;
evaluateBooleanBinaryExp(_Operator, _, {expr, _NodeRight})  -> maybe;
evaluateBooleanBinaryExp(_Operator, maybe, _A) -> maybe;
evaluateBooleanBinaryExp(_Operator, _, maybe)  -> maybe;
evaluateBooleanBinaryExp("==",Left,Right) -> Left == Right;
evaluateBooleanBinaryExp("=:=",Left,Right) -> Left =:= Right;
evaluateBooleanBinaryExp("/=",Left,Right) -> Left /= Right;
evaluateBooleanBinaryExp("=/=",Left,Right) -> Left =/= Right;
evaluateBooleanBinaryExp(">",Left,Right) -> Left > Right;
evaluateBooleanBinaryExp(">=",Left,Right) -> Left >= Right;
evaluateBooleanBinaryExp("<",Left,Right) -> Left < Right;
evaluateBooleanBinaryExp("=<",Left,Right) -> Left =< Right;
evaluateBooleanBinaryExp("andalso",Left,Right) when is_boolean(Left) andalso is_boolean(Right) ->
    Left andalso Right;
evaluateBooleanBinaryExp("and",Left,Right) when is_boolean(Left) andalso is_boolean(Right) ->
    Left and Right;
evaluateBooleanBinaryExp("orelse",Left,Right) when is_boolean(Left) andalso is_boolean(Right) ->
    Left orelse Right;
evaluateBooleanBinaryExp("or",Left,Right) when is_boolean(Left) andalso is_boolean(Right) ->
    Left or Right;
evaluateBooleanBinaryExp("xor",Left,Right) when is_boolean(Left) andalso is_boolean(Right) ->
    Left xor Right;
evaluateBooleanBinaryExp(_,_,_) ->
    false.

evaluateBooleanApplication(_Operator, {expr, _Node}) -> maybe;
evaluateBooleanApplication(_Operator, maybe) -> maybe;
evaluateBooleanApplication("not",Single) when is_boolean(Single) -> not (Single);
evaluateBooleanApplication("is_list",Single) -> is_list(Single);
evaluateBooleanApplication("is_integer",Single) -> is_integer(Single);
evaluateBooleanApplication("is_float",Single) -> is_float(Single);
evaluateBooleanApplication("is_number",Single) -> is_number(Single);
evaluateBooleanApplication("is_boolean",Single) -> is_boolean(Single);
evaluateBooleanApplication("is_atom",Single) -> is_atom(Single);
evaluateBooleanApplication("is_tuple",Single) -> is_tuple(Single);
evaluateBooleanApplication(_,_) -> false.

evaluateIsFun([]) -> false;
evaluateIsFun([F | []]) ->
    Type =  api_refac:type(F),
    Type == fun_expr orelse Type == implicit_fun; 
evaluateIsFun([F | [A | []]]) ->
    MatchExpr = ?MATCH(?T("fun(Args@@) -> Body@@ end"), F),
	if
	    MatchExpr ->
		length(?PP(Args@@)) == list_to_integer(?PP(A));
	    true ->
		api_refac:type(F) == implicit_fun andalso
		begin
		    FunInfo = api_refac:fun_define_info(F),
		    case FunInfo of
			{_M, _Fun, Ari} ->
			    Ari == list_to_integer(?PP(A));
			_ -> false
		    end
		end
    end;
evaluateIsFun(_) -> false. 

-spec(arithmetic_condition(Node::syntaxTree())->boolean()).
arithmetic_condition(Node) ->
    NodeType = api_refac:type(Node),
    if
        NodeType == integer orelse NodeType == variable -> true;
        NodeType == infix_expr ->
		(?MATCH(?T("Sum1@ + Sum2@"),Node) andalso
		    arithmetic_condition(Sum1@) andalso 
			arithmetic_condition(Sum2@)) orelse

		(?MATCH(?T("Sub1@ - Sub2@"),Node) andalso
		    arithmetic_condition(Sub1@) andalso 
			arithmetic_condition(Sub2@)) orelse

		(?MATCH(?T("Mult1@ * Mult2@"),Node) andalso
		    arithmetic_condition(Mult1@) andalso 
			arithmetic_condition(Mult2@)) orelse

		(?MATCH(?T("Div1@ div Div2@"), Node) andalso
		     arithmetic_condition(Div1@) andalso 
		     arithmetic_condition(Div2@) andalso
		     begin
			  Result = do_arithmetic(Div2@),
			  Result /= error andalso
			  ?PP(Result) /= "0"			  
		     end
		) orelse 
		(?MATCH(?T("Num@ rem Rem@"), Node) andalso
		    arithmetic_condition(Num@) andalso 
			arithmetic_condition(Rem@)
		); 
	true -> false
    end.

-spec(do_arithmetic(Node::syntaxTree()) -> syntaxTree() | error).
do_arithmetic(Node) ->
    case api_refac:type(Node) of
        integer -> Node;
	variable -> Node;
	infix_expr ->
	    IsSum = ?MATCH(?T("Sum1@ + Sum2@"),Node),
	    if
		IsSum -> core_arit_calc:do_operation('+',do_arithmetic(Sum1@),do_arithmetic(Sum2@));
		true -> IsSub = ?MATCH(?T("Sub1@ - Sub2@"),Node),
		     if
			 IsSub -> core_arit_calc:do_operation('-',do_arithmetic(Sub1@),do_arithmetic(Sub2@));
			 true -> IsMult = ?MATCH(?T("Mult1@ * Mult2@"),Node),
			      if
				  IsMult -> core_arit_calc:do_operation('*',do_arithmetic(Mult1@) ,do_arithmetic(Mult2@));
				  true -> IsDiv = ?MATCH(?T("Div1@ div Div2@"),Node),
				       if
					   IsDiv -> core_arit_calc:do_operation('div',do_arithmetic(Div1@),do_arithmetic(Div2@));
					   true -> 
					       IsRem = ?MATCH(?T("Num@ rem Rem@"), Node), 
					       if
						   IsRem -> core_arit_calc:do_operation('rem',do_arithmetic(Num@), do_arithmetic(Rem@));
						   true -> error
					       end
				       end
			      end
		     end
	    end;
	_ -> error
    end.

pos_to_node(Scope, Pos) -> 
    api_interface:pos_to_node(Scope, Pos, fun(Node) -> ?MATCH(?T("Var@ = Exp@"), Node) end).





