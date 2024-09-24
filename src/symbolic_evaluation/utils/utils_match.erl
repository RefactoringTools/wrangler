%% @private
-module(utils_match).
-export([match/2, matchElem/2, matchList/2, firstMatch/3]).

%% Include files
-include("wrangler.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return if two lists or two elements match. 
%% @spec match([syntaxTree()] | syntaxTree(),[syntaxTree()] | syntaxTree()) -> boolean()
%% @end
%%--------------------------------------------------------------------
-spec(match([syntaxTree()] | syntaxTree(),[syntaxTree()] | syntaxTree()) -> atom()).
match([],[]) -> true;
match(L1,L2) when is_list(L1) andalso is_list(L2) andalso length(L1) == length(L2)->
   matchList(L1,L2);
match(L1,L2) when is_list(L1) or is_list(L2) -> false;
match(A,B) -> matchElem(A,B).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return if two elements match. 
%% @spec matchElem(syntaxTree(),syntaxTree()) -> boolean()
%% @end
%%--------------------------------------------------------------------
-spec(matchElem(syntaxTree(),syntaxTree()) -> atom()).
matchElem(Node1,Node2) ->
    Node1Type = api_refac:type(Node1), 
    Node2Type = api_refac:type(Node2),
    if
	Node1Type == nil andalso Node2Type == nil -> true;
	Node1Type == application  -> 'maybe';
	Node2Type == variable orelse Node2Type == underscore -> true;
	Node1Type == infix_expr  -> 'maybe';
	Node1Type == variable  -> 'maybe';
	Node1Type == integer andalso Node2Type == integer -> utils_convert:convert_elem(Node1) == utils_convert:convert_elem(Node2);
	Node1Type == atom andalso Node2Type == atom -> utils_convert:convert_elem(Node1) == utils_convert:convert_elem(Node2);
        Node1Type == string andalso Node2Type == string -> utils_convert:convert_elem(Node1) == utils_convert:convert_elem(Node2);
	Node1Type == float andalso Node2Type == float -> utils_convert:convert_elem(Node1) == utils_convert:convert_elem(Node2);
	Node1Type == list andalso Node2Type == list -> 
	    matchElem(wrangler_syntax:list_head(Node1),wrangler_syntax:list_head(Node2)) andalso 
	    matchElem(wrangler_syntax:list_tail(Node1),wrangler_syntax:list_tail(Node2));
	Node1Type == tuple andalso Node2Type == tuple -> 
	    wrangler_syntax:tuple_size(Node1) == wrangler_syntax:tuple_size(Node2) andalso
	    matchList(wrangler_syntax:tuple_elements(Node1),wrangler_syntax:tuple_elements(Node2));
	true -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return if two lists match. 
%% @spec matchList([syntaxTree()],[syntaxTree()]) -> atom()
%% @end
%%--------------------------------------------------------------------
-spec(matchList([syntaxTree()],[syntaxTree()]) -> atom()).    
matchList([],[]) -> true;
matchList([H1 | T1],[H2 | T2])->
    Match = matchElem(H1,H2),
    if
	Match -> matchList(T1,T2);
	Match == 'maybe' -> 'maybe';
	true -> false
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to find a match. If a match is found, the function returns true and stop trying. If no match is found, returns false. 
%% @spec(firstMatch([{{modulename(),functionname(),arity()},syntaxTree(),syntaxTree()}],{modulename(),functionname(),arity()},syntaxTree()) -> noMatch | {match,syntaxTree(),syntaxTree()})
%% @end
%%--------------------------------------------------------------------
-spec(findFirstMatch([{{modulename(),functionname(),arity()},syntaxTree(),[syntaxTree()],syntaxTree()}],{modulename(),functionname(),arity()},syntaxTree()) -> noMatch | {match,syntaxTree(),syntaxTree()}).
findFirstMatch([{{M2,F2,A2},ArgPatt,Guards,Body} | T],{M,F,A},Arg) ->   
    Matches = M == M2 andalso F == F2 andalso A == A2,    
    if
	Matches ->
	    Match = match(Arg,ArgPatt),
	    if
		Match ->
		    GuardsSuceed = utils_guards:guardsSuceed(Arg, ArgPatt, Guards),
		    if
			GuardsSuceed == 'maybe' -> noMatch;
			GuardsSuceed -> {match,ArgPatt,Body};      
			true -> findFirstMatch(T,{M,F,A},Arg)
		    end;

		Match == 'maybe' -> noMatch;
		true  -> findFirstMatch(T,{M,F,A},Arg)
	    end;
	true  -> findFirstMatch(T,{M,F,A},Arg)
    end;
findFirstMatch(_,_,_) -> noMatch.
    
firstMatch({list, List}, {M,F,A},Arg) ->
    Info = case lists:keyfind(M,1,List) of
	false -> [];
	{_,Var} -> Var 
    end,
    findFirstMatch(Info, {M,F,A}, Arg);
firstMatch(Info, FunInfo, Arg) -> findFirstMatch(Info, FunInfo, Arg).

    
