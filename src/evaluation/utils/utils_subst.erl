-module(utils_subst).
-export([subst/3,getNode/1]).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Recursive function to create the list of substitutions.
%% @spec substsList([syntaxTree()] | syntaxTree(),[syntaxTree()] | syntaxTree()) -> [{string(),syntaxTree()}]
%% @end
%%--------------------------------------------------------------------
-spec(substsList([syntaxTree()] | syntaxTree(),[syntaxTree()] | syntaxTree()) -> [{string(),syntaxTree()}]).
substsList([],[]) -> [];
substsList([H | T], [H2 | T2]) -> elemSubstitution(H,H2) ++ substsList(T,T2);
substsList(A,B) -> elemSubstitution(A,B).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates correspondence between value to be substituted and value to substitute
%% @spec elemSubstitution(syntaxTree(),syntaxTree()) -> [{string(),syntaxTree()}]
%% @end
%%--------------------------------------------------------------------
-spec(elemSubstitution(syntaxTree(),syntaxTree()) -> [{string(),syntaxTree()}]).
elemSubstitution(NodeElem,NodeValue) ->
    ElemType = api_refac:type(NodeElem),
    ValueType = api_refac:type(NodeValue),
    if
	ElemType == list andalso ValueType == list -> 
	    [ { ?PP(wrangler_syntax:list_head(NodeElem)), 
		wrangler_syntax:list_head(NodeValue) }, 
	      { ?PP(wrangler_syntax:list_tail(NodeElem)), 
		wrangler_syntax:list_tail(NodeValue)  } ];
	ElemType == tuple andalso ValueType == tuple ->
	    substsList(wrangler_syntax:tuple_elements(NodeElem),
		       wrangler_syntax:tuple_elements(NodeValue));
	true -> [ {?PP(NodeElem),NodeValue} ]
    end. 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calls the do_subst function with stop_tdTP approach. 
%% @spec subst([syntaxTree()] | syntaxTree(),syntaxTree(), syntaxTree()) -> [syntaxTree()] | syntaxTree()
%% @end
%%--------------------------------------------------------------------	  
-spec(subst([syntaxTree()] | syntaxTree(),syntaxTree(), syntaxTree()) -> [syntaxTree()] | syntaxTree()). 
subst(Expr, ArgPatt, Arg) when is_list(Expr) ->
    Subst = substsList(ArgPatt,Arg),
    [subst(E, Subst)||E<-Expr];
subst(Expr, ArgPatt, Arg) ->
    subst(Expr, substsList(ArgPatt,Arg)).

subst(Expr, Subst) ->
    {Expr1, _} = api_ast_traverse:stop_tdTP(fun do_subst/2, Expr, Subst),
    Expr1.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function does the substitution in functions. When there is a function call, this function substitutes it by the result of applying that%% function. 
%% @spec do_subst(syntaxTree(),[{string(),syntaxTree()}]) -> {syntaxTree(),boolean()}
%% @end
%%--------------------------------------------------------------------   
-spec(do_subst(syntaxTree(),[{string(),syntaxTree()}]) -> {syntaxTree(),boolean()}).
do_subst(Node, Subst) ->  
    NodeType = api_refac:type(Node),
    if
	NodeType == variable orelse NodeType == match_expr ->
	    VarName = ?PP(Node),
		    case lists:keysearch(VarName, 1, Subst) of
                        {value, {VarName, Expr}} ->
			    E1 = if
                               Expr /= none ->			     
					 api_refac:reset_pos_and_range(Expr);                               
			       true -> ?TO_AST("[]")
                            end,
			    {E1,  true};
                        false ->
			    {Node, false}
                    end;
        NodeType == atom ->
            AtomValue = wrangler_syntax:atom_value(Node),
            case api_refac: is_meta_atom_name(AtomValue) of
                true ->
                    NewAtomValue=list_to_atom("_W_"++atom_to_list(AtomValue)),
                    case lists:keysearch(NewAtomValue, 1, Subst) of
                        {value, {NewAtomValue, Expr}} ->
                            {api_refac:reset_pos_and_range(Expr), true};
                        false ->
                            throw({error, lists:flatten(
                                            io_lib:format
                                              ("Meta atom ~p is not bound.", [AtomValue]))})
                    end;
                _ ->
                    {Node, false} 
            end;
	true -> {Node, false}
    end.

getNode([Node | []]) -> Node;
getNode(Node) -> Node.
