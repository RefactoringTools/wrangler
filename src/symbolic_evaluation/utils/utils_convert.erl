%% @private
-module(utils_convert).
%% Include files
-include("wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-compile([export_all]).

%%--------------------------------------------------------------------
%% @doc
%% Auxiliar function that receives an AST and returns a list.
%% @end
%% @private
%%--------------------------------------------------------------------
ast_to_list(ListNode, List) -> 
    ListType = api_refac:type(ListNode),
    case ListType of
	nil -> List;
	list ->
		    Head = convert_elem(wrangler_syntax:list_head(ListNode)),
		    case Head of
			error -> error;
			_ ->  
			    ast_to_list(wrangler_syntax:list_tail(ListNode), 
					List ++ [Head])
		    end;
	_ -> error
    end.
    

ast_to_tuple(Elem)->
    Str = ?PP(Elem) ++ ".",
    {ok, Tokens, _} = erl_scan:string(Str),
    Parse = erl_parse:parse_term(Tokens),
    case Parse of
	{ok, Tuple} -> Tuple; 
	_ -> error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Auxiliar function that converts an element depending on its type.
%% @end
%% @private
%%--------------------------------------------------------------------
convert_elem(Elem) ->
    ElemType = api_refac:type(Elem),
    case ElemType of
	integer -> wrangler_syntax:integer_value(Elem);
	atom -> wrangler_syntax:atom_value(Elem);
	string -> wrangler_syntax:string_value(Elem);
	float -> wrangler_syntax:float_value(Elem);
	nil -> [];
	list -> ast_to_list(Elem, []);
	tuple -> ast_to_tuple(Elem);
	_ -> error
    end.

convertable_type(Node) ->
    NodeType = api_refac:type(Node),
    Filter = lists:filter(fun(X) -> X == NodeType end, [integer, atom, string, float, nil, list]),
    if
	Filter /= [] -> true;
	NodeType == tuple -> ast_to_tuple(Node) /= error;
	true -> false
    end.
	
