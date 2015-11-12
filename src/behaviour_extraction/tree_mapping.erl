%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Implementation of the algorithm described in paper:
%%%   "diffX: an algorithm to detect changes in multi-version XML documents"
%%% by:
%%%   "Raihan Al-Ekram, Archana Adma and Olga Baysal"
%%% @end
%%% Created :  1 Jun 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(tree_mapping).

-export([mapping/2]).

%%--------------------------------------------------------------------
%% @doc
%% Takes two trees and returns a mapping between their common nodes.
%% @end
%%--------------------------------------------------------------------
-spec mapping(T1 :: tree:tree(), T2 :: tree:tree()) ->
		     da_map:da_map(tree:tree_node(), tree:tree_node()).
%% Procedure Mapping ( T1, T2, M )
%% Input T1, T2 : tree
%% Output M : map
%% Begin
mapping(T1, T2) ->
    OM = new_ast_node_da_map(),
%%  index the nodes of T2
    T2Idx = tree:index_by(fun ast_tree:value/1, T2),
%%  traverse T1 in a level-order sequence
    tree:breadth_fold(
%%   let x be the current node
      fun (X, M) -> 
%%   if (x, _ ) ∈ M then
	      case da_map:has_key(X, M) of
%%    skip current node
		  true -> M;
%%   end-if
		  false ->
%%   let y[] = all nodes from T2 equal to x
		      Y = case dict:find(ast_tree:value(X), T2Idx) of
			      {ok, Value} -> Value;
			      error -> []
			  end,
%%   M" = ∅
%%   for i = 1 to size of y[]
		      FOM2 = lists:foldl(
			       fun (YI, M2) ->
%%    if (_, y[i]) ∉ M
				       case da_map:has_value(YI, M) of
%%    then
					   false ->
%%      M' = ∅
					       OM1 = new_ast_node_da_map(),
%%      Match-Fragment ( x, y[i], M, M' )
					       M1 = match_fragment(X, YI, M, OM1, T1, T2),
%%      if size of M' > size of M"
					       case da_map:size(M1) > da_map:size(M2) of
%%      then
						   true -> M1;
%%        let M" = M'
						   false -> M2
%%      end-if
					       end;
%%    end-if
					   true -> M2
%%   end-for
				       end
			       end, new_ast_node_da_map(), Y),
%%   let M = M ∪ M"
		      da_map:union(M, FOM2)
%%  end-traverse
	      end
      end, OM, T1).
%% End

%%---------------------------------------------------------------------------
%% @doc
%% Tries to match the subtrees that start in the pair of input nodes.
%% Stores the new matched nodes in OM1, which is returned. It skips
%% nodes that have been matched in M.
%% @end
%%---------------------------------------------------------------------------
-spec match_fragment(X :: tree:tree_node(), Y :: tree:tree_node(),
                     M :: da_map:da_map(tree:tree_node(), tree:tree_node()),
                     OM1 :: da_map:da_map(tree:tree_node(), tree:tree_node()),
                     T1 :: tree:tree(), T2 :: tree:tree()) ->
			    da_map:da_map(tree:tree_node(), tree:tree_node()).
%% Procedure Match-Fragment ( x, y, M, M' )
%% Input x, y : node ; M : map
%% Output: M' : map
%% Begin
match_fragment(X, Y, M, OM1, T1, T2) ->
%%  if (x, _ ) ∉ M and ( _, y) ∉ M and node(x) = node(y)
    case (not da_map:has_key(X, M)) andalso (not da_map:has_value(Y, M)) andalso ast_tree:value(X) =:= ast_tree:value(Y) of
%%  then
%%   let M' = M' ∪ {(x, y)}
	true -> M1 = da_map:put(X, Y, OM1),
%%   for i = 1 to minimum of number of children between x and y
		ChildrenPairs = tolerant_zip(tree:get_children(X, T1), tree:get_children(Y, T2)),
		lists:foldl(fun ({ICofX, ICofY}, SM1) ->
%%    Match-Fragment ( i-th child of x, i-th child of y, M, M' )
				    match_fragment(ICofX, ICofY, M, SM1, T1, T2)
%%   end-for
			    end, M1, ChildrenPairs);
%%  end-if
	false -> OM1
    end.
%% End

%%--------------------------------------------------------------------
%% @doc
%% Creates a new double associative map for the type ast_node.
%% @end
%%--------------------------------------------------------------------
-spec new_ast_node_da_map() -> da_map:da_map(tree:tree_node(), tree:tree_node()).
new_ast_node_da_map() ->
    da_map:new().

%%--------------------------------------------------------------------
%% @doc
%% Zips two lists even if they have different length.
%% Takes two lists like: [1, 2, 3, ... ] and [a, b, c, ... ], and
%% returns a combined list [{1, a}, {2, b}, {3, c}, ... ], as long
%% as the shortest of the input lists.
%% @see lists:zip/2
%% @end
%%--------------------------------------------------------------------
-spec tolerant_zip(L1 :: list(X), L2 :: list(Y)) -> list({X, Y}).
tolerant_zip([H1|T1], [H2|T2]) -> [{H1, H2}|tolerant_zip(T1, T2)];
tolerant_zip(_, _) -> [].
