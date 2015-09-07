%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Removes artificial blocks that are expected to both return and
%%% export variables at the same time.
%%% @end
%%% Created :  6 Ago 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(fun_blocks_fix).

-export([export_variables/3]).

-include("cluster_records.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Removes blocks that are expected to return and export variables
%% at the same time. If possible it will just remove the last
%% item from the blocks.
%% @end
%%--------------------------------------------------------------------
-spec export_variables(LeftTree :: tree:tree(), RightTree :: tree:tree(),
		       Mapping :: da_map:da_map(tree:tree_node(),tree:tree_node())) ->
			      {Tree :: tree:tree(), Tree :: tree:tree()}.
export_variables(LeftTree, RightTree, _Mapping) ->
    LeftAB = lists:sort(tree:breadth_fold(fun find_artificial_blocks/2, [], LeftTree)),
    RightAB = lists:sort(tree:breadth_fold(fun find_artificial_blocks/2, [], RightTree)),
    ArtificialBlocks = [{LAB, RAB} || {{N, LAB}, {N, RAB}} <- lists:zip(LeftAB, RightAB)],
    lists:foldl(fun fix_artificial_block/2, {LeftTree, RightTree}, ArtificialBlocks).

%%--------------------------------------------------------------------
%% @doc
%% Accumulates Node together with its artificial block number if
%% it is an artificial block.
%% @end
%%--------------------------------------------------------------------
-spec find_artificial_blocks(Node :: tree:tree_node(),
			     Acc :: [{BlockNum :: non_neg_integer(),
				      BlockNode :: tree:tree_node()}]) ->
				    {BlockNum :: non_neg_integer(),
				     BlockNode :: tree:tree_node()}.
find_artificial_blocks(Node, List) ->
    case tree:get_property(artificial_block_number, Node) of
	{ok, Num} -> [{Num, Node}|List];
	_ -> List
    end.

%%--------------------------------------------------------------------
%% @doc
%% Takes a pair of artificial blocks LAB and RAB and fixes it if
%% necessary.
%% @end
%%--------------------------------------------------------------------
-spec fix_artificial_block(ArtificialBlockPair :: {LAB :: tree:tree_node(),
						   RAB :: tree:tree_node()},
			   Trees :: {LeftTree :: tree:tree(),
				     RightTree :: tree:tree()}) ->
				  {tree:tree(),tree:tree()}.
fix_artificial_block({LAB, RAB} = _ABS, {LeftTree, RightTree}) ->
    case {((fix_frontiers:node_exports_vars(LAB, LeftTree) orelse
	    fix_frontiers:node_exports_vars(RAB, RightTree))
	   andalso (is_last_child(LAB, LeftTree) orelse
		    is_last_child(RAB, RightTree))),
	  length(tree:get_children(LAB, LeftTree))} of
	{true, N} when N < 3 -> {remove_node(LAB, LeftTree),
				 remove_node(RAB, RightTree)};
	{true, _} -> {fix_node(LAB, LeftTree), fix_node(RAB, RightTree)};
	{false, _} -> {LeftTree, RightTree}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns true if the Node is the last child of its parent.
%% It assumes it has a parent.
%% @end
%%--------------------------------------------------------------------
-spec is_last_child(Node :: tree:tree_node(),
		    Tree :: tree:tree()) -> boolean().
is_last_child(Node, Tree) ->
    {ok, Parent} = tree:get_parent(Node, Tree),
    lists:last(tree:get_children(Parent, Tree)) =:= Node.

%%--------------------------------------------------------------------
%% @doc
%% Removes Node from its parent children list, but
%% puts its own childen in the place.
%% @end
%%--------------------------------------------------------------------
-spec remove_node(Node :: tree:tree_node(), Tree :: tree:tree()) -> tree:tree().
remove_node(Node, Tree) ->
    {ok, Parent} = tree:get_parent(Node, Tree),
    Children = tree:get_children(Node, Tree),
    NewParent = replace_in_node(Parent, [Node], Children),
    NewTree = tree:store_node(NewParent, Tree),    
    tree:collapse_node(Node, NewTree).

%%--------------------------------------------------------------------
%% @doc
%% Pulls the last children of Node to be its next sibbling.
%% It upgrades the last children of Node and places it after
%% the Node in the list of children of the parent of Node.
%% @end
%%--------------------------------------------------------------------
-spec fix_node(Node :: tree:tree_node(),
	       Tree :: tree:tree()) -> tree:tree().
fix_node(Node, Tree) ->
    {ok, Parent} = tree:get_parent(Node, Tree),
    {LastChild, RestChildren} = reverse_cons_dec(tree:get_children(Node, Tree)),
    NewParent = replace_in_node(Parent, [Node], [Node, LastChild]),
    NewNode = tree:set_value(fun_blocks:create_artificial_block_value(Tree, get_phs(RestChildren)), Node),
    NewTree = tree:store_node(NewNode, tree:store_node(NewParent, Tree)),    
    tree:upgrade_node(LastChild, NewTree).

%%--------------------------------------------------------------------
%% @doc
%% Deconstructs the reverse of List,
%% (as its natural constructor does).
%% @end
%%--------------------------------------------------------------------
-spec reverse_cons_dec(List :: [El]) -> {El, [El]}.
reverse_cons_dec(List) ->
    [H|T] = lists:reverse(List),
    {H, lists:reverse(T)}.

%%--------------------------------------------------------------------
%% @doc
%% Replaces the list of nodes Pattern with the list of
%% nodes Replacement, in the list of children of Node.
%% It assumes that Node is a block expression or clause.
%% @end
%%--------------------------------------------------------------------
-spec replace_in_node(Node :: tree:tree_node(),
		      Pattern :: [tree:tree_node()],
		      Replacement :: [tree:tree_node()]) ->
			     tree:tree_node().
replace_in_node(Node, Pattern, Replacement) ->
    Value = tree:get_value(Node),
    NewValue = fun_blocks:replace_children_in_body(
		 get_phs(Pattern), get_phs(Replacement), Value),
    tree:set_value(NewValue, Node).

%%--------------------------------------------------------------------
%% @doc
%% Returns the place holder for each of the nodes in
%% NodeList. It assumes there are no node-pairs.
%% @end
%%--------------------------------------------------------------------
-spec get_phs(NodeList :: [tree:tree_node()]) -> [tree:tree_ph()].
get_phs(NodeList) ->
    [begin
	 {node, PH} = tree:get_ph(Node), PH
     end || Node <- NodeList].

