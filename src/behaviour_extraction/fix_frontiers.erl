%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Moves the frontiers of the clusters so that functions can
%%% be extracted from them. This is done by reducing the size
%%% of the common clusters, and expanding the size of the
%%% exclusive clusters.
%%% @end
%%% Created :  2 Jun 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(fix_frontiers).

-export([fix_frontiers/5, delete_trivial_clusters/1, node_exports_vars/2]).

%%-----------------------------------------------------------------------
%% @doc
%% Moves the frontiers of the clusters to places where function
%% calls can be used to divide the trees.
%% This function updates both the Mapping and the CommCluster
%% dictionary. The right exclusive clusters are generated from the
%% fixed Mapping at {@link tree_clustering:cluster/3}
%% @end
%%-----------------------------------------------------------------------
-spec fix_frontiers(Pass :: 1 | 2,
		    Tree1 :: tree:tree(), Tree2 :: tree:tree(),
                    Mapping :: da_map:da_map(tree:tree_node(),
					     tree:tree_node()),
                    CommCluster :: cluster_dict:cluster_dict(tree:tree_node())) ->
			   {cluster_dict:cluster_dict(tree:tree_node()),
			    da_map:da_map(tree:tree_node(),
					  tree:tree_node())}.
fix_frontiers(Pass, Tree1, Tree2, Mapping, CommCluster) ->
    cluster_dict:growing_map_fold(fix_cluster(Pass, Tree1, Tree2),
				  Mapping, CommCluster).

%%-----------------------------------------------------------------------
%% @doc
%% Produces a function that, when growing_map_folded on the common cluster
%% dictionary with the mapping as accumulator, it fixes the frontiers
%% in both the common cluster dictionary and the accumulator.
%% @see cluster_dict:growing_map_fold/3
%% @end
%%-----------------------------------------------------------------------
-spec fix_cluster(Pass :: 1 | 2,
		  Tree1 :: tree:tree(), Tree2 :: tree:tree()) ->
          (fun ((Cluster :: cluster:cluster(tree:tree_node()),
                 Mapping :: da_map:da_map(tree:tree_node(),
					  tree:tree_node())) ->
		       {[cluster:cluster(tree:tree_node())], none | cluster:cluster(tree:tree_node()),
			da_map:da_map(tree:tree_node(), tree:tree_node())})).
fix_cluster(Pass, Tree1, Tree2) ->
    fun (Cluster, Mapping) ->
	    fix_cluster_mapfold(Pass, Tree1, Tree2, Cluster, Mapping)
    end.

%%-----------------------------------------------------------------------
%% @doc
%% It fixes the frontiers in both the current cluster and the
%% accumulator.
%% @end
%%-----------------------------------------------------------------------
-spec fix_cluster_mapfold(Pass :: 1 | 2,
			  Tree1 :: tree:tree(), Tree2 :: tree:tree(),
                          Cluster :: cluster:cluster(tree:tree_node()),
                          Mapping :: da_map:da_map(tree:tree_node(),
                                            tree:tree_node())) ->
				 {[cluster:cluster(tree:tree_node())],
				  none | cluster:cluster(tree:tree_node()),
				  da_map:da_map(tree:tree_node(),
						tree:tree_node())}.
fix_cluster_mapfold(Pass, Tree1, Tree2, Cluster, Mapping) ->
    case get_wrong_frontiers_fun(Pass, Tree1, Tree2, Cluster) of
	[WrongFrontier|_] -> detach_node_pair(WrongFrontier,
					      {Cluster, Mapping});
	[] -> {[], Cluster, Mapping}
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Returns a list of the node pairs that produce a wrong frontier.
%% @end
%%-----------------------------------------------------------------------
-spec get_wrong_frontiers_fun(Pass :: 1 | 2,
			      Tree1 :: tree:tree(), Tree2 :: tree:tree(),
			      Cluster :: cluster:cluster(tree:tree_node())) ->
				     [tree:tree_node()].
get_wrong_frontiers_fun(Pass, Tree1, Tree2, Cluster) ->
    Children = cluster:get_nodes(Cluster),
    lists:filter(
      is_wrong_frontier_filter(Pass, Tree1, Tree2, Cluster),
      lists:usort(Children)).

%%-----------------------------------------------------------------------
%% @doc
%% Produces a function that returns true when applied to a node pair
%% that produces a wrong frontier, it returns false otherwise.
%% @end
%%-----------------------------------------------------------------------
-spec is_wrong_frontier_filter(Pass :: 1 | 2,
			       Tree1 :: tree:tree(), Tree2 :: tree:tree(),
                               Cluster :: cluster:cluster(tree:tree_node())) ->
				      fun((tree:tree_node()) -> boolean()).
is_wrong_frontier_filter(Pass, Tree1, Tree2, Cluster) ->
    fun (NodePair) ->
	    is_wrong_frontier(Pass, NodePair, Tree1, Tree2, Cluster)
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Returns true when applied to a node pair that produces a wrong
%% frontier, it returns false otherwise.
%% @end
%%-----------------------------------------------------------------------
-spec is_wrong_frontier(Pass :: 1 | 2,
			NodePair :: tree:tree_node(),
                        Tree1 :: tree:tree(), Tree2 :: tree:tree(),
                        Cluster :: cluster:cluster(tree:tree_node())) ->
			       boolean().
is_wrong_frontier(Pass, NodePair, Tree1, Tree2, Cluster) ->
    {Node1, Node2} = tree:get_pair_tuple(NodePair),
    OkParent1 = tree:get_parent(Node1, Tree1),
    OkParent2 = tree:get_parent(Node2, Tree2),
    breaks_behaviour_info(Node1, Tree1)
	orelse breaks_behaviour_info(Node2, Tree2)
	orelse are_parents_wrong_frontier(Cluster, NodePair, OkParent1, OkParent2)
	orelse are_children_wrong_frontier(Tree1, Tree2, Cluster, Node1, Node2)
	orelse is_wrong_frontier_in_second_pass(Pass, Tree1, Tree2, Cluster,
						Node1, Node2, OkParent1, OkParent2).

%%-----------------------------------------------------------------------
%% @doc
%% Returns true when applied to a node pair that produces a wrong
%% frontier in terms of the second pass, (and Pass = 2).
%% @end
%%-----------------------------------------------------------------------
-spec is_wrong_frontier_in_second_pass(
	Pass :: 1 | 2, Tree1 :: tree:tree(), Tree2 :: tree:tree(),
	Cluster :: cluster:cluster(tree:tree_node()),
	Node1 :: tree:tree_node(), Node2 :: tree:tree_node(),
	PNode1 :: 'error' | {'ok', tree:tree_node()},
	PNode2 :: 'error' | {'ok', tree:tree_node()}) -> boolean().
is_wrong_frontier_in_second_pass(2, Tree1, Tree2, Cluster, Node1,
		                 Node2, OkParent1, OkParent2) ->
    frontier_breaks_artificial_block(Tree1, Tree2, Cluster, Node1, Node2)
	orelse frontier_exports_vars(Tree1, Tree2, Cluster, Node1, Node2, OkParent1, OkParent2);
is_wrong_frontier_in_second_pass(_, _, _, _, _, _, _, _) -> false.

%%-----------------------------------------------------------------------
%% @doc
%% Returns true if Node is the root of a declaration with name
%% `behaviour_info'.
%% @end
%%-----------------------------------------------------------------------
-spec breaks_behaviour_info(tree:tree_node(),tree:tree()) -> boolean().
breaks_behaviour_info(Node, Tree) ->
    NodeValue = tree:get_value(Node),
    case wrangler_syntax:type(NodeValue) of
	function -> NameRef = wrangler_syntax:function_name(NodeValue),
		    {ok, NameNode} = tree:get_node(NameRef, Tree),
		    NameValue = tree:get_value(NameNode),
		    case wrangler_syntax:type(NameValue) of
			atom -> wrangler_syntax:atom_value(NameValue) =:= behaviour_info;
			_ -> false
		    end;
	_ -> false
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Returns true when the node pair exports vars. Breaking through
%% here would imply that the exported vars are not accessible from
%% parent or sibling clusters.
%% @end
%%-----------------------------------------------------------------------
-spec frontier_exports_vars(Tree1 :: tree:tree(), Tree2 :: tree:tree(),
			    Cluster :: cluster:cluster(tree:tree_node()),
			    Node1 :: tree:tree_node(),
			    Node2 :: tree:tree_node(),
			    PNode1 :: 'error' | {'ok', tree:tree_node()},
			    PNode2 :: 'error' | {'ok', tree:tree_node()}) -> boolean().
frontier_exports_vars(Tree1, Tree2, Cluster, Node1, Node2, OkPNode1, OkPNode2) ->
    any_children_not_in_cluster_export_vars(Tree1, Tree2, Cluster, Node1, Node2)
	orelse (parents_not_in_cluster(Cluster, OkPNode1, OkPNode2)
		andalso (not (is_artificial_block(Node1) andalso
			      is_artificial_block(Node2)))
		andalso (node_exports_vars(Node1, Node2, Tree1, Tree2))).

%%-----------------------------------------------------------------------
%% @doc
%% Returns true if either Node1 (assumed to belong to Tree1),
%% or Node2 (assumed to belong to Tree2), export variables.
%% @end
%%-----------------------------------------------------------------------
-spec node_exports_vars(Node1 :: tree:tree_node(), Node2 :: tree:tree_node(),
			Tree1 :: tree:tree(), Tree2 :: tree:tree()) -> boolean().
node_exports_vars(Node1, Node2, Tree1, Tree2) ->
    node_exports_vars(Node1, Tree1) orelse node_exports_vars(Node2, Tree2).

%%-----------------------------------------------------------------------
%% @doc
%% Returns true if either Node (assumed to belong to Tree), exports
%% variables.
%% @end
%%-----------------------------------------------------------------------
-spec node_exports_vars(Node :: tree:tree_node(), Tree :: tree:tree()) ->
			       boolean().
node_exports_vars(Node, Tree) ->
    api_refac:exported_vars(ast_tree:tree_to_ast(Node, Tree)) =/= [].

%%-----------------------------------------------------------------------
%% @doc
%% It returns true if the either Node1 or Node2 have children
%% out of the cluster that export vars, except if they are the
%% root of an artificial block (which is solved later).
%% @end
%%-----------------------------------------------------------------------
-spec any_children_not_in_cluster_export_vars(
	Tree1 :: tree:tree(), Tree2 :: tree:tree(),
	Cluster :: cluster:cluster(tree:tree_node()),
	Node1 :: tree:tree_node(),
	Node2 :: tree:tree_node()) -> boolean().
any_children_not_in_cluster_export_vars(Tree1, Tree2, Cluster, Node1, Node2) ->
    lists:any(is_not_in_cluster_and_exports_vars(Cluster, Tree1, Tree2),
	      lists:zip(tree:get_children(Node1, Tree1),
			tree:get_children(Node2, Tree2))).

%%-----------------------------------------------------------------------
%% @doc
%% Returns a function that when applied to a tuple with two nodes
%% returns true if the nodes are out of the cluster and they export
%% vars, except if they are the root of an artificial block
%% (which is solved later).
%% @end
%%-----------------------------------------------------------------------
-spec is_not_in_cluster_and_exports_vars(
	Cluster :: cluster:cluster(tree:tree_node()),
	Tree1 :: tree:tree(),
	Tree2 :: tree:tree()) -> fun(({Node1 :: tree:tree_node(),
				       Node2 :: tree:tree_node()}) ->
					    boolean()).
is_not_in_cluster_and_exports_vars(Cluster, Tree1, Tree2) ->
    fun ({Node1, Node2}) ->
	    NodePair = tree:create_node_pair(Node1, Node2),
	    (not cluster:has_node(NodePair, Cluster))
		andalso (not (is_artificial_block(Node1) andalso
			      is_artificial_block(Node2)))
		andalso (node_exports_vars(Node1, Node2, Tree1, Tree2))
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Returns true PNode1 and PNode2 (parents of a node of the cluster),
%% exist and are out of the cluster.
%% @end
%%-----------------------------------------------------------------------
-spec parents_not_in_cluster(Cluster :: cluster:cluster(tree:tree_node()),
			     PNode1 :: 'error' | {'ok', tree:tree_node()},
			     PNode2 :: 'error' | {'ok', tree:tree_node()}) ->
				    boolean().
parents_not_in_cluster(_Cluster, error, error) -> false;
parents_not_in_cluster(Cluster, {ok, PNode1}, {ok, PNode2}) ->
    PNodePair = tree:create_node_pair(PNode1, PNode2),
    not (cluster:has_node(PNodePair, Cluster));
parents_not_in_cluster(_Cluster, _, _) -> true.

%%-----------------------------------------------------------------------
%% @doc
%% Returns true when the node pair breaks an artificial block.
%% This should only happen in the case when the frontier is at the
%% bottom of a common cluster, since the mapping algorithm already
%% tries to maximise the number of common nodes, and the artificial
%% blocks must always be common to both trees.
%% @end
%%-----------------------------------------------------------------------
-spec frontier_breaks_artificial_block(Tree1 :: tree:tree(), Tree2 :: tree:tree(),
					Cluster :: cluster:cluster(tree:tree_node()),
					Node1 :: tree:tree_node(),
					Node2 :: tree:tree_node()) -> boolean().
frontier_breaks_artificial_block(Tree1, Tree2, Cluster, Node1, Node2) ->
    any_children_not_in_cluster(Tree1, Tree2, Cluster, Node1, Node2)
	andalso are_artificial_block(Node1, Node2).

%%-----------------------------------------------------------------------
%% @doc
%% Returns true if Node is an artificial block.
%% @end
%%-----------------------------------------------------------------------
-spec is_artificial_block(tree:tree_node()) -> boolean().
is_artificial_block(Node) -> tree:get_property(is_artificial_block,
					       Node) =:= {ok, true}.

%%-----------------------------------------------------------------------
%% @doc
%% Returns true if both Node1 and Node2 are an artificial block.
%% @end
%%-----------------------------------------------------------------------
-spec are_artificial_block(tree:tree_node(),_) -> boolean().
are_artificial_block(Node1, Node2) ->
    is_artificial_block(Node1) andalso is_artificial_block(Node2).

%%-----------------------------------------------------------------------
%% @doc
%% Returns true when the node pair has a frontier with any of its
%% children. It is only applicable to node pairs.
%% @end
%%-----------------------------------------------------------------------
-spec any_children_not_in_cluster(Tree1 :: tree:tree(),
                                  Tree2 :: tree:tree(),
                                  Cluster :: cluster:cluster(tree:tree_node()),
                                  Node1 :: tree:tree_node(),
                                  Node2 :: tree:tree_node()) ->
          boolean().
any_children_not_in_cluster(Tree1, Tree2, Cluster, Node1, Node2) ->
    lists:any(is_not_in_cluster(Cluster),
	      lists:zip(tree:get_children(Node1, Tree1),
			tree:get_children(Node2, Tree2))).

%%-----------------------------------------------------------------------
%% @doc
%% Returns true when the node pair has a wrong frontier with its parents,
%% it returns false otherwise. It is only applicable to node pairs.
%% PNode1 and PNode2 are expected to be the result of calling
%% {@link tree:get_parent/2} on each side of the node pair.
%% @end
%%-----------------------------------------------------------------------
-spec are_parents_wrong_frontier(Cluster :: cluster:cluster(TreeNode),
                                 NodePair :: TreeNode,
                                 PNode1 :: 'error' | {'ok', tree:tree_node()},
                                 PNode2 :: 'error' | {'ok', tree:tree_node()}) ->
          boolean() when
      TreeNode :: tree:tree_node().
are_parents_wrong_frontier(_Cluster, _NodePair, error, error) -> false;
are_parents_wrong_frontier(Cluster, NodePair, {ok, PNode1}, {ok, PNode2}) ->
    PNodePair = tree:create_node_pair(PNode1, PNode2),
    ((not ast_tree:is_expression_or_function(NodePair))
     orelse ast_tree:breaks_funname(NodePair, PNode1)
     orelse ast_tree:breaks_funname(NodePair, PNode2)
    ) andalso (not cluster:has_node(PNodePair, Cluster));
are_parents_wrong_frontier(_Cluster, NodePair, _, _) ->
    not ast_tree:is_expression_or_function(NodePair).

%%-----------------------------------------------------------------------
%% @doc
%% Returns true when the node pair has a wrong frontier with its
%% children, it returns false otherwise. It is only applicable to node
%% pairs.
%% @end
%%-----------------------------------------------------------------------
-spec are_children_wrong_frontier(Tree1 :: tree:tree(),
                                  Tree2 :: tree:tree(),
                                  Cluster :: cluster:cluster(tree:tree_node()),
                                  Node1 :: tree:tree_node(),
                                  Node2 :: tree:tree_node()) ->
          boolean().
are_children_wrong_frontier(Tree1, Tree2, Cluster, Node1, Node2) ->
    not lists:all(is_child_wrong_frontier(Cluster, Node1, Node2),
		  lists:zip(tree:get_children(Node1, Tree1),
			    tree:get_children(Node2, Tree2))).

%%-----------------------------------------------------------------------
%% @doc
%% Produces a function that, when applied to a Node, it returns true
%% if and only if Node belongs to Cluster.
%% @end
%%-----------------------------------------------------------------------
-spec is_not_in_cluster(Cluster :: cluster:cluster(tree:tree_node())) ->
			   (fun (({Node1 :: tree:tree_node(), Node2 :: tree:tree_node()}) -> boolean())).
is_not_in_cluster(Cluster) ->
    fun ({Node1, Node2}) ->
	    NodePair = tree:create_node_pair(Node1, Node2),
	    not cluster:has_node(NodePair, Cluster)
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Produces a function that, when applied to a tuple with a pair of
%% nodes, it returns true if and only if (is an expression or a function,
%% and does not separate a function call from its name, or belongs to
%% Cluster.
%% @end
%%-----------------------------------------------------------------------
-spec is_child_wrong_frontier(Cluster :: cluster:cluster(tree:tree_node()),
			      PNode1 :: tree:tree_node(),
                              PNode2 :: tree:tree_node()) -> 
                                 (fun(({Node1 :: tree:tree_node(),
                                        Node2 :: tree:tree_node()}) -> 
                                          boolean())).
is_child_wrong_frontier(Cluster, PNode1, PNode2) ->
    fun ({Node1, Node2}) ->
	    NodePair = tree:create_node_pair(Node1, Node2),
	    (ast_tree:is_expression_or_function(Node1)
	     andalso ast_tree:is_expression_or_function(Node2)
	     andalso (not ast_tree:breaks_funname(NodePair, PNode1))
	     andalso (not ast_tree:breaks_funname(NodePair, PNode2)))
		orelse cluster:has_node(NodePair, Cluster)
    end.

%%-----------------------------------------------------------------------
%% @doc
%% It removes a node pair from the Cluster and from the Mapping.
%% @end
%%-----------------------------------------------------------------------
-spec detach_node_pair(NodePair :: tree:tree_node(),
                       {Cluster :: cluster:cluster(tree:tree_node()),
                        Mapping :: da_map:da_map(tree:tree_node(),
						 tree:tree_node())}) ->
			      {[cluster:cluster(tree:tree_node())], none,
			       da_map:da_map(tree:tree_node(),
					     tree:tree_node())}.
detach_node_pair(NodePair, {Cluster, Mapping}) ->
    {Node1, _Node2} = tree:get_pair_tuple(NodePair),
    NewClusters = cluster:remove_split(NodePair, Cluster),
    {NewClusters, none, da_map:delete_by_key(Node1, Mapping)}.

%%-----------------------------------------------------------------------
%% @doc
%% Removes from the CommClusterDict and detaches from the Mapping every
%% cluster with a single node in it.
%% @end
%%-----------------------------------------------------------------------
-spec delete_trivial_clusters({CommClusterDict :: cluster_dict:cluster_dict(
						    tree:tree_node()),
			       Mapping :: da_map:da_map(tree:tree_node(),
							tree:tree_node())}) ->
				     {cluster_dict:cluster_dict(tree:tree_node()),
				      da_map:da_map(tree:tree_node(), tree:tree_node())}.
delete_trivial_clusters({Comm, Mapping}) ->
    {TrivialClusters, NewComm} = cluster_dict:extract_trivial_clusters(Comm),
    {NewComm,
     lists:foldl(fun (X, Y) ->
			 {Node1, _Node2} = tree:get_pair_tuple(X),
			 da_map:delete_by_key(Node1, Y)
		 end, Mapping, lists:flatmap(fun cluster:get_nodes/1,
					     TrivialClusters))}.
