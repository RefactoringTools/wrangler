%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Functions to divide mapped trees in clusters
%%% @end
%%% Created :  2 Jun 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(tree_clustering).

-export([cluster/4]).

%%--------------------------------------------------------------------
%% @doc
%% Takes two trees and a Mapping between them, divides the
%% trees in contiguous clusters. The criteria for division
%% is that the nodes in a cluster must be contiguous and
%% either common to both trees, or exclusive to one of them.
%% It applies {@link fix_frontiers:fix_frontiers/4}.
%% The function returns a tuple with three elements:
%% the list of common clusters, the list of exclusive clusters
%% for Tree1, the list of exclusive clusters for Tree2.
%% @end
%%--------------------------------------------------------------------
-spec cluster(Pass, Mapping, Tree1 :: tree:tree(), Tree2 :: tree:tree()) ->
		     {Mapping,
		      cluster_dict:cluster_dict(tree:tree_node()),
		      cluster_dict:cluster_dict(tree:tree_node()),
		      cluster_dict:cluster_dict(tree:tree_node())} when
      Pass :: 1 | 2,
      Mapping :: da_map:da_map(tree:tree_node(),
			       tree:tree_node()).
cluster(Pass, Mapping, Tree1, Tree2) ->
    TmpMapping = detach_root(Mapping, Tree1, Tree2),
    Comm = da_map:fold(comm_cluster(Tree1, Tree2, TmpMapping),
		       cluster_dict:new(), TmpMapping),
    {NewComm, NewMapping} = fix_frontiers:delete_trivial_clusters(
			      fix_frontiers:fix_frontiers(Pass, Tree1, Tree2, TmpMapping, Comm)),
    Ex1 = tree:breadth_fold(tree_cluster(Tree1, NewMapping),
			    cluster_dict:new(), Tree1),
    Ex2 = tree:breadth_fold(tree_cluster(Tree2, NewMapping),
			    cluster_dict:new(), Tree2),
    {NewMapping, NewComm, Ex1, Ex2}.

%%--------------------------------------------------------------------
%% @doc
%% Detaches the root nodes of both trees in the Mapping.
%% This is intended to cause a chain reaction so that headers of
%% functions will be replicated in the exclusive trees even if they
%% are identical.
%% @end
%%--------------------------------------------------------------------
-spec detach_root(Mapping :: da_map:da_map(tree:tree_node(),
					   tree:tree_node()),
                  Tree1 :: tree:tree(), Tree2 :: tree:tree()) ->
			 da_map:da_map(tree:tree_node(),
				       tree:tree_node()).
detach_root(Mapping, Tree1, Tree2) ->
    Node1 = tree:get_root_node(Tree1),
    Node2 = tree:get_root_node(Tree2),
    da_map:delete_by_value(Node2,
      da_map:delete_by_key(Node1, Mapping)).
%%--------------------------------------------------------------------
%% @doc
%% Takes two trees, and produces a function that,
%% when folded against a mapping between the trees,
%% produces a cluster dictionary with the contiguous
%% mapped clusters of the tree.
%% @end
%%--------------------------------------------------------------------
-spec comm_cluster(Tree1 :: tree:tree(), Tree2 :: tree:tree(),
                   Mapping :: da_map:da_map(tree:tree_node(),
					    tree:tree_node())) ->
			  (fun((Node1 :: tree:tree_node(),
				Node2 :: tree:tree_node(),
				Comm :: cluster_dict:cluster_dict(tree:tree_node())) ->
				      cluster_dict:cluster_dict(tree:tree_node()))).
comm_cluster(Tree1, Tree2, Mapping) ->
    fun (Node1, Node2, Comm) ->
	    comm_cluster_fold(Tree1, Tree2, Mapping,
			      Node1, Node2, Comm)
    end.

%%---------------------------------------------------------------------
%% @doc
%% Adds the pair of nodes to the cluster dictionary ensuring that
%% they get merged to adjacent nodes. This is done by creating
%% a new cluster with the pair of nodes and their parents, (only
%% if the parents are also mapped), and merging this cluster
%% with the existing ones {@link cluster_dict:merge_cluster/2}.
%% @end
%%---------------------------------------------------------------------
-spec comm_cluster_fold(Tree1 :: tree:tree(), Tree2 :: tree:tree(),
                        Mapping :: da_map:da_map(tree:tree_node(),
						 tree:tree_node()),
                        Node1 :: tree:tree_node(),
			Node2 :: tree:tree_node(),
                        CommCluster :: cluster_dict:cluster_dict(tree:tree_node())) ->
			       cluster_dict:cluster_dict(tree:tree_node()).
comm_cluster_fold(Tree1, Tree2, Mapping, Node1, Node2, CommCluster) ->
    NodePair = tree:create_node_pair(Node1, Node2),
    case {tree:get_parent(Node1, Tree1),
	  tree:get_parent(Node2, Tree2)} of
	{error, _} -> add_to_cluster(NodePair, CommCluster);
	{_, error} -> add_to_cluster(NodePair, CommCluster);
	{{ok, PNode1}, {ok, PNode2}} ->
	    PosInParent1 = tree:get_pos_in_parent(Node1, PNode1),
	    PosInParent2 = tree:get_pos_in_parent(Node2, PNode2),
	    PNodePair = tree:create_node_pair(PNode1, PNode2),
	    add_to_cluster(NodePair, PNodePair, CommCluster,
			   da_map:has_pair(PNode1, PNode2, Mapping)
			   andalso (PosInParent1 =:= PosInParent2))
    end.

%%--------------------------------------------------------------------
%% @doc
%% Takes a Tree and a Common cluster dictionary and produces a
%% function that, when folded against a Tree will return a
%% cluster dictionary with the exclusive clusters of the tree.
%% @end
%%--------------------------------------------------------------------
-spec tree_cluster(Tree :: tree:tree(),
                   Mapping :: da_map:da_map(tree:tree_node(),
					    tree:tree_node())) ->
			  (fun((Node :: tree:tree_node(),
				Acc :: cluster_dict:cluster_dict(tree:tree_node())) ->
				      cluster_dict:cluster_dict(tree:tree_node()))).
tree_cluster(Tree, Mapping) ->
    fun (Node, Acc) ->
	    tree_cluster_fold(Tree, Mapping, Node, Acc)
    end.

%%---------------------------------------------------------------------
%% @doc
%% Adds the node to the cluster dictionary ensuring that
%% they get merged to adjacent nodes. This is done by creating
%% a new cluster with the nodes and its parent, (only
%% if the parent is not mapped), and merging this cluster
%% with the existing ones {@link cluster_dict:merge_cluster/2}.
%% @end
%%---------------------------------------------------------------------
-spec tree_cluster_fold(Tree :: tree:tree(),
                        Mapping :: da_map:da_map(tree:tree_node(),
						 tree:tree_node()),
                        Node :: tree:tree_node(),
                        ExCluster :: cluster_dict:cluster_dict(tree:tree_node())) ->
			       cluster_dict:cluster_dict(tree:tree_node()).
tree_cluster_fold(Tree, Mapping, Node, ExCluster) ->
    case is_mapped(Node, Mapping) of
	true -> ExCluster;
	false -> case tree:get_parent(Node, Tree) of
		      error -> add_to_cluster(Node, ExCluster);
		      {ok, PNode} ->
			  add_to_cluster(Node, PNode, ExCluster,
					 not (is_mapped(PNode,Mapping)))
		 end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns true if Node is Key or Value in the DaMap.
%% @end
%%--------------------------------------------------------------------
-spec is_mapped(Node :: NodeType,
		DaMap :: da_map:da_map(Key :: NodeType,
				       Value :: NodeType)) -> boolean().
is_mapped(Node, Mapping) ->
    da_map:has_key(Node, Mapping) orelse
    da_map:has_value(Node, Mapping).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new cluster with Node and merges it in the
%% ClusterDict.
%% @end
%%--------------------------------------------------------------------
-spec add_to_cluster(Node :: N,
                     ClusterDict :: cluster_dict:cluster_dict(N)) ->
			    cluster_dict:cluster_dict(N).
add_to_cluster(Node, ClusterDict) ->
    NewCluster = cluster:new_cluster(Node),
    cluster_dict:merge_cluster(NewCluster, ClusterDict).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new cluster with Node and merges it in the
%% ClusterDict. If InsertParent is true it also inserts
%% ParentNode in the same cluster.
%% @see add_to_cluster/2
%% @end
%%--------------------------------------------------------------------
-spec add_to_cluster(Node :: N, ParentNode :: N,
		     cluster_dict:cluster_dict(N), boolean()) -> cluster_dict:cluster_dict(N).
add_to_cluster(Node, ParentNode, ClusterDict, InsertParent) ->
    cluster_dict:merge_cluster(
      case InsertParent of
	  true -> cluster:new_parent_child_to_cluster(ParentNode, Node);
	  false -> cluster:new_cluster(Node)
      end, ClusterDict).
