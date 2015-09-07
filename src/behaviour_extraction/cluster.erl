%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Implements a cluster of nodes. The clusters save information about
%%% which parent-child relationships between its nodes. Clusters are
%%% assumed to have tree hierarchy (no loops), and, as such, a single
%%% root.
%%% @end
%%% Created :  2 Jun 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(cluster).

-export([new_cluster/1, get_root/1, new_parent_child_to_cluster/2,
         get_nodes/1, remove_split/2, has_node/2, merge_clusters/2,
         make_indirection_cluster/0, is_indirection_cluster/1,
	 size/1, show_cluster/1, show_cluster/2]).

-export_type([cluster/1]).

-record(cluster, {root_node, node_set, nodes_by_parent}).

-opaque cluster(NodeType) :: #cluster{root_node :: NodeType,
				      node_set :: sets:set(NodeType),
				      nodes_by_parent :: dict:dict(NodeType, sets:set(NodeType))}
			   | {indirection_cluster, tree:tree_ph()}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a cluster with a single node.
%% @end
%%--------------------------------------------------------------------
-spec new_cluster(Node :: NodeType) -> cluster(NodeType).
new_cluster(Node) -> #cluster{root_node = Node,
			      node_set = sets:from_list([Node]),
			      nodes_by_parent = dict:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a cluster with a Node and its ParentNode.
%% @end
%%--------------------------------------------------------------------
-spec new_parent_child_to_cluster(Node :: N, Node :: N) -> cluster(N) when N :: term().
new_parent_child_to_cluster(ParentNode, Node) ->
    #cluster{root_node = ParentNode,
	     node_set = sets:from_list([ParentNode, Node]),
	     nodes_by_parent = dict:from_list([{ParentNode, sets:from_list([Node])}])}.

%%--------------------------------------------------------------------
%% @doc
%% Returns the root node of the Cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_root(Cluster :: cluster(NodeType)) -> OutNode :: NodeType.
get_root(#cluster{root_node = RootNode}) -> RootNode.

%%--------------------------------------------------------------------
%% @doc
%% Returns a set with all the nodes in the Cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_nodes(Cluster :: cluster(NodeType)) -> [NodeType].
get_nodes(#cluster{node_set = Nodes}) -> sets:to_list(Nodes).

%%--------------------------------------------------------------------
%% @doc
%% Returns a boolean that indicates whether the Cluster contains the Node.
%% @end
%%--------------------------------------------------------------------
-spec has_node(Node :: NodeType, Cluster :: cluster(NodeType)) -> boolean() when
      NodeType :: term().
has_node(Node, #cluster{node_set = Nodes}) -> sets:is_element(Node, Nodes).

%%--------------------------------------------------------------------
%% @doc
%% Takes two clusters and returns a new cluster containing all the nodes or the
%% atom `disjoint' if they have no common nodes. It assumes that the clusters
%% contain subtrees that belong to global tree, and, as such, nor the individual
%% clusters nor the result cluster should contain any loops, and no node should
%% have several parents.
%% @end
%%--------------------------------------------------------------------
-spec merge_clusters(cluster(Node), cluster(Node)) -> {ok, cluster(Node)} | disjoint.
merge_clusters(#cluster{root_node = RootNode1,
			node_set = Nodes1,
			nodes_by_parent = NodesByParent1},
	       #cluster{root_node = RootNode2,
			node_set = Nodes2,
			nodes_by_parent = NodesByParent2}) ->
    case compute_new_root(RootNode1, Nodes1, RootNode2, Nodes2) of
	disjoint -> disjoint;
	{ok, NewRoot} -> {ok, #cluster{root_node = NewRoot,
				       node_set = sets:union(Nodes1, Nodes2),
				       nodes_by_parent = dict:merge(fun (_, V1, V2) ->
									    sets:union(V1, V2)
								    end,
								    NodesByParent1,
								    NodesByParent2)}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Takes two root nodes (RootNode1 and RootNode2), and two sets of nodes,
%% (Nodes1 and Nodes2), and returns which root node is the absolute root
%% node. If there is no common nodes it returns the atom `disjoint'.
%% @end
%%--------------------------------------------------------------------
-spec compute_new_root(RootNode1 :: Node, Nodes1 :: sets:set(Node),
		       RootNode2 :: Node, Nodes2 :: sets:set(Node)) ->
			      'disjoint' | {'ok', Node}.
compute_new_root(RootNode1, Nodes1, RootNode2, Nodes2) ->
    case {sets:is_disjoint(sets:del_element(RootNode1, Nodes1),
			   sets:del_element(RootNode2, Nodes2)),
	  sets:is_element(RootNode1, Nodes2),
	  sets:is_element(RootNode2, Nodes1)} of
	{false, true, true} -> throw({error, "Found loop when merging trees"});
	{false, false, false} -> throw({error, "Found node with two parents when merging trees"});
	{true, true, true} -> case RootNode1 =:= RootNode2 of
				  true -> {ok, RootNode1};
				  false -> throw({error, "Found loop when merging trees"})
			      end;
	{true, false, true} -> {ok, RootNode1};
	{true, true, false} -> {ok, RootNode2};
	{true, false, false} -> disjoint
    end.

%%--------------------------------------------------------------------
%% @doc
%% Removes the Node from the Cluster an returns the resulting subclusters created,
%% (those that were hold together by the removed node).
%% @end
%%--------------------------------------------------------------------
-spec remove_split(Node :: NodeType, Cluster :: cluster(NodeType)) -> [cluster(NodeType)].
remove_split(Node, #cluster{root_node = Node} = Cluster) ->
    {_, SubClusters} = get_removed_nodes_and_subclusters(Node, Cluster),
    SubClusters;
remove_split(Node, Cluster) ->
    {RemovedNodes, SubClusters} = get_removed_nodes_and_subclusters(Node, Cluster),
    ParentCluster = remove_nodes_from_cluster(RemovedNodes,Cluster),
    [ParentCluster|SubClusters].

%%--------------------------------------------------------------------
%% @doc
%% Expects a Node from the Cluster, and it returns a set with all the descendats of the Node,
%% and a list with all the subclusters (subtrees) that hang from the Node in the Cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_removed_nodes_and_subclusters(Node :: NodeType, Cluster :: cluster(NodeType)) ->
					       {sets:set(NodeType), [cluster(NodeType)]}.
get_removed_nodes_and_subclusters(Node, Cluster) ->
    Children = sets:to_list(get_children_set_for_node(Node, Cluster)),
    NodesAndSubClusters = [begin
			       SubClusterNodes = sets:add_element(
						   Child, expand_node_downwards(Child, Cluster,
										sets:new())),
			       {SubClusterNodes, make_sub_cluster(Child, SubClusterNodes, Cluster)}
			   end || Child <- Children],
    {ListOfSets, SubClusters} = lists:unzip(NodesAndSubClusters),
    RemovedNodes = sets:union([sets:from_list([Node])|ListOfSets]),
    {RemovedNodes, SubClusters}.

%%--------------------------------------------------------------------
%% @doc
%% Returns a subcluster of Cluster with root RootNode and nodes Nodes.
%% RootNode is assumed to be the root of Nodes in the Cluster. And Nodes are
%% suppoused to form a contiguous tree in the Cluster.
%% @end
%%--------------------------------------------------------------------
-spec make_sub_cluster(RootNode :: NodeType, Nodes :: sets:set(NodeType),
		       Cluster :: cluster(NodeType)) -> cluster(NodeType).
make_sub_cluster(Node, SubClusterNodes, #cluster{nodes_by_parent = NodesByParent}) ->
     #cluster{root_node = Node,
	      node_set = sets:add_element(Node, SubClusterNodes),
	      nodes_by_parent = keep_nodes_in_dict(SubClusterNodes, NodesByParent)}.

%%--------------------------------------------------------------------
%% @doc
%% Removes from the Cluster all the nodes in the Set.
%% @end
%%--------------------------------------------------------------------
-spec remove_nodes_from_cluster(Set :: sets:set(NodeType),
				Cluster :: cluster(NodeType)) ->
				       cluster(NodeType) when
      NodeType :: term().
remove_nodes_from_cluster(RemovedNodes,
			  #cluster{root_node = RootNode,
				   node_set = Nodes,
				   nodes_by_parent = NodesByParent}) ->
     #cluster{root_node = RootNode,
	      node_set = sets:subtract(Nodes, RemovedNodes),
	      nodes_by_parent = remove_nodes_from_dict(RemovedNodes, NodesByParent)}.

%%--------------------------------------------------------------------
%% @doc
%% Remove from the Dict all the nodes in the Set.
%% @end
%%--------------------------------------------------------------------
-spec remove_nodes_from_dict(Set :: sets:set(NodeType),
			     Dict :: dict:dict(NodeType, sets:set(NodeType))) ->
				    dict:dict(NodeType, sets:set(NodeType)) when
      NodeType :: term().
remove_nodes_from_dict(RemovedNodes, NodesByParent) ->
    dict:fold(fun (Key, Value, Dict) ->
		      case sets:is_element(Key, RemovedNodes) of
			  true -> Dict;
			  false -> dict:store(Key, sets:subtract(Value, RemovedNodes), Dict)
		      end
              end, dict:new(), NodesByParent).

%%--------------------------------------------------------------------
%% @doc
%% Removes from the Dict all the nodes which are not in the Set.
%% @end
%%--------------------------------------------------------------------
-spec keep_nodes_in_dict(Set :: sets:set(NodeType),
			 Dict :: dict:dict(NodeType, sets:set(NodeType))) ->
				    dict:dict(NodeType, sets:set(NodeType)) when
      NodeType :: term().
keep_nodes_in_dict(KeptNodes, NodesByParent) ->
    dict:fold(fun (Key, Value, Dict) ->
		      case sets:is_element(Key, KeptNodes) of
			  false -> Dict;
			  true -> dict:store(Key, sets:intersection(Value, KeptNodes), Dict)
		      end
              end, dict:new(), NodesByParent).

%%--------------------------------------------------------------------
%% @doc
%% Adds all the descendants of the Node provided in the Cluster to the Set.
%% @end
%%--------------------------------------------------------------------
-spec expand_node_downwards(Node :: NodeType, Cluster :: cluster(NodeType), Set :: sets:set(NodeType)) ->
				   sets:set(NodeType).
expand_node_downwards(Node, Cluster, Acc) ->
    sets:fold(fun (Child, InAcc) ->
		      expand_node_downwards(Child, Cluster, sets:add_element(Child, InAcc))
	      end, Acc, get_children_set_for_node(Node, Cluster)).

%%--------------------------------------------------------------------
%% @doc
%% Returns the immediate children nodes inside the cluster for the Node provided.
%% @end
%%--------------------------------------------------------------------
-spec get_children_set_for_node(Node :: NodeType, Cluster :: cluster(NodeType)) -> sets:set(NodeType).
get_children_set_for_node(Node, #cluster{nodes_by_parent = NodesByParent}) ->
    case dict:find(Node, NodesByParent) of
	{ok, Set} -> Set;
	error -> sets:new()
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates an indirection "fake" cluster.
%% @end
%%--------------------------------------------------------------------
-spec make_indirection_cluster() -> {reference(), cluster(_)}.
make_indirection_cluster() -> Ref = make_ref(),
			      {Ref, {indirection_cluster, Ref}}.

%%--------------------------------------------------------------------
%% @doc
%% Returns whether the cluster is an indirection "fake" cluster or not.
%% @end
%%--------------------------------------------------------------------
-spec is_indirection_cluster(cluster(_)) -> 'false' | {'true', reference()}.
is_indirection_cluster({indirection_cluster, Ref}) -> {true, Ref};
is_indirection_cluster(_) -> false.

% Functions for debugging

%%--------------------------------------------------------------------
%% @doc
%% Returns the number of nodes that the cluster has.
%% @end
%%--------------------------------------------------------------------
-spec size(cluster(_)) -> non_neg_integer().
size(#cluster{node_set = Nodes}) -> sets:size(Nodes).

%%--------------------------------------------------------------------
%% @doc
%% Returns a more readable version of custers. This is done by
%% transforming dicts and sets into lists.
%% @end
%%--------------------------------------------------------------------
-spec show_cluster(cluster(_)) -> #{}.
show_cluster(Clus) -> show_cluster(fun (X) -> X end, Clus).

%%--------------------------------------------------------------------
%% @doc
%% Returns a more readable version of custers and takes a function
%% that is suppoused to make the nodes in the clusters more readable.
%% This is done by transforming dicts and sets into lists and by
%% applying the supplied function to the nodes.
%% @end
%%--------------------------------------------------------------------
-spec show_cluster(fun((NodeType) -> any()), cluster(NodeType)) -> #{}.
show_cluster(Fun, #cluster{root_node = Node,
			   node_set = Set,
			   nodes_by_parent = Dict}) ->
    #{root_node => Fun(Node),
      node_set => lists:map(Fun, sets:to_list(Set)),
      nodes_by_parent => [{Fun(Key), lists:map(Fun, sets:to_list(Value))}
			  || {Key, Value} <- dict:to_list(Dict)]}.
