%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Implements a data type that congregates and manages clusters of
%%% nodes. It relies on the internal data type cluster.
%%% @end
%%% Created :  3 Jun 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(cluster_dict).

-export([new/0, merge_cluster/2, growing_map_fold/3,
	 get_cluster_for_node/2, show_cluster_dict/1,
	 show_cluster_dict/2, extract_trivial_clusters/1]).

-export_type([cluster_dict/1]).

-record(cluster_dict, {cluster_list}).

-opaque cluster_dict(Node) :: #cluster_dict{
				 cluster_list :: [cluster:cluster(Node)]}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty cluster dictionary.
%% @end
%%--------------------------------------------------------------------
-spec new() -> cluster_dict(any()).
new() -> #cluster_dict{cluster_list = []}.

%%--------------------------------------------------------------------
%% @doc
%% Mapfolds a function through a clusters dictionary. As opposed to the
%% traditional mapfold, this one expects an extra element in the
%% returned tuple, (the first one), which specifies which new clusters
%% must be map_folded later, (in addition to the original ones
%% from the cluster dictionary). The updated cluster (second element of
%% the tuple returned by the function), can be the atom `none', this
%% means that the Cluster will not be in the resulting list.
%% @end
%%--------------------------------------------------------------------
-spec growing_map_fold(fun((cluster:cluster(N),Acc) ->
				  {[cluster:cluster(N)],'none' | cluster:cluster(N),Acc}),
		       Acc, cluster_dict(N)) -> {cluster_dict(N), Acc}.
growing_map_fold(Fun, Acc, #cluster_dict{cluster_list = List}) ->
    {NewList, NewAcc} = growing_map_fold_list(Fun, Acc, List, []),
    {#cluster_dict{cluster_list = NewList}, NewAcc}.

%%--------------------------------------------------------------------
%% @doc
%% Mapfolds a function through a list of clusters. As opposed to the
%% traditional mapfold, this one expects an extra element in the
%% returned tuple, (the first one), which specifies which new clusters
%% must be map_folded later, (in addition to the original ones
%% from the list). The updated cluster (second element of the tuple
%% returned by the function), can be the atom `none', this means that
%% the Cluster will not be in the resulting list.
%% @end
%%--------------------------------------------------------------------
-spec growing_map_fold_list(fun((cluster:cluster(N), Acc) ->
				       {[cluster:cluster(N)],
					none | cluster:cluster(N),
					Acc}),
			    Acc, [cluster:cluster(N)],
			    [cluster:cluster(N)]) ->
				   {[cluster:cluster(N)], Acc}.
growing_map_fold_list(_Fun, Acc, [], ResList) -> {ResList, Acc};
growing_map_fold_list(Fun, Acc, [Cluster|RestOfClusters], ResList) ->
    {List, This, NewAcc} = Fun(Cluster, Acc),
    case This of
	none -> growing_map_fold_list(Fun, NewAcc, List ++ RestOfClusters, ResList);
	_Cluster -> growing_map_fold_list(Fun, NewAcc, List ++ RestOfClusters, [This|ResList])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Adds a Cluster to the ClusterDict and merges the new Cluster with
%% the clusters with common nodes in ClusterDict.
%% @end
%%--------------------------------------------------------------------
-spec merge_cluster(Cluster :: cluster:cluster(N),
		    ClusterDict :: cluster_dict(N)) -> cluster_dict(N).
merge_cluster(Cluster, #cluster_dict{cluster_list = List}) ->
    #cluster_dict{
       cluster_list = 
	   lists:foldl(fun (X, [Cl|Y]) ->
			       case cluster:merge_clusters(X, Cl) of
				   disjoint -> [Cl, X|Y];
				   {ok, NewCluster} -> [NewCluster|Y]
			       end
		       end, [Cluster], List)
      }.

%%--------------------------------------------------------------------
%% @doc
%% Finds the cluster that has the Node in the ClusterDict.
%% @end
%%--------------------------------------------------------------------
-spec get_cluster_for_node(Node :: N,
			   cluster_dict(N)) ->
				  {ok, Cluster :: cluster:cluster(N)} | error.
get_cluster_for_node(Node, #cluster_dict{cluster_list = List}) ->
    case lists:dropwhile(fun (X) -> not cluster:has_node(Node, X) end, List) of
	[] -> error;
	[H|_] -> {ok, H}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns a more readable version of custer dictionaries. This
%% is done by transforming dicts and sets into lists.
%% @end
%%--------------------------------------------------------------------
-spec show_cluster_dict(cluster_dict(_)) -> #{}.
show_cluster_dict(Clus) -> show_cluster_dict(fun (X) -> X end, Clus).

%%--------------------------------------------------------------------
%% @doc
%% Returns a more readable version of custer dictionaries and takes
%% a function that is suppoused to make the nodes in the clusters
%% more readable. This is done by transforming dicts and sets into
%% lists and by applying the supplied function to the nodes.
%% @end
%%--------------------------------------------------------------------
-spec show_cluster_dict(fun((NodeType) -> any()), cluster_dict(NodeType)) -> #{}.
show_cluster_dict(Fun, #cluster_dict{cluster_list = List}) ->
    #{cluster_list => [cluster:show_cluster(Fun, X) || X <- List]}.

%%--------------------------------------------------------------------
%% @doc
%% Removes clusters with only one node from the cluster dictionary.
%% It returns a list with the removed clusters.
%% @end
%%--------------------------------------------------------------------
-spec extract_trivial_clusters(cluster_dict(NodeType)) ->
				      {[cluster:cluster(NodeType)],
				       cluster_dict(NodeType)}.
extract_trivial_clusters(#cluster_dict{cluster_list = List}) ->
    {Trivial, NonTrivial} = lists:partition(fun (X) ->
						    (cluster:size(X) =< 1) orelse
							(ast_tree:is_function_or_macro(cluster:get_root(X))
							 andalso (cluster:size(X) =< 3))
					    end, List),
    {Trivial, #cluster_dict{cluster_list = NonTrivial}}.
