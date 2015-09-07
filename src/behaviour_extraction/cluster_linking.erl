%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Gathers information about how the clusters must be linked, in
%%% which order they must be in their respective files, and which
%%% extra functions must be created.
%%% @end
%%% Created :  4 Jul 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(cluster_linking).

-export([link_clusters/6, get_actual_name/2, get_original_name/2]).

-include("cluster_records.hrl").

%%-------------------------------------------------------------------------
%% @doc
%% It links the clusters so as to keep the relationships in the original
%% trees, while keeping the calls to exclusive clusters (from common cluster)
%% symmetrical, and keeping the number of artifacts used to a minimum.
%% It accumulates all information in an #acc{} record that should be enough
%% to generate the three trees directly.
%% @end
%%-------------------------------------------------------------------------
-spec link_clusters(ClusterDict, ClusterDict, ClusterDict, Tree, Tree, Mapping) ->
			   cluster_folding:acc() when
      ClusterDict :: cluster_dict:cluster_dict(tree:tree_node()),
      Tree :: tree:tree(),
      Mapping :: da_map:da_map(tree:tree_node(),
			       tree:tree_node()).
link_clusters(CommCluster, Ex1Cluster, Ex2Cluster,
	      Tree1, Tree2, Mapping) ->
    ClusterDicts = #cluster_dicts{comm_clus = CommCluster,
                                  ex_clus1 = Ex1Cluster,
                                  ex_clus2 = Ex2Cluster},
    TreeInfo = #tree_info{tree1 = Tree1,
                          tree2 = Tree2,
			  tree1_args = compute_args(Tree1),
			  tree2_args = compute_args(Tree2),
                          mapping = Mapping},
    InitialAcc = #acc{ph_map = dict:new(),
		      cluster_order = dict:new(),
		      cluster_labels = da_map:new(),
		      cluster_funname = dict:new(),
		      cluster_orifunname = dict:new(),
		      cluster_info = dict:new(),
		      cluster_dicts = ClusterDicts,
		      allocated_parents = sets:new(),
		      tree_info = TreeInfo,
		      exp_funs = sets:new(),
		      counters = #cluster_counters{invisible_counter = 1,
						   common_counter = 1,
						   common_pos_counter = 1,
						   exclusive_counter = -1,
						   exclusive_pos_counter_1 = 1,
						   exclusive_pos_counter_2 = 1},
		      var_for_module = compute_safe_module_var(Tree1, Tree2),
		      root_for_new_exclusive_funnames = compute_safe_root_for_new_funnames("callback_", Tree1, Tree2),
		      root_for_new_common_funnames = compute_safe_root_for_new_funnames("common_", Tree1, Tree2)
		     },
    AllocatedAcc = cluster_folding:fold_clusters(fun pre_allocate_comm_names/4, InitialAcc, ClusterDicts, TreeInfo),
    LinkedAcc = cluster_folding:fold_clusters(fun link_clusters_aux/4, AllocatedAcc, ClusterDicts, TreeInfo),
    solve_comm_collisions(LinkedAcc).

%%-------------------------------------------------------------------------
%% @doc
%% Solves the collisions of function names in the comm cluster.
%% @end
%%-------------------------------------------------------------------------
-spec solve_comm_collisions(Acc :: cluster_folding:acc()) -> cluster_folding:acc().
solve_comm_collisions(#acc{cluster_info = ClusterInfo} = Acc) ->
    CommFuns = [{Name, get_actual_name(Name, Acc), length(Vars) + case Leaf of
								      true -> 0;
								      false -> 1
								  end}
		 || {{comm, Name}, #cluster_info{
				      environment_info =
					  #environment_info{
					     free_variables = Vars,
					     is_left_leaf = Leaf}}}
			<- dict:to_list(ClusterInfo)],
    solve_collisions(CommFuns, sets:new(), Acc).

%%-------------------------------------------------------------------------
%% @doc
%% Solves the collisions in the function names in FunList,
%% iteratively adding them to AssignedNames set.
%% @end
%%-------------------------------------------------------------------------
-spec solve_collisions(FunList :: [{OriName :: string(), NewName :: string(),
				    Arity :: non_neg_integer()}],
		       AssignedNames :: sets:set({SName :: string(),
						  SArity :: non_neg_integer()}),
		       Acc :: cluster_folding:acc()) -> cluster_folding:acc().
solve_collisions([], _Set, Acc) -> Acc;
solve_collisions([{_OriName, Name, Arity}|T] = L, Set, Acc) ->
    case sets:is_element({Name, Arity}, Set) of
	false -> solve_collisions(T, sets:add_element({Name, Arity}, Set), Acc);
	true -> solve_collisions(L, 2, Set, Acc)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Solves the collisions in the function names in FunList,
%% tries to add a suffix number that will not conflict
%% starting with CurSuffixNumber.
%% @end
%%-------------------------------------------------------------------------
-spec solve_collisions(FunList :: [{OriName :: string(), NewName :: string(),
				    Arity :: non_neg_integer()}],
		       CurSuffixNumber :: non_neg_integer(),
		       AssignedNames :: sets:set({SName :: string(),
						  SArity :: non_neg_integer()}),
		       Acc :: cluster_folding:acc()) -> cluster_folding:acc().
solve_collisions([{OriName, Name, Arity}|T] = L, N, Set, Acc) ->
    NewName = Name ++ integer_to_list(N),
    case sets:is_element({NewName, Arity}, Set) of
	false -> NewAcc = map_invisible_label(OriName, NewName, Acc),
		 solve_collisions([{OriName, NewName, Arity}|T], Set, NewAcc);
	true -> solve_collisions(L, N + 1, Set, Acc)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Collect information about the functions defined and
%% the names of the arguments used, (it uses the mechanism
%% of edoc to find this).
%% @end
%%-------------------------------------------------------------------------
-spec compute_args(Tree :: tree:tree()) -> dict:dict({FunName :: string(),
						      Arity :: non_neg_integer()},
						     [ArgName :: string()]).
compute_args(Tree) ->
    dict:from_list(edoc_args:collect_fun_info(ast_tree:tree_to_ast(Tree))).

%%-------------------------------------------------------------------------
%% @doc
%% Finds a variable name starting with "Module" that is
%% not used in any of the trees (Tree1 and Tree2),
%% for keeping the name of the instance at any point.
%% @end
%%-------------------------------------------------------------------------
-spec compute_safe_module_var(Tree1 :: tree:tree(),
			      Tree2 :: tree:tree()) -> nonempty_string().
compute_safe_module_var(Tree1, Tree2) ->
    VarNames = sets:from_list(
		 lists:map(fun erlang:atom_to_list/1,
			   wrangler_misc:collect_var_names(ast_tree:tree_to_ast(Tree1)) ++
			       wrangler_misc:collect_var_names(ast_tree:tree_to_ast(Tree2)))),
    get_smallest_var("Module", 1, VarNames).

%%-------------------------------------------------------------------------
%% @doc
%% Gets a preffix with the smallest number such that
%% it does not exist in ExistingVarNames. Starts with
%% CurVarSuffix. CurVarSuffix = 1 means no number.
%% @end
%%-------------------------------------------------------------------------
-spec get_smallest_var(Var :: string(), CurVarSuffix :: pos_integer(),
		       ExistingVarNames :: sets:set(string())) -> nonempty_string().
get_smallest_var(Var, N, VarNames) ->
    TempVarName = case N of
		      1 -> Var;
		      _ -> Var ++ integer_to_list(N)
		  end,
    case sets:is_element(TempVarName, VarNames) of
	false -> TempVarName;
	true -> get_smallest_var(Var, N + 1, VarNames)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Finds a prefix starting by Prefix for which there
%% exists no function name that starts with it.
%% @end
%%-------------------------------------------------------------------------
-spec compute_safe_root_for_new_funnames(Prefix :: string(), Tree1 :: tree:tree(),
					 Tree2 :: tree:tree()) -> nonempty_string().
compute_safe_root_for_new_funnames(Prefix, Tree1, Tree2) ->
    List = get_risky_funnames_in_scope(Tree1) ++ get_risky_funnames_in_scope(Tree2),
    Number = compute_biggest_number(Prefix, List),
    Prefix ++ case Number of
		  1 -> "";
		  N -> integer_to_list(N) ++ "_"
	      end.

%%-------------------------------------------------------------------------
%% @doc
%% Finds the number that would make any name starting by the Prefix
%% followed by a number and an underscore unique in the list List,
%% (assuming that 1 means no number).
%% @end
%%-------------------------------------------------------------------------
-spec compute_biggest_number(Prefix :: string(),
			     List :: [string()]) -> integer().
compute_biggest_number(Prefix, List) ->
    {ok, RE} = re:compile("^" ++ Prefix ++ "\(\([0-9]*\)_\)?.*"),
    lists:max([0|lists:flatmap(fun (X) ->
				       case re:run(X, RE, [{capture, all, list}]) of
					   nomatch -> [];
					   {match, [_, _, Num]} -> [list_to_integer(Num)];
					   {match, [_]} -> [1]
				       end
                               end, List)]) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% Extracts all the names of functions that could be consfused.
%% @end
%%-------------------------------------------------------------------------
-spec get_risky_funnames_in_scope(Tree :: tree:tree()) -> [string()].
get_risky_funnames_in_scope(Tree1) ->
    [atom_to_list(FunName)
     || {_, FunName, _} <- api_refac:inscope_funs(
			     ast_tree:get_module_info(Tree1))].

%%-------------------------------------------------------------------------
%% @doc
%% Adds the parents of the clusters that whose name will be established by common clusters.
%% The alternatives of children of the calls from common clusters must have a common name.
%% A way around this is using fake redirection clusters, but we want to minimise the amount
%% of them we have.
%% @end
%%-------------------------------------------------------------------------
-spec pre_allocate_comm_names({ClustType, Cluster, tree:tree()}, leaf_node | {ph, PH :: term()},
			      [tree:tree_node()], cluster_folding:acc()) ->
				     cluster_folding:acc() when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
pre_allocate_comm_names({comm, _Cluster, _Tree}, _PH, Children,
			#acc{allocated_parents = AlloParents} = Acc) ->
    NewAlloParents = lists:foldl(fun sets:add_element/2, AlloParents, Children),
    Acc#acc{allocated_parents = NewAlloParents};
pre_allocate_comm_names({_Type, _Cluster, _Tree}, _PH, _Children, Acc) -> Acc.

%%-------------------------------------------------------------------------
%% @doc
%% Must allocate a name for the clusters if they have not been allocated or preallocated.
%% In the case of exclusive clusters this implies creating a symmetric dummy cluster.
%% If this is a common cluster it must allocate a name for all its alternative children (max 2).
%% In every case, place holder must be linked to the children clusters.
%% @end
%%-------------------------------------------------------------------------
-spec link_clusters_aux({ClustType, Cluster, tree:tree()}, leaf_node | {ph, PH :: term()},
			nonempty_list(tree:tree_node()), cluster_folding:acc()) ->
			       cluster_folding:acc() when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
link_clusters_aux({_ClusterType, _Cluster, _Tree}, local_calls,
		  LocalCalls, #acc{exp_funs = Funs} = Acc) ->
    Acc#acc{exp_funs = lists:foldl(fun sets:add_element/2, Funs, LocalCalls)};
link_clusters_aux({ClusterType, Cluster, _Tree}, leaf_node, [], Acc) ->
    {{ClusterType, Name}, AccWCluster} = insert_cluster_carefully(ClusterType, Cluster, Acc),
    SymmetricType = cluster_folding:isolate_type_class(ClusterType),
    EnvInfo = generate_environment_info(SymmetricType, Name, AccWCluster),
    LinkedAcc = update_environment_info(SymmetricType, EnvInfo, Name, AccWCluster),
    mark_as_leaf_by_label({ClusterType, Name}, LinkedAcc);
link_clusters_aux({ParentClusterType, ParentCluster, _Tree}, {ok, PH}, Children, Acc) ->
    AccWParent = insert_cluster_very_carefully(ParentClusterType, ParentCluster, Acc),
    ChildrenInfo = get_nodes_info(Children, AccWParent),
    link_ph_to_children(ParentClusterType, ChildrenInfo, PH, AccWParent).

%%-------------------------------------------------------------------------
%% @doc
%% It tries to link the PH with the children described by List
%% (generated by the function {@link cluster_mapping:get_nodes_info/2}),
%% in the simplest way, while keeping the symmetry of exclusive clusters.
%% @end
%%-------------------------------------------------------------------------
-spec link_ph_to_children(ClustType,
			  List :: [{{ok, {ClustType, string()}} | error, ClustType,
				     Cluster}],
                          PH :: term(), cluster_folding:acc()) -> 
                             cluster_folding:acc() when
                         Cluster :: cluster:cluster(tree:tree_node()),
                         ClustType :: comm | {ex, 1} | {ex, 2}.
link_ph_to_children(comm, [{_N, {ex, _}, _ChildCluster}], _PH, _Acc) ->
    throw({error, "Found common node linked to a single exclusive node"});
link_ph_to_children(_, [{_, ChildClusterType, ChildCluster}], PH, Acc) ->
    {{_, ChildName}, AccWChild} = insert_cluster_carefully(ChildClusterType, ChildCluster, Acc),
    make_ph_link(PH, cluster_folding:isolate_type_class(ChildClusterType), ChildName, AccWChild);
link_ph_to_children(comm, [{{ok, {{ex, _}, N}}, {ex, _}, _Child1Cluster},
		           {{ok, {{ex, _}, N}}, {ex, _}, _Child2Cluster}] = Data,
	            PH, Acc) ->
    link_using_label({ex, N}, PH, Data, Acc);
link_ph_to_children(comm, List, PH, Acc) ->
    link_using_label_from_list([{{invert_ex_type(NT), N}, {ex, N}}
			        || {{ok, {NT, N}}, {ex, _T}, _ChildCluster} <- List],
			       PH, List, Acc).

%%-------------------------------------------------------------------------
%% @doc
%% Returns the opposite exclusive cluster type.
%% @end
%%-------------------------------------------------------------------------
-spec invert_ex_type({'ex',number()}) -> {'ex',number()}.
invert_ex_type({ex, N}) -> {ex, 3 - N}.

%%-------------------------------------------------------------------------
%% @doc
%% It chooses a name (second element of the tuple in Candidates),
%% by checking that its condition label (first element of the tuple
%% in Candidates) is free. If everything fails it generates a new one.
%% After choosing a name it carefully links the PH with the nodes described by List
%% (generated by the function {@link cluster_mapping:get_nodes_info/2}).
%% Assumes that the parent node is of type comm. 
%% @end
%%-------------------------------------------------------------------------
-spec link_using_label_from_list(Candidates :: [{{ClustType, string()},
			                         {ClustType, string()}}],
				 PH :: term(),
				 List :: [{{ok, {ClustType, string()}}  | error,
					    ClustType, Cluster}],
                                 cluster_folding:acc()) -> 
                                    cluster_folding:acc() when
                                Cluster :: cluster:cluster(tree:tree_node()),
                                ClustType :: comm | {ex, 1} | {ex, 2}.
link_using_label_from_list([], PH, List, #acc{root_for_new_exclusive_funnames = FunNamePrefix} = Acc) ->
    {NewLabel, NewAcc} = get_exclusive_label(Acc, FunNamePrefix),
    link_using_label(NewLabel, PH, List, NewAcc);
link_using_label_from_list([{Cond, Label}|Rest], PH, List, Acc) ->
    case is_label_free(Cond, Acc) of
	true -> link_using_label(Label, PH, List, Acc);
	false -> link_using_label_from_list(Rest, PH, List, Acc)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% It carefully links the PH with the nodes described by List
%% (generated by the function {@link cluster_mapping:get_nodes_info/2}).
%% Assumes that we have decided to use the symmetric label Label, and
%% that the parent node is of type comm. 
%% @end
%%-------------------------------------------------------------------------
-spec link_using_label(Label :: {ex, string()}, PH :: term(),
		       List :: [{{ok, {ClustType, string()}}  | error,
				  ClustType, Cluster}],
                       cluster_folding:acc()) -> 
                          cluster_folding:acc() when
                      Cluster :: cluster:cluster(tree:tree_node()),
                      ClustType :: comm | {ex, 1} | {ex, 2}.
link_using_label({ex, Name}, PH, Data, Acc) ->
    NewAcc = lists:foldl(fun (Pos, InAcc) -> what_to_indirect(Name, Pos, InAcc) end,
			 Acc, lists:zip([1, 2], Data)),
    make_ph_link(PH, ex, Name, NewAcc).

%%-------------------------------------------------------------------------
%% @doc
%% Decides which fake indirection clusters must be created in order to forward
%% the label `{ex, Name}' to each of the nodes described by List (generated by
%% the function {@link cluster_mapping:get_nodes_info/2}), while keeping the
%% exclusive symmetry.
%% @end
%%-------------------------------------------------------------------------
-spec what_to_indirect(string(), {1 | 2, {{ok, {ClustType, string()}} | error,
					  ClustType, Cluster}}, Acc :: cluster_folding:acc()) ->
			      cluster_folding:acc() when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
what_to_indirect(Name, {Pos, {{ok, {{ex, Pos}, Name}}, {ex, Pos}, _ChildCluster}}, Acc) ->
    Acc;
what_to_indirect(Name, {Pos, {error, {ex, Pos}, ChildCluster}}, Acc) ->
    insert_cluster_with_name({{ex, Pos}, Name}, ChildCluster, Acc);
what_to_indirect(NameToUse, {Pos, {_ChildSearchResult, ChildType, ChildCluster}}, Acc) ->
    {DestName, Acc2} = insert_cluster_carefully(ChildType, ChildCluster, Acc),
    insert_indirection({{ex, Pos}, NameToUse}, DestName, Acc2).

%%-------------------------------------------------------------------------
%% @doc
%% Annotates in the Acc a link between the PH (Place Holder) and the
%% cluster with type ClusterType and name Name. It also updates the
%% environment information of the children node.
%% @end
%%-------------------------------------------------------------------------
-spec make_ph_link(PH :: term(), ClusterType :: SymmetricType, Name :: string(),
		   Acc :: cluster_folding:acc()) ->
			  cluster_folding:acc() when
      SymmetricType :: comm | ex.
make_ph_link(PH, ClusterType, Name, #acc{ph_map = PHMap} = Acc) ->
    LinkedAcc = Acc#acc{ph_map = dict:store(PH, {ClusterType, Name}, PHMap)},
    EnvInfo = generate_environment_info(ClusterType, Name, LinkedAcc),
    update_environment_info(ClusterType, EnvInfo, Name, LinkedAcc).

%%-------------------------------------------------------------------------
%% @doc
%% Gathers environment information and updates the environment info for the
%% top nodes of the clusters linked to the Name, with type SymType.
%% It is done when linking to ensure indirections get the right environment info.
%% @end
%%-------------------------------------------------------------------------
-spec generate_environment_info(SymType :: 'comm' | 'ex', Name :: string(),
				cluster_folding:acc()) -> cluster_folding:environment_info().
generate_environment_info(ClusterType, Name, Acc) ->
    Env = cluster_folding:find_free_vars_for_sym_label(ClusterType, Name, Acc),
    ExpEnv = cluster_folding:find_exported_vars_for_sym_label(ClusterType, Name, Acc),
    case ClusterType of
	ex -> #environment_info{
		 free_variables = Env,
		 free_left = cluster_folding:find_actual_free_vars_for_asym_label({{ex, 1}, Name}, 1, Acc),
		 free_right = cluster_folding:find_actual_free_vars_for_asym_label({{ex, 2}, Name}, 2, Acc),
		 export_variables = ExpEnv,
		 export_left = cluster_folding:find_actual_exported_vars_for_asym_label({{ex, 1}, Name}, 1, Acc),
		 export_right = cluster_folding:find_actual_exported_vars_for_asym_label({{ex, 2}, Name}, 2, Acc),
		 is_left_leaf = cluster_folding:get_leaf_as_is_or_default({{ex, 1}, Name}, 1, Acc),
		 is_right_leaf = cluster_folding:get_leaf_as_is_or_default({{ex, 2}, Name}, 2, Acc)};
	comm -> #environment_info{
		   free_variables = Env,
		   free_left = cluster_folding:find_actual_free_vars_for_asym_label({comm, Name}, 1, Acc),
		   free_right = cluster_folding:find_actual_free_vars_for_asym_label({comm, Name}, 2, Acc),
		   export_variables = ExpEnv,
		   export_left = cluster_folding:find_actual_exported_vars_for_asym_label({comm, Name}, 1, Acc),
		   export_right = cluster_folding:find_actual_exported_vars_for_asym_label({comm, Name}, 2, Acc),
		   is_left_leaf = cluster_folding:get_leaf_as_is_or_default({comm, Name}, 1, Acc),
		   is_right_leaf = cluster_folding:get_leaf_as_is_or_default({comm, Name}, 2, Acc)}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Updates the environment info for a symmetric label. That is,
%% in case of exclusive cluster, it updates it for both exclusive clusters.
%% @end
%%-------------------------------------------------------------------------
-spec update_environment_info('comm' | 'ex', cluster_folding:environment_info(), string(),
			      cluster_folding:acc()) -> cluster_folding:acc().
update_environment_info(comm, EnvInfo, Name, Acc) ->
    set_environment_info({comm, Name}, EnvInfo, Acc);
update_environment_info(ex, EnvInfo, Name, Acc) ->
    Acc1 = set_environment_info({{ex, 1}, Name}, EnvInfo, Acc),
    set_environment_info({{ex, 2}, Name}, EnvInfo, Acc1).

%%-------------------------------------------------------------------------
%% @doc
%% Updates the environment info for an asymmetric label.
%% @end
%%-------------------------------------------------------------------------
-spec update_environment_info_asym(ClusterType :: AsymType,
				   EnvInfo :: cluster_folding:environment_info(),
				   Name :: string(),
				   Acc :: cluster_folding:acc()) -> cluster_folding:acc() when
      AsymType :: 'comm' | {'ex', 1 | 2}.
update_environment_info_asym(Type, EnvInfo, Name, Acc) ->
    set_environment_info({Type, Name}, EnvInfo, Acc).

%%-------------------------------------------------------------------------
%% @doc
%% Updates the environment info for the label Label in the Acc.
%% @end
%%-------------------------------------------------------------------------
-spec set_environment_info({ClustType, string()}, cluster_folding:environment_info(),
			   Acc :: cluster_folding:acc()) -> cluster_folding:acc() when
                    ClustType :: comm | {ex, 1} | {ex, 2}.
set_environment_info(Label, EnvInfo, #acc{cluster_info = InfoMap} = Acc) ->
    NewClusInfo = set_environment_info_in_map(Label, EnvInfo, InfoMap),
    Acc#acc{cluster_info = NewClusInfo}.

%%-------------------------------------------------------------------------
%% @doc
%% Marks the cluster pointed by the Label as a leaf.
%% Assumes that the cluster has already been assigned a Label and a
%% `#environment_info' record.
%% @end
%%-------------------------------------------------------------------------
-spec mark_as_leaf_by_label(Label :: LabelType, Acc :: cluster_folding:acc()) ->
				   cluster_folding:acc() when
      LabelType :: {AsymType, string()},
      AsymType :: 'comm' | {'ex', 1 | 2}.
mark_as_leaf_by_label({ClusterType, Name}, #acc{cluster_info = InfoMap} = Acc) ->
    ClusInfo = tree:dict_get({ClusterType, Name}, InfoMap,
			     #cluster_info{environment_info = na}),
    case ClusInfo#cluster_info.environment_info of
	na -> throw({error, "Leaf cluster has no cluster information"});
	none -> throw({error, "Leaf cluster has empty cluster information"});
	#environment_info{is_left_leaf = LLeaf,
			  is_right_leaf = RLeaf} = EnvInfo ->
	    NewEnvInfo = mark_as_leaf_correct_side(ClusterType, LLeaf, RLeaf, EnvInfo),
            update_environment_info_asym(ClusterType, NewEnvInfo, Name, Acc)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Packs the data about whether the cluster is a leaf into the
%% environment info (EnvInfo).
%% @end
%%-------------------------------------------------------------------------
-spec mark_as_leaf_correct_side(ClusterType :: AsymType,
				LeftLeaf :: boolean(), RightLeaf :: boolean(),
				cluster_folding:environment_info()) ->
				       cluster_folding:environment_info() when
      AsymType :: 'comm' | {'ex', 1 | 2}.
mark_as_leaf_correct_side(ClusterType, LLeaf, RLeaf, EnvInfo) ->
    EnvInfo#environment_info{
      is_left_leaf = apply_if_match(1, ClusterType, true, LLeaf),
      is_right_leaf = apply_if_match(2, ClusterType, true, RLeaf)}.

%%-------------------------------------------------------------------------
%% @doc
%% Returns Value if Pos matches the ClusterType ({ex, Pos}) or the ClusterType is comm.
%% @end
%%-------------------------------------------------------------------------
-spec apply_if_match(Pos :: 1 | 2, ClusterType :: AsymType, Value :: Val, Default :: Val) -> Val when
      AsymType :: comm | {ex, 1} | {ex, 2}.
apply_if_match(_, comm, Value, _) -> Value;
apply_if_match(N, {ex, N}, Value, _) -> Value;
apply_if_match(_, _, _, Default) -> Default.

%%-------------------------------------------------------------------------
%% @doc
%% Updates the environment info for the label Label in the environment_info map.
%% @end
%%-------------------------------------------------------------------------
-spec set_environment_info_in_map(Label :: {ClustType, string()}, cluster_folding:environment_info(),
				  any()) -> any() when
                    ClustType :: comm | {ex, 1} | {ex, 2}.
set_environment_info_in_map(Label, EnvInfo, InfoMap) ->
    ClusInfo = tree:dict_get(Label, InfoMap, none),
    case ClusInfo of
	none -> InfoMap;
	_ -> dict:store(Label, ClusInfo#cluster_info{environment_info = EnvInfo}, InfoMap)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Takes a list of Nodes and returns a list of tuples with information
%% about each of them:
%% <ul>
%% <li>`{ok, Label} | error' - the result of querying for a label for the
%%                             cluster to which the node belongs</li>
%% <li>`ClustType' - the type of the cluster to which the node belongs</li>
%% <li>`Cluster' - the cluster to which the node belongs</li>
%% </ul>
%% @end
%%-------------------------------------------------------------------------
-spec get_nodes_info(nonempty_list(tree:tree_node()), cluster_folding:acc()) -> 
			    nonempty_list({{ok, {ClustType, string()}} | error, ClustType,
					   Cluster}) when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
get_nodes_info(Nodes, Acc) ->
    [begin
	 {ClusterType, Cluster} = get_cluster_for_node(Node, Acc),
	 LabelRes = find_label(Cluster, Acc),
	 {LabelRes, ClusterType, Cluster}
     end || Node <- Nodes].

%%-------------------------------------------------------------------------
%% @doc
%% Finds the cluster to which the provided Node belongs, by searching in all the
%% cluster dictionaries of the Acc. It throws an exception if it is found in more
%% or less than one cluster.
%% @end
%%-------------------------------------------------------------------------
-spec get_cluster_for_node(tree:tree_node(), cluster_folding:acc()) ->
				  {ClusterType, Cluster} when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClusterType :: comm | {ex, 1} | {ex, 2}.
get_cluster_for_node(Node, Acc) ->
    TypeList = cluster_folding:get_type_list(Acc#acc.cluster_dicts, Acc#acc.tree_info),
    case get_cluster_for_node_aux(Node, TypeList) of
	none -> throw({error, "Node not found"});
	ChildCluster -> ChildCluster
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Obtains a new unused exclusive label, by using the counters. It returns it in the
%% symmetric format ({ex, Name}).
%% @end
%%-------------------------------------------------------------------------
-spec get_exclusive_label(cluster_folding:acc(), any()) -> 
                             {{ex, string()}, cluster_folding:acc()}.
get_exclusive_label(Acc,FunNamePrefix) ->
    Number = get_counter(label, ex, Acc),
    TouchedAcc = set_counter(label, ex, Number + 1, Acc),
    {{ex, generate_fun_name_aux(Number, FunNamePrefix)}, TouchedAcc}.

%%-------------------------------------------------------------------------
%% @doc
%% Checks whether a label has been asigned to any cluster yet.
%% @end
%%-------------------------------------------------------------------------
-spec is_label_free({ClustType, string()}, cluster_folding:acc()) -> boolean() when
      ClustType :: comm | {ex, 1} | {ex, 2}.
is_label_free(Label, #acc{cluster_labels = LabelMap}) ->
    not da_map:has_key(Label, LabelMap).

%%-------------------------------------------------------------------------
%% @doc
%% Creates a fake cluster with name OriName that redirects to the cluster
%% with name DestName.
%% @end
%%-------------------------------------------------------------------------
-spec insert_indirection(OriName :: {ClustType, string()}, DestName :: {ClustType, string()},
			 cluster_folding:acc()) -> cluster_folding:acc() when
      ClustType :: comm | {ex, 1} | {ex, 2}.
insert_indirection({{ex, Pos}, NameToUse} = LabelToUse, {ClustType, DestName}, Acc) ->
    {IndPH, IndCluster} = cluster:make_indirection_cluster(),
    NamedAcc = insert_cluster_with_name(LabelToUse, IndCluster, Acc),
    EnvInfo = generate_environment_for_indirection(NameToUse, Pos, ClustType, DestName, NamedAcc),
    UpdatedAcc = update_environment_info_asym({ex, Pos}, EnvInfo, NameToUse, NamedAcc),
    make_ph_link(IndPH, cluster_folding:isolate_type_class(ClustType), DestName, UpdatedAcc).

%%-------------------------------------------------------------------------
%% @doc
%% Creates an `#environment_info{}' record for an indirection cluster, based
%% on the `#environment_info{}' records from the clusters indirected.
%% @end
%%-------------------------------------------------------------------------
-spec generate_environment_for_indirection(NameToUse :: string(), Pos :: 1 | 2,
					   AsymType, DestName :: string(),
					   Acc :: cluster_folding:acc()) ->
						  cluster_folding:environment_info() when
      AsymType :: comm | {ex, 1} | {ex, 2}.
generate_environment_for_indirection(NameToUse, Pos, ClustType, DestName, NamedAcc) ->
    EnvInfo1 = isolate_side(Pos, generate_environment_info(
				   cluster_folding:isolate_type_class(ClustType),
				   DestName, NamedAcc)),
    EnvInfo2 = generate_environment_info(ex, NameToUse, NamedAcc),
    merge_environment_infos(EnvInfo1, EnvInfo2).

%%-------------------------------------------------------------------------
%% @doc
%% Upgrades the variables of one side to represent both sides,
%% empties the other side so that it does not propagate its
%% variables, (both exported and free).
%% @end
%%-------------------------------------------------------------------------
-spec isolate_side(1 | 2, cluster_folding:environment_info()) ->
			  cluster_folding:environment_info().
isolate_side(1, #environment_info{
			 free_left = Free,
			 export_left = Export
			} = EnvInfo) ->
    EnvInfo#environment_info{
      free_variables = Free,
      free_right = [],
      export_variables = Export,
      export_right = []};
isolate_side(2, #environment_info{
			 free_right = Free,
			 export_right = Export
			} = EnvInfo) ->
    EnvInfo#environment_info{
      free_variables = Free,
      free_left = [],
      export_variables = Export,
      export_left = []}.

%%-------------------------------------------------------------------------
%% @doc
%% Merges two `#environment_info{}' records for an indirection cluster.
%% Only for use with indirections, since it sets leaf to false, (we know
%% in advance that indirection clusters cannot be leaves, but that is not
%% always the case).
%% @end
%%-------------------------------------------------------------------------
-spec merge_environment_infos(cluster_folding:environment_info(), cluster_folding:environment_info()) ->
				     cluster_folding:environment_info().
merge_environment_infos(#environment_info{free_variables = Env1,
					  free_left = Left1,
					  free_right = Right1,
					  export_variables = EEnv1,
					  export_left = ELeft1,
					  export_right = ERight1},
			#environment_info{free_variables = Env2,
					  free_left = Left2,
					  free_right = Right2,
					  export_variables = EEnv2,
					  export_left = ELeft2,
					  export_right = ERight2}) ->
    #environment_info{free_variables = cluster_folding:merge_environments(Env1, Env2),
		      free_left = cluster_folding:merge_environments(Left1, Left2),
		      free_right = cluster_folding:merge_environments(Right1, Right2),
		      export_variables = cluster_folding:merge_environments(EEnv1, EEnv2),
		      export_left = cluster_folding:merge_environments(ELeft1, ELeft2),
		      export_right = cluster_folding:merge_environments(ERight1, ERight2),
		      is_left_leaf = false,
		      is_right_leaf = false}.

%%-------------------------------------------------------------------------
%% @doc
%% Tries to obtain a label for the Cluster
%% @end
%%-------------------------------------------------------------------------
-spec find_label(Cluster, Acc :: cluster_folding:acc()) ->
			{ok, {ClustType, string()}} | error when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
find_label(Cluster, Acc) ->
    da_map:find_key(Cluster, Acc#acc.cluster_labels).

%%-------------------------------------------------------------------------
%% @doc
%% Obtains the cluster to which Node belongs. TypeList must be the
%% result of calling {@link cluster_folding:get_type_list/2}
%% @end
%%-------------------------------------------------------------------------
-spec get_cluster_for_node_aux(tree:tree_node(),
			       [{ClustType, ClusterDict, Tree | na}]) ->
				      (none | {ClustType, Cluster}) when
      ClustType :: comm | {ex, 1} | {ex, 2},
      ClusterDict :: cluster_dict:cluster_dict(tree:tree_node()),
      Cluster :: cluster:cluster(tree:tree_node()),
      Tree :: tree:tree().
get_cluster_for_node_aux(Node, TypeList) ->
    lists:foldl(fun ({ClusterType, ClusterDict, _Tree}, Acc) ->
                        keep_correct_cluster(Node, ClusterType, ClusterDict, Acc)
                end, none, TypeList).

%%-------------------------------------------------------------------------
%% @doc
%% Tries to find the cluster that has the node Node in the cluster
%% dictionary ClusterDict. If it is found it is accumulated. If it is
%% found twice an exception is thrown. It is assumed that the ClusterType
%% and the ClusterDict provide match.
%% @end
%%-------------------------------------------------------------------------
-spec keep_correct_cluster(tree:tree_node(), ClusterType,
			   ClusterDict, none | {ClusterType, Cluster}) ->
				  (none | {ClusterType, Cluster}) when
      ClusterDict :: cluster_dict:cluster_dict(tree:tree_node()),
      Cluster :: cluster:cluster(tree:tree_node()),
      ClusterType :: comm | {ex, 1} | {ex, 2}.
keep_correct_cluster(Node, ClusterType, ClusterDict, Acc) ->
    case {cluster_dict:get_cluster_for_node(Node, ClusterDict), Acc} of
	{{ok, Cluster}, none} -> {ClusterType, Cluster};
	{error, Acc} -> Acc;
	{_, _} -> throw({error, "Node belongs to several clusters"})
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Inserts a cluster only if it has not been preallocated and it
%% has not been inserted already.
%% @end
%%-------------------------------------------------------------------------
-spec insert_cluster_very_carefully(ClustType, Cluster, cluster_folding:acc()) ->
					   cluster_folding:acc() when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
insert_cluster_very_carefully(Type, Cluster, Acc) ->
    case is_preallocated(Cluster, Acc) of
	false -> {_Name, NewAcc} = insert_cluster_carefully(Type, Cluster, Acc),
		 NewAcc;
	true -> Acc
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Checks whether the Cluster was marked as preallocated in
%% the preallocation pass
%% @end
%%-------------------------------------------------------------------------
-spec is_preallocated(Cluster, Acc :: cluster_folding:acc()) ->
			     boolean() when
      Cluster :: cluster:cluster(tree:tree_node()).
is_preallocated(Cluster, Acc) ->
    sets:is_element(cluster:get_root(Cluster), Acc#acc.allocated_parents).

%%-------------------------------------------------------------------------
%% @doc
%% Inserts a cluster only if it has not been inserted already.
%% @end
%%-------------------------------------------------------------------------
-spec insert_cluster_carefully(ClustType, Cluster, cluster_folding:acc()) ->
				      {{ClustType, string()}, cluster_folding:acc()} when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
insert_cluster_carefully(Type, Cluster, Acc) ->
    case find_label(Cluster, Acc) of
	error -> {Name, NewAcc} = insert_cluster(Type, Cluster, Acc),
		 {Name, NewAcc};
	{ok, Name} -> {Name, Acc}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Stores a cluster in the Acc, it gives it a name, a position and,
%% it stores the environment information required. This function
%% must not be used if the cluster is exclusive and called from a
%% common cluster because the label is stored asymmetrically.
%% @end
%%-------------------------------------------------------------------------
-spec insert_cluster(ClustType, Cluster, cluster_folding:acc()) ->
			    {{ClustType, string()}, cluster_folding:acc()} when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
insert_cluster(Type, Cluster, Acc) ->
    {Label, NamedAcc} = give_name_to_cluster(Type, Cluster, Acc),
    AddedAcc = insert_cluster_with_name(Label, Cluster, NamedAcc),
    {Label, AddedAcc}.

%%-------------------------------------------------------------------------
%% @doc
%% Stores a cluster in the Acc, it gives it the label Label, a position and,
%% it stores the environment information required. In the case of exclusive
%% clusters it must be called for each of the alternatives so as to achieve
%% symmetry.
%% @end
%%-------------------------------------------------------------------------
-spec insert_cluster_with_name(Label :: {ClustType, string()}, Cluster,
			       cluster_folding:acc()) -> cluster_folding:acc() when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
insert_cluster_with_name({Type, _Name} = Label, Cluster, Acc) ->
    LabelledAcc = set_label(Cluster, Label, Acc),
    OrderedAcc = assign_new_pos(Type, Label, LabelledAcc),
    assign_info(Label, OrderedAcc).

%%-------------------------------------------------------------------------
%% @doc
%% Finds a name for the cluster.
%% It assumes that the cluster does not have a label yet.
%% @end
%%-------------------------------------------------------------------------
-spec give_name_to_cluster(ClustType, Cluster, Acc :: cluster_folding:acc()) ->
				  {{ClustType, string()}, cluster_folding:acc()} when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
give_name_to_cluster(Type, Cluster, #acc{root_for_new_exclusive_funnames = ExFunNamePrefix,
					 root_for_new_common_funnames = CommFunNamePrefix,
					 tree_info = TreeInfo} = Acc) ->
    FunNamePrefix = get_right_prefix(Type, ExFunNamePrefix, CommFunNamePrefix),
    SymmetricType = cluster_folding:isolate_type_class(Type),
    Number = get_counter(label, SymmetricType, Acc),
    {Name, NewNumber} = generate_fun_name(Number, Cluster,
					  get_tree_for_type(Type, TreeInfo),
					  FunNamePrefix),
    TouchedAcc = set_counter(label, SymmetricType, NewNumber, Acc),
    {NewName, FinalAcc} = indirect_if_label_is_not_generated(FunNamePrefix, Number, Name,
			                                     NewNumber, TouchedAcc),
    {{Type, NewName}, FinalAcc}.

%%-------------------------------------------------------------------------
%% @doc
%% If the label has not been generated we do not know it is
%% unique, so we create a new unique one to avoid confusion.
%% The original label is stored in case we can use it later.
%% @end
%%-------------------------------------------------------------------------
-spec indirect_if_label_is_not_generated(FunNamePrefix :: string(), Number :: non_neg_integer(),
					 Name :: string(), NewNumber :: non_neg_integer(),
					 Acc :: cluster_folding:acc()) ->
						{string(), cluster_folding:acc()}.
indirect_if_label_is_not_generated(FunNamePrefix, Number, Name, NewNumber, TouchedAcc) ->
    case NewNumber =:= Number of
	true -> indirect_name(Name, FunNamePrefix, TouchedAcc);
        false -> {Name, TouchedAcc}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Selects the right prefix for the callback, (depending
%% on whether it is the common or an exclusive cluster).
%% @end
%%-------------------------------------------------------------------------
-spec get_right_prefix(ClustType, ExFunPrefix :: string(),
		       CommFunPrefix :: string()) -> string() when
      ClustType :: comm  | {ex, 1}  | {ex, 2}.
get_right_prefix(Type, ExFunNamePrefix, CommFunNamePrefix) ->
    case Type of
	comm -> CommFunNamePrefix;
	{ex, _} -> ExFunNamePrefix
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns a new unique name for the function, based on
%% the preffix FunNamePreffix. It stores the original name
%% as "original name" and the new name as "invisible label".
%% @end
%%-------------------------------------------------------------------------
-spec indirect_name(Name :: string(), FunNamePreffix :: string(),
		    Acc :: cluster_folding:acc()) ->
			   {string(), cluster_folding:acc()}.
indirect_name(Name, FunNamePrefix, Acc) ->
    Number = get_counter(invisible_label, comm, Acc),
    NewName = FunNamePrefix ++ integer_to_list(Number) ++ "inv",
    TouchedAcc = set_counter(invisible_label, comm, Number + 1, Acc),
    {NewName, map_ori_and_invisible_label(NewName, Name, TouchedAcc)}.

%%-------------------------------------------------------------------------
%% @doc
%% Set NewName as the invisible label for Name.
%% @end
%%-------------------------------------------------------------------------
-spec map_invisible_label(NewName :: string(), Name :: string(),
			  Acc :: cluster_folding:acc()) ->
				 cluster_folding:acc().
map_invisible_label(NewName, Name, #acc{cluster_funname = Mapping} = Acc) ->
    Acc#acc{cluster_funname = dict:store(NewName, Name, Mapping)}.

%%-------------------------------------------------------------------------
%% @doc
%% Set NewName as the original name for Name.
%% @end
%%-------------------------------------------------------------------------
-spec map_original_label(NewName :: string(), Name :: string(),
			 Acc :: cluster_folding:acc()) ->
				cluster_folding:acc().
map_original_label(NewName, Name, #acc{cluster_orifunname = Mapping} = Acc) ->
    Acc#acc{cluster_orifunname = dict:store(NewName, Name, Mapping)}.

%%-------------------------------------------------------------------------
%% @doc
%% Set NewName as the original name and the invisible label
%% for Name.
%% @end
%%-------------------------------------------------------------------------
-spec map_ori_and_invisible_label(NewName :: string(), Name :: string(),
				  Acc :: cluster_folding:acc()) ->
					 cluster_folding:acc().
map_ori_and_invisible_label(NewName, Name, Acc) ->
    map_original_label(NewName, Name,
		       map_invisible_label(NewName, Name, Acc)).

%%-------------------------------------------------------------------------
%% @doc
%% Get the name that the function with name Name is supposed
%% to have.
%% @end
%%-------------------------------------------------------------------------
-spec get_actual_name(Name :: string(),
		      Acc :: cluster_folding:acc()) -> string().
get_actual_name(Name, #acc{cluster_funname = Mapping}) ->
    case dict:find(Name, Mapping) of
	{ok, Value} -> Value;
	error -> Name
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Gets the original name that the function with name Name had.
%% @end
%%-------------------------------------------------------------------------
-spec get_original_name(Name :: string(),
			Acc :: cluster_folding:acc()) -> string().
get_original_name(Name, #acc{cluster_orifunname = Mapping}) ->
    case dict:find(Name, Mapping) of
	{ok, Value} -> Value;
	error -> Name
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Fetches the tree corresponding to the ClustType from the TreeInfo,
%% in case of the `comm' cluster it returns the left tree.
%% @end
%%-------------------------------------------------------------------------
-spec get_tree_for_type(ClustType, TreeInfo :: cluster_folding:tree_info()) ->
			       tree:tree() when
      ClustType :: comm | {ex, 1} | {ex, 2}.
get_tree_for_type(comm, #tree_info{tree1 = Tree1}) -> Tree1;
get_tree_for_type({ex, 1}, #tree_info{tree1 = Tree1}) -> Tree1;
get_tree_for_type({ex, 2}, #tree_info{tree2 = Tree2}) -> Tree2.

%%-------------------------------------------------------------------------
%% @doc
%% Links the label to the cluster in the cluster_labels map.
%% @end
%%-------------------------------------------------------------------------
-spec set_label(Cluster, Label :: {ClustType, string()},
		Acc :: cluster_folding:acc()) -> 
                   cluster_folding:acc() when
               Cluster :: cluster:cluster(tree:tree_node()),
               ClustType :: comm  | {ex, 1}  | {ex, 2}.
set_label(Cluster, Label, #acc{cluster_labels = LabelMap} = Acc) ->
    Acc#acc{cluster_labels = da_map:put(Label, Cluster, LabelMap)}.

%%-------------------------------------------------------------------------
%% @doc
%% Assigns the next available position in the file for the cluster with
%% label Label.
%% @end
%%-------------------------------------------------------------------------
-spec assign_new_pos(ClustType, {ClustType, string()}, cluster_folding:acc()) -> cluster_folding:acc() when
      ClustType :: comm | {ex, 1} | {ex, 2}.
assign_new_pos(Type, Name, Acc) ->
    Number = get_counter(pos, Type, Acc),
    SetAcc = set_pos_for_cluster_name(Type, Number, Name, Acc),
    set_counter(pos, Type, Number + 1, SetAcc).

%%-------------------------------------------------------------------------
%% @doc
%% Assigns the position Pos in the file for the cluster with label Label.
%% @end
%%-------------------------------------------------------------------------
-spec set_pos_for_cluster_name(ClustType, Pos :: number(),
			       Label :: {ClustType, string()},
			       cluster_folding:acc()) ->
				      cluster_folding:acc() when
      ClustType :: comm | {ex, 1} | {ex, 2}.
set_pos_for_cluster_name(Type, Number, Label, #acc{cluster_order = Order} = Acc) ->
    Acc#acc{cluster_order = dict:store({Type, Number}, Label, Order)}.

%%-------------------------------------------------------------------------
%% @doc
%% Stores the environment information from the cluster with label Label.
%% @end
%%-------------------------------------------------------------------------
-spec assign_info(Label :: {ClustType, string()}, cluster_folding:acc()) ->
			 cluster_folding:acc() when
      ClustType :: comm | {ex, 1} | {ex, 2}.
assign_info({Type, _}= Label, #acc{cluster_info = InfoMap} = Acc) ->
    ClusterInfo = #cluster_info{cluster_type = Type,
                                environment_info = none},
    Acc#acc{cluster_info = dict:store(Label, ClusterInfo, InfoMap)}.

%% =================
%% Counter accessors
%% =================

%%-------------------------------------------------------------------------
%% @doc
%% Get the value of a counter from the acc record.
%% The two parameters CounterType and ClustType determine which counter to set.
%% For label counters symmetric type must be used (comm | ex).
%% @end
%%-------------------------------------------------------------------------
-spec get_counter(CounterType, ClustType | SymmetricType,
		  cluster_folding:acc()) -> number() when
      CounterType :: label | pos | invisible_label,
      ClustType :: comm | {ex, 1} | {ex, 2},
      SymmetricType :: comm | ex.
get_counter(CType, Type, Acc) ->
    Counters = get_counters(Acc),
    get_counter_aux(CType, Type, Counters).

%%-------------------------------------------------------------------------
%% @doc
%% Get the value of a counter from the counter record.
%% The two parameters CounterType and ClustType determine which counter to set.
%% @end
%%-------------------------------------------------------------------------
-spec get_counter_aux(CounterType, ClustType | SymmetricType, cluster_folding:cluster_counters()) -> number() when
      CounterType :: label | pos | invisible_label,
      ClustType :: comm | {ex, 1} | {ex, 2},
      SymmetricType :: comm | ex.
get_counter_aux(invisible_label, comm, #cluster_counters{invisible_counter = N}) -> N;
get_counter_aux(pos, comm, #cluster_counters{common_pos_counter = N}) -> N;
get_counter_aux(pos, {ex, 1}, #cluster_counters{exclusive_pos_counter_1 = N}) -> N;
get_counter_aux(pos, {ex, 2}, #cluster_counters{exclusive_pos_counter_2 = N}) -> N;
get_counter_aux(label, comm, #cluster_counters{common_counter = N}) -> N;
get_counter_aux(label, ex, #cluster_counters{exclusive_counter = N}) -> N.

%%-------------------------------------------------------------------------
%% @doc
%% Extracts the counters record from the acc record
%% @end
%%-------------------------------------------------------------------------
-spec get_counters(cluster_folding:acc()) -> cluster_folding:cluster_counters().
get_counters(#acc{counters = Counters}) -> Counters.

%%-------------------------------------------------------------------------
%% @doc
%% Sets a counter in the acc record to N.
%% The two parameters CounterType and ClustType determine which counter to set.
%% @end
%%-------------------------------------------------------------------------
-spec set_counter(CounterType, ClustType | SymmetricType, N, cluster_folding:acc()) ->
			 cluster_folding:acc() when
      CounterType :: label | pos | invisible_label, N :: number(),
      ClustType :: comm | {ex, 1} | {ex, 2},
      SymmetricType :: comm | ex.
set_counter(CType, Type, Number, Acc) ->
    Counters = get_counters(Acc),
    TouchedCounters = set_counter_aux(CType, Type, Number, Counters),
    set_counters(Acc, TouchedCounters).

%%-------------------------------------------------------------------------
%% @doc
%% Sets a counter in the counter record to N.
%% The two parameters CounterType and ClustType determine which counter to set.
%% @end
%%-------------------------------------------------------------------------
-spec set_counter_aux(CounterType, ClustType | SymmetricType, N, cluster_folding:cluster_counters()) ->
			     cluster_folding:cluster_counters() when
      CounterType :: label | pos | invisible_label, N :: number(),
      ClustType :: comm | {ex, 1} | {ex, 2},
      SymmetricType :: comm | ex.
set_counter_aux(invisible_label, comm, N, Counters) -> Counters#cluster_counters{invisible_counter = N};
set_counter_aux(pos, comm, N, Counters) -> Counters#cluster_counters{common_pos_counter = N};
set_counter_aux(pos, {ex, 1}, N, Counters) -> Counters#cluster_counters{exclusive_pos_counter_1 = N};
set_counter_aux(pos, {ex, 2}, N, Counters) -> Counters#cluster_counters{exclusive_pos_counter_2 = N};
set_counter_aux(label, comm, N, Counters) -> Counters#cluster_counters{common_counter = N};
set_counter_aux(label, ex, N, Counters) -> Counters#cluster_counters{exclusive_counter = N}.

%%-------------------------------------------------------------------------
%% @doc
%% Stores the counters record in the acc record
%% @end
%%-------------------------------------------------------------------------
-spec set_counters(cluster_folding:acc(),cluster_folding:cluster_counters()) ->
			  cluster_folding:acc().
set_counters(Acc, Counters) -> Acc#acc{counters = Counters}.

%%-------------------------------------------------------------------------
%% @doc
%% It takes a Number and a Cluster. It returns a name for the function
%% defined by Cluster (if it has a Name it returns it) and the Number.
%% If it must invent a Name it will use Number and return its successor
%% instead.
%% @end
%%-------------------------------------------------------------------------
-spec generate_fun_name(Number :: non_neg_integer(),
			Cluster :: cluster:cluster(tree:tree_node()),
			Tree :: tree:tree(), any()) -> 
			   {Name :: string(), NewNumber :: non_neg_integer()}.
generate_fun_name(Number,Cluster,Tree,Prefix) ->
     Parent = cluster:get_root(Cluster),
     Node = ast_tree:get_left_if_node_pair(Parent),
     generate_fun_name_aux(Number,Node,Tree,Prefix).

%%-------------------------------------------------------------------------
%% @doc
%% It takes a Number and a Node. It returns a name for the function
%% defined by Node (if it has a Name it returns it) and the Number.
%% If it must invent a Name it will use Number and return its successor
%% instead.
%% @end
%%-------------------------------------------------------------------------
-spec generate_fun_name_aux(Number :: non_neg_integer(),
			    Node :: tree:tree_node(), Tree :: tree:tree(),
			    any()) -> 
			       {Name :: string(),
				NewNumber :: non_neg_integer()}.
generate_fun_name_aux(Number,Node,Tree,Prefix) ->
    case ast_tree:get_fun_name(Node, Tree) of
	{ok, {Name, _}} -> {Name, Number};
	error -> {generate_fun_name_aux(Number,Prefix), Number + 1}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Creates a function name from a number.
%% @end
%%-------------------------------------------------------------------------
-spec generate_fun_name_aux(Number :: non_neg_integer(), any()) -> 
                               string().
generate_fun_name_aux(Number,FunNamePrefix) -> FunNamePrefix ++ integer_to_list(Number).

