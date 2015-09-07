%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Implements the functionality to traverse the clusters frontiers
%%% in roughly breadth first order.
%%% @end
%%% Created :  4 Jun 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(cluster_folding).

-include("cluster_records.hrl").

-export_type([acc/0, cluster_info/0, cluster_dicts/0,
	      tree_info/0, cluster_counters/0,
	      environment_info/0]).

-type acc() :: #acc{
		  cluster_labels :: da_map:da_map({comm | {ex, 1 | 2}, string()},
						  cluster:cluster(tree:tree_node())),
		  exp_funs :: sets:set({atom(), integer()})
		 }. %% Record for use as accumulator when folding through cluster borders.
                    %% Contains the following fields: <ul>
                    %% <li> `ph_map' - maps PH (tree place holders) to pairs
                    %%                 {symmetric_type, cluster_name}
                    %%                 (for creating function calls) </li>
                    %% <li> `cluster_order' - maps pairs {order_type, position} to
                    %%                        clusters (function) names </li>
                    %% <li> `cluster_labels' - da_maps (function) names to clusters </li>
                    %% <li> `cluster_info' - maps (function) names to records
                    %%                       with information about the cluster
                    %%                       (`cluster_info' records) </li>
                    %% <li> `cluster_funname' - maps generated (function) names to the actual
                    %%                          name that should be displayed in the file </li>
                    %% <li> `cluster_orifunname' - maps generated (function) names to the original
                    %%                             name that it was suppoused to have </li>
                    %% <li> `cluster_dicts' - contains the cluster dictionaries
                    %%                        (`cluster_dict' record) </li>
                    %% <li> `allocated_parents' - saves a set with the parents of clusters
                    %%                            whose name will be assigned by a common
                    %%                            cluster. This is checked in a first pass
                    %%                            so that we minimise the numbre of dummy
                    %%                            functions needed to balance exclusive
                    %%                            clusters </li>
                    %% <li> `tree_info' - contains the trees and the mapping
                    %%                    (`tree_info' record) </li>
                    %% <li> `exp_funs' - set with the local functions to export </li>
                    %% <li> `counters' - contains counters for function_name generation
                    %%                   (`counters' record) </li>
                    %% <li> `out_module' - contains the output module name
                    %%                     as an atom </li>
                    %% <li> `var_for_module' - name for the var that contains the 
                    %%                         module instance name, (guaranteed to
                    %%                         be unique for both modules) </li>
                    %% <li> `root_for_new_exclusive_funnames' - prefix for the name of the generated
                    %%                                          functions in exclusive modules, (all
                    %%                                          existing functions are guaranteed to
                    %%                                          not start by this prefix) </li>
                    %% <li> `root_for_new_common_funnames' - prefix for the name of the generated
                    %%                                       functions in common module, (all existing
                    %%                                       functions are guaranteed to not start by
                    %%                                       this prefix) </li></ul>

-type cluster_dicts() :: #cluster_dicts{
			    comm_clus :: cluster_dict:cluster_dict(tree:tree_node()),
			    ex_clus1 :: cluster_dict:cluster_dict(tree:tree_node()),
			    ex_clus2 :: cluster_dict:cluster_dict(tree:tree_node())
			   }. %% Record for keeping the updated cluster dictionaries.
                              %% Contains the following fields: <ul>
                              %% <li> `comm_clus' - Contains the cluster dictionary
                              %%                    with the common clusters </li>
                              %% <li> `ex_clus1' - Contains the cluster dictionary with
                              %%                   the exclusive clusters of side one </li>
                              %% <li> `ex_clus2' - Contains the cluster dictionary with
                              %%                   the exclusive clusters of side two</li></ul>

-type cluster_info() :: #cluster_info{}. %% Record for keeping information about a particular
                                         %% cluster. Contains the following fields: <ul>
                                         %% <li> `cluster_type' - Whether the cluster is common,
                                         %%                       exclusive1, or exclusive2 </li>
                                         %% <li> `environment_info' - Contains information like the free
                                         %%                           and bounded variables at the root
                                         %%                           of the cluster, (`environment_info'
                                         %%                           record) </li></ul>

-type tree_info() :: #tree_info{
			tree1 :: tree:tree(),
			tree2 :: tree:tree(),
			tree1_args :: dict:dict({FunName :: string(),
						 Arity :: non_neg_integer()},
						[ArgName :: string()]),
			tree2_args :: dict:dict({FunName :: string(),
						 Arity :: non_neg_integer()},
						[ArgName :: string()]),
			mapping :: da_map:da_map(tree:tree_node(), tree:tree_node())
		       }. %% Record for keeping the tree and mapping information.
                          %% Contains the following fields: <ul>
                          %% <li> `tree1' - Contains the cluster dictionary
                          %%                with the common clusters </li>
                          %% <li> `tree2' - Contains the cluster dictionary with
                          %%                the exclusive clusters of side one </li>
                          %% <li> `tree1_args' - Contains a dictionary with good names
                          %%                     for function arguments of tree1 </li>
                          %% <li> `tree2_args' - Contains a dictionary with good names
                          %%                     for funciton arguments of tree2 </li>
                          %% <li> `mapping' - Contains the cluster dictionary with
                          %%                  the exclusive clusters of side two</li></ul>

-type cluster_counters() :: #cluster_counters{}. %% Record that keeps counters used for naming
                                                 %% functions and ordering them in the files.
                                                 %% Contains the following fields: <ul>
                                                 %% <li> `invisible_counter' - Contains the counter for the
                                                 %%                            label identifiers used when the
                                                 %%                            used label is already provided
                                                 %%                            by the user </li>
                                                 %% <li> `common_counter' - Contains the counter for the
                                                 %%                         labels of the common cluster </li>
                                                 %% <li> `common_pos_counter' - Contains the counter for the
                                                 %%                             order of the common cluster </li>
                                                 %% <li> `exclusive_counter' - Contains the counter for the
                                                 %%                            labels of the exclusive
                                                 %%                            cluster </li>
                                                 %% <li> `exclusive_pos_counter_1' - Contains the counter for the
                                                 %%                                order of the left exclusive
                                                 %%                                clusters </li>
                                                 %% <li> `exclusive_pos_counter_2' - Contains the counter for the
                                                 %%                                order of the right exclusive
                                                 %%                                clusters </li></ul>

-type environment_info() :: #environment_info{}. %% Record that contains environment information
                                                 %% like the free and bound variables at the root
                                                 %% of a cluster. Contains the following fields: <ul>
                                                 %% <li> `free_variables' - Contains a list with
                                                 %%                         variables that are free</li>
                                                 %% <li> `free_left' - Contains a list with variables
                                                 %%                    that were free in the left tree</li>
                                                 %% <li> `free_right' - Contains a list with variables
                                                 %%                     that were free in the left tree</li>
                                                 %% <li> `export_variables' - Contains a list with
                                                 %%                           variables that are exported</li>
                                                 %% <li> `export_left' - Contains a list with variables
                                                 %%                      that are exported by the left tree</li>
                                                 %% <li> `export_right' - Contains a list with variables
                                                 %%                       that are exported by the left tree</li>
                                                 %% <li> `is_left_leaf' - Weather the cluster has children
                                                 %%                       from left side (in another cluster)</li>
                                                 %% <li> `is_right_leaf' - Weather the cluster has children
                                                 %%                        from right side (in another cluster)</li></ul>

-export([fold_clusters/4]).

-export([find_free_vars_for_sym_label/3,
	 find_exported_vars_for_sym_label/3,
	 find_actual_free_vars_for_asym_label/3,
	 find_actual_exported_vars_for_asym_label/3,
	 get_type_list/2, get_leaf_as_is_or_default/3,
	 isolate_type_class/1, get_children_from_node_list/5,
	 get_frontier_children_from_node_list/5, merge_environments/2,
	 is_in_scope/4]).

%%-------------------------------------------------------------------------
%% @doc
%% Folds a function through the frontiers of the clusters in roughly
%% breadth first order.
%% The arguments of the function are: <ul>
%% <li> `ClusType' - A symbol that represents the type the of
%%                   upper cluster of the frontier </li>
%% <li> `PH' - The place holder of the frontier </li>
%% <li> `Children' - A list of alternative nodes corresponding
%%                   to the place holder of the frontier </li>
%% <li> `Acc0' - The accumulator of the folding </li></ul>
%% @end
%%-------------------------------------------------------------------------
-spec fold_clusters(Fun, AccIn :: Acc, ClustInfo, TreeInfo) ->
			    AccOut :: Acc when
      Node :: tree:tree_node(),
      ClustInfo :: cluster_dicts(),
      TreeInfo :: tree_info(),
      Fun :: fun(({(comm | {ex, 1 | 2}),
		   ClusterIn :: cluster:cluster(tree:tree_node()),
		   Tree :: tree:tree() | na}, leaf_node | local_calls | {ph, PH},
		  Children, Acc0 :: Acc) -> Acc1 :: Acc),
      Children :: [Node | {atom(), non_neg_integer()}],
      PH :: term(),
      Acc :: term().
fold_clusters(Fun, Acc, ClustInfo,
	      #tree_info{tree1 = Tree1,
			 tree2 = Tree2} = TreeInfo) ->
    RootNode1 = tree:get_root_node(Tree1),
    RootNode2 = tree:get_root_node(Tree2),
    Queue = queue:from_list([RootNode1, RootNode2]),
    VisitedNodes = sets:new(),
    fold_clusters_aux(Queue, VisitedNodes, Fun, Acc, ClustInfo, TreeInfo).

%%-------------------------------------------------------------------------
%% @doc
%% Auxiliar function to {@link cluster_mapping:fold_clusters/4}.
%% It does the same but expects some initialised values: <ul>
%% <li> `Queue' - Queue with the list of nodes to visit </li>
%% <li> `VisitedNodes' - Set with the visited nodes </li></ul>
%% @end
%%-------------------------------------------------------------------------
-spec fold_clusters_aux(Queue, VisitedNodes, Fun, AccIn :: Acc,
			ClustInfo, TreeInfo) -> AccOut :: Acc when
      Node :: tree:tree_node(),
      Queue :: queue:queue(),
      VisitedNodes :: sets:set(),
      ClustInfo :: cluster_dicts(),
      TreeInfo :: tree_info(),
      Fun :: fun(({(comm | {ex, 1 | 2}),
		   ClusterIn :: cluster:cluster(tree:tree_node()),
		   Tree :: tree:tree() | na}, leaf_node | local_calls | {ph, PH},
		  Children, Acc0 :: Acc) -> Acc1 :: Acc),
      Children :: [Node | {atom(), non_neg_integer()}],
      PH :: term(),
      Acc :: term().
fold_clusters_aux(Queue, VisitedNodes, Fun, AccIn, ClustInfo, TreeInfo) ->
    case queue:out(Queue) of
	{empty, _} -> AccIn;
	{{value, Node}, NewQueue} ->
	    {NewVisitedNodes, {FinalQueue, AccOut}} =
		if_not_visited(Node, VisitedNodes, {NewQueue, AccIn},
			       fun (El, Acc) ->
				       fold_node(El, Acc, Fun, ClustInfo, TreeInfo)
			       end),
	    fold_clusters_aux(FinalQueue, NewVisitedNodes, Fun,
			      AccOut, ClustInfo, TreeInfo)
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Applies Fun to Node and Data if the node has not been visited before.
%% It returns the result of this application or the original Data.
%% It adds Node to VisitedNodes.
%% @end
%%-----------------------------------------------------------------------
-spec if_not_visited(Node, VisitedNodesIn, DataIn :: Acc, Fun) ->
				  {VisitedNodesOut, DataOut :: Acc} when
      Node :: tree:tree_node(),
      VisitedNodesIn :: VisitedNodes, VisitedNodesOut :: VisitedNodes,
      VisitedNodes :: sets:set(Node),
      Fun :: fun((Node, Data0 :: Acc) -> Data1 :: Acc),
      Acc :: term().
if_not_visited(Node, VisitedNodes, Data, Fun) ->
    case sets:is_element(Node, VisitedNodes) of
	false -> NewData = Fun(Node, Data),
		 NewVisitedNodes = sets:add_element(Node, VisitedNodes),
		 {NewVisitedNodes, NewData};
	true -> {VisitedNodes, Data}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Folds the function through a node. First it finds out some information
%% about the node, (like to which cluster it belongs, and its type).
%% @end
%%-------------------------------------------------------------------------
-spec fold_node(Node, {Queue, AccIn :: Acc}, Fun, ClustInfo, TreeInfo) ->
		       {Queue, AccOut :: Acc} when
      Node :: tree:tree_node(),
      Queue :: queue:queue(),
      ClustInfo :: cluster_dicts(),
      TreeInfo :: tree_info(),
      Fun :: fun(({(comm | {ex, 1 | 2}),
		   ClusterIn :: cluster:cluster(tree:tree_node()),
		   Tree :: tree:tree() | na}, leaf_node | local_calls | {ph, PH},
		  Children, Acc0 :: Acc) -> Acc1 :: Acc),
      Children :: [Node | {atom(), non_neg_integer()}],
      PH :: term(),
      Acc :: term().
fold_node(Node, {QueueIn, AccIn}, Fun, ClustInfo, TreeInfo) ->
    TypeList = get_type_list(ClustInfo, TreeInfo),
    lists:foldl(fun ({ClusType, ClusterDict, Tree}, {Queue0, Acc0}) ->
			case cluster_dict:get_cluster_for_node(Node, ClusterDict) of
			    {ok, Cluster} ->
				fold_node_aux({ClusType, Cluster, Tree},
					      {Queue0, Acc0}, Fun,
					      TreeInfo);
			    error -> {Queue0, Acc0}
			end
		end, {QueueIn, AccIn}, TypeList).

%%-------------------------------------------------------------------------
%% @doc
%% Extracts children and placeholders and folds the function through
%% the placeholders. It also updates the Queue to include the children
%% of the current Cluster.
%% @end
%%-------------------------------------------------------------------------
-spec fold_node_aux({(comm | {ex, 1 | 2}),
		     Cluster :: cluster:cluster(tree:tree_node()),
		     Tree :: tree:tree() | na},
		    {Queue0 :: Queue, Acc0 :: Acc},
		    Fun, TreeInfo :: tree_info()) ->
			   {Queue, AccOut :: Acc} when
      Node :: tree:tree_node(),
      Fun :: fun(({(comm | {ex, 1 | 2}),
		   ClusterIn :: cluster:cluster(tree:tree_node()),
		   Tree :: tree:tree() | na}, leaf_node | local_calls | {ph, PH},
		  Children, Acc0 :: Acc) -> Acc1 :: Acc),
      Children :: [Node | {atom(), non_neg_integer()}],
      PH :: term(),
      Queue :: queue:queue(),
      Acc :: term().
fold_node_aux({ClusType, Cluster, Tree}, {Queue0, Acc0}, Fun, TreeInfo) ->
    LeafNodes = cluster:get_nodes(Cluster),
    ChildrenList = get_children_from_node_list(ClusType, Cluster, Tree, TreeInfo, LeafNodes, false),
    {Queue1, Acc1} = fold_through_children_list(ClusType, Cluster, Tree, Queue0,
						Acc0, Fun, ChildrenList),
    notify_leaves_and_local_calls(ChildrenList, ClusType, Cluster, Tree, Fun, TreeInfo,
	                          LeafNodes, Queue1, Acc1).

%%-------------------------------------------------------------------------
%% @doc
%% Calls Fun to notify of leaf nodes or nodes with local calls when appropriate.
%% @end
%%-------------------------------------------------------------------------
-spec notify_leaves_and_local_calls(ChildrenList :: [tree:tree_node()],
				    ClustType, Cluster :: cluster:cluster(tree:tree_node()),
				    Tree :: 'na' | tree:tree(),
				    Fun, TreeInfo :: cluster_folding:tree_info(),
				    LeafNodes :: [tree:tree_node()],
				    Queue0 :: queue:queue(),
				    Acc) -> {Queue1 :: queue:queue(),
					     Acc1 :: cluster_folding:acc()} when
      ClustType :: comm | {ex, 1} | {ex, 2},
      Node :: tree:tree_node(),
      Fun :: fun(({(comm | {ex, 1 | 2}),
		   ClusterIn :: cluster:cluster(tree:tree_node()),
		   Tree :: tree:tree() | na}, leaf_node | local_calls | {ph, PH},
		  Children, Acc0 :: Acc) -> Acc1 :: Acc),
      Children :: [Node | {atom(), non_neg_integer()}],
      PH :: term().
notify_leaves_and_local_calls(ChildrenList, ClusType, Cluster, Tree, Fun, TreeInfo,
                              LeafNodes, Queue1, Acc1) ->
    case {ChildrenList, local_calls_and_module_macros(LeafNodes, Tree, TreeInfo)} of
	{[], {[], []}} -> {Queue1, Fun({ClusType, Cluster, Tree}, leaf_node, [], Acc1)};
	{_, {[], _}} -> {Queue1, Acc1};
	{_, {LocalCalls, _}} -> {Queue1, Fun({ClusType, Cluster, Tree},
					     local_calls, LocalCalls, Acc1)}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Folds Fun through the ChildrenList, adds the nodes in ChildrenList to the queue.
%% @end
%%-------------------------------------------------------------------------
-spec fold_through_children_list(ClustType, Cluster :: cluster:cluster(tree:tree_node()),
				 Tree :: tree:tree(),
				 Queue0 :: queue:queue(),
				 Acc :: cluster_folding:acc(), Fun,
				 ChildrenList :: [{PH, Children}]) ->
					{Queue1 :: queue:queue(),
					 Acc1 :: cluster_folding:acc()} when
      ClustType :: comm | {ex, 1} | {ex, 2},
      Node :: tree:tree_node(),
      Fun :: fun(({(comm | {ex, 1 | 2}),
		   ClusterIn :: cluster:cluster(tree:tree_node()),
		   Tree :: tree:tree() | na}, leaf_node | local_calls | {ph, PH},
		  Children, Acc0 :: Acc) -> Acc1 :: Acc),
      Children :: [Node | {atom(), non_neg_integer()}].
fold_through_children_list(ClusType, Cluster, Tree, Queue0, Acc0, Fun,
	                   ChildrenList) ->
    {_OnlyPH, OnlyChildrenList} = lists:unzip(ChildrenList),
    Queue1 = lists:foldl(fun queue:in/2, Queue0, lists:append(OnlyChildrenList)),
    Acc1 = lists:foldl(fun ({PH, Children}, Acc) ->
	                       Fun({ClusType, Cluster, Tree}, {ok, PH}, Children, Acc)
                       end, Acc0, ChildrenList),
    {Queue1, Acc1}.

%%-------------------------------------------------------------------------
%% @doc
%% Finds the names and arities of the local calls and module macros in the
%% nodes LeafNodes. It uses Tree for it or for the left tree if the atom
%% `na' is passed.
%% @end
%%-------------------------------------------------------------------------
-spec local_calls_and_module_macros(LeafNodes :: [tree:tree_node()],
				    Tree :: tree:tree() | na,
				    TreeInfo :: cluster_folding:tree_info()) ->
					   {[{atom(), integer()}],
					    [{atom(), integer()}]}.
local_calls_and_module_macros(LeafNodes, Tree, TreeInfo) ->
    lists:foldl(fun (Node, Acc) ->
			acc_local_calls_and_module_macros(Node, Tree, TreeInfo, Acc)
		end, {[], []}, LeafNodes).

%%-------------------------------------------------------------------------
%% @doc
%% Accumulates the names and arities of all the local calls and module macros
%% in the subtree of Node. It uses Tree for it or for the left tree if the atom
%% `na' is passed.
%% @end
%%-------------------------------------------------------------------------
-spec acc_local_calls_and_module_macros(Node :: wrangler_syntax:syntaxTree(),
					Tree :: tree:tree() | na,
					TreeInfo :: cluster_folding:tree_info(),
					Acc :: {[{atom(), integer()}],
						[{atom(), integer()}]}) ->
					       {[{atom(), integer()}],
						[{atom(), integer()}]}.
acc_local_calls_and_module_macros(Node, na, #tree_info{tree1 = Tree}, Acc) ->
    Value = tree:get_value(ast_tree:get_left_if_node_pair(Node)),
    acc_local_calls_and_module_macros_value(Tree, Value, Acc);
acc_local_calls_and_module_macros(Node, Tree, _TreeInfo, Acc) ->
    Value = tree:get_value(Node),
    acc_local_calls_and_module_macros_value(Tree, Value, Acc).

%%-------------------------------------------------------------------------
%% @doc
%% Accumulates the names and arities of all the local calls and module macros
%% in the subtree of Node.
%% @end
%%-------------------------------------------------------------------------
-spec acc_local_calls_and_module_macros_value(Tree :: tree:tree(),
					      Node :: wrangler_syntax:syntaxTree(),
					      Acc :: {[{atom(), integer()}],
						      [{atom(), integer()}]}) ->
						     {[{atom(), integer()}],
						      [{atom(), integer()}]}.
acc_local_calls_and_module_macros_value(Tree, Value, {LocalAcc, MacroAcc} = Acc) ->
    case wrangler_syntax:type(Value) of
	application -> {acc_call_if_local(Tree, Value, LocalAcc), MacroAcc};
	implicit_fun -> {acc_implicit_fun_if_local(Tree, Value, LocalAcc), MacroAcc};
	macro -> {LocalAcc, acc_macro_if_module(Tree, Value, MacroAcc)};
	_ -> Acc
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Accumulates the name and arity in Node if it corresponds to a local call.
%% @end
%%-------------------------------------------------------------------------
-spec acc_call_if_local(Tree :: tree:tree(),
			Node :: wrangler_syntax:syntaxTree(),
			Acc :: [{Name :: atom(), Arity :: integer()}]) ->
			       [{atom(), integer()}].
acc_call_if_local(Tree, Value, Acc) ->
    OpValue = application_operator_tree(Tree, Value),
    case wrangler_syntax:type(OpValue) of
	atom -> Arity = length(wrangler_syntax:application_arguments(Value)),
		acc_fun_ref_if_local(OpValue, Arity, Tree, Acc);
	_ -> Acc
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Accumulates the name and arity in Node if it corresponds to
%% an implicit local function.
%% @end
%%-------------------------------------------------------------------------
-spec acc_implicit_fun_if_local(Tree :: tree:tree(),
				Node :: wrangler_syntax:syntaxTree(),
				Acc :: [{Name :: atom(), Arity :: integer()}]) ->
				       [{atom(), integer()}].
acc_implicit_fun_if_local(Tree, Value, Acc) ->
    AQuali = implicit_fun_name_tree(Tree, Value),
    case wrangler_syntax:type(AQuali) of
	arity_qualifier ->
	    NameST = arity_qualifier_body_tree(Tree, AQuali),
	    ArityST = arity_qualifier_argument_tree(Tree, AQuali),
	    acc_arity_qualifier_if_local(Tree, NameST, ArityST, Acc);
	_ -> Acc
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Accumulates the name in NameST and arity in ArityST if they
%% correspond to a local function call.
%% @end
%%-------------------------------------------------------------------------
-spec acc_arity_qualifier_if_local(Tree :: tree:tree(),
				   NameST :: wrangler_syntax:syntaxTree(),
				   ArityST :: wrangler_syntax:syntaxTree(),
				   Acc :: [{Name :: atom(), Arity :: integer()}]) ->
					  [{atom(), integer()}].
acc_arity_qualifier_if_local(Tree, NameST, ArityST, Acc) ->
    case {wrangler_syntax:type(NameST), wrangler_syntax:type(ArityST)} of
	{atom, integer} ->
	    Arity = wrangler_syntax:integer_value(ArityST),
	    acc_fun_ref_if_local(NameST, Arity, Tree, Acc);
	_ -> Acc
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Accumulates the name in NameSt and Arity if it is a local call.
%% @end
%%-------------------------------------------------------------------------
-spec acc_fun_ref_if_local(NameST :: wrangler_syntax:syntaxTree(),
			   Arity :: integer(), Tree :: tree:tree(),
			   Acc :: [{Name :: atom(), Arity :: integer()}]) ->
				  [{atom(), integer()}].
acc_fun_ref_if_local(NameST, Arity, Tree, Acc) ->
    Name = wrangler_syntax:atom_value(NameST),
    case erl_internal:bif(Name, Arity) orelse
	is_in_scope_modinfo(Name, Arity, ast_tree:get_inscope_funs(Tree)) of
	false -> [{Name, Arity}|Acc];
	true -> Acc
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Accumulates the name and arity of the macro call at Node, if it is static.
%% @end
%%-------------------------------------------------------------------------
-spec acc_macro_if_module(Tree :: tree:tree(),
			  Node :: wrangler_syntax:syntaxTree(),
			  Acc :: [{Name :: atom(), Arity :: integer()}]) ->
				 [{atom(), integer()}].
acc_macro_if_module(Tree, Value, Acc) ->
    NameValue = macro_name_tree(Tree, Value),
    case wrangler_syntax:type(NameValue) of
	variable -> Name = wrangler_syntax:variable_literal(NameValue),
		    Arity = wrangler_syntax:macro_arguments(Value),
		    case {Name, Arity} of
			{"MODULE", none} -> [{Name, Arity}|Acc];
			_ -> Acc
		    end;
	_ -> Acc
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Mononode wrapper for {@link wrangler_syntax:arity_qualifier_body/1}.
%% @see mono_node_wrapper/3
%% @end
%%-------------------------------------------------------------------------
-spec arity_qualifier_body_tree(Tree :: tree:tree(),
				Node :: wrangler_syntax:syntaxTree()) ->
				       wrangler_syntax:syntaxTree().
arity_qualifier_body_tree(Tree, Value) ->
    mono_node_wrapper(fun wrangler_syntax:arity_qualifier_body/1, Value, Tree).

%%-------------------------------------------------------------------------
%% @doc
%% Mononode wrapper for {@link wrangler_syntax:arity_qualifier_argument/1}.
%% @see mono_node_wrapper/3
%% @end
%%-------------------------------------------------------------------------
-spec arity_qualifier_argument_tree(Tree :: tree:tree(),
				    Node :: wrangler_syntax:syntaxTree()) ->
					   wrangler_syntax:syntaxTree().
arity_qualifier_argument_tree(Tree, Value) ->
    mono_node_wrapper(fun wrangler_syntax:arity_qualifier_argument/1, Value, Tree).

%%-------------------------------------------------------------------------
%% @doc
%% Mononode wrapper for {@link wrangler_syntax:implicit_fun_name/1}.
%% @see mono_node_wrapper/3
%% @end
%%-------------------------------------------------------------------------
-spec implicit_fun_name_tree(Tree :: tree:tree(),
			     Node :: wrangler_syntax:syntaxTree()) ->
				    wrangler_syntax:syntaxTree().
implicit_fun_name_tree(Tree, Value) ->
    mono_node_wrapper(fun wrangler_syntax:implicit_fun_name/1, Value, Tree).

%%-------------------------------------------------------------------------
%% @doc
%% Mononode wrapper for {@link wrangler_syntax:application_operator/1}.
%% @see mono_node_wrapper/3
%% @end
%%-------------------------------------------------------------------------
-spec application_operator_tree(Tree :: tree:tree(),
				Node :: wrangler_syntax:syntaxTree()) ->
				       wrangler_syntax:syntaxTree().
application_operator_tree(Tree, Value) ->
    mono_node_wrapper(fun wrangler_syntax:application_operator/1, Value, Tree).

%%-------------------------------------------------------------------------
%% @doc
%% Mononode wrapper for {@link wrangler_syntax:macro_name/1}.
%% @see mono_node_wrapper/3
%% @end
%%-------------------------------------------------------------------------
-spec macro_name_tree(Tree :: tree:tree(),
		      Node :: wrangler_syntax:syntaxTree()) ->
			     wrangler_syntax:syntaxTree().
macro_name_tree(Tree, Value) ->
    mono_node_wrapper(fun wrangler_syntax:macro_name/1, Value, Tree).

%%-------------------------------------------------------------------------
%% @doc
%% Takes the value of a tree node, applies a function on syntax trees that
%% returns a child, and returns the value of that child.
%% This function is necessary because the trees store each node of the
%% syntax tree isolated, because of this, traditional functions that operate
%% on syntax tree, when applicated to tree nodes, return placeholders,
%% this function prevents this.
%% @end
%%-------------------------------------------------------------------------
-spec mono_node_wrapper(fun((wrangler_syntax:syntaxTree()) ->
				   wrangler_syntax:syntaxTree()),
			NodeValue :: wrangler_syntax:syntaxTree(),
			Tree :: tree:tree()) -> wrangler_syntax:syntaxTree().
mono_node_wrapper(Fun, ParentAST, Tree) ->
    ValueNode = Fun(ParentAST),
    {ok, Value} = tree:get_node(ValueNode, Tree),
    tree:get_value(Value).

%%-------------------------------------------------------------------------
%% @doc
%% Gets all the children for a list of nodes and maps them to
%% the placeholders on the nodes, (their left alternative if common).
%% @end
%%-------------------------------------------------------------------------
-spec get_children_from_node_list(ClustType, Cluster, Tree, TreeInfo,
				  Nodes :: [Node]) ->
					 [{PH, Children}] when
      ClustType :: comm | {ex, 1} | {ex, 2},
      Cluster :: cluster:cluster(tree:tree_node()),
      Tree :: tree:tree() | na,
      TreeInfo :: tree_info(),
      Node :: tree:tree_node(),
      PH :: term(), Children :: [Node].
get_children_from_node_list(ClusType, Cluster, Tree, TreeInfo, Nodes) ->
    get_children_from_node_list(ClusType, Cluster, Tree, TreeInfo, Nodes, true).

%%-------------------------------------------------------------------------
%% @doc
%% Gets all the children for a list of nodes and maps them to
%% the placeholders on the nodes, (their left alternative if common).
%% It only gets the children that are in a different cluster, not the ones
%% that are in the same.
%% @end
%%-------------------------------------------------------------------------
-spec get_frontier_children_from_node_list(ClustType, Cluster, Tree, TreeInfo,
				  Nodes :: [Node]) ->
					 [{PH, Children}] when
      ClustType :: comm | {ex, 1} | {ex, 2},
      Cluster :: cluster:cluster(tree:tree_node()),
      Tree :: tree:tree() | na,
      TreeInfo :: tree_info(),
      Node :: tree:tree_node(),
      PH :: term(), Children :: [Node].
get_frontier_children_from_node_list(ClusType, Cluster, Tree, TreeInfo, Nodes) ->
    get_children_from_node_list(ClusType, Cluster, Tree, TreeInfo, Nodes, false).

%%-------------------------------------------------------------------------
%% @doc
%% Gets all the children for a list of nodes and maps them to
%% the placeholders on the node, (their left alternative if common).
%% It only gets the children that are in a different cluster if All is false.
%% @end
%%-------------------------------------------------------------------------
-spec get_children_from_node_list(ClustType, Cluster, Tree, TreeInfo,
				  Nodes :: [Node], All :: boolean()) ->
					 [{PH, Children}] when
      ClustType :: comm | {ex, 1} | {ex, 2},
      Cluster :: cluster:cluster(tree:tree_node()),
      Tree :: tree:tree() | na,
      TreeInfo :: tree_info(),
      Node :: tree:tree_node(),
      PH :: term(), Children :: [Node].
get_children_from_node_list(ClusType, Cluster, Tree, TreeInfo, Nodes, All) ->
    lists:filter(fun ({_, X}) ->
			 (not lists:all(fun (Y) -> cluster:has_node(Y, Cluster) end, X)) orelse All
		 end, 
		 lists:append(
		   [lists:zip(
		      ast_tree:get_placeholders_from_node(Node),
		      get_children_from_node(ClusType, Tree, TreeInfo, Node)
		     ) || Node <- Nodes])).

%%-------------------------------------------------------------------------
%% @doc
%% Finds all the possible children for the Node. It returns them in order
%% and coupled if they have a mapping. The alternatives for each child are
%% grouped in sublists (usually with one element):
%% If there are two exclusive nodes [exclusive1, exclusive2], if there
%% is a common node [{node_pair, comm1, comm2}], if there are two common
%% [{node_pair, comm1, comm2}, {node_pair, comm1, comm2}].
%% @end
%%-------------------------------------------------------------------------
-spec get_children_from_node(ClustType, Tree, TreeInfo, Node) ->
				    Children when
      ClustType :: comm | {ex, 1} | {ex, 2},
      Tree :: na | tree:tree(),
      TreeInfo :: tree_info(),
      Node :: tree:tree_node(),
      Children :: [[Node]].
get_children_from_node(comm, na, #tree_info{tree1 = Tree1,
				            tree2 = Tree2,
				            mapping = Mapping}, Node) ->
    {Node1, Node2} = tree:get_pair_tuple(Node),
    Children1 = tree:get_children(Node1, Tree1),
    Children2 = tree:get_children(Node2, Tree2),
    expand_alternatives(Children1, Children2, Mapping);
get_children_from_node({ex, N}, Tree,
		       #tree_info{
		          mapping = Mapping
		         }, Node) ->
    Children = tree:get_children(Node, Tree),
    [[to_node_pair(ChildNode, N, Mapping)] || ChildNode <- Children].

%%-------------------------------------------------------------------------
%% @doc
%% It takes children nodes by pairs, if they map they are combined
%% into a single node, else they are sequenced into a sublist
%% @end
%%-------------------------------------------------------------------------
-spec expand_alternatives(Children1 :: [Node], Children2 :: [Node],
			  Mapping) -> [[Node]] when
      Mapping :: da_map:da_map(tree:tree_node(), tree:tree_node()),
      Node :: tree:tree_node().
expand_alternatives([], [], _Mapping) -> [];
expand_alternatives([Child1|Children1], [Child2|Children2], Mapping) ->
    case da_map:has_pair(Child1, Child2, Mapping) of
	true -> NodePair = tree:create_node_pair(Child1, Child2),
		[[NodePair]];
	false -> [[to_node_pair(Child1, 1, Mapping),
		   to_node_pair(Child2, 2, Mapping)]]
    end ++ expand_alternatives(Children1, Children2, Mapping).

%%-------------------------------------------------------------------------
%% @doc
%% Checks whether the node has a mapping and transforms it into
%% a node pair if so. Leaves it as is otherwise. It expects
%% a node which is not a node pair
%% @end
%%-------------------------------------------------------------------------
-spec to_node_pair(Node :: Node, Side,
		   Mapping) -> Node when
      Mapping :: da_map:da_map(tree:tree_node(),
			       tree:tree_node()),
      Side :: 1 | 2,
      Node :: tree:tree_node().
to_node_pair(Node, 1, Mapping) ->
    case da_map:find_value(Node, Mapping) of
	error -> Node;
	{ok, Value} -> tree:create_node_pair(Node, Value)
    end;
to_node_pair(Node, 2, Mapping) ->
    case da_map:find_key(Node, Mapping) of
	error -> Node;
	{ok, Key} -> tree:create_node_pair(Key, Node)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns a list with the correspondence of types, clusters, and trees.
%% This can be used to find in which cluster and in which tree a node is.
%% @end
%%-------------------------------------------------------------------------
-spec get_type_list(cluster_dicts(), tree_info()) ->
			   [{ClustType, ClusterDict, Tree | na}] when
      ClustType :: comm | {ex, 1} | {ex, 2},
      ClusterDict :: cluster_dict:cluster_dict(tree:tree_node()),
      Tree :: tree:tree().
get_type_list(ClustInfo, TreeInfo) ->
    [{comm, ClustInfo#cluster_dicts.comm_clus, na},
     {{ex, 1}, ClustInfo#cluster_dicts.ex_clus1, TreeInfo#tree_info.tree1},
     {{ex, 2}, ClustInfo#cluster_dicts.ex_clus2, TreeInfo#tree_info.tree2}].

%%-------------------------------------------------------------------------
%% @doc
%% Gets the free variables for the top nodes of the clusters linked to the Name,
%% with type SymType. It first tires to get them from Acc, otherwise it infers
%% it from the nodes themselves.
%% @end
%%-------------------------------------------------------------------------
-spec find_free_vars_for_sym_label(SymType :: 'comm' | 'ex', string(),
				   Name :: cluster_folding:acc()) -> [string()].
find_free_vars_for_sym_label(comm, Name, Acc) ->
    find_free_vars_for_asym_label({comm, Name}, Acc);
find_free_vars_for_sym_label(ex, Name, Acc) ->
    Environment1 = find_free_vars_for_asym_label({{ex, 1}, Name}, Acc),
    Environment2 = find_free_vars_for_asym_label({{ex, 2}, Name}, Acc),
    merge_environments(Environment1, Environment2).

%%-------------------------------------------------------------------------
%% @doc
%% Gets the exported variables for the top nodes of the clusters linked to
%% the Name, with type SymType. It first tires to get them from Acc,
%% otherwise it infers it from the nodes themselves.
%% @end
%%-------------------------------------------------------------------------
-spec find_exported_vars_for_sym_label(SymType :: 'comm' | 'ex', string(),
				       Name :: cluster_folding:acc()) -> [string()].
find_exported_vars_for_sym_label(comm, Name, Acc) ->
    find_exported_vars_for_asym_label({comm, Name}, Acc);
find_exported_vars_for_sym_label(ex, Name, Acc) ->
    Environment1 = find_exported_vars_for_asym_label({{ex, 1}, Name}, Acc),
    Environment2 = find_exported_vars_for_asym_label({{ex, 2}, Name}, Acc),
    merge_environments(Environment1, Environment2).

%%-------------------------------------------------------------------------
%% @doc
%% Gets the free variables for the top node of the cluster linked to AsymLabel,
%% considering both trees if the node is common.
%% It first tries to get them from Acc, otherwise it infers it from the node itself.
%% @end
%%-------------------------------------------------------------------------
-spec find_free_vars_for_asym_label(AsymLabel :: {ClustType, string()},
				    Acc :: cluster_folding:acc()) -> [string()] when
      ClustType :: comm | {ex, 1} | {ex, 2}.
find_free_vars_for_asym_label(AsymLabel, #acc{tree_info = TreeInfo,
					      cluster_info = InfoMap} = Acc) ->
    ClusInfo = tree:dict_get(AsymLabel, InfoMap, #cluster_info{environment_info = na}),
    case ClusInfo#cluster_info.environment_info of
	na -> [];
	none -> Vars = get_free_vars(AsymLabel, get_root_node_from_label(AsymLabel, Acc), TreeInfo),
		Vars;
	#environment_info{free_variables = FreeVars} ->
	    FreeVars
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Gets the exported variables for the top node of the cluster linked to AsymLabel,
%% considering both trees if the node is common.
%% It first tries to get them from Acc, otherwise it infers it from the node itself.
%% @end
%%-------------------------------------------------------------------------
-spec find_exported_vars_for_asym_label(AsymLabel :: {ClustType, string()},
				    Acc :: cluster_folding:acc()) -> [string()] when
      ClustType :: comm | {ex, 1} | {ex, 2}.
find_exported_vars_for_asym_label(AsymLabel, #acc{cluster_info = InfoMap} = Acc) ->
    ClusInfo = tree:dict_get(AsymLabel, InfoMap, #cluster_info{environment_info = na}),
    case ClusInfo#cluster_info.environment_info of
	na -> [];
	none -> get_exported_vars(AsymLabel, get_root_node_from_label(AsymLabel, Acc), Acc);
	#environment_info{export_variables = ExportedVars} -> ExportedVars
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Gets the vars bound in a subtree of the Node that are accessed from
%% outside the subtree of the Node, (afterwards), from the perspective
%% of the output module in which the Node is.
%% @end
%%-------------------------------------------------------------------------
-spec get_exported_vars(AsymLabel :: {ClustType, string()},
			Node :: indirection_cluster | {ok, tree:tree_node()},
			Acc :: cluster_folding:acc()) -> [string()] when
      ClustType :: comm | {ex, 1} | {ex, 2}.
get_exported_vars(_, indirection_cluster, _) -> [];
get_exported_vars({comm, _}, {ok, Node}, #acc{tree_info = #tree_info{tree1 = Tree1, tree2 = Tree2}}) ->
    {Node1, Node2} = tree:get_pair_tuple(Node),
    merge_environments(get_exported_vars_from_tree(Node1, Tree1), get_exported_vars_from_tree(Node2, Tree2));
get_exported_vars({{ex, 1}, _}, {ok, Node}, #acc{tree_info = #tree_info{tree1 = Tree1}}) ->
    get_exported_vars_from_tree(Node, Tree1);
get_exported_vars({{ex, 2}, _}, {ok, Node}, #acc{tree_info = #tree_info{tree2 = Tree2}}) ->
    get_exported_vars_from_tree(Node, Tree2).

%%-------------------------------------------------------------------------
%% @doc
%% Computes the exported vars from a Node and the Tree it is in.
%% @end
%%-------------------------------------------------------------------------
-spec get_exported_vars_from_tree(Node :: tree:tree_node(), Tree :: tree:tree()) -> [string()].
get_exported_vars_from_tree(Node, Tree) ->
    {Vars, _Pos} = lists:unzip(api_refac:exported_vars(ast_tree:tree_to_ast(Node, Tree))),
    lists:map(fun erlang:atom_to_list/1, Vars).

%%-------------------------------------------------------------------------
%% @doc
%% Gets the vars bound in a subtree of the Node that are accessed from
%% outside the subtree of the Node, (afterwards), from the perspective
%% of the target input module number Pos in case it is a node pair.
%% @end
%%-------------------------------------------------------------------------
-spec get_exported_vars_sided(AsymLabel :: {ClustType, string()}, Pos :: 1 | 2,
			      Node :: indirection_cluster | {ok, tree:tree_node()},
			      Acc :: cluster_folding:acc()) -> [string()] when
      ClustType :: comm | {ex, 1} | {ex, 2}.
get_exported_vars_sided(_, _Pos, indirection_cluster, _) -> [];
get_exported_vars_sided({comm, _}, Pos, {ok, Node}, #acc{tree_info = #tree_info{tree1 = Tree1, tree2 = Tree2}}) ->
    {Node1, Node2} = tree:get_pair_tuple(Node),
    case Pos of
	1 -> get_exported_vars_from_tree(Node1, Tree1);
	2 -> get_exported_vars_from_tree(Node2, Tree2)
    end;
get_exported_vars_sided({{ex, 1}, _}, _Pos, {ok, Node}, #acc{tree_info = #tree_info{tree1 = Tree1}}) ->
    get_exported_vars_from_tree(Node, Tree1);
get_exported_vars_sided({{ex, 2}, _}, _Pos, {ok, Node}, #acc{tree_info = #tree_info{tree2 = Tree2}}) ->
    get_exported_vars_from_tree(Node, Tree2).

%%-------------------------------------------------------------------------
%% @doc
%% Gets the free variables for the top node of the cluster linked to AsymLabel,
%% considering only the tree specified by Pos.
%% It first tries to get them from Acc, otherwise it infers it from the node itself.
%% @end
%%-------------------------------------------------------------------------
-spec find_actual_free_vars_for_asym_label(AsymLabel :: {ClustType, string()}, Pos :: 1 | 2,
					   Acc :: cluster_folding:acc()) -> [string()] when
      ClustType :: comm | {ex, 1} | {ex, 2}.
find_actual_free_vars_for_asym_label(AsymLabel, Pos, #acc{tree_info = TreeInfo,
							  cluster_info = InfoMap} = Acc) ->
    ClusInfo = tree:dict_get(AsymLabel, InfoMap, #cluster_info{environment_info = na}),
    case {ClusInfo#cluster_info.environment_info, Pos} of
	{na,_} -> [];
	{none,_} -> get_free_vars_sided(AsymLabel, Pos,
					get_root_node_from_label(AsymLabel, Acc),
					TreeInfo);
	{#environment_info{free_left = LeftVars}, 1} -> LeftVars;
	{#environment_info{free_right = RightVars}, 2} -> RightVars
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Gets the exported variables for the top node of the cluster linked to AsymLabel,
%% considering only the tree specified by Pos.
%% It first tries to get them from Acc, otherwise it infers it from the node itself.
%% @end
%%-------------------------------------------------------------------------
-spec find_actual_exported_vars_for_asym_label(AsymLabel :: {ClustType, string()}, Pos :: 1 | 2,
					   Acc :: cluster_folding:acc()) -> [string()] when
      ClustType :: comm | {ex, 1} | {ex, 2}.
find_actual_exported_vars_for_asym_label(AsymLabel, Pos, #acc{cluster_info = InfoMap} = Acc) ->
    ClusInfo = tree:dict_get(AsymLabel, InfoMap, #cluster_info{environment_info = na}),
    case {ClusInfo#cluster_info.environment_info, Pos} of
	{na,_} -> [];
	{none,_} -> get_exported_vars_sided(AsymLabel, Pos, get_root_node_from_label(AsymLabel, Acc), Acc);
	{#environment_info{export_left = LeftVars}, 1} -> LeftVars;
	{#environment_info{export_right = RightVars}, 2} -> RightVars
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Obtains information about whether the cluster is a leaf or not. If the
%% information is no there it defaults to false.
%% @end
%%-------------------------------------------------------------------------
-spec get_leaf_as_is_or_default(AsymLabel :: {ClustType, string()}, Pos :: 1 | 2,
				Acc :: cluster_folding:acc()) -> boolean() when
      ClustType :: comm | {ex, 1} | {ex, 2}.
get_leaf_as_is_or_default(AsymLabel, Pos, #acc{cluster_info = InfoMap}) ->
    ClusInfo = tree:dict_get(AsymLabel, InfoMap, #cluster_info{environment_info = na}),
    case {ClusInfo#cluster_info.environment_info, Pos} of
	{na, _} -> false;
	{none, _} -> false;
	{#environment_info{is_left_leaf = LeftLeaf}, 1} -> LeftLeaf;
	{#environment_info{is_right_leaf = RightLeaf}, 2} -> RightLeaf
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Finds the root of the cluster that is linked to a label.
%% @end
%%-------------------------------------------------------------------------
-spec get_root_node_from_label({ClustType, string()},
                               cluster_folding:acc()) -> 
				      indirection_cluster | {ok, tree:tree_node()} when
      ClustType :: comm  | {ex, 1}  | {ex, 2}.
get_root_node_from_label(Label, Acc) ->
    Cluster = get_cluster_from_label(Label, Acc),
    case cluster:is_indirection_cluster(Cluster) of
	{true, _} -> indirection_cluster;
	false -> {ok, cluster:get_root(Cluster)}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Finds the cluster that is linked to a label.
%% @end
%%-------------------------------------------------------------------------
-spec get_cluster_from_label({ClustType, string()}, Acc :: cluster_folding:acc()) ->
 				    cluster:cluster(Node) when
      ClustType :: comm | {ex, 1} | {ex, 2},
      Node :: tree:tree_node().
get_cluster_from_label(Label, #acc{cluster_labels = LabelMap}) ->
    {ok, Cluster} = da_map:find_value(Label, LabelMap),
    Cluster.

%%-------------------------------------------------------------------------
%% @doc
%% Merges the list of free vars of two nodes. It removes the repeated
%% ones.
%% @end
%%-------------------------------------------------------------------------
-spec merge_environments([string()], [string()]) -> [string()].
merge_environments(Env1, Env2) -> nub(Env1 ++ Env2).

%%-------------------------------------------------------------------------
%% @doc
%% Returns the same List but without duplicates. Keeps the order of the
%% first occurrence of each element.
%% @end
%%-------------------------------------------------------------------------
-spec nub(List :: [Element]) -> [Element].
nub(List) -> nub(List, sets:new()).

%%-------------------------------------------------------------------------
%% @doc
%% Auxiliar function to {@link nub/1}. It returns the same List but without
%% duplicates and removing the elements of the set Repated.
%% @end
%%-------------------------------------------------------------------------
-spec nub(List :: [Element], Repeated :: sets:set(Element)) -> [Element].
nub([H|T], Set) ->
    case sets:is_element(H, Set) of
	true -> nub(T, Set);
	false -> [H|nub(T, sets:add_element(H, Set))]
    end;
nub([], _) -> [].

%%-------------------------------------------------------------------------
%% @doc
%% Returns the list of free vars of the node or the arg names
%% if the Node is a function definition. This is used
%% to generate the function header when creating new
%% functions.
%% @end
%%-------------------------------------------------------------------------
-spec get_free_vars(AsymLabel :: {ClustType, string()},
		    {ok, Node :: tree:tree_node()} | indirection_cluster,
		    TreeInfo) -> [string()] when
      ClustType :: comm | {ex, 1} | {ex, 2},
      TreeInfo :: tree_info().
get_free_vars(_AsymLabel, indirection_cluster, _TreeInfo) -> [];
get_free_vars(AsymLabel, {ok, TreeNode}, TreeInfo) ->
    case ast_tree:is_a_function(TreeNode) of
	false -> get_free_vars_normal_node(TreeNode);
	true -> get_arg_names(AsymLabel, TreeNode, TreeInfo)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns the list of free variables of the node.
%% @end
%%-------------------------------------------------------------------------
-spec get_free_vars_normal_node(Node :: tree:tree_node()) -> [string()].
get_free_vars_normal_node(TreeNode) ->
    case tree:is_node_pair(TreeNode) of
	true -> {TreeNode1, TreeNode2} = tree:get_pair_tuple(TreeNode),
		merge_environments(
		  lists:map(fun atom_to_list/1, api_refac:free_var_names(tree:get_value(TreeNode1))),
		  lists:map(fun atom_to_list/1, api_refac:free_var_names(tree:get_value(TreeNode2))));
	false -> lists:map(fun atom_to_list/1, api_refac:free_var_names(tree:get_value(TreeNode)))
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Obtains the list of argument names for the function node TreeNode,
%% from the dictionary ArgNameDict. It assumes that TreeNode is a
%% function node.
%%-------------------------------------------------------------------------
-spec get_arg_names(AsymLabel :: {ClustType, string()},
		    TreeNode :: tree:tree_node(),
		    TreeInfo :: cluster_folding:tree_info()) ->
			   [string()] when
      ClustType :: comm | {ex, 1} | {ex, 2}.
get_arg_names({ex, 2}, TreeNode, #tree_info{tree2 = Tree,
					    tree2_args = Dict}) ->
    get_arg_names_aux(TreeNode, Tree, Dict);
get_arg_names(_, TreeNode, #tree_info{tree1 = Tree,
				      tree1_args = Dict}) ->
    get_arg_names_aux(TreeNode, Tree, Dict).

%%-------------------------------------------------------------------------
%% @doc
%% Obtains the list of argument names for the function node TreeNode,
%% from the dictionary ArgNameDict. It assumes that TreeNode is a
%% function node.
%% @end
%%-------------------------------------------------------------------------
-spec get_arg_names_aux(TreeNode :: tree:tree_node(), Tree :: tree:tree(),
			ArgNameDict :: dict:dict({Name :: atom(),
						  Arity :: integer()},
						 [string()])) -> [string()].
get_arg_names_aux(TreeNode, Tree, Dict) ->
    {ok, NameArity} = ast_tree:get_fun_name(TreeNode, Tree),
    dict:fetch(NameArity, Dict).

%%-------------------------------------------------------------------------
%% @doc
%% Returns the list of free vars of the node. This is
%% used to generate the function header when creating new
%% functions.
%% @end
%%-------------------------------------------------------------------------
-spec get_free_vars_sided(AsymLabel :: {ClustType, string()},
			  Pos :: 1 | 2,
			  {ok, Node :: tree:tree_node()}
			  | indirection_cluster, TreeInfo) -> [string()] when
      ClustType :: comm | {ex, 1} | {ex, 2},
      TreeInfo :: tree_info().
get_free_vars_sided(_AsymLabel, _, indirection_cluster, _TreeInfo) -> [];
get_free_vars_sided(AsymLabel, Pos, {ok, TreeNode}, TreeInfo) ->
    case tree:is_node_pair(TreeNode) of
	true -> {TreeNode1, TreeNode2} = tree:get_pair_tuple(TreeNode),
		case Pos of
		    1 -> get_free_vars_sided_single_node({ex, 1}, TreeNode, TreeInfo, TreeNode1);
                    2 -> get_free_vars_sided_single_node({ex, 2}, TreeNode, TreeInfo, TreeNode2)
		end;
	false -> get_free_vars_sided_single_node(AsymLabel, TreeNode, TreeInfo, TreeNode)
    end.


%%-------------------------------------------------------------------------
%% @doc
%% Auxiliary function to {@link get_free_vars_sided/4}. It returns the
%% free vars of the SingleNode, where NodePair is the node pair that
%% contains SingleNode, and SingleNode one of its alternatives.
%% @end
%%-------------------------------------------------------------------------
-spec get_free_vars_sided_single_node(AsymLabel :: {ClustType, string()},
				      NodePair :: tree:tree_node(), TreeInfo,
				      SingleNode :: tree:tree_node()) -> [string()] when
      ClustType :: comm | {ex, 1} | {ex, 2},
      TreeInfo :: tree_info().
get_free_vars_sided_single_node(AsymLabel,TreeNode,TreeInfo,TreeNode1) ->
    case ast_tree:is_a_function(TreeNode) of
	false -> lists:map(fun atom_to_list/1, api_refac:free_var_names(tree:get_value(TreeNode1)));
	true -> get_arg_names(AsymLabel, TreeNode1, TreeInfo)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns the symmetric cluster type. That is, removes the information
%% about which exclusive cluster it is.
%% @end
%%-------------------------------------------------------------------------
-spec isolate_type_class(ClustType) -> SymmetricType when
      ClustType :: comm | {ex, 1} | {ex, 2},
      SymmetricType :: comm | ex.
isolate_type_class(comm) -> comm;
isolate_type_class({ex, _}) -> ex.

%%-------------------------------------------------------------------------
%% @doc
%% Returns true if the function specified (Name and Arity) is in
%% both of sets of inscope functions (Inscope1 and Inscope2), or
%% false if it is in none. This implementation does not contemplate
%% that it may be in scope for one and not inscope for another, since
%% that would cause wrong behaviour in the common module.
%% @end
%%-------------------------------------------------------------------------
-spec is_in_scope(Name, Arity, sets:set({Name, Arity}), sets:set({Name, Arity})) -> boolean().
is_in_scope(Name, Arity, Inscope1, Inscope2) ->
    case {is_in_scope_modinfo(Name, Arity, Inscope1),
	  is_in_scope_modinfo(Name, Arity, Inscope2)} of
	{true, true} -> true;
	{false, false} -> false;
	_ -> exit({error, {ambiguous_function, {Name, Arity}, not_implemented}})
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns true if the function specified (Name and Arity) is in
%% the set of Inscope functions
%% @end
%%-------------------------------------------------------------------------
-spec is_in_scope_modinfo(Name, Arity, sets:set({Name, Arity})) -> boolean().
is_in_scope_modinfo(Name, Arity, Inscope) ->
    sets:is_element({Name, Arity}, Inscope).
