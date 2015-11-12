%%-------------------------------------------------------------------------
%% Record for use as accumulator when folding through cluster borders
%%-------------------------------------------------------------------------

-record(acc,
	{
	  ph_map,
	  cluster_order,
	  cluster_labels,
	  cluster_funname,
	  cluster_orifunname,
	  cluster_info,
	  cluster_dicts,
	  allocated_parents,
	  tree_info,
	  exp_funs,
	  counters,
	  out_module,
	  var_for_module,
	  root_for_new_exclusive_funnames,
	  root_for_new_common_funnames
	}).

%%-------------------------------------------------------------------------
%% Record for keeping the updated cluster dictionaries
%%-------------------------------------------------------------------------

-record(cluster_dicts,
	{
	  comm_clus,
	  ex_clus1,
	  ex_clus2
	}).


%%-------------------------------------------------------------------------
%% Record for keeping information about one single cluster
%%-------------------------------------------------------------------------

-record(cluster_info,
	{
	  cluster_type,
	  environment_info
	}).

%%-------------------------------------------------------------------------
%% Record for keeping the tree and mapping information
%%-------------------------------------------------------------------------

-record(tree_info,
	{
	  tree1,
	  tree2,
	  tree1_args,
	  tree2_args,
	  mapping
	}).


%%-------------------------------------------------------------------------
%% Record that keeps counters used for naming functions
%%-------------------------------------------------------------------------

-record(cluster_counters,
	{
	  invisible_counter,
	  common_counter,
	  common_pos_counter,
	  exclusive_counter,
	  exclusive_pos_counter_1,
	  exclusive_pos_counter_2
	}).




%%-------------------------------------------------------------------------
%% Contains environment information like the free
%% and bound variables at the root of a cluster
%%-------------------------------------------------------------------------

-record(environment_info,
	{
	  free_variables,
	  free_left,
	  free_right,
	  export_variables,
	  export_left,
	  export_right,
	  is_left_leaf,
	  is_right_leaf
	}).

%%-------------------------------------------------------------------------
%% Record for use as accumulator when mapping clusters.
%% It collects global information about the generated modules.
%%-------------------------------------------------------------------------

-record(col_data, {beh_funcs, module_info1, module_info2,
		   inscope_funs1, inscope_funs2, var_for_module}).
