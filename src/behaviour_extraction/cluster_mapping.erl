%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Implements the functionality that composes the clusters into
%%% three trees. One with the common clusters, two with each set of
%%% exclusive clusters. It also creates the AST joints, (calls to
%%% functions), which link the clusters together in the resulting
%%% modules.
%%% @end
%%% Created : 22 Jul 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(cluster_mapping).

-export([clusters_to_ast/2]).

-include("cluster_records.hrl").

-export_type([col_data/0]).

-type col_data() :: #col_data{
		       beh_funcs :: sets:set({atom(), integer()}),
		       module_info1 :: sets:set({atom(), integer()}),
		       module_info2 :: sets:set({atom(), integer()}),
		       inscope_funs1 :: [{atom(), atom(), integer()}],
		       inscope_funs2 :: [{atom(), atom(), integer()}],
		       var_for_module :: string()
		      }. %% Record for use as accumulator when mapping clusters.
                         %% It collects global information about the generated
                         %% modules.
                         %% Contains the following fields: <ul>
                         %% <li> `beh_funcs' - contains the list of functions
                         %%                    that form the behaviour and must
                         %%                    be exported by instances </li>
                         %% <li> `module_info1' - contains the information returned
                         %%                       by the parser for the left tree </li>
                         %% <li> `module_info2' - contains the information returned
                         %%                       by the parser for the right tree </li>
                         %% <li> `inscope_funs1' - contains a list with the functions
                         %%                        that do not need qualified calls from
                         %%                        the left module {Mod, Fun, Arity}</li>
                         %% <li> `inscope_funs2' - contains a list with the functions
                         %%                        that do not need qualified calls from
                         %%                        the right module {Mod, Fun, Arity}</li>
                         %% <li> `var_for_module' - name for the var that contains the 
                         %%                         module instance name, (guaranteed to
                         %%                         be unique for both modules) </li></ul>

%%-------------------------------------------------------------------------
%% @doc
%% It generates three ASTs, one for each cluster dictionary.
%% @end
%%-------------------------------------------------------------------------
-spec clusters_to_ast(cluster_folding:acc(), atom()) ->
			     [wrangler_syntax:syntaxTree()].
clusters_to_ast(#acc{cluster_dicts = ClusterDicts,
		     tree_info = #tree_info{tree1 = Tree1,
					    tree2 = Tree2} = TreeInfo,
		     var_for_module = VarModule} = Acc,
		OutModule) ->
    NewAcc = Acc#acc{out_module = OutModule},
    {ASTs, _} = lists:mapfoldl(fun (X, InColData) ->
				       cluster_dict_to_ast(X, NewAcc, InColData)
			       end,
			       #col_data{beh_funcs = sets:new(),
					 module_info1 = ast_tree:get_module_info(Tree1),
					 module_info2 = ast_tree:get_module_info(Tree2),
					 inscope_funs1 = ast_tree:get_inscope_funs(Tree1),
					 inscope_funs2 = ast_tree:get_inscope_funs(Tree2),
					 var_for_module = VarModule
					},
			       cluster_folding:get_type_list(ClusterDicts, TreeInfo)),
    copy_used_imports_records_and_macros(ASTs, Tree1, Tree2).

%%-------------------------------------------------------------------------
%% @doc
%% Takes three ASTs (Comm, Ex1, and Ex2), and adds to Comm the macros
%% from Ex1 and Ex2 that are needed.
%% @end
%%-------------------------------------------------------------------------
-spec copy_used_imports_records_and_macros(ASTs :: [wrangler_syntax:syntaxTree()],
					   Tree1 :: tree:tree(), Tree2 :: tree:tree()) ->
						  [ClusterDict] when
      ClusterDict :: cluster_dict:cluster_dict(tree:tree_node()).
copy_used_imports_records_and_macros([Comm, Ex1, Ex2], Tree1, Tree2) ->
    Macros1 = ast_tree:get_macros_list(Tree1),
    Macros2 = ast_tree:get_macros_list(Tree2),
    Macros = merge_macros(Macros1, Macros2),
    NewComm = add_attr_deps(Comm, Macros),
    [NewComm, Ex1, Ex2].

%%-------------------------------------------------------------------------
%% @doc
%% Combines in a single list Macro1 and Macro2 by removing from
%% Macro2 the tuples whose second element is already the second
%% element of a tuple in Macro1.
%% @end
%%-------------------------------------------------------------------------
-spec merge_macros(Macros1 :: [{Macro1AST :: wrangler_syntax:syntaxTree(),
				Macro1Info :: [term()]}],
		   Macros2 :: [{Macro2AST :: wrangler_syntax:syntaxTree(),
				Macro2Info :: [term()]}]) ->
			  [{MacroRAST :: wrangler_syntax:syntaxTree(),
			    MacroRInfo :: [term()]}].
merge_macros(Macros1, Macros2) ->
    IdxMacros1 = index_macros(Macros1),
    Macros1 ++ lists:filter(is_not_in(IdxMacros1), Macros2).

%%-------------------------------------------------------------------------
%% @doc
%% Takes a list of tuples with two elements and adds the second
%% element of each to a set.
%% @end
%%-------------------------------------------------------------------------
-spec index_macros(Macros :: [{MacroAST :: wrangler_syntax:syntaxTree(),
			       MacroInfo :: [term()]}]) ->
			  sets:set(MacroInfo :: [term()]).
index_macros(Macros) ->
    index_macros(Macros, sets:new()).

%%-------------------------------------------------------------------------
%% @doc
%% Takes a list of tuples with two elements and adds the second
%% element of each to the set Acc0.
%% @end
%%-------------------------------------------------------------------------
-spec index_macros(Macros :: [{MacroAST :: wrangler_syntax:syntaxTree(),
			       MacroInfo :: [term()]}],
		   Acc0 :: sets:set(MacroInfo :: [term()])) ->
			  Acc1 :: sets:set(MacroInfo :: [term()]).
index_macros([], Set) -> Set;
index_macros([{_AST, ProvidedList}|T], Set) ->
    index_macros(T, sets:add_element(ProvidedList, Set)).

%%-------------------------------------------------------------------------
%% @doc
%% Returns a function that takes a tuple with two elements and
%% returns true if the second element of the tuple is not in Set.
%% @end
%%-------------------------------------------------------------------------
-spec is_not_in(Set :: sets:set(ElType)) ->
		       fun(({term(), El :: ElType}) -> boolean()).
is_not_in(Idx) ->
    fun ({_AST, Deps}) ->
	    is_not_in(Deps, Idx)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns true if El is not in Set.
%% @end
%%-------------------------------------------------------------------------
-spec is_not_in(El :: ElType, Set :: sets:set(ElType)) -> boolean().
is_not_in(Idx, DepList) ->
    not sets:is_element(Idx, DepList).

%%-------------------------------------------------------------------------
%% @doc
%% Accumulates in NeededMacrosASTs0 the ASTs of the Macros
%% that help satisfy dependencies in Tree, (recursively).
%% @end
%%-------------------------------------------------------------------------
-spec add_attr_deps(Tree :: wrangler_syntax:syntaxTree(),
		    Macros :: [{MacroAST :: wrangler_syntax:syntaxTree(),
				MacroInfo :: [term()]}]) ->
			   UpdatedTree :: [ASTs :: wrangler_syntax:syntaxTree()].
add_attr_deps(Tree, Macros) ->
    Needed = add_attr_deps([], [Tree], Macros),
    add_attribs(Needed, Tree).

%%-------------------------------------------------------------------------
%% @doc
%% Accumulates in NeededMacrosASTs0 the ASTs of the Macros
%% that help satisfy dependencies in ASTList, (recursively).
%% @end
%%-------------------------------------------------------------------------
-spec add_attr_deps(NeededMacroASTs0 :: [NeededMacroAST0 :: wrangler_syntax:syntaxTree()],
		    ASTList :: [ASTs :: wrangler_syntax:syntaxTree()],
		    Macros :: [{MacroAST :: wrangler_syntax:syntaxTree(),
				MacroInfo :: [term()]}]) ->
			   NeededMacroASTs1 :: [NeededMacroAST1 :: wrangler_syntax:syntaxTree()].
add_attr_deps(NeededList, List, Macros) ->
    Used = get_used_refs(List),
    {Needed, Rest} = get_deps(Used, Macros),
    case Needed of
	[] -> NeededList;
	_ -> add_attr_deps([lists:reverse(Needed)|NeededList], Needed, Rest)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Gets a list with ordered sets of non qualified function references,
%% used macros with arity, and used records in the ASTs of the ASTList.
%% @end
%%-------------------------------------------------------------------------
-spec get_used_refs(ASTList :: [AST :: wrangler_syntax:syntaxTree()]) ->
			   [ordsets:ordset(any())].
get_used_refs(ASTList) ->
    AST = wrangler_syntax:form_list(lists:flatten(ASTList)),
    [ordsets:from_list(wrangler_misc:collect_non_qualified_fun_refs(AST)),
     ordsets:from_list(wrangler_misc:collect_used_macros_with_arity(AST)),
     ordsets:from_list(wrangler_misc:collect_used_records(AST))].

%%-------------------------------------------------------------------------
%% @doc
%% Partitions the list of Macros in the ones that that
%% provide anything on the Used list and the ones that not,
%% for the first ones only the AST is returned.
%% @end
%%-------------------------------------------------------------------------
-spec get_deps(Used :: ordsets:ordset([term()]),
	       Macros :: [{MacroAST :: wrangler_syntax:syntaxTree(),
			   MacroInfo :: [term()]}]) ->
		      {[MacrosComply :: wrangler_syntax:syntaxTree()],
		       [MacrosDont :: {MacroDAST :: wrangler_syntax:syntaxTree(),
				       MacroDInfo :: [term()]}]}.
get_deps(Used, Macros) ->
    {Comply, Dont} = lists:partition(provides(Used), Macros),
    {ASTs, _} = lists:unzip(Comply),
    {ASTs, Dont}.

%%-------------------------------------------------------------------------
%% @doc
%% Returns a function that returns true if Provided contains any
%% element in common with Used.
%% @end
%%-------------------------------------------------------------------------
-spec provides(Used :: ordsets:ordset([El])) ->
		      fun(({term(), Provided :: ordsets:ordset([El])}) -> boolean()).
provides(Used) ->
    fun ({_, Provided}) ->
	    lists:any(fun non_empty_intersection/1, lists:zip(Used, Provided))
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns true if Set1 and Set2 are not disjoint.
%% @end
%%-------------------------------------------------------------------------
-spec non_empty_intersection({Set1 :: [El], Set2 :: [El]}) -> boolean().
non_empty_intersection({Set1, Set2}) ->
    not ordsets:is_disjoint(Set1, Set2).

%%-------------------------------------------------------------------------
%% @doc
%% Adds the List of attributes to the AST, at the right position.
%% @end
%%-------------------------------------------------------------------------
-spec add_attribs(NestedListOfASTs :: DeepList, wrangler_syntax:syntaxTree()) ->
			 [wrangler_syntax:syntaxTree()] when
      DeepList :: [wrangler_syntax:syntaxTree() | DeepList].
add_attribs(List, AST) ->
    lists:foldl(swap(fun api_refac:insert_an_attr/2), AST, lists:flatten(List)).

%%-------------------------------------------------------------------------
%% @doc
%% Swaps the arguments of Fun.
%% @end
%%-------------------------------------------------------------------------
-spec swap(fun((A, B) -> C)) -> fun((B,A) -> C).
swap(Fun) -> fun (X, Y) -> Fun(Y, X) end.

%%-------------------------------------------------------------------------
%% @doc
%% Creates an AST for the cluster dictionary described by the tuple ClusDictInfo.
%% ClusDictInfo is expected to be an element of the list produced by calling
%% {@link cluster_folding:get_type_list/2}
%% @end
%%-------------------------------------------------------------------------
-spec cluster_dict_to_ast(ClusDictInfo :: {ClustType, ClusterDict, Tree | na},
			  cluster_folding:acc(), col_data()) ->
				 wrangler_syntax:syntaxTree() when
      ClustType :: comm | {ex, 1 | 2} | ex,
      ClusterDict :: cluster_dict:cluster_dict(tree:tree_node()),
      Tree :: tree:tree().
cluster_dict_to_ast({ClustType, _ClusterDict, Tree}, Acc, ColData) ->
    {OrderedASTs, NewColData} = lists:mapfoldl(fun ({Pos, Clus}, InColData) ->
						       cluster_to_ast(ClustType, Pos, Clus,
								      Tree, Acc, InColData)
					       end, ColData,
					       get_clusters_in_order(ClustType, Acc)),
    case {ClustType, OrderedASTs} of
	{comm, []} -> throw({error, "Could not find any commonalities"});
	{{ex, _N}, []} -> throw({error, "Cannot render exclusive cluster to AST"});
	{{ex, _}, [First|Rest]} -> {insert_in_main_tree(First, Rest, Acc, NewColData), NewColData};
	{comm, All} -> {add_skeleton(All, Acc, NewColData), NewColData}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Creates an AST of the cluster Cluster starting with the root node.
%% It adds a function declaration header when appropriate.
%% @end
%%-------------------------------------------------------------------------
-spec cluster_to_ast(ClustType, Level :: pos_integer(), Label, tree:tree(),
		     Acc :: cluster_folding:acc(), col_data()) ->
			    {wrangler_syntax:syntaxTree(), col_data()} when
      Label :: {ClustType, string()},
      ClustType :: comm | {ex, 1} | {ex, 2}.
cluster_to_ast({ex, _} = Type, 1, Label, Tree, Acc, ColData) ->
    Clust = fetch_cluster_for_label(Label, Acc),
    cluster_to_ast_aux(Type, Clust, Tree, Acc, ColData);
cluster_to_ast(Type, _N, Label, Tree, Acc, ColData) ->
    Clust = fetch_cluster_for_label(Label, Acc),
    IsLeaf = cluster_folding:get_leaf_as_is_or_default(Label, extract_pos(Type), Acc),
    expand_or_generate_call(cluster:is_indirection_cluster(Clust), Type,
			    Label, ColData, Acc, Tree, Clust, IsLeaf).

%%-------------------------------------------------------------------------
%% @doc
%% If PH is false assume it is an indirection cluster and generate
%% a call, otherwise, expand the PH to an AST.
%% @end
%%-------------------------------------------------------------------------
-spec expand_or_generate_call(PH :: 'false' | {'true', tree:tree_ph()},
			      ClustType, Label, ColData :: col_data(),
			      Acc :: cluster_folding:acc(),
			      Tree :: tree:tree() | na,
			      Cluster :: cluster:cluster(tree:tree_node()),
			      IsLeaf :: boolean()) ->
				     {wrangler_syntax:syntaxTree(), col_data()} when
      Label :: {ClustType, string()},
      ClustType :: comm | {ex, 1} | {ex, 2}.
expand_or_generate_call(false, Type, Label, ColData, Acc, Tree, Clust, IsLeaf) ->
     {ResultTree, NewColData} = cluster_to_ast_aux(Type, Clust, Tree, Acc, ColData),
     fix_function(Type, Label, Acc, IsLeaf, ResultTree, NewColData);
expand_or_generate_call({true, PH}, Type, Label, ColData, Acc, _Tree, _Clust, _IsLeaf) ->
     {Call, NewColData} = generate_call_from_ph(Type, PH, Acc, ColData),
     {add_header(Label, [Call], Acc, false), NewColData}.

%%-------------------------------------------------------------------------
%% @doc
%% Removes artificial blocks, modifies the name of the function header,
%% adds a function header, and redirects local calls that where moved
%% to a different module where appropriate.
%% @end
%%-------------------------------------------------------------------------
-spec fix_function(ClustType, Label, Acc :: cluster_folding:acc(), IsLeaf :: boolean(),
		   ResultTree :: wrangler_syntax:syntaxTree(), ColData :: col_data()) ->
			  {wrangler_syntax:syntaxTree(), col_data()} when
      Label :: {ClustType, string()},
      ClustType :: comm | {ex, 1} | {ex, 2}.
fix_function(Type, {_, Name} = Label, Acc, IsLeaf, ResultTree, NewColData) ->
    case has_function_declaration(ResultTree) of
	true -> NewResultTree = set_function_name(cluster_linking:get_actual_name(Name, Acc),
						  ResultTree),
		local_fix:fix_localities(IsLeaf orelse (Type =/= comm),
					 NewResultTree, true, NewColData);
	false -> WithoutBlock = remove_block(ResultTree),
		 local_fix:fix_localities(IsLeaf orelse (Type =/= comm),
					  add_header(Label, WithoutBlock, Acc, false),
					  false, NewColData)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Changes the name of a function declaration while
%% keeping the attributes.
%% @end
%%-------------------------------------------------------------------------
-spec set_function_name(Name :: string() | atom(),
			Function :: wrangler_syntax:syntaxTree()) -> any().
set_function_name(Name, Result) ->
    wrangler_syntax:copy_attrs(
      Result, wrangler_syntax:function(
		wrangler_syntax:atom(Name),
		wrangler_syntax:function_clauses(Result))).

%%-------------------------------------------------------------------------
%% @doc
%% If passed a block expression returns the children, otherwise
%% returns the same as the only element in a list.
%% @end
%%-------------------------------------------------------------------------
-spec remove_block(wrangler_syntax:syntaxTree()) ->
			  [wrangler_syntax:syntaxTree()].
remove_block(Node) ->
    case has_block(Node) of
	true -> get_children(Node);
	false -> [Node]
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns whether the AST node is a block expression.
%% @end
%%-------------------------------------------------------------------------
-spec has_block(wrangler_syntax:syntaxTree()) -> boolean().
has_block(Node) ->
    wrangler_syntax:type(Node) =:= block_expr.

%%-------------------------------------------------------------------------
%% @doc
%% Returns the children of an AST node.
%% @end
%%-------------------------------------------------------------------------
-spec get_children(Node :: wrangler_syntax:syntaxTree()) ->
			  [wrangler_syntax:syntaxTree()].
get_children(Node) ->
    lists:reverse(api_ast_traverse:fold_subtrees(fun (SubTree, Acc) -> [SubTree|Acc] end, [], Node)).

%%-------------------------------------------------------------------------
%% @doc
%% Creates an AST of the cluster Cluster starting with the root node.
%% It does not add a function declaration header.
%% @end
%%-------------------------------------------------------------------------
-spec cluster_to_ast_aux(ClustType, Cluster, tree:tree(),
			 cluster_folding:acc(), col_data()) ->
				{wrangler_syntax:syntaxTree(), col_data()} when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
cluster_to_ast_aux(Type, Clust, Tree, Acc, ColData) ->
    Node = cluster:get_root(Clust),
    expand_node_in_cluster(Node, Type, Clust, Tree, Acc, ColData).

%%-------------------------------------------------------------------------
%% @doc
%% Creates an AST of the cluster Clust starting with node Node.
%% @end
%%-------------------------------------------------------------------------
-spec expand_node_in_cluster(tree:tree_node(), ClustType, Cluster, tree:tree(),
			     cluster_folding:acc(), col_data()) ->
				    {wrangler_syntax:syntaxTree(), col_data()} when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
expand_node_in_cluster(Node, Type, Clust, Tree, #acc{tree_info = TreeInfo} = Acc, ColData) ->
    ChildrenPHs = cluster_folding:get_children_from_node_list(Type, Clust, Tree, TreeInfo, [Node]),
    {Results, NewColData} = lists:mapfoldl(fun ({PH, Children}, InColData) ->
						   node_or_call(Children, PH, Type, Clust, Tree, Acc,
								is_form_list(Node), InColData) end,
					   ColData, ChildrenPHs),
    {ast_tree:replace_in_node(Results, tree:get_value(ast_tree:get_left_if_node_pair(Node))), NewColData}.

%%-------------------------------------------------------------------------
%% @doc
%% Returns true if Node is a form_list (but not a node_pair).
%% @end
%%-------------------------------------------------------------------------
-spec is_form_list(TreeNode :: tree:tree_node()) -> boolean().
is_form_list(Node) ->
    (not tree:is_node_pair(Node))
	andalso (wrangler_syntax:type(tree:get_value(Node)) =:= form_list).

%%-------------------------------------------------------------------------
%% @doc
%% If Node belongs to the cluster Clust, it will be expanded and returned,
%% otherwise, a call to the appropriate function will be returned instead.
%% @end
%%-------------------------------------------------------------------------
-spec node_or_call(nonempty_list(tree:tree_node()), term(), ClustType, Cluster, tree:tree(),
		   cluster_folding:acc(), IsFormList :: boolean(), col_data()) ->
			  {wrangler_syntax:syntaxTree(), col_data()} when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
node_or_call([Node|_], PH, Type, Clust, Tree,
	     #acc{ph_map = PHMap} = Acc, IsFormList, ColData) ->
    case cluster:has_node(Node, Clust) of
	true -> expand_node_in_cluster(Node, Type, Clust, Tree, Acc, ColData);
	false -> {ok, Label} = dict:find(PH, PHMap),
		 {Call, NewColData} = generate_call_from_ph(Type, PH, Acc, ColData),
		 {case IsFormList of
		      false -> Call;
		      true -> add_header(Label, [Call], Acc, true)
		  end, NewColData}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Generates a call to the appropriate function for the placeholder.
%% Information about the call to generate is read from the Acc.
%% @end
%%-------------------------------------------------------------------------
-spec generate_call_from_ph(ClustType, term(), cluster_folding:acc(),
			    col_data()) ->
				   {wrangler_syntax:syntaxTree(), col_data()} when
      ClustType :: comm | {ex, 1} | {ex, 2}.
generate_call_from_ph(Type, PH, #acc{ph_map = PHMap,
				     out_module = OutModule,
				     var_for_module = ModuleVar} = Acc, ColData) ->
    {ok, {DestType, Name}} = dict:find(PH, PHMap),
    Vars = calculate_vars(Type, DestType, Name, Acc),
    ExportedVars = calculate_vars_exported(Type, DestType, Name, Acc),
    NewColData = collect_call(Type, DestType, Name, length(Vars), ColData),
    {wrap_with_match_expression(ExportedVars,
				generate_call(DestType,
					      cluster_linking:get_actual_name(Name, Acc),
					      Vars,OutModule,ModuleVar)),
     NewColData}.

%%-------------------------------------------------------------------------
%% @doc
%% Adds the FunName and Arity to the list of callbacks
%% of the behaviour, (if it is a comm to ex call).
%% @end
%%-------------------------------------------------------------------------
-spec collect_call(Type :: ClusterType, DestType :: ClusterType,
		   FunName :: string(), Arity :: non_neg_integer(),
		   ColData :: col_data()) -> any() when
      ClusterType :: comm | {ex, 1} | {ex, 2}.
collect_call(Type, DestType, FunName, Arity, ColData) ->
    case {Type, DestType} of
	{comm, ex} -> local_fix:add_fun_to_col(ColData, FunName, Arity);
	_ -> ColData
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Wraps a call with a match expression (to receive the exported vars).
%% @end
%%-------------------------------------------------------------------------
-spec wrap_with_match_expression(
	ExportedVars :: [Variable :: wrangler_syntax:syntaxTree()],
	Call :: wrangler_syntax:syntaxTree()) ->
					wrangler_syntax:syntaxTree().
wrap_with_match_expression([], Call) -> Call;
wrap_with_match_expression(ExportedVars, Call) ->
    wrangler_syntax:match_expr(
      wrangler_syntax:tuple(ExportedVars), Call).

%%-------------------------------------------------------------------------
%% @doc
%% Calculates the vars to be passed in a call to `{SymClusterType, Name}',
%% from a function of type `ClustType'.
%% @end
%%-------------------------------------------------------------------------
-spec calculate_vars(ClustType, SymClusterType, Name, cluster_folding:acc()) ->
			    [wrangler_syntax:syntaxTree()] when
      SymClusterType :: comm | ex,
      ClustType :: comm | {ex, 1} | {ex, 2},
      Name :: string().
calculate_vars({ex, Pos}, DestType, Name, Acc) ->
    AllVars = cluster_folding:find_free_vars_for_sym_label(DestType, Name, Acc),
    ActualVars = sets:from_list(cluster_folding:find_actual_free_vars_for_asym_label(
				  {DestType, Name}, Pos, Acc)),
    IsLeaf = cluster_folding:get_leaf_as_is_or_default({DestType, Name}, Pos, Acc),
    VarInstances = replace_unknown_with_none(AllVars, ActualVars),
    add_module_if_appropriate_call(IsLeaf, VarInstances);
calculate_vars(_Type, DestType, Name, Acc) ->
    Vars = cluster_folding:find_free_vars_for_sym_label(DestType, Name, Acc),
    lists:map(fun wrangler_syntax:variable/1, Vars).

%%-------------------------------------------------------------------------
%% @doc
%% Calculates the vars to be received in a call to `{SymClusterType, Name}',
%% from a function of type `ClustType'.
%% @end
%%-------------------------------------------------------------------------
-spec calculate_vars_exported(ClustType, SymClusterType, Name, cluster_folding:acc()) ->
			    [wrangler_syntax:syntaxTree()] when
      SymClusterType :: comm | ex,
      ClustType :: comm | {ex, 1} | {ex, 2},
      Name :: string().
calculate_vars_exported({ex, Pos}, DestType, Name, Acc) ->
    AllVars = cluster_folding:find_exported_vars_for_sym_label(DestType, Name, Acc),
    ActualVars = sets:from_list(cluster_folding:find_actual_exported_vars_for_asym_label(
				  {DestType, Name}, Pos, Acc)),
    [wrangler_syntax:variable(filter_external_vars(ActualVars, Var)) || Var <- AllVars];
calculate_vars_exported(_Type, DestType, Name, Acc) ->
    lists:map(fun wrangler_syntax:variable/1,
	      cluster_folding:find_exported_vars_for_sym_label(DestType, Name, Acc)).

%%-------------------------------------------------------------------------
%% @doc
%% Adds a `?MODULE' macro to the beginning of the list of parameters VarInstances if
%% IsLeaf is false.
%% @end
%%-------------------------------------------------------------------------
-spec add_module_if_appropriate_call(IsLeaf :: boolean(),
				     VarInstances :: wrangler_syntax:syntaxTree()) ->
					    wrangler_syntax:syntaxTree().
add_module_if_appropriate_call(IsLeaf, VarInstances) ->
    case IsLeaf of
	true -> VarInstances;
	false -> [wrangler_syntax:macro(wrangler_syntax:variable("MODULE"))|VarInstances]
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Generates the list of parameters for a call. Takes a list of VariableNames
%% and a set of variable names that are available at this scope (AvailableVariables).
%% Returns the list of VariableNames as AST variables but replacing the ones
%% that are not in scope with AST atoms `none'.
%% @end
%%-------------------------------------------------------------------------
-spec replace_unknown_with_none(VariableNames :: [string()],
				AvailableVariables :: sets:set(string())) ->
				       [wrangler_syntax:syntaxTree()].
replace_unknown_with_none([H|T], Set) ->
    case sets:is_element(H, Set) of
	true -> [wrangler_syntax:variable(H)|replace_unknown_with_none(T, Set)];
	false -> [wrangler_syntax:atom(none)|replace_unknown_with_none(T, Set)]
    end;
replace_unknown_with_none([], _) -> [].

%%-------------------------------------------------------------------------
%% @doc
%% Generates a function call using the Type, Name and Vars provided.
%% @end
%%-------------------------------------------------------------------------
-spec generate_call(Type :: SymClusterType, Name :: string(),
		    Vars :: [string()], OutModule :: atom(), any()) -> 
                       wrangler_syntax:syntaxTree() when
                   SymClusterType :: comm  | ex.
generate_call(Type,Name,Vars,OutModule,ModuleVar) ->
    wrangler_syntax:application(
      case Type of
	  ex -> wrangler_syntax:variable(ModuleVar);
	  comm -> wrangler_syntax:atom(OutModule)
      end, wrangler_syntax:atom(Name), Vars).

%%-------------------------------------------------------------------------
%% @doc
%% Finds the label that was assigned to the cluster Cluster in Acc.
%% @end
%%-------------------------------------------------------------------------
-spec fetch_cluster_for_label({ClustType, string()}, Acc :: cluster_folding:acc()) -> Cluster when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
fetch_cluster_for_label(Label, #acc{cluster_labels = LabelDaMap}) ->
    {ok, Clus} = da_map:find_value(Label, LabelDaMap),
    Clus.

%%-------------------------------------------------------------------------
%% @doc
%% Adds a header to the function body in Node, it uses the information in
%% Acc to decide which arguments to include.
%% @end
%%-------------------------------------------------------------------------
-spec add_header({ClustType, string()},
		 Node :: [wrangler_syntax:syntaxTree()],
                 Acc :: cluster_folding:acc(),
		 DisableModule :: boolean()) -> 
                    wrangler_syntax:syntaxTree() when
                ClustType :: comm  | {ex, 1}  | {ex, 2}.
add_header({Type, Name} = Label, Nodes,
	   #acc{var_for_module = ModuleVar} = Acc, DisableModule) ->
    VarNormalVars = compute_vars_header(Label, Acc),
    ExportedVars = compute_exported_vars_header(Label, Acc),
    IsLeaf = cluster_folding:get_leaf_as_is_or_default(Label, extract_pos(Type), Acc),
    Vars = add_module_if_appropriate_header(Type, VarNormalVars,
					    IsLeaf orelse DisableModule, ModuleVar),
    wrangler_syntax:function(
      wrangler_syntax:atom(case DisableModule of
			       true -> cluster_linking:get_original_name(Name, Acc);
			       false -> cluster_linking:get_actual_name(Name, Acc)
			   end),
      [wrangler_syntax:clause(Vars, none, Nodes ++ add_exported_tuple(ExportedVars))]).

%%-------------------------------------------------------------------------
%% @doc
%% Adds the list of exported variables to a tuple, (to return by
%% the function).
%% @end
%%-------------------------------------------------------------------------
-spec add_exported_tuple([wrangler_syntax:syntaxTree()]) ->
				[wrangler_syntax:syntaxTree()].
add_exported_tuple([]) -> [];
add_exported_tuple(List) -> [wrangler_syntax:tuple(List)].

%%-------------------------------------------------------------------------
%% @doc
%% Adds the Module variable at the beginning of the parameter leaf if
%% appropriate.
%% @end
%%-------------------------------------------------------------------------
-spec add_module_if_appropriate_header(comm  | {ex, 1  | 2},
				       Vars :: [wrangler_syntax:syntaxTree()],
				       IsLeaf :: boolean(), any()) -> 
					  [wrangler_syntax:syntaxTree()].
add_module_if_appropriate_header(Type,Vars,IsLeaf,ModuleVar) ->
    case {IsLeaf, Type} of
        {false, comm} -> [wrangler_syntax:variable(ModuleVar)|Vars];
        _ -> Vars
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Computes the vars to be used as parameters of the function with
%% label Label.
%% @end
%%-------------------------------------------------------------------------
-spec compute_vars_header(Label, Acc) -> [wrangler_syntax:syntaxTree()] when
      ClustType :: comm  | {ex, 1}  | {ex, 2},
      Label :: {ClustType, string()},
      Acc :: cluster_folding:acc().
compute_vars_header({Type, Name} = Label, Acc) ->
    NormalVars = cluster_folding:find_free_vars_for_sym_label(
		   cluster_folding:isolate_type_class(Type), Name, Acc),
    ActualVars = sets:from_list(get_internal_vars(Label, Acc, NormalVars)),
    [wrangler_syntax:variable(filter_external_vars(ActualVars, Var)) || Var <- NormalVars].

%%-------------------------------------------------------------------------
%% @doc
%% Computes the vars to be used as return value of the function with
%% label Label, if any.
%% @end
%%-------------------------------------------------------------------------
-spec compute_exported_vars_header(Label, Acc) -> [wrangler_syntax:syntaxTree()] when
      ClustType :: comm  | {ex, 1}  | {ex, 2},
      Label :: {ClustType, string()},
      Acc :: cluster_folding:acc().
compute_exported_vars_header({Type, Name} = Label, Acc) ->
    AllVars = cluster_folding:find_exported_vars_for_sym_label(
		   cluster_folding:isolate_type_class(Type), Name, Acc),
    ActualVars = sets:from_list(get_internal_exported_vars(Label, Acc, AllVars)),
    replace_unknown_with_none(AllVars, ActualVars).

%%-------------------------------------------------------------------------
%% @doc
%% Retrieves vars that are in the scope of the function with label Label.
%% If the function is common, the internal vars are all the vars, because
%% of this, in this case, the function will let through AllVars which are
%% expected as a parameter to avoid recomputation.
%% @end
%%-------------------------------------------------------------------------
-spec get_internal_vars(Label, Acc, AllVars :: [string()]) ->
			       [string()] when
      ClustType :: comm  | {ex, 1}  | {ex, 2},
      Label :: {ClustType, string()},
      Acc :: cluster_folding:acc().
get_internal_vars({comm, _Name}, _Acc, AllVars) -> AllVars;
get_internal_vars({{ex, Pos}, _} = Label, Acc, _AllVars) ->
    cluster_folding:find_actual_free_vars_for_asym_label(Label, Pos, Acc).

%%-------------------------------------------------------------------------
%% @doc
%% Retrieves vars that are in the scope of the function with label Label.
%% If the function is common, the internal vars are all the vars, because
%% of this, in this case, the function will let through AllVars which are
%% expected as a parameter to avoid recomputation.
%% @end
%%-------------------------------------------------------------------------
-spec get_internal_exported_vars(Label, Acc, AllVars :: [string()]) ->
			       [string()] when
      ClustType :: comm  | {ex, 1}  | {ex, 2},
      Label :: {ClustType, string()},
      Acc :: cluster_folding:acc().
get_internal_exported_vars({comm, _Name}, _Acc, AllVars) -> AllVars;
get_internal_exported_vars({{ex, Pos}, _} = Label, Acc, _AllVars) ->
    cluster_folding:find_actual_exported_vars_for_asym_label(Label, Pos, Acc).

%%-------------------------------------------------------------------------
%% @doc
%% Adds an underscore to the beginning of every string in the List that is
%% not in the Set.
%% @end
%%-------------------------------------------------------------------------
-spec filter_external_vars(Set :: sets:set(string()), List :: string()) -> string().
filter_external_vars(ActualVars, Var) ->
    case not sets:is_element(Var, ActualVars) of
	true -> [$_  |Var];
        false -> Var
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Extracts the side of a TypeClass. It defaults to 1 in case of comm, (no side).
%% @end
%%-------------------------------------------------------------------------
-spec extract_pos(TypeClass :: 'comm' | {'ex', 1 | 2}) -> 1 | 2.
extract_pos(comm) -> 1;
extract_pos({ex, Pos}) -> Pos.

%%-------------------------------------------------------------------------
%% @doc
%% Returns true if the Node represents a function declaration,
%% returns false otherwise.
%% @end
%%-------------------------------------------------------------------------
-spec has_function_declaration(wrangler_syntax:syntaxTree()) -> boolean().
has_function_declaration(Node) ->
   case wrangler_syntax:type(Node) of
       function -> true;
       _ -> false
   end.

%%-------------------------------------------------------------------------
%% @doc
%% Obtains the list of clusters of the ClustType in order, (tupled with
%% the position as first element).
%% @end
%%-------------------------------------------------------------------------
-spec get_clusters_in_order(ClustType, cluster_folding:acc()) ->
				   [{pos_integer(), Cluster}] when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
get_clusters_in_order(ClustType, Acc) ->
    get_clusters_in_order(ClustType, 1, Acc).

%%-------------------------------------------------------------------------
%% @doc
%% Obtains the list of clusters of the ClustType in order, starting with
%% Pos, (and tupled with the position as first element).
%% @end
%%-------------------------------------------------------------------------
-spec get_clusters_in_order(ClustType, Pos :: pos_integer(),
			    cluster_folding:acc()) -> [{pos_integer(), Cluster}] when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
get_clusters_in_order(ClustType, Pos, Acc) ->
    case get_cluster_with_pos({ClustType, Pos}, Acc) of
	{ok, Cluster} -> [{Pos, Cluster}|get_clusters_in_order(ClustType, Pos + 1, Acc)];
	error -> []
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Tries to find the cluster with type ClustType and position Pos. It returns `{ok, Cluster}' on
%% success or `error' on failure.
%% @end
%%-------------------------------------------------------------------------
-spec get_cluster_with_pos({ClustType, Pos :: pos_integer()},
			   cluster_folding:acc()) -> 'error' | {'ok', Cluster} when
      Cluster :: cluster:cluster(tree:tree_node()),
      ClustType :: comm | {ex, 1} | {ex, 2}.
get_cluster_with_pos(TypePos, #acc{cluster_order = ClusterOrderMap}) ->
    dict:find(TypePos, ClusterOrderMap).

%%-------------------------------------------------------------------------
%% @doc
%% Assumes that First has the boiler-plate for an exclusive module. It adds
%% the list of syntax trees Rest (which is suppoused to contain the function
%% definitions) at the appropriate place of First. It also adds the functions in
%% Rest to a new export attribute.
%% @end
%%-------------------------------------------------------------------------
-spec insert_in_main_tree(wrangler_syntax:syntaxTree(), [wrangler_syntax:syntaxTree()],
			  cluster_folding:acc(), col_data()) -> cluster_folding:acc().
insert_in_main_tree(First, Rest, Acc, ColData) ->
    Forms = [First|Rest],
    ST = wrangler_recomment:recomment_forms(Forms, []),
    SortedList = get_functions_to_export(ColData, Forms),
    BehAttr = create_behaviour_attribute(Acc),
    api_refac:insert_an_attr(
      insert_export_if_not_empty(ST, SortedList),
      BehAttr).

%%-------------------------------------------------------------------------
%% @doc
%% Create an export with the SortedList of function names and arities,
%% if it is not an empty list.
%% @end
%%-------------------------------------------------------------------------
-spec insert_export_if_not_empty(SyntaxTree :: wrangler_syntax:syntaxTree(),
				 SortedList :: [{FunName :: atom() | string(),
						 Arity :: non_neg_integer()}]) ->
					wrangler_syntax:syntaxTree().
insert_export_if_not_empty(ST, []) -> ST;
insert_export_if_not_empty(ST, SortedList) ->
    ExAttr = create_export_attribute(SortedList),
    api_refac:insert_an_attr(ST, ExAttr).

%%-------------------------------------------------------------------------
%% @doc
%% Creates a behaviour attribute to add to behaviour instances.
%% @end
%%-------------------------------------------------------------------------
-spec create_behaviour_attribute(Acc :: cluster_folding:acc()) ->
					wrangler_syntax:syntaxTree().
create_behaviour_attribute(#acc{out_module = OutMod}) ->
    wrangler_syntax:attribute(wrangler_syntax:atom(behaviour),
			      [wrangler_syntax:atom(OutMod)]).

%%-------------------------------------------------------------------------
%% @doc
%% Gets a list of the functions that have not been exported yet but
%% should, because they are part of the behaviour interface.
%% @end
%%-------------------------------------------------------------------------
-spec get_functions_to_export(ColData :: col_data(),
			      Forms :: [wrangler_syntax:syntaxTree()]) ->
				     FunsToExport :: [{FunName :: string(),
						       Arity :: non_neg_integer()}].
get_functions_to_export(ColData, Forms) ->
    ModInfo = get_modinfo(Forms),
    ToExport = lists:filter(fun ({FunName, Arity}) ->
				    not api_refac:is_exported({list_to_atom(FunName),
							       Arity}, ModInfo)
			    end, sets:to_list(ColData#col_data.beh_funcs)),
    lists:sort(ToExport).

%%-------------------------------------------------------------------------
%% @doc
%% Gets module information as returned by
%% {@link wrangler_syntax_lib:analyze_forms/1}.
%% @end
%%-------------------------------------------------------------------------
-spec get_modinfo(Forms :: wrangler_syntax:syntaxTree()
			 | [wrangler_syntax:syntaxTree()]) ->
			 [{Key, term()}] when     
      Key :: attributes | errors | exports | functions | imports
	   | module | records | rules | warnings.
get_modinfo(Forms) ->
    wrangler_syntax_lib:analyze_forms(
      wrangler_recomment:recomment_forms(Forms, [])).

%%-------------------------------------------------------------------------
%% @doc
%% Adds all the module boiler-plate and the export information to a
%% list of syntax trees that are suppoused to contain the function
%% definitions.
%% @end
%%-------------------------------------------------------------------------
-spec add_skeleton(wrangler_syntax:syntaxTree(), cluster_folding:acc(),
		   col_data()) -> wrangler_syntax:syntaxTree().
add_skeleton(All, Acc, ColData) ->
    wrangler_syntax:form_list(
      [add_module(Acc)|add_exports(add_behaviour_info(get_beh_funcs(ColData), All))]).

%%-------------------------------------------------------------------------
%% @doc
%% Extracts the list of functions in the behaviour interface from
%% the coldata record.
%% @end
%%-------------------------------------------------------------------------
-spec get_beh_funcs(ColData :: col_data()) -> [{FunName :: string(),
						Arity :: non_neg_integer()}].
get_beh_funcs(#col_data{beh_funcs = BehFuncs}) ->
    lists:sort(sets:to_list(BehFuncs)).

%%-------------------------------------------------------------------------
%% @doc
%% Returns the module declaration for the common module.
%% @end
%%-------------------------------------------------------------------------
-spec add_module(Acc :: cluster_folding:acc()) -> any().
add_module(Acc) ->
    wrangler_syntax:attribute(wrangler_syntax:atom(module),
			      [wrangler_syntax:atom(Acc#acc.out_module)]).

%%-------------------------------------------------------------------------
%% @doc
%% Adds all the functions in Forms to an export attribute and
%% adds the export attribute to the beggining of the list.
%% @end
%%-------------------------------------------------------------------------
-spec add_exports(Forms :: [wrangler_syntax:syntaxTree()]) ->
			 [wrangler_syntax:syntaxTree()].
add_exports(All) ->
    Funs = get_functions(get_modinfo(All)),
    [create_export_attribute(Funs)|All].

%%-------------------------------------------------------------------------
%% @doc
%% Extracts the function list from the ModInfo list.
%% @end
%%-------------------------------------------------------------------------
-spec get_functions(ModInfo :: [{Key :: atom(), term()}]) ->
			   [{FunName :: string(), Arity :: non_neg_integer()}].
get_functions(Info) ->
    lists:sort(
      case lists:keysearch(functions, 1, Info) of
	  {value, {functions, Funs}} -> Funs;
	  false -> []
      end).

%%-------------------------------------------------------------------------
%% @doc
%% Creates an export declaration with the list
%% of functions FunsToExport.
%% @end
%%-------------------------------------------------------------------------
-spec create_export_attribute(FunsToExport :: [{FunName :: string(),
						Arity :: non_neg_integer()}]) ->
				     wrangler_syntax:syntaxTree().
create_export_attribute(Funs) ->
    wrangler_syntax:attribute(
      wrangler_syntax:atom(export),
      [wrangler_syntax:list(
	 [wrangler_syntax:arity_qualifier(
	    wrangler_syntax:atom(FunName),
	    wrangler_syntax:integer(Arity))
          || {FunName, Arity} <- Funs])]).

%%-------------------------------------------------------------------------
%% @doc
%% Adds a behaviour_info function with the list
%% of functions FunList.
%% @end
%%-------------------------------------------------------------------------
-spec add_behaviour_info(FunList :: [{FunName :: string(),
				      Arity :: non_neg_integer()}],
			 Rest :: [wrangler_syntax:syntaxTree()]) ->
				[wrangler_syntax:syntaxTree()].
add_behaviour_info(Functions, All) ->
    [wrangler_syntax:function(
       wrangler_syntax:atom(behaviour_info),
       [wrangler_syntax:clause(
	  [wrangler_syntax:atom(callbacks)],
	  none,
	  [wrangler_syntax:list(
	     [wrangler_syntax:tuple(
		[wrangler_syntax:atom(FunName),
		 wrangler_syntax:integer(Arity)])
	      || {FunName, Arity} <- Functions])]),
	wrangler_syntax:clause([wrangler_syntax:variable("_Other")], none,
			       [wrangler_syntax:atom(undefined)])])|All].
