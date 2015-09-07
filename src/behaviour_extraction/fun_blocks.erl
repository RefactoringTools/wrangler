%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Tools to join several instructions in the same block.
%%% This is done to avoid the creation of several contiguous functions
%%% which could be expressed as a single one.
%%% @end
%%% Created :  4 Ago 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(fun_blocks).

-export([add_blocks_to_trees/6, get_body/1]).

-export([replace_children_in_body/3, create_artificial_block_value/2]).

-include("cluster_records.hrl").

-record(candidate, {left_parent, right_parent, left_node, right_node,
		    left_pos, right_pos}).

-type candidate() :: #candidate{
			left_parent :: tree:tree_node(),
			right_parent :: tree:tree_node(),
			left_node :: tree:tree_node(),
			right_node :: tree:tree_node(),
			left_pos :: pos_integer(),
			right_pos :: pos_integer()
		       }. %% Record for keeping information about the candidates for
                          %% block creation.
                          %% Contains the following fields: <ul>
                          %% <li> `left_parent' - contains the parent of the node on the
                          %%                      left tree </li>
                          %% <li> `right_parent' - contains the parent of the node on the
                          %%                       right tree </li>
                          %% <li> `left_node' - contains the left tree version of the node </li>
                          %% <li> `right_node' - contains the right tree version of the node </li>
                          %% <li> `left_pos' - contains the position of the left node in its
                          %%                   parent's body list </li>
                          %% <li> `right_pos' - contains the position of the right node in its
                          %%                    parent's body list </li></ul>

%%--------------------------------------------------------------------
%% @doc
%% Searches for functions with several consecutive mapped/unmapped
%% children in the frontier and groups them into block expressions.
%% @end
%%--------------------------------------------------------------------
-spec add_blocks_to_trees(tree:tree(),tree:tree(),
			  da_map:da_map(tree:tree_node(),tree:tree_node()),
			  cluster_dict:cluster_dict(tree:tree_node()),
			  cluster_dict:cluster_dict(tree:tree_node()),
			  cluster_dict:cluster_dict(tree:tree_node())) -> {tree:tree(),tree:tree()}.
add_blocks_to_trees(LeftTree, RightTree, Mapping, CommCluster, ExLeftClus, ExRightClus) ->
    ClusterDicts = #cluster_dicts{comm_clus = CommCluster,
                                  ex_clus1 = ExLeftClus,
                                  ex_clus2 = ExRightClus},
    TreeInfo = #tree_info{tree1 = LeftTree,
                          tree2 = RightTree,
			  tree1_args = dict:new(), %% dummy dict for efficiency
			  tree2_args = dict:new(), %% dummy dict for efficiency
                          mapping = Mapping},
    Candidates = find_candidates(ClusterDicts, TreeInfo),
    GroupedCandidates = group_candidates(Candidates),
    BlockInfo = find_valid_blocks(GroupedCandidates),
    add_blocks(LeftTree, RightTree, BlockInfo).

%%--------------------------------------------------------------------
%% @doc
%% Finds a list of candidate parent-children node pairs where
%% to place artificial blocks.
%% @end
%%--------------------------------------------------------------------
-spec find_candidates(ClusterDicts :: cluster_folding:cluster_dicts(),
		      TreeInfo :: cluster_folding:tree_info()) ->
			     Candidates :: [candidate()].
find_candidates(ClusterDicts, TreeInfo) ->
    cluster_folding:fold_clusters(
      fun (_P, PH, Children, Acc) ->
	      collect_block_candidates(PH, Children, Acc, TreeInfo)
      end, [], ClusterDicts, TreeInfo).

%%--------------------------------------------------------------------
%% @doc
%% Accumulates candidate parent-children node pairs where to
%% place artificial blocks.
%% @end
%%--------------------------------------------------------------------
-spec collect_block_candidates(PH :: term(),
			       ChildAlternatives :: [tree:tree_node()
						     | {atom(), non_neg_integer()}],
			       Acc :: [candidate()],
			       TreeInfo :: cluster_folding:tree_info()) -> 
				      UpdatedAcc :: [candidate()].
collect_block_candidates(PH, Children, Acc, #tree_info{tree1 = LeftTree, tree2 = RightTree})
  when not is_atom(PH) ->
    {LeftChild, RightChild} = get_two_children(Children),
    {ok, LeftParent} = tree:get_parent(LeftChild, LeftTree),
    {ok, RightParent} = tree:get_parent(RightChild, RightTree),
    case {clause_or_block_body_pos(LeftChild, LeftParent),
	  clause_or_block_body_pos(RightChild, RightParent)} of
	{{ok, LeftPos}, {ok, RightPos}} -> [#candidate{left_parent = LeftParent,
						       right_parent = RightParent,
						       left_node = LeftChild,
						       right_node = RightChild,
						       left_pos = LeftPos,
						       right_pos = RightPos}|Acc];
	_ -> Acc
    end;
collect_block_candidates(_, _, Acc, _TreeInfo) -> Acc.

%%--------------------------------------------------------------------
%% @doc
%% Takes a list of alternative children for a node and
%% returns a tuple with the two alternatives (left and right),
%% as they were in the original input trees (no node pairs).
%% @end
%%--------------------------------------------------------------------
-spec get_two_children(NodeAlternatives :: [tree:tree_node()]) ->
			      NodePair :: {tree:tree_node(), tree:tree_node()}.
get_two_children([NodePair]) -> tree:get_pair_tuple(NodePair);
get_two_children([LeftAlternative, RightAlternative]) ->
    {ast_tree:get_left_if_node_pair(LeftAlternative),
     ast_tree:get_right_if_node_pair(RightAlternative)}.

%%--------------------------------------------------------------------
%% @doc
%% Finds the position of the ChildNode in the body of
%% the ParentNode.
%% @end
%%--------------------------------------------------------------------
-spec clause_or_block_body_pos(ChildNode :: tree:tree_node(),
			       ParentNode :: tree:tree_node()) ->
				      {ok, Position :: integer()} | error.
clause_or_block_body_pos(Child, Parent) ->
    ParentValue = tree:get_value(Parent),
    {node, ChildPH} = tree:get_ph(Child),
    case wrangler_syntax:type(ParentValue) of
	clause -> Body = wrangler_syntax:clause_body(ParentValue),
		  position_of(ChildPH, Body);
	block_expr -> Body = wrangler_syntax:block_expr_body(ParentValue),
		      position_of(ChildPH, Body);
	_ -> error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Finds the position of Element in List.
%% @end
%%--------------------------------------------------------------------
-spec position_of(Element, List :: [Element]) -> {ok, integer()} | error when
      Element :: term().
position_of(Element, List) ->
    position_of(Element, List, 1).

%%--------------------------------------------------------------------
%% @doc
%% Finds the position of Element in List, assuming that
%% the position for the first element is CurrPos.
%% @end
%%--------------------------------------------------------------------
-spec position_of(Element, List :: [Element], CurrPos :: integer()) ->
			 {ok, integer()} | error when
      Element :: term().
position_of(_, [], _) -> error;
position_of(Element, [Element|_], Pos) -> {ok, Pos};
position_of(Element, [_|List], Pos) ->
    position_of(Element, List, Pos + 1).

%%--------------------------------------------------------------------
%% @doc
%% Groups Candidates in sublists which have the same pair
%% of LeftParent and RightParent.
%% @end
%%--------------------------------------------------------------------
-spec group_candidates(Candidates :: [candidate()]) -> GroupedCandidates :: [[candidate()]].
group_candidates(Candidates) ->
    filter_singletons(
      group_by(fun (#candidate{left_parent = LP,
			       right_parent = RP}) -> {LP, RP}
               end, lists:usort(Candidates))).

%%--------------------------------------------------------------------
%% @doc
%% Removes from List all the sublists with less than two
%% elements.
%% @end
%%--------------------------------------------------------------------
-spec filter_singletons(List :: [[Element :: term()]]) ->
			       [[Element :: term()]].
filter_singletons([]) -> [];
filter_singletons([[]|Rest]) -> filter_singletons(Rest);
filter_singletons([[_]|Rest]) -> filter_singletons(Rest);
filter_singletons([El|Rest]) -> [El|filter_singletons(Rest)].

%%--------------------------------------------------------------------
%% @doc
%% Returns a list with information about the places
%% where it makes sense to introduce an artificial
%% block expression.
%% It takes a list of grouped candidates.
%% @see group_candidates/1
%% @end
%%--------------------------------------------------------------------
-spec find_valid_blocks(GroupedCandidates :: [[candidate()]]) ->
			       BlocksInfo :: [{{LeftParent :: tree:tree_node(),
						RightParent :: tree:tree_node()},
					       Block :: [{LeftPos :: non_neg_integer(),
							  RightPos :: non_neg_integer()}]}].
find_valid_blocks(GroupedCandidates) ->
    lists:append(lists:map(fun compute_blocks/1, GroupedCandidates)).

%%--------------------------------------------------------------------
%% @doc
%% Takes a list of candidates and produces a list of consecutive
%% blocks (bigger than one in length). Candidates are assumed to
%% have the same pair of parent nodes.
%% @end
%%--------------------------------------------------------------------
-spec compute_blocks(CandidateGroup :: nonempty_list(candidate())) ->
			    [{{LeftParent :: tree:tree_node(),
			       RightParent :: tree:tree_node()},
			      Block :: [{LeftPos :: non_neg_integer(),
					 RightPos :: non_neg_integer()}]}].
compute_blocks([#candidate{
		   left_parent = LeftParent,
		   right_parent = RightParent
		  }|_] = List) ->
    SortList = sort_using(fun (#candidate{left_pos = LeftPos}) -> LeftPos end, List),
    SimpleList = [{LP, RP} || #candidate{left_pos = LP, right_pos = RP} <- SortList],
    Res = compute_blocks_aux(SimpleList, nan, nan, []),
    lists:zip(lists:duplicate(length(Res), {LeftParent, RightParent}), Res).

%%--------------------------------------------------------------------
%% @doc
%% Takes a list of pairs of numbers sorted by the left number and
%% finds the subsequences (bigger than one in length) that are
%% consecutive for both numbers of the pair. The last three
%% arguments are accumulators that can be initialised with `nan'
%% atoms and the empty list.
%% @end
%%--------------------------------------------------------------------
-spec compute_blocks_aux([{LeftPos :: non_neg_integer(), RightPos :: non_neg_integer()}],
			 LastLeftPos :: 'nan' | non_neg_integer(),
			 LastRightPos :: 'nan' | non_neg_integer(),
			 ConsecutivePositions :: [{LeftPos :: non_neg_integer(),
						   RightPos :: non_neg_integer()}]) ->
				[[{LeftPos :: non_neg_integer(),
				   RightPos :: non_neg_integer()}]].
compute_blocks_aux([], _, _, [_, _|_] = List) -> [lists:reverse(List)];
compute_blocks_aux([], _, _, _) -> [];
compute_blocks_aux([{LN, RN}|Rest], LN, RN, List) ->
    compute_blocks_aux(Rest, LN + 1, RN + 1, [{LN, RN}|List]);
compute_blocks_aux([{LN, RN}|Rest], _, _, [_, _|_] = List) ->
    [lists:reverse(List)|compute_blocks_aux(Rest, LN + 1, RN + 1, [{LN, RN}])];
compute_blocks_aux([{LN, RN}|Rest], _, _, _) ->
    compute_blocks_aux(Rest, LN + 1, RN + 1, [{LN, RN}]).

%%--------------------------------------------------------------------
%% @doc
%% Adds the artificial blocks in BlockInfo to the trees.
%% @end
%%--------------------------------------------------------------------
-spec add_blocks(LeftTree :: tree:tree(),
		 RightTree :: tree:tree(),
		 BlockInfo :: [{{LeftParent :: tree:tree_node(),
				 RightParent :: tree:tree_node()},
				Block :: [{LeftPos :: non_neg_integer(),
					   RightPos :: non_neg_integer()}]}]) ->
			{NewLeftTree :: tree:tree(),
			 NewRightTree :: tree:tree()}.
add_blocks(LeftTree, RightTree, BlockInfo) ->
    NumeratedBlockInfo = lists:zip(lists:seq(1, length(BlockInfo)), BlockInfo),
    lists:foldl(fun add_block_expression/2, {LeftTree, RightTree}, NumeratedBlockInfo).

%%--------------------------------------------------------------------
%% @doc
%% Adds the artificial block BlockInfo with sequential number
%% BlockNumber to the trees.
%% @end
%%--------------------------------------------------------------------
-spec add_block_expression({BlockNumber :: non_neg_integer(),
			    BlockInfo :: {{LeftParent :: tree:tree_node(),
					   RightParent :: tree:tree_node()},
					  Block :: [{LeftPos :: non_neg_integer(),
						     RightPos :: non_neg_integer()}]}},
			   {LeftTree :: tree:tree(), RightTree :: tree:tree()}) ->
				  {NewLeftTree :: tree:tree(),
				   NewRightTree :: tree:tree()}.
add_block_expression({N, {{LeftParent, RightParent}, Block}}, {LeftTree, RightTree}) ->
    {LeftBlock, RightBlock} = lists:unzip(Block),
    {add_block_expression(N, LeftParent, LeftBlock, LeftTree),
     add_block_expression(N, RightParent, RightBlock, RightTree)}.

%%--------------------------------------------------------------------
%% @doc
%% Adds the artificial block Block under the node TreeNode
%% in Tree, and sets its sequential number to Number.
%% Block is a list of numbers that indicate the positions
%% of the children of TreeNode that must be added to the
%% block, and they must be consecutive and ascending.
%% TreeNode is assumed to be a clause or a block expression.
%% @end
%%--------------------------------------------------------------------
-spec add_block_expression(Number :: non_neg_integer(),
			   TreeNode :: tree:tree_node(),
			   Block :: [non_neg_integer()],
			   Tree :: tree:tree()) -> tree:tree().
add_block_expression(N, ParentNode, Block, Tree) ->
    ParentValue = tree:get_value(ParentNode),
    ChildrenPHs = fetch_block(Block, ParentValue),
    ParentValueUpdater = body_updater(ParentValue, ChildrenPHs),
    GroupNodeValue = create_artificial_block_value(Tree, ChildrenPHs),
    {GroupNode, NewTree} = tree:group_children(ParentNode, ChildrenPHs, ParentValueUpdater,
					       GroupNodeValue, Tree),
    tree:store_node(set_block_props(N, GroupNode), NewTree).

%%--------------------------------------------------------------------
%% @doc
%% Adds the artificial block tags to the provided TreeNode.
%% This tags are used later in
%% {@link fun_blocks_fix:find_artificial_blocks/2}
%% and {@link fix_frontiers:is_artificial_block/1} in order
%% to identify and map the artificial blocks.
%% Number is a sequential number that identifies a pair of
%% artificial blocks.
%% @end
%%--------------------------------------------------------------------
-spec set_block_props(Number :: non_neg_integer(),
		      TreeNode :: tree:tree_node()) ->
			     tree:tree_node().
set_block_props(N, GroupNode) ->
    tree:set_property(
      is_artificial_block, true,
      tree:set_property(
	artificial_block_number, N,
	GroupNode)).

%%--------------------------------------------------------------------
%% @doc
%% Creates a block expression with ChildrenPHs as subtrees and
%% sets the position and the range to something useful.
%% @end
%%--------------------------------------------------------------------
-spec create_artificial_block_value(Tree :: tree:tree(),
				    ChildrenPHs :: [tree:tree_ph()]) ->
					   wrangler_syntax:syntaxTree().
create_artificial_block_value(Tree, ChildrenPHs) ->
    ArtificialBlock = wrangler_syntax:block_expr(ChildrenPHs),
    {ok, FirstNode} = tree:get_node(hd(ChildrenPHs), Tree),
    {ok, LastNode} = tree:get_node(lists:last(ChildrenPHs), Tree),
    {S1, _E1} = wrangler_ast_server:get_range(tree:get_value(FirstNode)),
    {_S2, E2} = wrangler_ast_server:get_range(tree:get_value(LastNode)),
    ArtificialBlockWP = wrangler_syntax:set_pos(ArtificialBlock, S1),
    wrangler_misc:update_ann(ArtificialBlockWP, {range, {S1, E2}}).

%%--------------------------------------------------------------------
%% @doc
%% Fetches from ASTNode the elements indicated by Block.
%% Assumes that Block is an ascending list of consecutive integers,
%% which denotes the positions in the body of ASTNode of the subtrees
%% to fetch. Assumes that ASTNode is either a clause or a block
%% expression.
%% @end
%%--------------------------------------------------------------------
-spec fetch_block(Block :: [non_neg_integer()],
		  ASTNode :: wrangler_syntax:syntaxTree()) ->
			 [wrangler_syntax:syntaxTree()].
fetch_block(Block, ASTNode) ->
    Body = get_body(ASTNode),
    lists:sublist(Body, hd(Block), length(Block)).

%%--------------------------------------------------------------------
%% @doc
%% Returns the body of a clause or block expression (ClauseOrBlock).
%% @end
%%--------------------------------------------------------------------
-spec get_body(wrangler_syntax:syntaxTree()) -> [wrangler_syntax:syntaxTree()].
get_body(ClauseOrBlock) ->
    case wrangler_syntax:type(ClauseOrBlock) of
	clause -> wrangler_syntax:clause_body(ClauseOrBlock);
	block_expr -> wrangler_syntax:block_expr_body(ClauseOrBlock)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Produces a function that when applied to a PlaceHolder,
%% returns the result of replacing ChildrenPHs with PlaceHolder
%% in ParentValue. It assumes that ParentValue is a clause or
%% a block expression.
%% @end
%%--------------------------------------------------------------------
-spec body_updater(ParentValue :: wrangler_syntax:syntaxTree(),
		   ChildrenPHs :: [tree:tree_ph()]) ->
			  fun ((PlaceHolder :: tree:tree_ph()) ->
				      wrangler_syntax:syntaxTree()).
body_updater(ParentValue, ChildrenPHs) ->
    fun (PH) ->
	    replace_children_in_body(ChildrenPHs, [PH], ParentValue)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Replaces the old subtrees (OldChildren) in the body of NodeValue
%% with the new subtrees (NewChildren), leaving the rest untouched.
%% It assumes that the node (NodeValue) is a clause or a block expression.
%% @end
%%--------------------------------------------------------------------
-spec replace_children_in_body(OldChildren :: [El],
			       NewChildren :: [El],
			       NodeValue :: wrangler_syntax:syntaxTree()) ->
				      wrangler_syntax:syntaxTree().
replace_children_in_body(OldChildren, NewChildren, NodeValue) ->
    OldBody = get_body(NodeValue),
    NewBody = tree:replace_with_elements(OldChildren, OldBody, NewChildren),
    replace_block_or_clause_body(NodeValue, NewBody).

%%--------------------------------------------------------------------
%% @doc
%% Modifies the body of a clause or block expression (BlockOrClause).
%% This is done by re-creating it with the new body and copying
%% the attributes from the old one.
%% @end
%%--------------------------------------------------------------------
-spec replace_block_or_clause_body(BlockOrClause :: wrangler_syntax:syntaxTree(),
				   Body :: [wrangler_syntax:syntaxTree()]) ->
					  wrangler_syntax:syntaxTree().
replace_block_or_clause_body(BlockOrClause, Body) ->
    wrangler_syntax:copy_attrs(
      BlockOrClause,
      case wrangler_syntax:type(BlockOrClause) of
	  clause -> modify_clause_body(BlockOrClause, Body);
	  block_expr -> wrangler_syntax:block_expr(Body)
      end).

%%--------------------------------------------------------------------
%% @doc
%% Re-creates Clause with Body as body of the clause.
%% @end
%%--------------------------------------------------------------------
-spec modify_clause_body(Clause :: wrangler_syntax:syntaxTree(),
			 Body :: [wrangler_syntax:syntaxTree()]) ->
				wrangler_syntax:syntaxTree().
modify_clause_body(Clause, Body) ->
    wrangler_syntax:clause(
      wrangler_syntax:clause_patterns(Clause),
      wrangler_syntax:clause_guard(Clause),
      Body).

%%--------------------------------------------------------------------
%% @doc
%% Groups a List into a list of lists, where each sublist has all the
%% elements for which Fun produces the same value.
%% @end
%%--------------------------------------------------------------------
-spec group_by(Fun :: fun((El) -> Key), List :: [El]) -> [[El]] when
      El :: term(), Key :: term().
group_by(Fun, List) ->
    Dict = lists:foldl(fun (El, Dic) -> dict:append(Fun(El), El, Dic) end,
		       dict:new(), List),
    dict:fold(fun acc_dict/3, [], Dict).

%%--------------------------------------------------------------------
%% @doc
%% Accumulator for aggregating the values of a dict into a list.
%% Aimed at being used as folding function of a dict.
%% @end
%%--------------------------------------------------------------------
-spec acc_dict(_Key, Element :: El, List :: [El]) -> [El] when
      El :: term().
acc_dict(_, Value, Acc) ->
    [Value|Acc].

%%--------------------------------------------------------------------
%% @doc
%% Sorts the List by comparing the result of applying the function
%% Fun to each element. Use sorting function SortingFun.
%% @end
%%--------------------------------------------------------------------
-spec sort_using_with_fun(Fun :: fun((Element) -> SortableElement),
			  List :: [Element],
			  fun((fun((El, El) -> boolean()),
			       [El]) -> [El])) -> [Element] when
      El :: term(),
      Element :: term(),
      SortableElement :: term().
sort_using_with_fun(Fun, List, SortingFun) ->
    SortingFun(fun (X, Y) -> Fun(X) =< Fun(Y) end, List).

%%--------------------------------------------------------------------
%% @doc
%% Sorts List using the elements mapped by Mapping instead.
%% @see lists:sort/2
%% @see sort_using_with_fun/3
%% @end
%%--------------------------------------------------------------------
-spec sort_using(Mapping :: fun((Element) -> SortableElement),
		 List :: [Element]) ->
			[Element] when
      Element :: term(),
      SortableElement :: term().
sort_using(Fun, List) ->
    sort_using_with_fun(Fun, List, fun lists:sort/2).

