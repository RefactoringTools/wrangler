%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Implements an indexed tree with isolated nodes.
%%% @end
%%% Created :  3 Jun 2015 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(tree).

-export([new/0, get_root_node/1, get_parent/2, get_node/2, group_children/5,
	 get_children/2, store_node/2, breadth_fold/3, index_by/2]).

-export([new_node_ph/1, get_child_ph/1, set_value/2, get_value/1,
	 set_property/3, get_property/2, copy_properties/2]).

-export([create_node_pair/2, get_pair_tuple/1, is_node_pair/1,
	 get_ph/1, replace_with_elements/3, collapse_node/2,
	 upgrade_node/2, get_pos_in_parent/2]).

-export([dict_get/3, get_data/1, set_data/2]).

-export_type([tree/0]).

-export_type([tree_node/0, tree_ph/0]).

-record(tree, {root_ph, nodes_by_ph, nodes_ph_by_children_ph, extra_data}).
-record(tree_node, {ph, value, children, properties}).

-opaque tree() :: #tree{}.
-opaque tree_node() :: #tree_node{} | {'node_pair', #tree_node{}, #tree_node{}}.
-opaque tree_ph() :: reference().

%%--------------------------------------------------------------------
%% @doc
%% Creates a node from a placeholder. See {@link get_child_ph/1}.
%% @end
%%--------------------------------------------------------------------
-spec new_node_ph(tree_ph()) -> tree_node().
new_node_ph(PH) ->
    #tree_node{ph = PH,
	       children = [],
	       properties = dict:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Generates a placeholder for a child of the node. A child generated
%% with the placeholder will be linked automatically to the node.
%% For the link to be created, the version of the node updated by
%% this function must be stored in the tree afterwards,
%% {@link store_node/2}, as well as the child generated with the
%% placeholder.
%% @end
%%--------------------------------------------------------------------
-spec get_child_ph(tree_node()) -> {tree_node(), tree_ph()}.
get_child_ph(#tree_node{children = Children} = Node) ->
    NewPH = make_ref(),
    {Node#tree_node{children = Children ++ [NewPH]},
     NewPH}.

%%--------------------------------------------------------------------
%% @doc
%% Sets the value for a tree_node. It does not update the tree,
%% obviously. For updating the tree the node must be stored
%% afterwards, {@link store_node/2}.
%% @end
%%--------------------------------------------------------------------
-spec set_value(term(), tree_node()) -> tree_node().
set_value(Value, Node) ->
    Node#tree_node{value = Value}.

%%--------------------------------------------------------------------
%% @doc
%% Extracts the value from a tree_node.
%% @end
%%--------------------------------------------------------------------
-spec get_value(tree_node()) -> term().
get_value(#tree_node{value = Value}) ->
    Value.

%%--------------------------------------------------------------------
%% @doc
%% Sets the value property for a tree_node. It does not update the tree,
%% obviously. For updating the tree the node must be stored
%% afterwards, {@link store_node/2}.
%% @end
%%--------------------------------------------------------------------
-spec set_property(Key :: term(), Value :: term(), Node :: tree_node()) -> tree_node().
set_property(Key, Value, #tree_node{properties = Properties} = Node) ->
    Node#tree_node{properties = dict:store(Key, Value, Properties)}.

%%--------------------------------------------------------------------
%% @doc
%% Extracts the value of a property from a tree_node. Returns error
%% if it is not set.
%% @end
%%--------------------------------------------------------------------
-spec get_property(Key :: term(), Node :: tree_node()) ->
			  {ok, Value :: term()} | error.
get_property(Key, #tree_node{properties = Properties}) ->
    dict:find(Key, Properties).

%%--------------------------------------------------------------------
%% @doc
%% Creates a empty tree
%% @end
%%--------------------------------------------------------------------
-spec new() -> tree().
new() -> RootPH = make_ref(),
	 Tree = #tree{root_ph = RootPH, nodes_by_ph = dict:new(),
		      nodes_ph_by_children_ph = dict:new()},
	 store_node(new_node_ph(RootPH), Tree).

%%--------------------------------------------------------------------
%% @doc
%% Returns the root node of the Tree.
%% @end
%%--------------------------------------------------------------------
-spec get_root_node(Tree :: tree()) -> tree:tree_node().
get_root_node(#tree{root_ph = RootPH} = Tree) ->
    {ok, RootNode} = get_node(RootPH, Tree),
    RootNode.

%%--------------------------------------------------------------------
%% @doc
%% Returns the parent of Node in the Tree.
%% @end
%%--------------------------------------------------------------------
-spec get_parent(Node :: tree:tree_node(), Tree :: tree()) ->
			error | {ok, tree:tree_node()}.
get_parent(#tree_node{ph = ChildPH} = _Node,
	   #tree{nodes_ph_by_children_ph = ChildrenIdx} = Tree) ->
    case dict:find(ChildPH, ChildrenIdx) of
	{ok, ParentPH} -> get_node(ParentPH, Tree);
	error -> error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Finds a node by using its placeholder. It returns `error' if it
%% cannot be found.
%% @end
%%--------------------------------------------------------------------
-spec get_node(tree_ph(), tree()) ->
		      'error' | {'ok', tree_node()}.
get_node(PH, #tree{nodes_by_ph = PHIdx}) ->
    dict:find(PH, PHIdx).

%%--------------------------------------------------------------------
%% @doc
%% Returns the children of Node in the Tree.
%% @end
%%--------------------------------------------------------------------
-spec get_children(Node :: tree:tree_node(), Tree :: tree()) ->
			  [tree:tree_node()].
get_children(#tree_node{children = ChildrenPHs}, Tree) ->
    [begin
	 {ok, Child} = get_node(ChildPH, Tree),
	 Child
     end || ChildPH <- ChildrenPHs].

%%--------------------------------------------------------------------
%% @doc
%% Finds the position of Node in the list of children
%% of Parent.
%% @end
%%--------------------------------------------------------------------
-spec get_pos_in_parent(Node :: tree:tree_node(),
			Parent :: tree:tree_node()) -> error | pos_integer().
get_pos_in_parent(#tree_node{ph = PH},
		  #tree_node{children = Children}) ->
    find_pos(PH, Children).

%%--------------------------------------------------------------------
%% @doc
%% Find the position of El in the List.
%% @end
%%--------------------------------------------------------------------
-spec find_pos(El :: ElType, List :: [ElType]) -> 'error' | pos_integer().
find_pos(El, List) -> find_pos(El, List, 1).

%%--------------------------------------------------------------------
%% @doc
%% Find the position of El in the List, assuming the first
%% element of List has position CurrPos.
%% @end
%%--------------------------------------------------------------------
-spec find_pos(El :: ElType, List :: [ElType], CurrPos :: pos_integer()) ->
		      'error' | pos_integer().
find_pos(_El, [], _Pos) -> error;
find_pos(El, [El|_], Pos) -> Pos;
find_pos(El, [_|T], Pos) -> find_pos(El, T, Pos + 1).

%%--------------------------------------------------------------------
%% @doc
%% Adds a new node to the tree, or updates an existing one.
%% @end
%%--------------------------------------------------------------------
-spec store_node(Node :: tree:tree_node(), Tree :: tree()) ->
			tree().
store_node(#tree_node{
	      ph = PH,
	      children = Children
	     } = Node,
	   #tree{
	      nodes_by_ph = PhIdx,
	      nodes_ph_by_children_ph = ChildIdx
	     } = Tree) ->
    Tree#tree{
      nodes_by_ph = dict:store(PH, Node, PhIdx),
      nodes_ph_by_children_ph = lists:foldl(fun (ChildPH, Acc) ->
						    dict:store(ChildPH, PH, Acc)
					    end, ChildIdx, Children)
     }.


%%-----------------------------------------------------------------------
%% @doc
%% Folds a function through a tree in breadth first order.
%% @end
%%-----------------------------------------------------------------------
-spec breadth_fold(Fun :: FFun,
		   AccIn :: Acc, Tree :: tree()) ->
			  AccOut :: Acc when
      FFun :: fun((CurrNode :: tree_node(), Acc) -> Acc),
      Acc :: term().
breadth_fold(Fun, AccIn, Tree) ->
    Queue = queue:in(get_root_node(Tree), queue:new()),
    breadth_fold_aux(Fun, AccIn, Queue, Tree).

%%-----------------------------------------------------------------------
%% @doc
%% Auxiliar function to {@link breadth_fold/3}
%% Folds a function through the nodes of the tree that are in the Queue.
%% For each node folded, its children are added to the end of the Queue.
%% @end
%%-----------------------------------------------------------------------
-spec breadth_fold_aux(Fun :: FFun,
		       AccIn :: Acc, Queue :: queue:queue(tree_node()),
		       Tree :: tree()) ->
			      AccOut :: Acc when
      FFun :: fun((CurrNode :: tree_node(), Acc) -> Acc),
      Acc :: term().
breadth_fold_aux(Fun, AccIn, Queue, Tree) ->
    case queue:out(Queue) of
	{empty, _} -> AccIn;
	{{value, Node}, RestQueue} ->
	    AccOut = Fun(Node, AccIn),
	    Children = get_children(Node, Tree),
	    NewQueue = lists:foldl(fun queue:in/2, RestQueue, Children),
	    breadth_fold_aux(Fun, AccOut, NewQueue, Tree)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns a map with the nodes of the Tree as values and the
%% result of applying Fun to them as keys.
%% @end
%%--------------------------------------------------------------------
-spec index_by(Fun :: fun((Node) -> Idx),
	       Tree :: tree()) ->
		      dict:dict(Idx, Node) when
      Node :: tree:tree_node(),
      Idx :: term().
index_by(Fun, #tree{nodes_by_ph = PhMap}) ->
    dict:fold(fun (_, X, Acc) ->
		      Key = Fun(X),
		      List = dict_get(Key, Acc, []),
		      dict:store(Key, [X|List], Acc)
	      end, dict:new(), PhMap).


%%--------------------------------------------------------------------
%% @doc
%% Joins two nodes into a single node (node pair)
%% @end
%%--------------------------------------------------------------------
-spec create_node_pair(Node1 :: tree:tree_node(),
                       Node2 :: tree:tree_node())
		      -> tree:tree_node().
create_node_pair(Node1, Node2) ->
    {node_pair, Node1, Node2}.


%%--------------------------------------------------------------------
%% @doc
%% Splits a node pair into the original two nodes
%% @end
%%--------------------------------------------------------------------
-spec get_pair_tuple(NodePair :: tree:tree_node()) ->
          {tree:tree_node(), tree:tree_node()}.
get_pair_tuple({node_pair, Node1, Node2}) ->
    {Node1, Node2}.

%%--------------------------------------------------------------------
%% @doc
%% Moves the children with PH ChildrenPHs to a new node, and
%% puts the new node in the place of the children, it returns
%% both the new node and the updated tree.
%% A value, both for the parent and the new node which reflect
%% the changes, is required.
%% All nodes are assumed to NOT be node pairs, (before and after).
%% @end
%%--------------------------------------------------------------------
-spec group_children(tree_node(), [tree_ph()],
		     fun((PH :: tree_node()) -> Value),
		     Value, tree()) ->
			    {tree_node(), tree:tree()} when
      Value :: term().
group_children(#tree_node{children = AllChildren} = ParentNode,
	       ChildrenPHs, NewParentValueGen, GroupNodeValue, Tree) ->
    GroupNodePH = make_ref(),
    GroupNode = #tree_node{ph = GroupNodePH,
			   value = GroupNodeValue,
			   children = ChildrenPHs,
			   properties = dict:new()},
    NewChildren = replace_with_elements(ChildrenPHs, AllChildren, [GroupNodePH]),
    NewParentNode = ParentNode#tree_node{value = NewParentValueGen(GroupNodePH),
					 children = NewChildren},
    {GroupNode, store_node(NewParentNode, store_node(GroupNode, Tree))}.

%%--------------------------------------------------------------------
%% @doc
%% Replaces the first occurrence of Pattern in List with the elements
%% in the list Replacement. It assumes there no partial matches.
%% @end
%%--------------------------------------------------------------------
-spec replace_with_elements(Pattern :: [El], List :: [El],
			    Replacement :: [El]) ->
				   [El] when
      El :: term().
replace_with_elements([PH|ChildrenPHs], [PH|AllChildren], GroupNodePH) ->
    replace_with_elements(ChildrenPHs, AllChildren, GroupNodePH);
replace_with_elements([], AllChildren, GroupNodePH) ->
    GroupNodePH ++ AllChildren;
replace_with_elements(ChildrenPHs, [PH|AllChildren], GroupNodePH) ->
    [PH|replace_with_elements(ChildrenPHs, AllChildren, GroupNodePH)].

%%--------------------------------------------------------------------
%% @doc
%% Removes the node from the tree, and attaches its children to
%% its parent in the place it was. It expects the value of the
%% parent to be updated appropriately.
%% @end
%%--------------------------------------------------------------------
collapse_node(#tree_node{children = ChildrenPHs,
			 ph = PH} = Node, Tree) ->
    {ok, Parent} = get_parent(Node, Tree),
    NewParent = replace_in_node_children(Parent, [PH], ChildrenPHs),
    remove_ph_from_index(PH, store_node(NewParent, Tree)).

%%--------------------------------------------------------------------
%% @doc
%% Moves a node upwards a level. It expects the value of the
%% two ancestors to be updated appropriately.
%% @end
%%--------------------------------------------------------------------
upgrade_node(#tree_node{ph = PH} = Node, Tree) ->
    {ok, #tree_node{ph = ParentPH} = Parent} = get_parent(Node, Tree),
    {ok, GrandParent} = get_parent(Parent, Tree),
    NewParent = replace_in_node_children(Parent, [PH], []),
    NewGrandParent = replace_in_node_children(GrandParent, [ParentPH], [ParentPH, PH]),
    store_node(NewGrandParent, store_node(NewParent, Tree)).

%%--------------------------------------------------------------------
%% @doc
%% Replaces the first occurrence of Pattern in the list of children of
%% Node with the list Replacement. It assumes there are no
%% partial matches.
%% @end
%%--------------------------------------------------------------------
-spec replace_in_node_children(Node :: tree:tree_node(),
			       Pattern :: [tree:tree_ph()],
			       Replacement :: [tree:tree_ph()]) ->
				      tree:tree_node().
replace_in_node_children(#tree_node{children = Children} = Parent,
			 PatternList, ReplacementList) ->
    Parent#tree_node{children = replace_with_elements(PatternList, Children,
						      ReplacementList)}.

%%--------------------------------------------------------------------
%% @doc
%% Removes the placeholder PH from the index of the tree Tree.
%% @end
%%--------------------------------------------------------------------
-spec remove_ph_from_index(PH :: tree:tree_ph(),
			   Tree :: tree:tree()) ->
				  tree:tree().
remove_ph_from_index(PH, #tree{nodes_by_ph = NodByPH} = Tree) ->
    Tree#tree{nodes_by_ph = dict:erase(PH, NodByPH)}.

%%--------------------------------------------------------------------
%% @doc
%% Returns whether a node is a node pair
%% @end
%%--------------------------------------------------------------------
-spec is_node_pair(Node :: tree:tree_node()) ->
          boolean().
is_node_pair({node_pair, _Node1, _Node2}) -> true;
is_node_pair(_) -> false.

%%--------------------------------------------------------------------
%% @doc
%% Copies the properties stored in the nodes of OriTree into
%% the nodes of DestTree. Both trees must have the same exact
%% topology.
%% @end
%%--------------------------------------------------------------------
-spec copy_properties(tree:tree(),tree:tree()) -> tree:tree().
copy_properties(OriTree, DestTree) ->
    copy_properties({get_root_node(OriTree),
		     get_root_node(DestTree)}, OriTree, DestTree).

%%--------------------------------------------------------------------
%% @doc
%% Copies the properties stored in the subtree with root
%% in OriNode of OriTree into the subtree with root in
%% DestTree of DestTree. Both subtrees must have the same
%% exact topology.
%% @end
%%--------------------------------------------------------------------
-spec copy_properties({OriNode :: tree:tree_node(),
		       DestNode :: tree:tree_node()},
		      OriTree :: tree:tree(), DestTree :: tree:tree()) ->
			     tree:tree().
copy_properties({OriNode, DestNode}, OriTree, DestTree) ->
    ChildrenNodes = get_zipped_children(OriNode, DestNode, OriTree, DestTree),
    NewDestNode = copy_properties_for_node(OriNode, DestNode),
    NewDestTree = store_node(NewDestNode, DestTree),
    lists:foldl(fun (Nodes, DT) -> copy_properties(Nodes, OriTree, DT) end,
		NewDestTree, ChildrenNodes).

%%--------------------------------------------------------------------
%% @doc
%% Zips the children of Node1 (from Tree1) and Node2
%% (from Tree2), assuming they have the same number of
%% children.
%% @end
%%--------------------------------------------------------------------
-spec get_zipped_children(Node1 :: tree:tree_node(),
			  Node2 :: tree:tree_node(),
			  Tree1 :: tree:tree(),
			  Tree2 :: tree:tree()) ->
				 [{tree:tree_node(),tree:tree_node()}].
get_zipped_children(OriNode, DestNode, OriTree, DestTree) ->
    OriChildren = get_children(OriNode, OriTree),
    DestChildren = get_children(DestNode, DestTree),
    lists:zip(OriChildren, DestChildren).

%%--------------------------------------------------------------------
%% @doc
%% Copies the properties stored in OriNode into DestNode.
%% @end
%%--------------------------------------------------------------------
-spec copy_properties_for_node(OriNode :: tree:tree_node(),
			       DestNode :: tree:tree_node()) ->
				      tree:tree_node().
copy_properties_for_node(#tree_node{properties = Prop},
			 #tree_node{properties = Prop2} = DestNode) ->
    DestNode#tree_node{properties = dict:merge(fun (_, _, Val2) -> Val2 end, Prop, Prop2)}.

%%--------------------------------------------------------------------
%% @doc
%% Returns the placeholder for a node or node pair Node. That is,
%% an unique identifier for the Node, that is used as placeholder
%% in the original position of the node in its parent.
%% If Node is a node pair, two placeholders are returned.
%% @end
%%--------------------------------------------------------------------
-spec get_ph(Node :: tree:tree_node()) ->
		    {'node', tree:tree_ph()}
			| {'node_pair', tree:tree_ph(), tree:tree_ph()}.
get_ph({node_pair, #tree_node{ph = PH1}, #tree_node{ph = PH2}}) -> {node_pair, PH1, PH2};
get_ph(#tree_node{ph = PH}) -> {node, PH}.

%%--------------------------------------------------------------------
%% @doc
%% Returns the extra information stored on the tree.
%% This can be any term and is stored with {@link set_data/2}.
%% @end
%%--------------------------------------------------------------------
-spec get_data(Tree :: tree:tree()) -> Data :: any().
get_data(#tree{extra_data = Data}) -> Data.

%%--------------------------------------------------------------------
%% @doc
%% Sets the extra information stored on the tree.
%% This can be any term and is retrieved from the tree with
%% {@link get_data/1}.
%% @end
%%--------------------------------------------------------------------
-spec set_data(Data :: any(), Tree :: tree:tree()) -> NewTree :: tree:tree().
set_data(Data, Tree) -> Tree#tree{extra_data = Data}.

%%--------------------------------------------------------------------
%% @doc
%% Like {@link dict:fetch/2} but allows to specify a default value.
%% @end
%%--------------------------------------------------------------------
-spec dict_get(Key :: KeyType,
	       Dict :: dict:dict(KeyType, ValueType),
	       DefaultValue :: ValueType) -> ValueType.
dict_get(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
	{ok, Value} -> Value;
	error -> Default
    end.
