%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Implements functions to convert between Wrangler AST and tree
%%% @end
%%% Created :  1 Jun 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(ast_tree).

-export([ast_to_tree/5, is_expression_or_function/1, is_a_function/1,
	 get_fun_name/2, get_placeholders_from_node/1,
	 get_left_if_node_pair/1, get_right_if_node_pair/1, value/1,
	 reparse/1, replace_in_node/2, tree_to_ast/1,
	 tree_to_ast/2, show_node/1, breaks_funname/2,
	 get_macros_list/1, get_macro_info/1, is_function_or_macro/1]).

-export([get_file_name/1, get_search_paths/1, get_tab_width/1,
	 get_inscope_funs/1, get_module_info/1]).
-export([set_file_name/2, set_search_paths/2, set_tab_width/2,
	 set_inscope_funs/2, set_module_info/2]).

-record(tree_module_info, {file_name, search_paths, tab_width,
			   inscope_funs, module_info}).

-type tree_module_info() :: #tree_module_info{}. %% Record to store metadata in the tree.
                                                 %% Contains the following fields: <ul>
                                                 %% <li> `file_name' - the name of the file with the
                                                 %%                    source parsed to generate this
                                                 %%                    tree </li>
                                                 %% <li> `search_paths' - list of paths used by wrangler
                                                 %%                       used when parsing </li>
                                                 %% <li> `tab_width' - spaces per tab of the source </li>
                                                 %% <li> `inscope_funs' - list of external functions
                                                 %%                       in scope for the module </li>
                                                 %% <li> `module_info' - information about the module
                                                 %%                      returned by
                                                 %% {@link wrangler_ast_server:parse_annotate_file/5} </li></ul>

%%--------------------------------------------------------------------
%% @doc
%% Transforms a Syntax Tree into a tree with isolated nodes for
%% easier manipulation.
%% @end
%%--------------------------------------------------------------------
-spec ast_to_tree(AST :: wrangler_syntax:syntaxTree(),
		  FileName :: string(), ModuleInfo :: [{atom(), term()}],
		  SearchPaths :: [string()], TabWidth :: integer()) -> tree:tree().
ast_to_tree(AST, FileName, ModuleInfo, SearchPaths, TabWidth) ->
    set_file_name(
      FileName,
      set_search_paths(
	SearchPaths,
	set_tab_width(
	  TabWidth,
	  set_inscope_funs(
	    inscope_fun_name(ModuleInfo),
	    set_module_info(
	      ModuleInfo,
	      ast_to_tree_aux(AST, tree:new())))))).

%%--------------------------------------------------------------------
%% @doc
%% Gets the filename from the tree metadata record.
%% @end
%%--------------------------------------------------------------------
-spec get_file_name(Tree :: tree:tree()) -> string().
get_file_name(Tree) ->
    (get_data(Tree))#tree_module_info.file_name.

%%--------------------------------------------------------------------
%% @doc
%% Sets the filename in the tree metadata record.
%% @end
%%--------------------------------------------------------------------
-spec set_file_name(FileName :: string(), Tree :: tree:tree()) -> tree:tree().
set_file_name(FileName, Tree) ->
    set_data((get_data(Tree))#tree_module_info{file_name = FileName}, Tree).

%%--------------------------------------------------------------------
%% @doc
%% Gets the list of paths in the search path from the tree metadata record.
%% @end
%%--------------------------------------------------------------------
-spec get_search_paths(Tree :: tree:tree()) -> [string()].
get_search_paths(Tree) ->
    (get_data(Tree))#tree_module_info.search_paths.

%%--------------------------------------------------------------------
%% @doc
%% Sets the list of paths in the search path in the tree metadata record.
%% @end
%%--------------------------------------------------------------------
-spec set_search_paths(SearchPaths :: [string()], Tree :: tree:tree()) -> tree:tree().
set_search_paths(SearchPaths, Tree) ->
    set_data((get_data(Tree))#tree_module_info{search_paths = SearchPaths}, Tree).

%%--------------------------------------------------------------------
%% @doc
%% Gets the tab width from the tree metadata record.
%% @end
%%--------------------------------------------------------------------
-spec get_tab_width(Tree :: tree:tree()) -> integer().
get_tab_width(Tree) ->
    (get_data(Tree))#tree_module_info.tab_width.

%%--------------------------------------------------------------------
%% @doc
%% Sets the tab width in the tree metadata record.
%% @end
%%--------------------------------------------------------------------
-spec set_tab_width(TabWidth :: integer(), Tree :: tree:tree()) -> tree:tree().
set_tab_width(TabWidth, Tree) ->
    set_data((get_data(Tree))#tree_module_info{tab_width = TabWidth}, Tree).

%%--------------------------------------------------------------------
%% @doc
%% Gets the list of functions in scope from the tree metadata record.
%% @end
%%--------------------------------------------------------------------
-spec get_inscope_funs(Tree :: tree:tree()) -> [{atom(),atom(),integer()}].
get_inscope_funs(Tree) ->
    (get_data(Tree))#tree_module_info.inscope_funs.

%%--------------------------------------------------------------------
%% @doc
%% Sets the list of functions in scope in the tree metadata record.
%% @end
%%--------------------------------------------------------------------
-spec set_inscope_funs(InscopeFuns :: [{atom(),atom(),integer()}],
		       Tree :: tree:tree()) -> tree:tree().
set_inscope_funs(InscopeFuns, Tree) ->
    set_data((get_data(Tree))#tree_module_info{inscope_funs = InscopeFuns}, Tree).

%%--------------------------------------------------------------------
%% @doc
%% Gets the module info from the tree metadata record.
%% @end
%%--------------------------------------------------------------------
-spec get_module_info(Tree :: tree:tree()) -> [{atom(), term()}].
get_module_info(Tree) ->
    (get_data(Tree))#tree_module_info.module_info.

%%--------------------------------------------------------------------
%% @doc
%% Sets the module info in the tree metadata record.
%% @end
%%--------------------------------------------------------------------
-spec set_module_info(ModuleInfo :: [{atom(), term()}],
		      Tree :: tree:tree()) -> tree:tree().
set_module_info(ModuleInfo, Tree) ->
    set_data((get_data(Tree))#tree_module_info{module_info = ModuleInfo}, Tree).

%%--------------------------------------------------------------------
%% @doc
%% Gets metadata from the tree.
%% @end
%%--------------------------------------------------------------------
-spec get_data(Tree :: tree:tree()) -> tree_module_info().
get_data(Tree) ->
    case tree:get_data(Tree) of
	undefined -> #tree_module_info{};
	Else -> Else
    end.

%%--------------------------------------------------------------------
%% @doc
%% Adds the metadata to the tree.
%% @end
%%--------------------------------------------------------------------
-spec set_data(Data :: tree_module_info(), Tree :: tree:tree()) -> tree:tree().
set_data(Data, Tree) -> tree:set_data(Data, Tree).

%%--------------------------------------------------------------------
%% @doc
%% Auxiliar function to {@link ast_tree/1}. It uses the specified tree
%% as destination.
%% @end
%%--------------------------------------------------------------------
-spec ast_to_tree_aux(
          AST :: wrangler_syntax:syntaxTree(),
          Tree :: tree:tree()) -> tree:tree().
ast_to_tree_aux(AST, Tree) ->
    TreeNode = tree:get_root_node(Tree),
    ast_to_tree_aux_ph(AST, Tree, TreeNode).

%%--------------------------------------------------------------------
%% @doc
%% Auxiliar function to {@link ast_tree/1}. It uses the specified tree
%% as destination. Hangs the result from the placeholder TreePH.
%% @end
%%--------------------------------------------------------------------
-spec ast_to_tree_aux(AST :: wrangler_syntax:syntaxTree(),
                      TreePH :: tree:tree_ph(),
                      Tree :: tree:tree()) -> tree:tree().
ast_to_tree_aux(AST, TreePH, Tree) ->
    TreeNode = tree:new_node_ph(TreePH),
    ast_to_tree_aux_ph(AST, Tree, TreeNode).

%%--------------------------------------------------------------------
%% @doc
%% Auxiliar function to {@link ast_tree_aux/2} and
%% {@link ast_tree_aux/3}.
%% It stores the isolated root of the AST in the node TreeNode
%% and recursively processes each of the subtrees of the AST.
%% @end
%%--------------------------------------------------------------------
-spec ast_to_tree_aux_ph(AST :: wrangler_syntax:syntaxTree(),
                         Tree :: tree:tree(),
                         TreeNode :: tree:tree_node()) ->
				tree:tree().
ast_to_tree_aux_ph(AST, Tree, TreeNode) ->
    {IsolatedNode, {RevChildren, RevChildPlaceHolders, NewTreeNode}} =
	api_ast_traverse:mapfold_subtrees(
	  fun isolate_node/2, {[], [], TreeNode}, AST),
    NextLevel = reverse_zip(RevChildren, RevChildPlaceHolders),
    FinalTreeNode = tree:set_value(IsolatedNode, NewTreeNode),
    FinalTree = tree:store_node(FinalTreeNode, Tree),
    lists:foldl(fun ({X, Y}, T) -> ast_to_tree_aux(X, Y, T) end,
		FinalTree, NextLevel).

%%--------------------------------------------------------------------
%% @doc
%% Auxiliar function to {@link ast_to_tree_aux_ph/3}.
%% It stores the isolated root of the AST in the node TreeNode
%% and recursively processes each of the subtrees of the AST.
%% @end
%%--------------------------------------------------------------------
-spec isolate_node(ASTNode :: wrangler_syntax:syntaxTree(),
                   {RevChildren :: [wrangler_syntax:syntaxTree()],
                    RevChildPlaceHolders :: [tree:tree_ph()],
                    TreeNode :: tree:tree_node()}) ->
			  {tree:tree_ph(),
			   {RevChildren :: [wrangler_syntax:syntaxTree()],
			    RevChildPlaceHolders :: [tree:tree_ph()],
			    TreeNode :: tree:tree_node()}}.
isolate_node(ASTNode, {RevChildren, RevChildPlaceHolders, TreeNode}) ->
    {NewTreeNode, PH} = tree:get_child_ph(TreeNode),
    {PH, {[ASTNode|RevChildren], [PH|RevChildPlaceHolders], NewTreeNode}}.

%%--------------------------------------------------------------------
%% @doc
%% Zips two lists with the same length.
%% It returns the result reversed.
%% Equivalent to lists:reverse(lists:zip(L1, L2)) but more efficient
%% @see lists:zip/2
%% @end
%%--------------------------------------------------------------------
-spec reverse_zip(L1 :: list(X), L2 :: list(Y)) -> list({X, Y}).
reverse_zip(L1, L2) -> reverse_zip_aux(L1, L2, []).

%%--------------------------------------------------------------------
%% @doc
%% Auxiliar function to {@link reverse_zip/2}.
%% @end
%%--------------------------------------------------------------------
reverse_zip_aux([H1|T1], [H2|T2], Acc) ->
    reverse_zip_aux(T1, T2, [{H1, H2}|Acc]);
reverse_zip_aux([], [], Acc) -> Acc.

%%--------------------------------------------------------------------
%% @doc
%% Checks the category of the Node and returns true if it is
%% "expression" or if the node is a function declaration.
%% It returns false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_expression_or_function(Node :: tree:tree_node()) ->  boolean().
is_expression_or_function(TreeNode) ->
    is_expression(TreeNode) orelse is_a_function(TreeNode).

%%--------------------------------------------------------------------
%% @doc
%% Returns true if the node is a macro of a function application,
%% (for both its alternatives).
%% @end
%%--------------------------------------------------------------------
-spec is_function_or_macro(Node :: tree:tree_node()) ->  boolean().
is_function_or_macro(TreeNode) ->
    case tree:is_node_pair(TreeNode) of
	true -> {Node1, Node2} = tree:get_pair_tuple(TreeNode),
		is_function_or_macro(Node1) and is_function_or_macro(Node2);
	false -> Node = tree:get_value(TreeNode),
		 case wrangler_syntax:type(Node) of
		     application -> true;
		     macro -> true;
		     _ -> false
		 end
    end.

%%%--------------------------------------------------------------------
%% @doc
%% Checks the category of the Node and returns true if it is
%% "expression". It returns false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_expression(Node :: tree:tree_node()) ->
			   boolean().
is_expression(TreeNode) ->
    case tree:is_node_pair(TreeNode) of
	true -> {Node1, Node2} = tree:get_pair_tuple(TreeNode),
		is_expression(Node1) and is_expression(Node2);
	false -> Node = tree:get_value(TreeNode),
		 api_refac:syntax_category(Node) =:= expression
    end.

%%--------------------------------------------------------------------
%% @doc
%% It returns `true' if the node represents a function, otherwise
%% it returns `false'.
%% @end
%%--------------------------------------------------------------------
-spec is_a_function(Node :: tree:tree_node()) ->
			   boolean().
is_a_function(TreeNode) ->
    case tree:is_node_pair(TreeNode) of
	true -> {Node1, _Node2} = tree:get_pair_tuple(TreeNode),
		is_a_function(Node1);
	false -> Node = tree:get_value(TreeNode),
		 case wrangler_syntax:type(Node) of
		     function -> true;
		     _ -> false
		 end
    end.

%%--------------------------------------------------------------------
%% @doc
%% If the node represents a function it returns a tuple with the
%% name of the function as a string and the arity as a number.
%% The Tree must match the node if exclusive or be the left tree
%% if common.
%% @end
%%--------------------------------------------------------------------
-spec get_fun_name(Node :: tree:tree_node(),
		   Tree :: tree:tree()) ->
          {ok, {string(), non_neg_integer()}} | error.
get_fun_name(TreeNode, Tree) ->
    case tree:is_node_pair(TreeNode) of
	true -> {Node1, _Node2} = tree:get_pair_tuple(TreeNode),
		get_fun_name(Node1, Tree);
	false -> Node = tree:get_value(TreeNode),
		 case wrangler_syntax:type(Node) of
		     function ->
			 {NameAST, Arity} = wrangler_syntax_lib:analyze_function(
					      tree_to_ast(TreeNode, Tree)),
			 {ok, {atom_to_list(NameAST), Arity}};
		     _ -> error
		 end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Extracts placeholders from the node. It may be a node pair or
%% a simple node. If it is a node pair it uses the left alternative.
%% It uses the wrangler_tools to accumulate them.
%% @end
%%-------------------------------------------------------------------------
-spec get_placeholders_from_node(Nodes :: Node) -> [PH] when
      Node :: tree:tree_node(), PH :: term().
get_placeholders_from_node(Node) ->
    IndNode = tree:get_value(get_left_if_node_pair(Node)),
    lists:reverse(api_ast_traverse:fold_subtrees(
		    fun (X, Y) -> [X|Y] end, [], IndNode)).


%%-------------------------------------------------------------------------
%% @doc
%% Returns the left node if it is a node pair.
%% @end
%%-------------------------------------------------------------------------
-spec get_left_if_node_pair(tree:tree_node()) -> tree:tree_node().
get_left_if_node_pair(Node) ->
    case tree:is_node_pair(Node) of
	true -> {Node1, _Node2} = tree:get_pair_tuple(Node),
		Node1;
	false -> Node
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns the right node if it is a node pair.
%% @end
%%-------------------------------------------------------------------------
-spec get_right_if_node_pair(tree:tree_node()) -> tree:tree_node().
get_right_if_node_pair(Node) ->
    case tree:is_node_pair(Node) of
	true -> {_Node1, Node2} = tree:get_pair_tuple(Node),
		Node2;
	false -> Node
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns a "semantic" representation of the node, (which is used for
%% comparing two nodes and decide if they can be mergerd).
%% @end
%%-------------------------------------------------------------------------
-spec value(tree:tree_node()) -> any().
value(Node) ->
    case tree:is_node_pair(Node) of
	true -> {Node1, Node2} = tree:get_pair_tuple(Node),
		{node_pair, value(Node1), value(Node2)};
	false -> SynNode = tree:get_value(Node),
		 case count_children(SynNode) of
		     0 -> wrangler_prettypr:format(SynNode);
		     N -> {api_refac:syntax_category(SynNode), wrangler_syntax:type(SynNode), N}
		 end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Counts the number of subtrees in a syntaxTree node.
%% @end
%%-------------------------------------------------------------------------
-spec count_children(wrangler_syntax:syntaxTree()) -> non_neg_integer().
count_children(Node) ->
    api_ast_traverse:fold_subtrees(
      fun (_, Counter) -> Counter + 1 end, 0, Node).

%%-------------------------------------------------------------------------
%% @doc
%% Gets a list of the AST blocks containing macros and information about
%% the macros, records, and imports provided by them.
%% @end
%%-------------------------------------------------------------------------
-spec get_macros_list(Tree :: tree:tree()) -> [{MacroAST :: wrangler_syntax:syntaxTree(),
						MacroInfo :: [term()]}].
get_macros_list(Tree) ->
    {AST, FileName, TabWidth, SearchPaths} = get_info_from_tree(Tree),
    List = lists:filter(fun is_import_record_or_macro/1,
			wrangler_syntax:form_list_elements(AST)),
    NewList = group_by_ifdefs(List),
    lists:foldl(fun (PartialAST, Acc) ->
			imports_and_macros(FileName, TabWidth, SearchPaths, PartialAST, Acc)
		end, [], NewList).

%%-------------------------------------------------------------------------
%% @doc
%% It groups in sublists a list of ASTs representing macros. Each sublist
%% of the output will contain either the outter block of a if(n)def-endif
%% or a single macro.
%% @end
%%-------------------------------------------------------------------------
-spec group_by_ifdefs(ASTs :: [wrangler_syntax:syntaxTree()]) ->
			     GroupedASTs :: [[wrangler_syntax:syntaxTree()]].
group_by_ifdefs(List) ->
    group_by_ifdefs({}, 0, List, []).

%%-------------------------------------------------------------------------
%% @doc
%% Auxiliar function of {@link group_by_ifdefs/1}. It groups in sublists
%% a list of ASTs representing macros. Each sublist of the output will
%% contain either the outter block of a if(n)def-endif or a single macro.
%% Type is `{}' if it has not been calculated and contains as only element
%% the type of macro of the head of the ASTFeed. Level contains the number
%% of if(n)def-endif blocks in which we are inside currently. ASTFeed
%% contains the remaining macros to group. ASTAcc contains the list
%% of macros that will make the begining of the next sublist reversed.
%% @end
%%-------------------------------------------------------------------------
-spec group_by_ifdefs(Type :: {} | {atom()}, Level :: non_neg_integer(),
		      ASTFeed :: [wrangler_syntax:syntaxTree()],
		      ASTAcc :: [wrangler_syntax:syntaxTree()]) ->
			     GroupedASTs :: [[wrangler_syntax:syntaxTree()]].
group_by_ifdefs({}, 0, [], Acc) ->
    [lists:reverse(Acc)];
group_by_ifdefs({}, Level, [Head|_] = List, Acc) ->
    group_by_ifdefs({attrib_type(Head)}, Level, List, Acc);
group_by_ifdefs({ifdef}, Level, [Head|Tail], Acc) ->
    group_by_ifdefs({}, Level + 1, Tail, [Head|Acc]);
group_by_ifdefs({ifndef}, Level, [Head|Tail], Acc) ->
    group_by_ifdefs({}, Level + 1, Tail, [Head|Acc]);
group_by_ifdefs({endif}, 1, [Head|Tail], Acc) ->
    [lists:reverse([Head|Acc])
     |group_by_ifdefs({}, 0, Tail, [])];
group_by_ifdefs({endif}, Level, [Head|Tail], Acc) when Level > 1 ->
    group_by_ifdefs({}, Level - 1, Tail, [Head|Acc]);
group_by_ifdefs({_}, 0, [Head|Tail], []) ->
    [[Head]|group_by_ifdefs({}, 0, Tail, [])];
group_by_ifdefs({_}, Level, [Head|Tail], Acc) when Level > 0 ->
    group_by_ifdefs({}, Level, Tail, [Head|Acc]).

%%-------------------------------------------------------------------------
%% @doc
%% Obtains the type of macro represeted by Node.
%% @end
%%-------------------------------------------------------------------------
-spec attrib_type(Node :: wrangler_syntax:syntaxTree()) -> atom().
attrib_type(Node) ->
    wrangler_syntax:atom_value(wrangler_syntax:attribute_name(Node)).

%%-------------------------------------------------------------------------
%% @doc
%% Extracts information about the macro in ASTNode and adds it to the Acc.
%% @end
%%-------------------------------------------------------------------------
-spec imports_and_macros(FileName :: string(), TabWidth :: integer(), SearchPaths :: [string()],
			 ASTNode :: wrangler_syntax:syntaxTree(),
			 Acc0 :: Acc) -> Acc1 :: Acc when
      Acc :: [{MacroAST :: wrangler_syntax:syntaxTree(), MacroInfo :: [term()]}].
imports_and_macros(FileName, TabWidth, SearchPaths, PartialAST, Acc) ->
    WPartialAST = wrangler_syntax:form_list(PartialAST),
    {_, Info} = reparse_annotate_file(WPartialAST, FileName, TabWidth, SearchPaths),
    {Defs, _} = parse_for_macro_info(WPartialAST, FileName, TabWidth, SearchPaths),
    [{PartialAST, [flatten_imported_funs(Info),
		   flatten_macros(Defs),
		   flatten_records(Info)]}|Acc].

%%-------------------------------------------------------------------------
%% @doc
%% Extracts the imported functions from the ModuleInfo as returned by
%% {@link wrangler_ast_server:parse_annotate_file/5}.
%% @end
%%-------------------------------------------------------------------------
-spec flatten_imported_funs(ModuleInfo :: [{atom(), term()}]) -> [{atom(), integer()}].
flatten_imported_funs(Info) ->
    ordsets:from_list(lists:append(
		     [lists:append(
			[Funs || {_Mod, Funs} <- List])
                      || {imports, List} <- Info])).

%%-------------------------------------------------------------------------
%% @doc
%% Transforms a list of macros from the macro definition as returned by
%% {@link parse_for_macro_info/4} into tuples with the name and the arity.
%% It removes the automatic macros like `?MODULE'.
%% @end
%%-------------------------------------------------------------------------
-spec flatten_macros([any()]) -> [any()].
flatten_macros(MDefs) ->
    ordsets:subtract(ordsets:from_list(lists:flatmap(fun flatten_macros_aux/1, MDefs)),
		     ordsets:from_list([{'FILE', 0}, {'LINE', 0}, {'MACHINE', 0}, {'BEAM', 0},
					{'MODULE', 0}, {'MODULE_STRING', 0}])).

%%-------------------------------------------------------------------------
%% @doc
%% Transforms a macro from the macro definition as returned by
%% {@link parse_for_macro_info/4} into a tuple with the name and the arity.
%% @end
%%-------------------------------------------------------------------------
-spec flatten_macros_aux(MacroDef :: {{any(), atom()}, 'none' | [any()]}) ->
				[{MacroName :: atom(), MacroArity :: non_neg_integer()}].
flatten_macros_aux({{_,Name}, {Args, _Toks}}) ->
    [{Name, length_or_none(Args)}];
flatten_macros_aux({{_, Name}, ArgToks}) when is_list(ArgToks) ->
    [{Name, length_or_none(Args)} || {_Arity, {Args, _Toks}} <- ArgToks];
flatten_macros_aux({{_, _Name}, _ArgToks}) -> [].

%%-------------------------------------------------------------------------
%% @doc
%% Returns the arity of a macro from the macro definition as returned by
%% {@link parse_for_macro_info/4}.
%% @end
%%-------------------------------------------------------------------------
-spec length_or_none(Args :: 'none' | [any()]) -> non_neg_integer().
length_or_none(none) -> 0;
length_or_none(Args) -> length(Args).

%%-------------------------------------------------------------------------
%% @doc
%% Extracts the record name list from the ModuleInfo as returned by
%% {@link wrangler_ast_server:parse_annotate_file/5}.
%% @end
%%-------------------------------------------------------------------------
-spec flatten_records(ModuleInfo :: [{atom(), term()}]) -> [atom()].
flatten_records(Info) ->
    ordsets:from_list(lists:append(
			[[RecordName || {RecordName, _Fields} <- List]
			 || {records, List} <- Info])).

%%-------------------------------------------------------------------------
%% @doc
%% Returns true if the AST represents a macro supported by this refactoring.
%% @end
%%-------------------------------------------------------------------------
-spec is_import_record_or_macro(AST :: wrangler_syntax:syntaxTree()) -> boolean().
is_import_record_or_macro(PartialAST) ->
    case wrangler_syntax:type(PartialAST) of
	attribute -> Name = wrangler_syntax:attribute_name(PartialAST),
		     case wrangler_syntax:type(Name) of
			 atom -> lists:member(wrangler_syntax:atom_value(Name),
					      [include_lib, include, define, record,
					       ifdef, ifndef, else, endif]);
			 _ -> false
		     end;
	_ -> false
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Reparses the AST in Tree to renew its information (like attributes). It
%% copies the node properties stored in the previous node. It requires that
%% the AST topology before and after reparsing is the same.
%% @end
%%-------------------------------------------------------------------------
-spec reparse(Tree :: tree:tree()) -> tree:tree().
reparse(Tree) ->
    {AST, FileName, TabWidth, SearchPaths} = get_info_from_tree(Tree),
    {NewAST, InfoL} = reparse_annotate_file(AST, FileName, TabWidth, SearchPaths),
    NewTree = ast_to_tree(NewAST, FileName, InfoL, SearchPaths, TabWidth),
    tree:copy_properties(Tree, NewTree).

%%-------------------------------------------------------------------------
%% @doc
%% Parses the AST in Tree in order to extract information about the macros defined and
%% used. It returns the macro information gathered by {@link wrangler_epp:parse_file/5}.
%% @end
%%-------------------------------------------------------------------------
-spec get_macro_info(Tree :: tree:tree()) ->
			    {[{wrangler_syntax:syntaxTree(), term()}], [term()]}.
get_macro_info(Tree) ->
    {AST, FileName, TabWidth, SearchPaths} = get_info_from_tree(Tree),
    parse_for_macro_info(AST, FileName, TabWidth, SearchPaths).

%%-------------------------------------------------------------------------
%% @doc
%% Parses an AST in order to extract information about the macros defined and
%% used. It returns the macro information gathered by {@link wrangler_epp:parse_file/5}.
%% @end
%%-------------------------------------------------------------------------
-spec parse_for_macro_info(AST :: wrangler_syntax:syntaxTree(), FileName :: string(),
			   TabWidth :: integer(), SearchPaths :: [string()]) ->
				  {[{wrangler_syntax:syntaxTree(), term()}], [term()]}.
parse_for_macro_info(AST, FileName, TabWidth, SearchPaths) ->
    {TempName, FileFormat} = temporarily_write_file(FileName, TabWidth, AST),
    {MDefs, MUses} = case wrangler_epp:parse_file(TempName, SearchPaths, [], TabWidth, FileFormat) of
			 {ok, _, {MD, MU}} -> {MD, MU};
			 _ -> throw({error, "Refactoring failed because Wrangler could not parse the target module."})
		     end,
    file:delete(TempName),
    {MDefs, MUses}.

%%-------------------------------------------------------------------------
%% @doc
%% Extracts the AST and some metadata stored in the tree.
%% @end
%%-------------------------------------------------------------------------
-spec get_info_from_tree(Tree :: tree:tree()) ->
				{AST :: wrangler_syntax:syntaxTree(), FileName :: string(),
				 TabWidth :: integer(), SearchPaths :: [string()]}.
get_info_from_tree(Tree) ->
    AST = tree_to_ast(Tree),
    FileName = get_file_name(Tree),
    TabWidth = get_tab_width(Tree),
    SearchPaths = get_search_paths(Tree),
    {AST, FileName, TabWidth, SearchPaths}.

%%-------------------------------------------------------------------------
%% @doc
%% Reparses the AST to renew the information and attributes gathered during parsing.
%% @end
%%-------------------------------------------------------------------------
-spec reparse_annotate_file(AST :: wrangler_syntax:syntaxTree(), FileName :: string(),
			    TabWidth :: integer(), SearchPaths :: [string()]) ->
				   {NewAST :: wrangler_syntax:syntaxTree(),
				    ModInfo :: [{atom(), term()}]}.
reparse_annotate_file(AST, FileName, TabWidth, SearchPaths) ->
    {TempName, FileFormat} = temporarily_write_file(FileName, TabWidth, AST),
    {ok, {NewAST, InfoL}} = wrangler_ast_server:parse_annotate_file(TempName, true, SearchPaths, TabWidth, FileFormat),
    file:delete(TempName),
    {NewAST, InfoL}.

%%-------------------------------------------------------------------------
%% @doc
%% Writes an AST tree to a file in the dir of FileName, but with a name
%% that is not taken by another file.
%% @end
%%-------------------------------------------------------------------------
-spec temporarily_write_file(FileName :: string(), TabWidth :: integer(),
			     AST :: wrangler_syntax:syntaxTree()) ->
				    {string(), 'dos' | 'mac' | 'unix'}.
temporarily_write_file(FileName, TabWidth, AST) ->
    FileFormat = wrangler_misc:file_format(FileName),
    TempName = test_server:temp_name(FileName),
    case file:write_file(TempName, wrangler_prettypr:print_ast(FileFormat, AST, TabWidth)) of
	ok -> ok;
	Error -> exit(Error)
    end,
    {TempName, FileFormat}.

%%-------------------------------------------------------------------------
%% @doc
%% Extracts an AST from a Tree that contains an AST.
%% @end
%%-------------------------------------------------------------------------
-spec tree_to_ast(Tree :: tree:tree()) -> AST :: wrangler_syntax:syntaxTree().
tree_to_ast(Tree) ->
    tree_to_ast(tree:get_root_node(Tree), Tree).

%%-------------------------------------------------------------------------
%% @doc
%% Extracts an AST from a Tree that contains an AST, taking Node as the root node.
%% @end
%%-------------------------------------------------------------------------
-spec tree_to_ast(Node :: tree:tree_node(), Tree :: tree:tree()) -> wrangler_syntax:syntaxTree().
tree_to_ast(Node, Tree) ->
    ASTNode = tree:get_value(Node),
    Children = tree:get_children(Node, Tree),
    ReplacedChildren = lists:map(fun (X) -> tree_to_ast(X, Tree) end, Children),
    replace_in_node(ReplacedChildren, ASTNode).

%%-------------------------------------------------------------------------
%% @doc
%% Replaces the list of nodes Replacements in the subtrees of Node.
%% @end
%%-------------------------------------------------------------------------
-spec replace_in_node(Replacements :: [wrangler_syntax:syntaxTree()],
		      Node :: wrangler_syntax:syntaxTree()) ->
			     wrangler_syntax:syntaxTree().
replace_in_node(Replacements, Node) ->
    {Replaced, _} = api_ast_traverse:mapfold_subtrees(fun (_, [El|Rest]) -> {El, Rest} end, Replacements, Node),
    Replaced.

%%-------------------------------------------------------------------------
%% @doc
%% Returns true if separating Node from ParentNode (assuming ParentNode is the parent node of Node),
%% would separate a function call (in some form) from the indicator of the name/arity/module of
%% the destination function. 
%% @end
%%-------------------------------------------------------------------------
-spec breaks_funname(NodePair :: tree:tree_node(), ParentNode :: tree:tree_node()) -> boolean().
breaks_funname(NodePair, PNode) ->
    {Node1, Node2} = tree:get_pair_tuple(NodePair),
    breaks_funname_aux(Node1, PNode) orelse
	breaks_funname_aux(Node2, PNode).

%%-------------------------------------------------------------------------
%% @doc
%% Returns true if separating Node from ParentNode (assuming ParentNode is the parent node of Node),
%% would separate a function call (in some form) from the indicator of the name/arity/module of
%% the destination function.
%% It assumes Node is not a node pair.
%% @end
%%-------------------------------------------------------------------------
-spec breaks_funname_aux(Node :: tree:tree_node(), ParentNode :: tree:tree_node()) -> boolean().
breaks_funname_aux(Node, PNode) ->
    PNodeVal = tree:get_value(PNode),
    case wrangler_syntax:type(PNodeVal) of
	application -> {node, Ref} = tree:get_ph(Node),
		       wrangler_syntax:application_operator(PNodeVal) =:= Ref;
	implicit_fun -> true;
	module_qualifier -> true;
	arity_qualifier -> true;
	_ -> false
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Extracts from ModuleInfo (obtained from {@link wrangler_syntax_lib:analyze_forms/1}),
%% the list of functions in the scope of the module that are not defined in the module.
%% @end
%%-------------------------------------------------------------------------
-spec inscope_fun_name(ModuleInfo :: [{atom(), term()}]) ->
			      sets:set({FunctionName :: atom(), Arity :: integer()}).
inscope_fun_name(ModInfo) ->
    {value, {_, ModuleName}} = lists:keysearch(module, 1, ModInfo),
    sets:from_list([{Name, Arity} || {Mod, Name, Arity} <- api_refac:inscope_funs(ModInfo),
				     Mod =/= ModuleName]).

%%-------------------------------------------------------------------------
%% @doc
%% Transforms a node or node pair into a compact representation for debugging.
%% @end
%%-------------------------------------------------------------------------
-spec show_node(tree:tree_node()) -> {{'node', tree:tree_ph()} |
				      {'node_pair', tree:tree_ph(), tree:tree_ph()},
				      any()}.
show_node(Node) -> {tree:get_ph(Node), ast_tree:value(Node)}.
