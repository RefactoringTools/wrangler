%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Top level for the behaviour generation refactoring.
%%% @end
%%% Created : 21 Jun 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(infer_behaviour).

-export([infer_behaviour/6]).

-include("../../include/wrangler_internal.hrl").

%%-------------------------------------------------------------------------
%% @doc
%% Takes two input files (LeftFile and RightFile) and an output
%% file (OutputFile), and tries to create a behaviour definition in
%% OutputFile with the common parts of the inputs files while
%% transforming the input files into behaviour instances of OutputFile.
%% @end
%%-------------------------------------------------------------------------
-spec infer_behaviour(LeftFile :: string(), RightFile :: string(),
		      OutputFile :: string(), SearchPaths :: [string()],
		      Editor :: atom(), TabWidth :: non_neg_integer()) -> {'ok',[any()]}.
infer_behaviour(LeftFile, RightFile, OutputFile, SearchPaths, Editor, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:infer_behaviour(~p, ~p, ~p, ~p, ~p, ~p).\n",
		 [?MODULE, LeftFile, RightFile, OutputFile, SearchPaths, Editor, TabWidth]),
    case get_file_name(LeftFile, SearchPaths) of
	{ok, LeftFileName} ->
	    case get_file_name(RightFile, SearchPaths) of
		{ok, RightFileName} ->		    
		    OutputFileName = get_target_file_name(LeftFileName, OutputFile),
		    OutputModName = list_to_atom(filename:basename(OutputFileName, ".erl")),
		    case filelib:is_file(OutputFileName) of
			false -> create_new_file(OutputFileName, OutputModName),
				 infer_behaviour_aux(LeftFileName, RightFileName, OutputFileName,
						     SearchPaths, emacs, TabWidth);
			true -> throw({error, "File \"" ++ OutputFileName ++ "\" already exists."})
		    end;
		{error, Reason} ->
		    throw({error, lists:flatten(Reason)})
	    end;
	{error, Reason} ->
	    throw({error, lists:flatten(Reason)})
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Returns the file name for ModOrFileName, which may or may not be
%% a file name or a module name.
%% @end
%%-------------------------------------------------------------------------
-spec get_file_name(ModOrFileName :: string() | atom() | any(),
		    SearchPaths :: [string()]) -> string().
get_file_name(ModOrFileName, SearchPaths) ->
    case filelib:is_file(ModOrFileName) of
	true ->
	    {ok, ModOrFileName};
	false ->
	    wrangler_misc:modname_to_filename(
	      tolerant_list_to_atom(ModOrFileName), SearchPaths)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Transforms a list into an atom or leaves it as is if it already
%% is an atom.
%% @end
%%-------------------------------------------------------------------------
-spec tolerant_list_to_atom(Atom :: atom() | string()) -> atom().
tolerant_list_to_atom(Atom) when is_atom(Atom) -> Atom;
tolerant_list_to_atom(List) when is_list(List) -> list_to_atom(List).

%%-------------------------------------------------------------------------
%% @doc
%% Returns a file name to use as target file, it may or may not exist.
%% If no file name is provided the dir of CurrentFName is used.
%% @end
%%-------------------------------------------------------------------------
-spec get_target_file_name(CurrentFName :: string(),
			   TargetModOrFileName :: atom() | string() | any()) ->
				  binary() | string().
get_target_file_name(CurrentFName, TargetModOrFileName) ->
    ErrMsg = {error, "Illegal target module/file name."},
    TargetModName = case filename:extension(TargetModOrFileName) of
			".erl" -> filename:basename(TargetModOrFileName, ".erl");
			[] -> filename:basename(TargetModOrFileName, ".erl");
			_ -> throw(ErrMsg)
		    end,
    TargetFileName = filename:join([filename:dirname(CurrentFName), filename:dirname(TargetModOrFileName),
				    TargetModName ++ ".erl"]),
    case api_refac:is_fun_name(TargetModName) of
	true -> case TargetFileName of
		    _ when is_binary(TargetFileName) -> binary_to_list(TargetFileName);
		    _ -> TargetFileName
		end;
	_ -> throw(ErrMsg)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Creates an almost empty new file with file name TargetFName.
%% @end
%%-------------------------------------------------------------------------
-spec create_new_file(TargetFName :: binary() | string(),
		      TargetModName :: atom()) -> 'ok'.
create_new_file(TargetFName, TargetModName) ->
    S = "-module("++atom_to_list(TargetModName)++").",
    case file:write_file(TargetFName, list_to_binary(S)) of 
	ok -> ok;
	{error, Reason} ->
	    Msg = io_lib:format("Wrangler could not write to file ~s: ~w \n",
				[TargetFName, Reason]),
	    throw({error, Msg})
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Auxiliar function to {@link infer_behaviour/6}, does the actual
%% transformation but expects parameters to have more strict types,
%% and the destination file to exist.
%% @end
%%-------------------------------------------------------------------------
-spec infer_behaviour_aux(LeftFile :: string(), RightFile :: string(),
			  OutputFile :: string(), SearchPaths :: [string()],
			  Editor :: atom(), TabWidth :: non_neg_integer()) -> {'ok',[any()]}.
infer_behaviour_aux(LeftFile, RightFile, OutputFile, SearchPaths, Editor, TabWidth) ->
    OutModule = list_to_atom(filename:basename(OutputFile, ".erl")),
    {OriLeftTree, OriRightTree} =
	get_trees(LeftFile, RightFile, SearchPaths, TabWidth),
    {LeftTree, RightTree} = add_blocks_to_trees(OriLeftTree, OriRightTree),
    {Mapping, CommClus, ExLeftClus, ExRightClus} = trees_to_clusters(2, LeftTree, RightTree),
    LinkInfo = cluster_linking:link_clusters(CommClus, ExLeftClus, ExRightClus,
					     LeftTree, RightTree, Mapping),
    [CommTree, ExLeftTree, ExRightTree] = cluster_mapping:clusters_to_ast(LinkInfo, OutModule),
    wrangler_write_file:write_refactored_files([{{File, File}, AST}
						|| {File, AST} <- [{LeftFile, ExLeftTree},
								   {RightFile, ExRightTree},
								   {OutputFile, CommTree}]],
					       Editor, TabWidth, "").

%%-------------------------------------------------------------------------
%% @doc
%% Parses LeftFile and RightFile and creates a tree with detached
%% nodes for each.
%% @end
%%-------------------------------------------------------------------------
-spec get_trees(LeftFile :: string(), RightFile :: string(),
		SearchPaths :: [string()], TabWidth :: non_neg_integer()) -> {tree:tree(),tree:tree()}.
get_trees(LeftFile, RightFile, SearchPaths, TabWidth) ->
    {ok, {LeftAST, InfoL}} = wrangler_ast_server:parse_annotate_file(LeftFile, true, SearchPaths, TabWidth),
    {ok, {RightAST, InfoR}} = wrangler_ast_server:parse_annotate_file(RightFile, true, SearchPaths, TabWidth),
    LeftTree = ast_tree:ast_to_tree(LeftAST, LeftFile, InfoL, SearchPaths, TabWidth),
    RightTree = ast_tree:ast_to_tree(RightAST, RightFile, InfoR, SearchPaths, TabWidth),
    {LeftTree, RightTree}.

%%-------------------------------------------------------------------------
%% @doc
%% Makes a first pass of the clustering algorithm and tries to create
%% artificial blocks to improve the result.
%% @end
%%-------------------------------------------------------------------------
-spec add_blocks_to_trees(LeftTree :: tree:tree(), RightTree :: tree:tree()) -> {tree:tree(),tree:tree()}.
add_blocks_to_trees(LeftTree, RightTree) ->
    {Mapping, CommCluster, ExLeftClus, ExRightClus} = trees_to_clusters(1, LeftTree, RightTree),
    {NewLeftTree, NewRightTree} = create_blocks(LeftTree, RightTree, Mapping,
						CommCluster, ExLeftClus, ExRightClus),
    fix_blocks(NewLeftTree, NewRightTree, Mapping).

%%-------------------------------------------------------------------------
%% @doc
%% Maps LeftTree and RightTree, and returns the Mapping, and three clusters:
%% the common one, the exclusive to LeftTree, and the exclusive to RightTree.
%% The frontiers of the clusters are fixed slightly differently depending on
%% the Pass.
%% @end
%%-------------------------------------------------------------------------
-spec trees_to_clusters(Pass :: 1 | 2, LeftTree :: tree:tree(), RightTree :: tree:tree()) ->
			       {Mapping :: da_map:da_map(tree:tree_node(),tree:tree_node()),
				CommCluster :: cluster_dict:cluster_dict(tree:tree_node()),
				ExLeftClus :: cluster_dict:cluster_dict(tree:tree_node()),
				ExRightClus :: cluster_dict:cluster_dict(tree:tree_node())}.
trees_to_clusters(Pass, LeftTree, RightTree) ->
    Mapping = tree_mapping:mapping(LeftTree, RightTree),
    tree_clustering:cluster(Pass, Mapping, LeftTree, RightTree).

%%-------------------------------------------------------------------------
%% @doc
%% Creates artificial blocks to group several functions into one where
%% convenient.
%% @end
%%-------------------------------------------------------------------------
-spec create_blocks(LeftTree :: tree:tree(), RightTree :: tree:tree(),
		    Mapping :: da_map:da_map(tree:tree_node(),tree:tree_node()),
		    CommClus :: cluster_dict:cluster_dict(tree:tree_node()),
		    ExLeftClus :: cluster_dict:cluster_dict(tree:tree_node()),
		    ExRightClus :: cluster_dict:cluster_dict(tree:tree_node())) -> {tree:tree(),tree:tree()}.
create_blocks(LeftTree, RightTree, Mapping, CommCluster, ExLeftClus, ExRightClus) ->
    {NewLeftTree, NewRightTree} = fun_blocks:add_blocks_to_trees(LeftTree, RightTree, Mapping,
								 CommCluster, ExLeftClus, ExRightClus),
    {ast_tree:reparse(NewLeftTree),
     ast_tree:reparse(NewRightTree)}.

%%-------------------------------------------------------------------------
%% @doc
%% Removes or fixes those blocks that will both return and export vars
%% since this cannot be solved at postprocessing.
%% @end
%%-------------------------------------------------------------------------
-spec fix_blocks(LeftTree :: tree:tree(), RightTree :: tree:tree(),
		 Mapping :: da_map:da_map(tree:tree_node(),tree:tree_node())) ->
			{tree:tree(),tree:tree()}.
fix_blocks(LeftTree, RightTree, Mapping) ->
    {NewLeftTree, NewRightTree} = fun_blocks_fix:export_variables(LeftTree, RightTree, Mapping),
    {ast_tree:reparse(NewLeftTree), ast_tree:reparse(NewRightTree)}.

