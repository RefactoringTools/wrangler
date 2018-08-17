%%%-------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Creates a file with the skeleton of a behaviour instance.
%%% It adds the behaviour declaration if it is not there already.
%%% @end
%%% Created :  7 Jul 2015 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(refac_create_behav_instance).

-behaviour(gen_refac).

%% Include files
-include("wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1]).

-export([create_behav_instance/5]).

%%%===================================================================
%%% gen_refac callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prompts for parameter inputs
%%
%% @spec input_par_prompts() -> [string()]
%% @end
%%--------------------------------------------------------------------
input_par_prompts() ->
    ["Module name of the behaviour instance to create : "].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Select the focus of the refactoring.
%%
%% @spec select_focus(Args :: #args{}) ->
%%                {ok, syntaxTree()} |
%%                {ok, none}
%% @end
%%--------------------------------------------------------------------
select_focus(_Args) ->
    {ok, none}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec check_pre_cond(Args :: #args{}) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
check_pre_cond(_Args) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Selective transformation or not.
%%
%% @spec selective() -> boolean()
%% @end
%%--------------------------------------------------------------------
selective() ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function does the actual transformation.
%%
%% @spec transform(Args :: #args{}) -> 
%%            {ok, [{filename(), filename(), syntaxTree()}]} |
%%            {error, Reason}
%% @end
%%--------------------------------------------------------------------
transform(#args{current_file_name = File,
		user_inputs = [DestModule],
		search_paths = SearchPaths} = _Args) ->
    OriMod = filename:basename(File, ".erl"),
    DestFile = case wrangler_misc:modname_to_filename(list_to_atom(DestModule), SearchPaths) of
		   {ok, OldFileName} ->
		       OldFileName;
		   {error, _Msg} ->
		       NewFileName = filename:dirname(File) ++ "/" ++
			   DestModule ++ ".erl",
		       wrangler_misc:create_files([NewFileName]),
		       NewFileName
	       end,
    case api_refac:is_behaviour_instance_of(DestFile, list_to_atom(OriMod)) of
	false -> {ok,AST} = api_refac:get_ast(DestFile),
		 BehavDefAST = create_behav_use_declaration(OriMod),
		 {ok, [{{DestFile, DestFile}, api_refac:insert_an_attr(AST, BehavDefAST)}]};
	true -> {ok, []}
    end.

create_behav_use_declaration(OriMod) ->
    wrangler_syntax:attribute(wrangler_syntax:atom(behaviour),
                              [wrangler_syntax:atom(OriMod)]).

%%--------------------------------------------------------------------
%% @doc
%% Creates a file with the skeleton of a behaviour instance.
%% It adds the behaviour declaration if it is not there already.
%% @spec create_behav_instance(TargetFileName :: string(),
%%                             DestModule :: atom(),
%%                             SearchPaths :: [string()],
%%                             Editor :: wrangler_refacs:context(),
%%                             TabWidth :: integer()) ->
%%                                 {'ok', UpdatedFiles :: [string()]}
%% @end
%%--------------------------------------------------------------------
create_behav_instance(FileName, DestModule, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
	       search_paths=SearchPaths,
	       user_inputs = [atom_to_list(DestModule)],
	       tabwidth=TabWidth},
    {ok, Res}=transform(Args),
    wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"").


