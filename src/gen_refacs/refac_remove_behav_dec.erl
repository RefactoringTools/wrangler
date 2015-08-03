%%%-------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela
%%% @doc
%%% Removes the callback declaration from a module
%%% and arity specified.
%%% @end
%%% Created : 10 Jun 2015 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(refac_remove_behav_dec).

-behaviour(gen_refac).

%% Include files
-include("../../include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1]).

-export([remove_behav_dec/4]).

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
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Select the focus of the refactoring.
%%
%% @spec select_focus(Args::#args{}) ->
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
transform(#args{current_file_name = File} = _Args) ->
    ?STOP_TD_TP([?RULE(
		    ?T("behaviour_info(_Args@) -> _Body@@;"),
		    begin
			wrangler_syntax:empty_node()
		    end,
		    true)],
		[File]).

%%--------------------------------------------------------------------
%% @doc
%% Removes the callback declaration from a module
%% and arity specified.
%% @spec remove_behav_dec(TargetFileName :: string(),
%%                        SearchPaths :: [string()],
%%                        Editor :: wrangler_refacs:context(),
%%                        TabWidth :: integer()) ->
%%                               {'ok', UpdatedFiles :: [string()]}
%% @end
%%--------------------------------------------------------------------
remove_behav_dec(FileName, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
	       search_paths=SearchPaths,
	       user_inputs = [],
	       tabwidth=TabWidth},
    {ok, Res}=transform(Args),
    wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"").

