%%%-------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela
%%% @doc
%%% Searches the reflected calls to the list of functions
%%% provided and replaces them with concrete calls.
%%% @end
%%% Created : 16 Jun 2015 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(refac_instantiate_calls).

-behaviour(gen_refac).

%% Include files
-include("../../include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1]).

-export([instantiate_calls/6]).

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
    ["Instance module name : ",
     "List of functions [{Fun, Arity}, ... ] : "].

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
%% @spec check_pre_cond(Args#args{}) -> ok | {error, Reason}
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
    true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function does the actual transformation.
%%
%% @spec transform(Args::#args{}) -> 
%%            {ok, [{filename(), filename(), syntaxTree()}]} |
%%            {error, Reason}
%% @end
%%--------------------------------------------------------------------
transform(#args{current_file_name = File,
		user_inputs = [ModuleName, StrArgList]} = _Args) ->
    ArgList = wrangler_syntax:concrete(
		wrangler_misc:parse_annotate_expr(StrArgList)),
    ?STOP_TD_TP([?RULE(
		    ?T("Mod@:Func@(Args@@)"),
		    begin
			?TO_AST(ModuleName ++ ":Func@(Args@@)",
				wrangler_syntax:get_pos(_This@))
		    end,
		    begin
			ModuleName =/= lists:flatten(?PP(Mod@))
			    andalso (wrangler_syntax:type(Mod@) =/= atom)
			    andalso lists:member({list_to_atom(lists:flatten(?PP(Func@))),
						  length(Args@@)}, ArgList)
		    end)],
		[File]).

instantiate_calls(FileName, ModuleName, ArgList, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
	       search_paths=SearchPaths,
	       user_inputs = [ModuleName, ArgList],
	       tabwidth=TabWidth},
    {ok, Res}=transform(Args),
    wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"").



%%%===================================================================
%%% Internal functions
%%%===================================================================
