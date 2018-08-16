%%%-------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Searches the reflected calls to the functions whose name correspond
%%% to one of the functions declared in the behaviour_info(callbacks)
%%% and instantiates them.
%%% @end
%%% Created : 16 Jun 2015 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(refac_instantiate_calls).

-behaviour(gen_refac).

%% Include files
-include("wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1]).

-export([instantiate_calls/5]).

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
    ["Instance module name : "].

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
		user_inputs = [ModuleName]} = _Args) ->
    [ASTFuncs|_] = collect_callbacks(File),
    ArgList = hd(lists:map(fun wrangler_syntax:concrete/1, ASTFuncs)),
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

%%--------------------------------------------------------------------
%% @doc
%% Searches the reflected calls to the functions whose name correspond
%% to one of the functions declared in the behaviour_info(callbacks)
%% function of the file FileName, and statically hardcodes them to
%% point to ModuleName.
%% @spec instantiate_calls(TargetFileName :: string(),
%%                         DestModule :: string(),
%%                         SearchPaths :: [string()],
%%                         Editor :: wrangler_refacs:context(),
%%                         TabWidth :: integer()) ->
%%                             {'ok', UpdatedFiles :: [string()]}
%% @end
%%--------------------------------------------------------------------
instantiate_calls(FileName, ModuleName, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
	       search_paths=SearchPaths,
	       user_inputs = [ModuleName],
	       tabwidth=TabWidth},
    {ok, Res}=transform(Args),
    wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"").



%%%===================================================================
%%% Internal functions
%%%===================================================================

collect_callbacks(File) ->
    ?FULL_TD_TU([?COLLECT(?T("behaviour_info(callbacks) -> Funcs@@;"),
			  Funcs@@, true)],
		[File]).
