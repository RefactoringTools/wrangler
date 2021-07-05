%% @hidden
%% @private
-module(refac_new_dummy_argument2).

-behaviour(gen_refac_2).

-compile(export_all).

%% Include files
-include("wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1,
         check_pre_cond/1, selective/0,
         transform_orig_file/1, transform_client_files/1]).

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
    ["Module:",
     "Function:",
     "DummyPar:"].

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
%% @spec check_pre_cond(Args::#args{}) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
check_pre_cond(_Args=#args{current_file_name=File,
                      user_inputs=[M0, F0, A0, DummyPar]}) ->
	M = list_to_atom(M0),
	F = list_to_atom(F0),
	A = list_to_integer(A0),
	Dummy = list_to_atom(DummyPar),
    Contains = ?STOP_TD_TU([?COLLECT(?T("f@(Args@@) when Guard@@-> Bs@@;"),
		                    wrangler_misc:collect_var_names(_This@),
                            api_refac:fun_define_info(f@) == {M, F, A})],  % not all definitions, only related to {M,F,A}
							[File]),
    DefVars = lists:merge(Contains),  % [[...]] -> [...]
    % ?wrangler_io("\DefVars: ~p\n",[DefVars]),
	case lists:member(Dummy, DefVars) of
        true ->
            {error, "Parameter is used."};
        false ->
            % is_exported not enough, could just exist without export
			% case api_refac:is_exported({F, A + 1}, File) of
            case lists:member({F, A + 1}, api_refac:defined_funs(File)) of
				true ->
					{error, "The function already exist with that number of arguments."};
				false ->
					ok
			end
    end.

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
%% @spec transform(Args::#args{}) ->
%%            {ok, [{filename(), filename(), syntaxTree()}]} |
%%            {error, Reason}
%% @end
%%--------------------------------------------------------------------
transform_orig_file(_Args=#args{current_file_name=File,
                      user_inputs=[M0, F0, A0, DummyPar]}) ->
    M = list_to_atom(M0),
    F = list_to_atom(F0),
    A = list_to_integer(A0),
    ?FULL_TD_TP([rule_def({M,F,A}, DummyPar),
                rule_appl({M,F,A}, "0")],
                [File]).

transform_client_files(_Args=#args{current_file_name=File,
                      search_paths=SearchPaths,
                      user_inputs=[M0, F0, A0, DummyPar]}) ->
    M = list_to_atom(M0),
    F = list_to_atom(F0),
    A = list_to_integer(A0),
    ?FULL_TD_TP([rule_appl({M,F,A}, "0")],
          api_refac:client_files(File, SearchPaths)).


rule_def({M,F,A}, DummyPar) ->
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"),
          begin
              ?TO_AST("f@(Args@@,"++DummyPar++") when Guard@@-> Bs@@;",
                      wrangler_syntax:get_pos(_This@))
          end,
          api_refac:fun_define_info(f@) == {M, F, A}).

rule_appl({M,F,A}, DummyVal) ->
    ?RULE(?FUN_APPLY(M,F,A),
          begin
              Args=api_refac:get_app_args(_This@),
              NewArgs = Args ++ [wrangler_syntax:integer(DummyVal)],
              api_refac:update_app_args(_This@, NewArgs)
          end,
          true).
