%% @hidden
%% @private
-module(refac_new_dummy_argument).

-behaviour(gen_refac).

-compile(export_all).

%% Include files
-include("wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

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
     "DummyPar"].

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
transform(_Args=#args{current_file_name=File,
                      user_inputs=[M0, F0, A0, DummyPar]}) ->
    M = list_to_atom(M0),
    F = list_to_atom(F0),
    A = list_to_integer(A0),
	DummyVal = "0",
    Contains = ?STOP_TD_TU([?COLLECT(?T("f@(Args@@) when Guard@@-> Bs@@;"),
									% wrangler_misc:collect_var_names(_This@),
                                    Args@@,
                                    api_refac:fun_define_info(f@) == {M, F, A})],  % not all definitions, only related to {M,F,A}
							[File]),
    _VariableNames = lists:merge(Contains),  % [[...]] -> [...]
    {ok, Res}=transform_in_cur_file(_Args, {M,F,A}, DummyPar, DummyVal),
    case api_refac:is_exported({F,A}, File) of
        true ->
            {ok, Res1}=transform_in_client_files(_Args, {M,F,A}, DummyPar),
            {ok, Res++Res1};
        false ->
            {ok, Res}
    end.

%% transform the current file.
transform_in_cur_file(_Args=#args{current_file_name=File}, MFA, DummyPar, DummyVal)->
    ?FULL_TD_TP([rule1(MFA, DummyPar),
                 rule2(MFA),
                 rule3(MFA, DummyVal),
                 rule4(MFA)],
                [File]).

%% transform the client files.
transform_in_client_files(_Args=#args{current_file_name=File,
                                      search_paths=SearchPaths},
                          MFA, DummyPar) ->
    ?FULL_TD_TP([rule3(MFA, DummyPar),
                 rule4(MFA)],
          api_refac:client_files(File, SearchPaths)).

rule1({M, F, A}, DummyPar) ->
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"),
          begin
              ?TO_AST("f@(Args@@,"++DummyPar++") when Guard@@-> Bs@@;",
                      wrangler_syntax:get_pos(_This@))
          end,
          api_refac:fun_define_info(f@) == {M, F, A}).


rule2({M,F,A}) ->
    ?RULE(?T("F@"),
          api_refac:make_arity_qualifier(F, A + 1),
          api_refac:type(F@) == arity_qualifier andalso
          api_refac:fun_define_info(F@) == {M, F, A}).


rule3({M,F,A}, DummyVal) ->
    ?RULE(?FUN_APPLY(M,F,A),
          begin
              Args=api_refac:get_app_args(_This@),
              NewArgs = Args ++ [wrangler_syntax:integer(DummyVal)],
              api_refac:update_app_args(_This@, NewArgs)
          end,
          true).

rule4({M, F, A}) ->
    ?RULE(?T("fun M@:f@/A@"),
          begin
              % Vars@@ = lists:map(fun(X) -> new_empty_expr(X,variable) end, generate_unique_vars(A)),
              Vars@@ = lists:map(fun wrangler_syntax:variable/1, generate_unique_vars(A)),
              ?TO_AST("fun(Vars@@) -> f@(Vars@@) end",
                      % f@(NewVars) would create mult(X,Y,0,0) ->
                      % it'd apply rule3, so we need only to call f with orig args
                      wrangler_syntax:get_pos(_This@))
          end,
          api_refac:fun_define_info(f@) == {M, F, A}).

% utility functions
generate_unique_vars(N) ->
    generate_unique_vars(N, []).
generate_unique_vars(0, L) ->
    L;
generate_unique_vars(N, L) ->
    Cur = list_to_atom("A" ++ integer_to_list(N)),
    generate_unique_vars(N - 1, [Cur] ++ L).
