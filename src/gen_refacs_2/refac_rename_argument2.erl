%% @hidden
%% @private
-module(refac_rename_argument2).

-behaviour(gen_refac_2).

-compile(export_all).

%% Include files
-include("wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1,
         check_pre_cond/1, selective/0,
         rule_def/1, rule_appl/1]).

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
     "Arity:",
     "Index:",
     "New Arg Name:"].

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
                      user_inputs=[M0, F0, A0, Ith, NewName]}) ->
	M = list_to_atom(M0),
	F = list_to_atom(F0),
	A = list_to_integer(A0),
	I = list_to_integer(Ith),
    NewNameAtom = list_to_atom(NewName),
    Contains = ?STOP_TD_TU([?COLLECT(?T("f@(Args@@) when Guard@@-> Bs@@;"),
		                    wrangler_misc:collect_var_names(_This@),
                            api_refac:fun_define_info(f@) == {M, F, A})],  % not all definitions, only related to {M,F,A}
							[File]),
    DefVars = lists:merge(Contains),  % [[...]] -> [...]
    % ?wrangler_io("\DefVars: ~p\n",[DefVars]),
	case lists:member(NewNameAtom, DefVars) of
        true ->
            {error, "Argument name is already in use."};
        false ->
            case I=<A andalso I>=0 of
                true ->
                    ok;
                false ->
                    {error, "The given position is invalid."}
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


rule_def(_Args=#args{user_inputs=[M0, F0, A0, Ith, NewName]}) ->
    M = list_to_atom(M0),
    F = list_to_atom(F0),
    A = list_to_integer(A0),
    I = list_to_integer(Ith),
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"),
          begin
              Args1@@ = lists:sublist(Args@@,I),
              Args2@@ = lists:nthtail(I+1, Args@@),
              ?wrangler_io("Args1@@: ~p\n", [Args1@@]),
              ?wrangler_io("Args2@@: ~p\n", [Args2@@]),
              ?TO_AST("f@(Args1@@, "++NewName++", Args2@@) when Guard@@-> Bs@@;")
          end,
          api_refac:fun_define_info(f@) == {M, F, A}).

rule_appl(_Args=#args{user_inputs=[M0, F0, A0, Ith, NewName]}) ->
    M = list_to_atom(M0),
    F = list_to_atom(F0),
    A = list_to_integer(A0),
    ?RULE(?FUN_APPLY(M,F,A),
           ok,
           false).
