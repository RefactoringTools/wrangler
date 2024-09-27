%% @hidden
%% @private
-module(refac_rename_function2).

-behaviour(gen_refac_2).

-compile(export_all).
-compile(nowarn_export_all).

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
     "New name for function:"].

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
			   user_inputs=[M0, F0, A0, NewName]}) ->
    _M = list_to_atom(M0),
    _F = list_to_atom(F0),
    _A = list_to_integer(A0),
    NewNameAtom = list_to_atom(NewName),
    case api_refac:is_fun_name(NewName) of
        false ->
            {error, "New function name is invalid."};
        true ->
            FnameList = get_fun_name_from_FA(api_refac:defined_funs(File)),
            case lists:member(NewNameAtom, FnameList) of
                true ->
                    {error, "A function already exist with that name."};
                false ->
                    ok
            end
    end.

get_fun_name_from_FA([{X,_Y} | Xs]) ->
    [X] ++ get_fun_name_from_FA(Xs);
get_fun_name_from_FA([]) -> [].


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


rule_def(_Args=#args{user_inputs=[M0, F0, A0, NewName]}) ->
    M = list_to_atom(M0),
    F = list_to_atom(F0),
    A = list_to_integer(A0),
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"),
          ?TO_AST(NewName++"(Args@@) when Guard@@-> Bs@@;",
                      wrangler_syntax:get_pos(_This@)),
          api_refac:fun_define_info(f@) == {M, F, A}).

rule_appl(_Args=#args{user_inputs=[M0, F0, A0, NewName]}) ->
    M = list_to_atom(M0),
    F = list_to_atom(F0),
    A = list_to_integer(A0),
    NewNameAtom = list_to_atom(NewName),
    ?RULE(?FUN_APPLY(M,F,A),
        api_refac:update_app_fun(_This@, wrangler_syntax:atom(NewNameAtom)),
        true).
