%% @hidden
%% @private
-module(refac_rename_function).

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
                      user_inputs=[_M0, _F0, _A0, NewName]}) ->
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
transform(_Args=#args{current_file_name=File,
                      search_paths=SearchPaths,
                      user_inputs=[M0, F0, A0, NewName]}) ->
    M = list_to_atom(M0),
    F = list_to_atom(F0),
    A = list_to_integer(A0),
    NewNameAtom = list_to_atom(NewName),
    case api_refac:is_exported({F,A}, File) of
        true ->
            {ok, Res}=transform_in_client_files({M,F,A}, File, SearchPaths, NewNameAtom),
            case Res of
                [] ->
                    %% no client files have been changed.
                    transform_in_cur_file({M,F,A}, NewName, NewNameAtom, File, false);
                _ ->
                    %% some clients files have been changed.
                    {ok, Res1}=transform_in_cur_file({M,F,A}, NewName, NewNameAtom, File, true),
                    {ok, Res1++Res}
            end;
        false ->
            %% function is not exported.
            transform_in_cur_file({M,F,A}, NewName, NewNameAtom, File, false)
    end.

transform_in_cur_file({M,F,A}, NewName, NewNameAtom, File, true)->
    ?FULL_TD_TP([rule1({M,F,A}, NewName, NewNameAtom),
                 rule2({M,F,A}, NewNameAtom),
                 rule3({M,F,A}, NewNameAtom),
                 rule4({M,F,A})],
                [File]);
transform_in_cur_file({M,F,A}, NewName, NewNameAtom, File, false) ->
    ?FULL_TD_TP([rule1({M,F,A}, NewName, NewNameAtom),
                 rule2({M,F,A}, NewNameAtom),
                 rule4({M,F,A})], [File]).

transform_in_client_files({M,F,A}, File, SearchPaths, NewFunName) ->
    ?FULL_TD_TP([rule2({M,F,A}, NewFunName),
                 rule4({M,F,A})],
                api_refac:client_files(File, SearchPaths)).

% definition
rule1({M, F, A}, NewName, _NewNameAtom) ->
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"),
          begin
              % api_refac:make_arity_qualifier(NewNameAtom, A),
              ?TO_AST(NewName++"(Args@@) when Guard@@-> Bs@@;",
                      wrangler_syntax:get_pos(_This@))
          end,
          api_refac:fun_define_info(f@) == {M,F,A}).

% application
rule2({M,F,A}, NewNameAtom) ->
    ?RULE(?FUN_APPLY(M,F,A),
        begin
            api_refac:update_app_fun(_This@, wrangler_syntax:atom(NewNameAtom))
        end,
        true).

% export list handling
rule3({M,F,A}, NewNameAtom) ->
    ?RULE(?T("F@"),
            api_refac:make_arity_qualifier(NewNameAtom, A),
            api_refac:type(F@) == arity_qualifier andalso
            api_refac:fun_define_info(F@) == {M, F, A}).

% expansion
rule4({M, F, A}) ->
    ?RULE(?T("fun M@:f@/A@"),
          begin
              % ?TO_AST("fun M@:"++NewName++"/A@",
              Vars@@ = lists:map(fun wrangler_syntax:variable/1, generate_unique_vars(A)),
              ?TO_AST("fun(Vars@@) -> f@(Vars@@) end",
                      % f@(NewVars) would create mult(X,Y,0,0) ->
                      % it'd apply rule3, so we need only to call f with orig args
                    wrangler_syntax:get_pos(_This@))
          end,
          api_refac:fun_define_info(f@) == {M, F, A}).

% simplify
rule5({M,F,A}) ->
    ?RULE(?T("fun (Vars@@) -> M@:f@(Args@@) end"),
          begin
              ArgNames = lists:map(fun decode_value/1, Args@@),
              A@ = wrangler_syntax:integer(length(ArgNames)),
              ?TO_AST("fun M@:f@/A@", wrangler_syntax:get_pos(_This@))
          end,
          lists:map(fun decode_value/1, Args@@) == lists:map(fun decode_value/1, Vars@@) andalso
          api_refac:fun_define_info(f@) == {M, F, A}).

decode_value(Node) ->
    case wrangler_syntax:revert(Node) of
        {Type, _, Val} -> {Type, Val}
    end.

% utility functions
generate_unique_vars(N) ->
  generate_unique_vars(N, []).
generate_unique_vars(0, L) ->
  L;
generate_unique_vars(N, L) ->
  Cur = list_to_atom("A" ++ integer_to_list(N)),
  generate_unique_vars(N - 1, [Cur] ++ L).
