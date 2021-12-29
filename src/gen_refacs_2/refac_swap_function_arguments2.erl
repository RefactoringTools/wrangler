%% @hidden
%% @private
-module(refac_swap_function_arguments2).

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
     "Argument Position 1:",
     "Argument Position 2:"].

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

check_pre_cond(_Args=#args{user_inputs=[_M0, _F0, A0, I, J]}) ->
	A = list_to_integer(A0),
    Ith=list_to_integer(I),
    Jth=list_to_integer(J),
    case Ith /= Jth of 
        true ->
            case Ith>=1 andalso Ith=<A of 
                true ->
                    case Jth >= 1 andalso Jth =< A of 
                        true ->
                            ok;
                        false ->
                            {error, "Index 2 is invalid."}
                    end;
                false ->
                    {error, "Index 1 is invalid."}
            end;
        false ->
            {error, "Index 1 and Index 2 are the same."}
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


%% transform the function definition itself.
rule_def(_Args=#args{user_inputs=[M0, F0, A0, I, J]}) ->
    M = list_to_atom(M0),
	F = list_to_atom(F0),
	A = list_to_integer(A0),
    Ith=list_to_integer(I),
    Jth=list_to_integer(J),
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"), 
          begin 
              NewArgs@@=swap(Args@@,Ith,Jth),
              ?TO_AST("f@(NewArgs@@) when Guard@@->Bs@@;")
          end,
          api_refac:fun_define_info(_This@) == {M, F, A}
         ).

%% Transform the different kinds of function applications.
rule_appl(_Args=#args{user_inputs=[M0, F0, A0, I, J]}) ->
    M = list_to_atom(M0),
	F = list_to_atom(F0),
	A = list_to_integer(A0),
    Ith=list_to_integer(I),
    Jth=list_to_integer(J),
    ?RULE(?FUN_APPLY(M,F,A),
          begin
              Args=api_refac:get_app_args(_This@), 
              NewArgs=swap(Args, Ith, Jth),
              api_refac:update_app_args(_This@,NewArgs)
          end,
          true
        ).

%% utility functions.
swap(List, I, J) when is_list(List) ->
    Ith = lists:nth(I, List),
    Jth = lists:nth(J, List),
    T = list_to_tuple(List),
    T1=setelement(J, setelement(I, T, Jth), Ith),
    tuple_to_list(T1);
swap(Node, I, J) ->
    Str=lists:flatten(
          io_lib:format(
            "fun(List) ->
                    Ith = lists:nth(~p, List),
                    Jth = lists:nth(~p, List),
                    T = list_to_tuple(List),
                    T1=setelement(~p, setelement(~p, T, Jth), Ith),
                    tuple_to_list(T1)
            end(~s)", [I, J, J, I, ?PP(Node)])),
    ?TO_AST(Str).
