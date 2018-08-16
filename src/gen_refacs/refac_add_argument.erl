%% @hidden
%% @private
-module(refac_add_argument).

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
     "Arity:", 
     "Index:", 
     "New parameter name:"].

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
%% @spec transform(Args::#args{}) -> 
%%            {ok, [{filename(), filename(), syntaxTree()}]} |
%%            {error, Reason}
%% @end
%%--------------------------------------------------------------------
transform(_Args=#args{current_file_name=File, 
                      user_inputs=[M0, F0, A0, Nth0, NewPar]}) ->
    M = list_to_atom(M0),
    F = list_to_atom(F0),
    A = list_to_integer(A0),
    Nth = list_to_integer(Nth0),
    ?STOP_TD_TP([rule1({M, F, A}, Nth, NewPar),
                 rule2({M, F, A}, Nth, NewPar)
                ],
                [File]).

rule1({M, F, A}, Nth, NewPar) ->
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"),
          begin
              Args1@@=lists:sublist(Args@@, Nth-1),
              Args2@@=lists:nthtail(Nth-1, Args@@),
              ?TO_AST("f@(Args1@@,"++NewPar++", Args2@@) when Guard@@-> Bs@@;", 
                      wrangler_syntax:get_pos(_This@))
          end,
          api_refac:fun_define_info(f@) == {M, F, A} andalso
          length(Args@@)==A).

rule2({M, F, A}, Nth, NewPar) ->
    ?RULE(?T("{call, M@, F@, [Args@@]}"),
          begin
              Args1@@=lists:sublist(Args@@, Nth-1),
              Args2@@=lists:nthtail(Nth-1, Args@@),
              ?TO_AST("{call, M@, F@, [Args1@@,"++NewPar++", Args2@@]}",
                     wrangler_syntax:get_pos(_This@))
          end,
          api_refac:fun_define_info(F@)=={M, F, A} andalso
          length(Args@@)==A
         ).

%% this is still some layout problems.
