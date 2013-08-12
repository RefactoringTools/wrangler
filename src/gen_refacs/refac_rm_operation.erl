%% @hidden
%% @private
-module(refac_rm_operation).

-behaviour(gen_refac).

-compile(export_all).

%% Include files
-include_lib("../../include/wrangler.hrl").

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
    ["operation name to be removed:"].

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
                      user_inputs=[OpName]})->
    Op = list_to_atom(OpName),
    ?STOP_TD_TP([rule0(next_state,   Op),
                 rule0(precondition, Op),
                 rule0(postcondition,Op),
                 rule1(Op)
                ],
                [File]).
  
rule0(FunName, OpName) ->
    ?RULE(?T("f@(Args1@@, {call, M@, Op@, As@}, Args2@@) when Guards@@ -> Bs@@;"),
          wrangler_syntax:empty_node(),
          wrangler_syntax:is_atom(f@, FunName) andalso
          wrangler_syntax:is_atom(Op@,OpName)).


rule1(OpName) ->
    ?RULE(?T("{call, M@, F@, Args@@}"),
          wrangler_syntax:empty_node(),
          api_refac:is_expr(_This@) andalso wrangler_syntax:is_atom(F@, OpName)).

rm_operation(FileName, OpName, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
               user_inputs=[OpName],
               search_paths=SearchPaths,
               tabwidth=TabWidth},
    case check_pre_cond(Args) of
        ok -> 
            {ok, Res}=transform(Args),
            wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"");
        {error, Reason} ->
            {error, Reason}
    end.
    
