%% @hidden
%% @private
-module(refac_rm_op_arg).

-behaviour(gen_refac).

-compile(export_all).

%% Include files
-include("../../include/wrangler.hrl").

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
    ["operation name: ", "parameter name: ", "parameter index: "].

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
                      user_inputs=[OpName, Index]})->
    Op = list_to_atom(OpName),
    Nth =list_to_integer(Index),
    ?STOP_TD_TP([rule1(Op, Nth),
                 rule2(Op, Nth)
                ],
                [File]).
  
rule1(Op, Ith) ->
    ?RULE(?T("f@(Args@@) -> sut_result(?SUT:f@(Args1@@))."),
          begin
              NewArgs@@ = delete(Ith, Args@@),
              NewArgs1@@= delete(Ith, Args1@@),
              ?TO_AST("f@(NewArgs@@) ->
                            sut_result(?SUT:f@(NewArgs1@@)).")
          end,
          element(2,api_refac:fun_define_info(f@))==Op). 


rule2(Op, Ith) ->
     ?RULE(?T("{call, M@, F@, [Args@@]}"),
           begin
               NewArgs@@ = delete(Ith, Args@@),
               ?TO_AST("{call, M@, F@, [NewArgs@@]}", 
                       wrangler_syntax:get_pos(_This@))
           end,
           element(2,api_refac:fun_define_info(F@))==Op). 

          
%%%===================================================================
%%% Internal functions
%%%===================================================================

delete(Ith, Args) when is_list(Args) ->
    lists:sublist(Args, Ith-1)++ lists:nthtail(Ith, Args);
delete(Ith, Arg) ->
    Str=lists:flatten(
          io_lib:format(
            "fun(ArgList) ->
                    lists:sublist(ArgList, ~p-1) ++
                      lists:nthtail(~p, ArgList)
            end(~s)", [Ith, Ith, ?PP(Arg)])),
    ?TO_AST(Str).


rm_op_arg(FileName, OpName, Index, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
               user_inputs=[OpName, Index],
               search_paths=SearchPaths,
               tabwidth=TabWidth},
    case check_pre_cond(Args) of
        ok -> 
            {ok, Res}=transform(Args),
            wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"");
        {error, Reason} ->
            {error, Reason}
    end.
    


