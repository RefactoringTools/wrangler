%% @private
-module(refac_remove_arg).

-behaviour(gen_refac).

-compile(export_all).

%% Include files
-include("../include/wrangler.hrl").

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
    ["Parameter Index : "].

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
select_focus(_Args=#args{current_file_name=File, 
                         cursor_pos=Pos}) ->
    interface_api:pos_to_fun_def(File, Pos).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec check_pre_cond(Args::#args{}) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
check_pre_cond(Args=#args{focus_sel=FunDef,
                          user_inputs=[I]}) ->
    Ith=list_to_integer(I),
    {_M,_F,A} = refac_api:fun_define_info(FunDef),                 
    case Ith>=1 andalso Ith=<A of 
        true ->
            check_pre_cond_1(Args);
        false ->
            {error, "Index is invalid."}
    end.
   

check_pre_cond_1(_Args=#args{focus_sel=FunDef,
                          user_inputs=[I]}) ->
    Ith=list_to_integer(I),
    IthArgs=?FULL_TD_TU([?COLLECT(?T("f@(Args@@)when Guard@@-> Bs@@;"),
                                  lists:nth(Ith, Args@@),
                                  true)],
                        FunDef),
    case lists:all(fun(A)->refac_api:type(A)==variable end, IthArgs) of 
        true ->  
            case lists:all(fun(A)->length(refac_api:var_refs(A))==0 end, IthArgs) of
                true ->
                    ok;
                _ ->
                    {error, "Parameter is used."}
            end;
        _  ->
            {error, "The parameter selectted is not a variable."}
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
transform(Args=#args{current_file_name=File,focus_sel=FunDef, 
                     user_inputs=[I]}) ->
    {M,F,A} = refac_api:fun_define_info(FunDef),
    Ith = list_to_integer(I),
    {ok, Res}=transform_in_cur_file(Args, {M,F,A}, Ith),
    case refac_api:is_exported({F,A}, File) of
        true ->
            {ok, Res1}=transform_in_client_files(Args, {M,F,A}, Ith),
            {ok, Res++Res1};
        false ->
            {ok, Res}
    end.

transform_in_cur_file(_Args=#args{current_file_name=File}, MFA, I) ->
    ?FULL_TD_TP([rule1(MFA,I),
                 rule2(MFA,I),
                 rule3(MFA),
                 rule4(MFA,I),
                 rule5(MFA,I)],
             [File]).


transform_in_client_files(_Args=#args{current_file_name=File,
                                      search_paths=SearchPaths}, 
                          MFA, I) ->
    ?FULL_TD_TP([rule2(MFA,I),
                 rule3(MFA),
                 rule4(MFA,I),
                 rule5(MFA,I)],
                refac_api:client_files(File, SearchPaths)).


rule1({M,F,A}, Ith) ->
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"), 
          begin NewArgs@@=delete(Ith, Args@@),
                ?QUOTE("f@(NewArgs@@) when Guard@@->Bs@@;")
          end,
          refac_api:fun_define_info(f@)=={M,F, A}
         ).

rule2({M,F,A}, Ith) ->
    ?RULE(?T("F@(Args@@)"), 
          begin NewArgs@@=delete(Ith, Args@@),
                ?QUOTE("F@(NewArgs@@)")
          end,
          refac_api:fun_define_info(F@) == {M, F, A}).

rule3({M,F,A}) ->
    ?RULE(?T("F@"),
          refac_api:make_arity_qualifier(F, A-1), 
          refac_api:type(F@)==arity_qualifier andalso
          refac_api:fun_define_info(F@) == {M, F, A}).


rule4({M,F,A}, Ith)->
    ?RULE(?T("Fun@(N@@, M@, F@, [Args@@])"),
          begin
              NewArgs@@=delete(Ith, Args@@),
              ?QUOTE("Fun@(N@@, M@, F@, [NewArgs@@])")
          end,
          begin
              case refac_api:fun_define_info(Fun@) of
                  {erlang, apply, _} -> 
                      refac_api:fun_define_info(F@)=={M,F,A};
                  _ -> false
              end
          end).

rule5({M,F,A}, Ith)->
    ?RULE(?T("F@(Fun@, [Args@@])"),
          begin
              NewArgs@@=delete(Ith, Args@@),
              ?QUOTE("F@(Fun@, [NewArgs@@])")
          end,
          begin
              case refac_api:fun_define_info(F@) of
                  {erlang, apply, _} -> 
                      refac_api:fun_define_info(Fun@)=={M,F,A};
                  _ -> false
              end
          end).


%%%===================================================================
%%% Internal functions
%%%===================================================================

delete(Ith, List) ->
    lists:sublist(List, Ith-1)++ lists:nthtail(Ith, List).

