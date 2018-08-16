%% Copyright (c) 2013, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%% @hidden
%% @private
-module(refac_remove_an_argument).

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
    api_interface:pos_to_fun_def(File, Pos).

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
    {_M,_F,A} = api_refac:fun_define_info(FunDef),
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
    case lists:all(fun(A) -> api_refac:type(A) == variable end, IthArgs) of
        true ->  
            case lists:all(fun(A) -> length(api_refac:var_refs(A)) == 0 end, IthArgs) of
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
    {M,F,A} = api_refac:fun_define_info(FunDef),
    Ith = list_to_integer(I),
    {ok, Res}=transform_in_cur_file(Args, {M,F,A}, Ith),
    case api_refac:is_exported({F,A}, File) of
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
                 rule4(MFA, I)],
                [File]).


transform_in_client_files(_Args=#args{current_file_name=File,
                                      search_paths=SearchPaths}, 
                          MFA, I) ->
    ?FULL_TD_TP([rule2(MFA,I),
                 rule3(MFA)],
                api_refac:client_files(File, SearchPaths)).


rule1({M,F,A}, Ith) ->
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"), 
          begin NewArgs@@=delete(Ith, Args@@),
                ?TO_AST("f@(NewArgs@@) when Guard@@->Bs@@;")
          end,
          api_refac:fun_define_info(f@) == {M, F, A}
         ).

%% Transform the different kinds of function applications.
rule2({M,F,A}, Ith) ->
    ?RULE(?FUN_APPLY(M,F,A),
          begin
              Args = api_refac:get_app_args(_This@),
              NewArgs=delete(Ith, Args),
              api_refac:update_app_args(_This@, NewArgs)
          end,
          true).

rule3({M,F,A}) ->
    ?RULE(?T("F@"),
          api_refac:make_arity_qualifier(F, A - 1),
          api_refac:type(F@) == arity_qualifier andalso
          api_refac:fun_define_info(F@) == {M, F, A}).

rule4({_M, F, A}, Ith) ->
    ?RULE(?T("Spec@"), 
          api_spec:rm_arg_type_from_spec(_This@, Ith),
          api_spec:is_type_spec(Spec@, {F, A})).


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

