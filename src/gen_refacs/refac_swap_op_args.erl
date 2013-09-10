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

%%@author  Huiqing Li <H.Li@kent.ac.uk>
%% @hidden
%% @private
-module(refac_swap_op_args).

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
    ["operation name:", "new parameter order:"].

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
                      user_inputs=[OpName, NewParOrder0]}) ->
    io:format("NewParOrder0:~p\n", [NewParOrder0]),
    NewParOrder = [list_to_integer(I)||I<-string:tokens(NewParOrder0, [$,])],
    io:format("NewParOrder:~p\n", [NewParOrder]),
    ?FULL_TD_TP([rule1(OpName, NewParOrder),
                 rule2(OpName, NewParOrder),
                 rule3(OpName, NewParOrder)
                ],
                [File]).

rule1(Op, NewParOrder) ->
    ?RULE(?T("f@(Args@@) -> Body@@;"),
          begin
              NewArgs@@=re_order_list(Args@@, NewParOrder),
              ?TO_AST("f@(NewArgs@@) -> Body@@;")
          end,
          ?PP(f@)==Op).

rule2(Op, NewParOrder) ->
     ?RULE(?T("M@:F@(Args@@)"),
          begin
              NewArgs@@=re_order_list(Args@@, NewParOrder),
              ?TO_AST("M@:F@(NewArgs@@)")
          end,
           ?PP(F@)==Op
          ).

rule3(Op, NewParOrder) ->
    ?RULE(?T("{call, M@, F@, [Args@@]}"),
          begin
              NewArgs@@=re_order_list(Args@@, NewParOrder),
              ?TO_AST("{call, M@, F@, [NewArgs@@]}",
                      wrangler_syntax:get_pos(_This@))
          end,
          ?PP(F@)==Op).

re_order_list(List, NewOrder) ->
    [lists:nth(I, List)||I<-NewOrder].

