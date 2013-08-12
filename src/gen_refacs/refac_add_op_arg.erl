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
-module(refac_add_op_arg).

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
    ["operation name:", "new argument name:",  "new argument index:", "generator:"].

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
                      user_inputs=[OpName, NewArgName,Index, NewArgGen]}) ->
    Op  = list_to_atom(OpName),
    Nth = list_to_integer(Index),
    ?FULL_TD_TP([rule1(Op, NewArgName, Nth),
                 rule2(Op, NewArgName, Nth),
                 rule3(Op, NewArgName, Nth),
                 rule4(Op, NewArgGen, Nth)
                ],
                [File]).


rule1(Op, NewArg, Nth) ->
    ?RULE(?T("f@(Args@@) -> Body@@;"),
          begin
              Args01@@=lists:sublist(Args@@, Nth-1),
              Args02@@=lists:nthtail(Nth-1, Args@@),
              ?TO_AST("f@(Args01@@,"++NewArg++", Args02@@) ->
                            Body@@;")
          end,
          element(2,api_refac:fun_define_info(f@))==Op).

rule2(Op, NewArg, Nth) ->
     ?RULE(?T("M@:F@(Args@@)"),
          begin
              Args01@@=lists:sublist(Args@@, Nth-1),
              Args02@@=lists:nthtail(Nth-1, Args@@),
              ?TO_AST("M@:F@(Args01@@,"++NewArg++", Args02@@)")
          end,
           element(2,api_refac:fun_define_info(F@))==Op
           ).

rule3(Op, NewArg, Nth) ->
    ?RULE(?T("{call, M@, F@, [Args@@]}"),
          begin
              Args1@@=lists:sublist(Args@@, Nth-1),
              Args2@@=lists:nthtail(Nth-1, Args@@),
              ?TO_AST("{call, M@, F@, [Args1@@,"++NewArg++", Args2@@]}",
                      wrangler_syntax:get_pos(_This@))
          end,
          element(2, api_refac:fun_define_info(F@))==Op andalso
          api_refac:is_pattern(_This@)).

rule4(Op, NewArgGen, Nth) ->
    ?RULE(?T("{call, M@, F@, [Args@@]}"),
          begin
              Args1@@=lists:sublist(Args@@, Nth-1),
              Args2@@=lists:nthtail(Nth-1, Args@@),
              ?TO_AST("{call, M@, F@, [Args1@@,"++NewArgGen++"(), Args2@@]}",
                     wrangler_syntax:get_pos(_This@))
          end,
          element(2, api_refac:fun_define_info(F@))==Op andalso
          api_refac:is_expr(_This@)).


add_op_arg(FileName, OpName, NewArgName, Index, NewArgGen, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
               user_inputs=[OpName, NewArgName, Index, NewArgGen],
               search_paths=SearchPaths,
               tabwidth=TabWidth},
    case check_pre_cond(Args) of
        ok -> 
            {ok, Res}=transform(Args),
            wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"");
        {error, Reason} ->
            {error, Reason}
    end.
    

%% %% this is still some layout problems.
%% rule1({M, F, A}, Nth, NewPar) ->
%%     ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"),
%%           begin
%%               Args1@@=lists:sublist(Args@@, Nth-1),
%%               Args2@@=lists:nthtail(Nth-1, Args@@),
%%               ?TO_AST("f@(Args1@@,"++NewPar++", Args2@@) when Guard@@-> Bs@@;", 
%%                       wrangler_syntax:get_pos(_This@))
%%           end,
%%           api_refac:fun_define_info(f@) == {M, F, A} andalso
%%           length(Args@@)==A).
