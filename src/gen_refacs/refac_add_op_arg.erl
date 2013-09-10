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
    ["operation name:", "new argument name:",  "new argument index:"].

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
                      user_inputs=[OpName, NewArgName,Index]}) ->
    Nth = list_to_integer(Index),
    NewArgGen="gen_"++camelCase_to_camel_case(NewArgName),
    ?FULL_TD_TP([rule1(OpName, NewArgName, Nth),
                 rule2(OpName, NewArgName, Nth),
                 rule3(OpName, NewArgName, Nth),
                 rule4(OpName, NewArgGen, Nth)
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
          ?PP(f@)==Op).
        
rule2(Op, NewArg, Nth) ->
     ?RULE(?T("M@:F@(Args@@)"),
          begin
              Args01@@=lists:sublist(Args@@, Nth-1),
              Args02@@=lists:nthtail(Nth-1, Args@@),
              ?TO_AST("M@:F@(Args01@@,"++NewArg++", Args02@@)")
          end,
           ?PP(F@)==Op
          ).

rule3(Op, NewArg, Nth) ->
    ?RULE(?T("{call, M@, F@, [Args@@]}"),
          begin
              Args1@@=lists:sublist(Args@@, Nth-1),
              Args2@@=lists:nthtail(Nth-1, Args@@),
              ?TO_AST("{call, M@, F@, [Args1@@,"++NewArg++", Args2@@]}",
                      wrangler_syntax:get_pos(_This@))
          end,
          ?PP(F@)==Op andalso api_refac:is_pattern(_This@)).

rule4(Op, NewArgGen, Nth) ->
    ?RULE(?T("{call, M@, F@, [Args@@]}"),
          begin
              Args1@@=lists:sublist(Args@@, Nth-1),
              Args2@@=lists:nthtail(Nth-1, Args@@),
              ?TO_AST("{call, M@, F@, [Args1@@,"++NewArgGen++"(), Args2@@]}",
                     wrangler_syntax:get_pos(_This@))
          end,
           ?PP(F@)==Op andalso api_refac:is_expr(_This@)).


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
    

camelCase_to_camel_case(Name) when is_atom(Name) ->
    camelCase_to_camel_case(atom_to_list(Name));
camelCase_to_camel_case(Name) ->
    case Name of 
        [H|T] when (H >= 65) and (90 >= H)->
            camelCase_to_camel_case_1([H+32|T],[]);
        [H|T] when H==45 ->
            camelCase_to_camel_case_1([95|T],[]);
        _  ->
            camelCase_to_camel_case_1(Name,[])
    end.

camelCase_to_camel_case_1([], Acc) ->
    lists:reverse(Acc);
camelCase_to_camel_case_1([H|T], Acc)
  when  (H >= 65) and (90 >= H)->
    case Acc of 
        [95|_] ->
            camelCase_to_camel_case_1(T, [H + (97 - 65) |Acc]);
        _ ->
            camelCase_to_camel_case_1(T, [H + (97 - 65), 95|Acc])
    end;
camelCase_to_camel_case_1([H|T], Acc) when H==45->
    camelCase_to_camel_case_1(T, [95|Acc]);
camelCase_to_camel_case_1([H|T], Acc)->
    camelCase_to_camel_case_1(T, [H|Acc]).
    

%%Notes: 
%% 1) actual generator is not generated;
%% 2).name conflict is not checked yet.
