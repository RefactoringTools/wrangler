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
%%
%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%@doc This refactoring can be used when the order of input elements of a WS
%%     operation has been changed. To invoke this refactoring, point the cursor
%%     to wrapper function for this operation, then select  
%%    're-order WS operation arguments' from the 'Refactorings for QuickCheck' sub-menu.
%%    This refactoring does not work with ```eqc_statem''' group syntax yet.
%%@hidden
%%@private
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
    ["new parameter order (indexes separated by comma, eg.: 3,2,1):"].

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
    Msg ="You have not selected an operation wrapper function!",
    case api_interface:pos_to_fun_def(File, Pos) of 
        {ok, FunDef} ->
            {_M, F, A} = api_refac:fun_define_info(FunDef),
            A1=op_arity(FunDef, atom_to_list(F), A),
            {ok, {F,A1}};
        {error, _}->
            {error, Msg}
    end.
        
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec check_pre_cond(Args::#args{}) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
check_pre_cond(_Args=#args{
                 focus_sel = {_FunName, Arity},
                 user_inputs=[NewOrder]}) ->
    OriginalOrder = lists:seq(1, Arity),
    try [list_to_integer(I)||I<-string:tokens(NewOrder, [$,])] of 
        NewOrder1 ->
            case lists:sort(NewOrder1) == OriginalOrder of 
                true ->
                    ok;
                false ->
                    {error, "Invalid parameter order: "++NewOrder++"."}
            end
    catch
        _E1:_E2 ->
            {error, "Invalid parameter order: "++NewOrder++"."}
    end.
       

op_arity(_,_OpName, Arity) when Arity/=1 -> 
    Arity;
op_arity(FunDef, OpName,1) ->
    Res=?STOP_TD_TU(
           [?COLLECT(?T("f@({Args@@}) -> Body@@;"),
                     length(Args@@),
                     ?PP(f@)==OpName
                    )], FunDef),
    case Res of 
        [] ->
            1;
        [A]->A
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
transform(_Args=#args{current_file_name=File, 
                      focus_sel = {OpName0, Arity},
                      user_inputs=[NewParOrder0]}) ->
    OpName = atom_to_list(OpName0),
    NewParOrder = [list_to_integer(I)||I<-string:tokens(NewParOrder0, [$,])],
    Style = check_style(File),
    ?FULL_TD_TP([rule11(OpName, Arity, NewParOrder, Style),
                 rule12(OpName, Arity, NewParOrder, Style),
                 rule21(OpName, Arity, NewParOrder, Style),
                 rule22(OpName, Arity, NewParOrder, Style),
                 rule31(OpName, Arity, NewParOrder, Style),
                 rule32(OpName, Arity, NewParOrder, Style)
                ],
                [File]).

rule11(Op, Arity, NewParOrder, Style) ->
    ?RULE(?T("f@({Args@@}) -> Body@@;"),
          begin
              NewArgs@@=re_order_list(Args@@, NewParOrder),
              ?TO_AST("f@({NewArgs@@}) -> Body@@;")
          end,
          ?PP(f@)==Op andalso Style==tuple 
          andalso length(Args@@)==Arity).

rule12(Op, Arity, NewParOrder, Style) ->
    ?RULE(?T("f@(Args@@) -> Body@@;"),
          begin
              NewArgs@@=re_order_list(Args@@, NewParOrder),
              ?TO_AST("f@(NewArgs@@) -> Body@@;")
          end,
          ?PP(f@)==Op andalso Style == non_tuple
          andalso length(Args@@)==Arity).

rule21(Op, Arity, NewParOrder, Style) ->
     ?RULE(?T("M@:F@({Args@@})"),
          begin
              NewArgs@@=re_order_list(Args@@, NewParOrder),
              ?TO_AST("M@:F@({NewArgs@@})")
          end,
           ?PP(F@)==Op andalso Style==tuple andalso
           length(Args@@)==Arity
          ).

rule22(Op, Arity, NewParOrder,Style) ->
    ?RULE(?T("M@:F@(Args@@)"),
          begin
              NewArgs@@=re_order_list(Args@@, NewParOrder),
              ?TO_AST("M@:F@(NewArgs@@)")
          end,
          ?PP(F@)==Op andalso Style==non_tuple andalso
          length(Args@@)==Arity
          ).

rule31(Op, Arity, NewParOrder, Style) ->
    ?RULE(?T("{call, M@, F@, [{Args@@}]}"),
          begin
              NewArgs@@=re_order_list(Args@@, NewParOrder),
              ?TO_AST("{call, M@, F@, [{NewArgs@@}]}",
                      wrangler_syntax:get_pos(_This@))
          end,
          ?PP(F@)==Op andalso Style==tuple andalso
          length(Args@@)==Arity).

rule32(Op, Arity, NewParOrder, Style) ->
    ?RULE(?T("{call, M@, F@, [Args@@]}"),
          begin
              NewArgs@@=re_order_list(Args@@, NewParOrder),
              ?TO_AST("{call, M@, F@, [NewArgs@@]}",
                      wrangler_syntax:get_pos(_This@))
          end,
          ?PP(F@)==Op andalso Style==non_tuple
          andalso length(Args@@)==Arity).

re_order_list(List, NewOrder) ->
    [lists:nth(I, List)||I<-NewOrder].


check_style(File) ->
    Styles=?STOP_TD_TU(
              [?COLLECT(?T("{call, M@, F@, [{Args@@}]}"),
                           tuple, api_refac:is_pattern(_This@)),
               ?COLLECT(?T("{call, M@, F@, [Args@@]}"),
                           non_tuple, api_refac:is_pattern(_This@))],
              [File]),
    case lists:usort(Styles) of 
        [tuple] -> tuple;
        [non_tuple] -> non_tuple;
        [non_tuple, tuple] ->
            throw({error, "Mixed coding style used."});
        [] ->
             tuple
    end.
                 
 

swap_op_args(FileName, OpName, NewArgsOrder, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
               user_inputs=[OpName, NewArgsOrder],
               search_paths=SearchPaths,
               tabwidth=TabWidth},
    case check_pre_cond(Args) of
        ok -> 
            {ok, Res}=transform(Args),
            wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"");
        {error, Reason} ->
            {error, Reason}
    end.
    
