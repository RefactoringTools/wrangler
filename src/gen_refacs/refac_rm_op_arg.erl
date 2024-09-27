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
%%@doc This refactoring can be used when a  parameter has been removed from a
%%     WS operation. To invoke this refactoring, point the cursor to the parameter to 
%%     be removed in the wrapper function for this operation, then select  
%%    'remove a WS operation argument' from the 'Refactorings for QuickCheck' sub-menu.
%%    1) This refactoring forces the removal of the parameter, hence could lead to 
%%       code that does not compile.
%%    2) This refactoring does not work with ```eqc_statem''' group syntax yet.
%%@hidden
%%@private
-module(refac_rm_op_arg).

-behaviour(gen_refac).

-compile(export_all).
-compile(nowarn_export_all).

%% Include files
-include("wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
         check_pre_cond/1, selective/0, 
         transform/1]).

-export([rm_op_arg/7]).

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
    [].

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
    Msg ="You have not selected an operation parameter!",
    case api_interface:pos_to_fun_def(File, Pos) of 
        {ok, FunDef} ->
            case  api_interface:pos_to_var_name(FunDef, Pos) of 
                {ok, {VarName, _}}->
                    {_M, F, A} = api_refac:fun_define_info(FunDef),
                    Index = collect_arg_index(FunDef, atom_to_list(F), atom_to_list(VarName)),
                    case Index of 
                        [] ->
                            throw({error, Msg});
                        [I] ->
                            {ok,{F,A,I}}
                    end;
                {error, _} ->
                    throw({error, Msg})
            end;
        {error, Msg} ->
            throw({error, Msg})
    end.
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
                      focus_sel = {OpName0, Arity, Index}})->
    Style = check_style(File),
    OpName = atom_to_list(OpName0),
    ?FULL_TD_TP([rule11(OpName, Index, Arity, Style),
                 rule12(OpName, Index, Arity, Style),
                 rule21(OpName,  Index, Arity),
                 rule31(OpName, Index, Arity, Style),
                 rule32(OpName, Index, Arity, Style)
                ],
                [File]).
  


rule11(Op, Index, Arity, Style) ->
    ?RULE(?T("f@({Args@@}) -> Body@@;"),
          case length(Args@@) of 
              1 ->
                  ?TO_AST("f@()->Body@@;", 
                         wrangler_syntax:get_pos(_This@));
              _ ->
                  NewArgs@@ = delete(Index, Args@@),
                  ?TO_AST("f@({NewArgs@@})->Body@@;", 
                          wrangler_syntax:get_pos(_This@))
          end,
          ?PP(f@)==Op andalso Arity==1 andalso
          Style==tuple).

rule12(Op, Index, Arity, Style) ->
    ?RULE(?T("f@(Args@@)-> Body@@;"),
          begin
              NewArgs@@ = delete(Index, Args@@),
              ?TO_AST("f@(NewArgs@@)->Body@@;", 
                      wrangler_syntax:get_pos(_This@))
          end,
          ?PP(f@)==Op andalso length(Args@@)==Arity andalso
          Style == non_tuple).
        
rule21(Op, Index, _Arity) ->
     ?RULE(?T("M@:F@(Args@@)"),
          begin
              NewArgs@@ = delete(Index, Args@@),
              ?TO_AST("M@:F@(NewArgs@@)")
          end,
           ?PP(F@)==Op).

rule31(Op, Index, Arity, Style) ->
    ?RULE(?T("{call, M@, F@, [{Args@@}]}"),
          case length(Args@@) of 
              1 ->
                  ?TO_AST("{call, M@, F@, []}",
                          wrangler_syntax:get_pos(_This@));
              _ ->
                  NewArgs@@ = delete(Index, Args@@),
                  ?TO_AST("{call, M@, F@, [{NewArgs@@}]}",
                          wrangler_syntax:get_pos(_This@))
          end,
          ?PP(F@)==Op andalso Arity==1 
          andalso api_refac:is_pattern(_This@) andalso
          Style==tuple).

rule32(Op, Index, Arity, Style) ->
    ?RULE(?T("{call, M@, F@, [Args@@]}"),
           begin
               NewArgs@@ = delete(Index, Args@@),
               ?TO_AST("{call, M@, F@, [NewArgs@@]}",
                      wrangler_syntax:get_pos(_This@))
            end,
           ?PP(F@)==Op andalso length(Args@@)==Arity 
           andalso Style==non_tuple).
        
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


  
collect_arg_index(FunDef, OpName, VarName) ->
        ?STOP_TD_TU(
           [?COLLECT(?T("f@(Args1@@, OldVarName@, Args2@@) when Guards@@ -> Body@@;"),
                     length(Args1@@)+1,
                     ?PP(f@)==OpName andalso api_refac:type(OldVarName@) == variable  andalso
                     ?PP(OldVarName@)==VarName),
            ?COLLECT(?T("f@({Args1@@, OldVarName@, Args2@@}) when Guards@@ -> Body@@;"),
                     length(Args1@@)+1,
                     ?PP(f@)==OpName andalso api_refac:type(OldVarName@) == variable  andalso
                     ?PP(OldVarName@)==VarName)
           ],FunDef).
        
 

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
                 

rm_op_arg(FileName, OpName, Arity, Index, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
               user_inputs=[OpName, Index],
               focus_sel = {OpName, Arity, Arity-Index+1},
               search_paths=SearchPaths,
               tabwidth=TabWidth},
    {ok, Res}=transform(Args),
    wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"").
