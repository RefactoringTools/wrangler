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
%%@doc This refactoring can be used when a WS operation has been removed from a
%%     WS. To invoke this refactoring, point the cursor to the wrapper function 
%%     of thie operation, then select 'remove a WS operation' from the 
%%     Refactorings for QuickCheck' sub-menu.
%%    This refactoring does not work with ```eqc_statem''' group syntax yet.
%%@hidden
%%@private
-module(refac_rm_op).

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
    Msg ="You have not selected an operation wrapper function!",
    case api_interface:pos_to_fun_def(File, Pos) of 
        {ok, FunDef} ->
            case is_a_wrapper_fun(FunDef) of 
                true -> 
                    {_M, F, A} = api_refac:fun_define_info(FunDef),
                    {ok, {F,A}};
                false ->
                    {error, Msg}
            end;
        {error, Msg} ->
            {error, Msg}
    end.
        
is_a_wrapper_fun(F) ->
    Res=?STOP_TD_TU(
           [?COLLECT(?T("M@:F@(Args@@)"),
                     true,
                     ?PP(M@)=="?SUT"
                    )],
           F),
    lists:usort(Res)==[true].

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
                      focus_sel={F, _A}})->
    OpName = atom_to_list(F),
    ?STOP_TD_TP([rule0(next_state, OpName),
                 rule0(precondition, OpName),
                 rule0(postcondition,OpName),
                 rule1(OpName),
                 rule2(OpName)
                ],
                [File]).
  
rule0(FunName, OpName) ->
    ?RULE(?T("f@(Args1@@, {call, M@, Op@, As@}, Args2@@) when Guards@@ -> Bs@@;"),
          wrangler_syntax:empty_node(),
          ?PP(Op@)==OpName andalso wrangler_syntax:is_atom(f@, FunName)).

rule1(OpName) ->
    ?RULE(?T("{call, M@, F@, Args@@}"),
          wrangler_syntax:empty_node(),
          api_refac:is_expr(_This@) andalso ?PP(F@)==OpName).

rule2(OpName) ->
    ?RULE(?T("f@(Args@@@) when Guards@@@ -> Bs@@@."),
          wrangler_syntax:empty_node(),
          ?PP(f@)==OpName).

rm_operation(FileName, OpName, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
               user_inputs=[OpName],
               focus_sel = {OpName,0}, 
               search_paths=SearchPaths,
               tabwidth=TabWidth},
    {ok, Res}=transform(Args),
    wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"").
 
