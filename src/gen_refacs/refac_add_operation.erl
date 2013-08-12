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
-module(refac_add_operation).

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
                      user_inputs=[StateIndex, StateCode, 
                                   PreCondIndex, PreCondCode, 
                                   PostCondIndex, PostCondCode,
                                   CmdsIndex, CmdsCode]}) ->
    ?STOP_TD_TP([rule0(next_state, StateIndex, StateCode),
                 rule0(precondition, PreCondIndex, PreCondCode),
                 rule0(postcondition,PostCondIndex, PostCondCode),
                 rule1(CmdsIndex, CmdsCode)
                ],
                [File]).


rule0(FunName, Nth, Code) ->
    ?RULE(?T("f@(Args@@@) when Guards@@@ -> Bs@@@."),
          begin
              %% StartPos = wrangler_syntax:get_pos(_This@),
              ?MATCH(?T("f1@(Args@@) when Guards@@ -> Bs@@;"), ?TO_AST(Code, {1,1})),
              NewArgs@@@=lists:sublist(Args@@@, Nth)++[Args@@]++lists:nthtail(Nth, Args@@@),
              NewGuards@@@ =lists:sublist(Guards@@@, Nth)++[Guards@@]++lists:nthtail(Nth, Guards@@@),
              NewBs@@@ =lists:sublist(Bs@@@, Nth)++[Bs@@]++lists:nthtail(Nth, Bs@@@),
              ?TO_AST("f@(NewArgs@@@) when NewGuards@@@ ->NewBs@@@")
          end,
          wrangler_syntax:is_atom(f@, FunName)).


rule1(Nth, Code) ->
    ?RULE(?T("[{Call@, M@, F@, Args@@},Cmds1@@]"),
          case Nth of 
              0 ->
                  ?TO_AST("["++Code++"{Call@, M@, F@, Args@@},"
                          ++"Cmds1@@]",wrangler_syntax:get_pos(_This@));
              1 -> 
                  ?TO_AST("[{Call@, M@, F@, Args@@}\n,      "++Code
                          ++"      Cmds1@@]",
                          wrangler_syntax:get_pos(_This@));
              _ ->
                  begin
                      Cmds2@@ = lists:sublist(Cmds1@@, Nth-1),
                      Cmds3@@ = lists:nthtail(Nth-1,Cmds1@@),
                      ?TO_AST("[{Call@, M@, F@, Args@@},\n"++
                                  "       Cmds2@@,\n       "++
                                  Code++"        Cmds3@@]",
                              wrangler_syntax:get_pos(_This@))
                  end
          end,
           wrangler_syntax:is_atom(Call@, call)).


add_op(File, NextStateIndex, NextStateCode, PreCondIndex, PreCondCode, 
           PostCondIndex, PostCondCode, CmdsIndex, CmdsCode, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=File, 
                user_inputs=[NextStateIndex, NextStateCode, 
                             PreCondIndex, PreCondCode, 
                             PostCondIndex, PostCondCode,
                             CmdsIndex, CmdsCode],
                search_paths=SearchPaths,
                tabwidth=TabWidth},
    case check_pre_cond(Args) of
        ok -> 
            {ok, Res}=transform(Args),
            wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"");
        {error, Reason} ->
            {error, Reason}
    end.
