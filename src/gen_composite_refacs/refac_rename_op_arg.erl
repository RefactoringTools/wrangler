%% Copyright (c) 2010, Huiqing Li, Simon Thompson
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
%%
%%@doc This refactoring can be used when a parameter of a WS operation has 
%%     been renamed. To invoke this refactoring, point the cursor to the original 
%%     variable name in the wrapper function, then select  
%%    'rename WS operation argument' from the 'Refactorings for QuickCheck' sub-menu.
%%    This refactoring does not work with ```eqc_statem''' group syntax yet.
%%@hidden
%%@private
-module(refac_rename_op_arg).

-export([composite_refac/1, input_par_prompts/0, select_focus/1]).

-include("wrangler.hrl").

-behaviour(gen_composite_refac).

-compile(export_all).

input_par_prompts() ->["New parameter name: "].

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
        
 
composite_refac(_Args=#args{
                  current_file_name = File, 
                  focus_sel = {OpName0, Arity, Index},
                  user_inputs=[NewVarName]
                 }) ->
    OpName = atom_to_list(OpName0),
    Refacs = [ ?while(
                  begin
                      PreCands=collect_cands_in_precondition(File, OpName, Index,NewVarName),
                      {PreCands/=[], PreCands}
                  end,
                  fun([{Loc, OldVarName1}|_]) ->
                          ?refac_(rename_var,
                                  [File, {precondition, 2}, {range, {File, [Loc]}, OldVarName1}, 
                                   mk_new_par_name(OldVarName1, NewVarName), [File]])
                  end
                 ),
               ?while(
                   begin
                       PreCands=collect_cands_in_postcondition(File, OpName, Index, NewVarName),
                       {PreCands/=[], PreCands}
                   end,
                   fun([{Loc, OldVarName1}|_]) ->
                           ?refac_(rename_var,
                                   [File, {postcondition, 3}, {range, {File, [Loc]}, OldVarName1}, 
                                    mk_new_par_name(OldVarName1, NewVarName), [File]])
                   end
               ),
               ?while(
                   begin
                       NextCands=collect_cands_in_nextstate(File, OpName, Index, NewVarName),
                       {NextCands/=[], NextCands}
                   end,
                  fun([{{Start, End}, OldVarName1}|_]) ->
                          ?refac_(rename_var,
                                  [File, {next_state, 3}, {range, {File, [{Start, End}]}, OldVarName1}, 
                                   mk_new_par_name(OldVarName1, NewVarName), [File]])
                  end
                 ),
               ?while(
                   begin
                       NextCands=collect_cands_in_op(File, OpName, Index, NewVarName),
                       {NextCands/=[], NextCands}
                   end,
                  fun([{{Start, End}, OldVarName1}|_]) ->
                          ?refac_(rename_var,
                                  [File, {OpName, Arity}, {range, {File, [{Start, End}]}, OldVarName1}, 
                                   mk_new_par_name(OldVarName1, NewVarName), [File]])
                   end
                  )
             ],                
    ?non_atomic(Refacs).


mk_new_par_name(_OldParName=[95|_], NewParName) ->
    "_"++NewParName;
mk_new_par_name(_, NewParName) -> 
    NewParName.


collect_cands_in_precondition(File, OpName, Index, NewVarName) ->
    ?STOP_TD_TU(
       [?COLLECT(?T("precondition(S@, {call, ?MODULE, OpName@, [Args@@]}) when Guards@@ -> Body@@;"),
                  {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                  ?PP(OpName@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                 not lists:member(?PP(lists:nth(Index,Args@@)), [NewVarName, "_"++NewVarName])),
        ?COLLECT(?T("precondition(S@, {call, ?MODULE, OpName@, [{Args@@}]}) when Guards@@ -> Body@@;"),
                  {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                  ?PP(OpName@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                  not lists:member(?PP(lists:nth(Index,Args@@)), [NewVarName, "_"++NewVarName]))
       ],[File]).


collect_cands_in_postcondition(File, OpName, Index, NewVarName) ->
    ?STOP_TD_TU(
       [?COLLECT(?T("postcondition(S@, {call, ?MODULE, OpName@, [Args@@]}, Res@) when Guards@@ -> Body@@;"),
                  {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                  ?PP(OpName@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                 not lists:member(?PP(lists:nth(Index,Args@@)), [NewVarName, "_"++NewVarName])),
        ?COLLECT(?T("postcondition(S@, {call, ?MODULE, OpName@, [{Args@@}]}, Res@) when Guards@@ -> Body@@;"),
                  {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                  ?PP(OpName@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                  not lists:member(?PP(lists:nth(Index,Args@@)), [NewVarName, "_"++NewVarName]))
       ],[File]).


collect_cands_in_nextstate(File, OpName, Index, NewVarName) ->
    ?STOP_TD_TU(
       [?COLLECT(?T("next_state(S@, R@, {call, ?MODULE, OpName@, [Args@@]}) when Guards@@ -> Body@@;"),
                  {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                  ?PP(OpName@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                 not lists:member(?PP(lists:nth(Index,Args@@)), [NewVarName, "_"++NewVarName])),
        ?COLLECT(?T("next_state(S@, R@, {call, ?MODULE, OpName@, [{Args@@}]}) when Guards@@ -> Body@@;"),
                  {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                  ?PP(OpName@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                  not lists:member(?PP(lists:nth(Index,Args@@)), [NewVarName, "_"++NewVarName]))
       ],[File]).


collect_cands_in_op(File, OpName, Index, NewVarName) ->
     ?STOP_TD_TU(
        [?COLLECT(?T("f@(Args@@) when Guards@@ -> Body@@;"),
                  {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                  ?PP(f@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                  not lists:member(?PP(lists:nth(Index,Args@@)), [NewVarName, "_"++NewVarName])),
         ?COLLECT(?T("f@({Args@@}) when Guards@@ -> Body@@;"),
                  {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                  ?PP(f@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                  not lists:member(?PP(lists:nth(Index,Args@@)), [NewVarName, "_"++NewVarName]))
        ],[File]).
        
