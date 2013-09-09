%%@hidden
%%@private
-module(refac_rename_op_arg).

-export([composite_refac/1, input_par_prompts/0, select_focus/1]).

-include("../../include/wrangler.hrl").

-behaviour(gen_composite_refac).

-compile(export_all).

input_par_prompts() ->
    ["WS operation name: ", 
     "WS operation arity: ", 
     "Index of parameter to be renamed: ",
     "New parameter name: "]. 

select_focus(_Args) ->
    {ok, none}.

composite_refac(_Args=#args{
                  current_file_name = File, 
                  user_inputs=[OpName, Arity0, Index0, NewParName]
                 }) ->
    Index=list_to_integer(Index0),
    Arity=list_to_integer(Arity0),
    Op = list_to_atom(OpName),
    Refacs = [ ?while(
                  begin
                      PreCands=collect_cands_in_precondition(File, OpName,Index, NewParName),
                      {PreCands/=[], PreCands}
                  end,
                  fun([{{Start, End}, OldVarName}|_]) ->
                          ?refac_(rename_var,
                                  [File, {precondition, 2}, {range, {File, [{Start, End}]}, OldVarName}, 
                                   mk_new_par_name(OldVarName, NewParName), [File]])
                  end
                 ),
               ?while(
                  begin
                      PostCands=collect_cands_in_postcondition(File, OpName,Index, NewParName),
                      {PostCands/=[], PostCands}
                  end,
                  fun([{Loc, OldVarName}|_])->
                          ?refac_(rename_var,
                                  [File, {postcondition, 3},  {range, {File, [Loc]}, OldVarName}, 
                                   mk_new_par_name(OldVarName, NewParName), [File]])
                  end
                 ),
               ?while(
                  begin
                      NextCands=collect_cands_in_nextstate(File, OpName,Index, NewParName),
                      {NextCands/=[], NextCands}
                  end,
                  fun([{Loc, OldVarName}|_])->
                          ?refac_(rename_var,
                                  [File, {next_state, 3},  {range, {File, [Loc]}, OldVarName}, 
                                   mk_new_par_name(OldVarName, NewParName), [File]])
                  end
                 ),
               ?while(
                  begin
                      OpCands=collect_cands_in_op(File, OpName,Index, NewParName),
                      {OpCands/=[], OpCands}
                   end,
                   fun([{Loc, OldVarName}|_])->
                           ?refac_(rename_var,
                                   [File, {Op, Arity},  {range, {File, [Loc]}, OldVarName}, 
                                     mk_new_par_name(OldVarName, NewParName), [File]])
                   end
                  )               
             ],                
        ?non_atomic(Refacs).


mk_new_par_name(_OldParName=[95|_], NewParName) ->
    "_"++NewParName;
mk_new_par_name(_, NewParName) -> 
    NewParName.


collect_cands_in_precondition(File, OpName, Index,NewParName) ->
    ?STOP_TD_TU(
       [?COLLECT(?T("precondition(S@,{call, Mod@, OpName@, [Args@@]}) when Guards@@ -> Body@@;"),
                 {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                 ?PP(OpName@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                 not lists:member(?PP(lists:nth(Index,Args@@)), [NewParName, "_"++NewParName]))
       ],[File]).

collect_cands_in_postcondition(File, OpName, Index, NewParName) ->
    ?STOP_TD_TU(
       [?COLLECT(?T("postcondition(S@,{call, ?MODULE, OpName@, [Args@@]}, Res@) when Guards@@ -> Body@@;"),
                 {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                 ?PP(OpName@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                 not lists:member(?PP(lists:nth(Index,Args@@)), [NewParName, "_"++NewParName]))
       ], [File]).

collect_cands_in_nextstate(File, OpName, Index, NewParName) ->
    ?STOP_TD_TU(
       [?COLLECT(?T("next_state(S@, R@,{call, ?MODULE, OpName@, [Args@@]}) when Guards@@ -> Body@@;"),
                 {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                 ?PP(OpName@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                 not lists:member(?PP(lists:nth(Index,Args@@)), [NewParName, "_"++NewParName]))
       ], [File]).

collect_cands_in_op(File, OpName, Index, NewParName) ->
    ?STOP_TD_TU(
       [?COLLECT(?T("f@(Args@@) when Guards@@ -> Body@@;"),
                 {api_refac:start_end_loc(lists:nth(Index, Args@@)), ?PP(lists:nth(Index, Args@@))},
                 ?PP(f@)==OpName andalso api_refac:type(lists:nth(Index,Args@@))==variable andalso
                 not lists:member(?PP(lists:nth(Index,Args@@)), [NewParName, "_"++NewParName]))
       ],[File]).
        
