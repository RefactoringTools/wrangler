-module(wls_code_action_fold_expression).

-behaviour(wls_code_actions).

-export([ title/0
        , id/0
        , command_args/3
        , precondition/2
        , execute_command/1
        ]).

-export([ calculate_regions/2
        , recalculate_regions/2]).


-include_lib("wls_core.hrl").
-include_lib("kernel/include/logger.hrl").

-spec title() -> binary().
title() -> <<"Fold expression">>.

-spec id() -> action_id().
id() -> <<"fold-expression">>.

-spec command_args(els_core:uri(), els_core:range(), state()) -> map().
command_args(Uri, Range, _State) ->
  #{ 'range' => Range
   , 'uri' => Uri
  }.

-spec precondition(els_core:uri(), els_core:range()) -> boolean().
precondition(Uri, Range) ->
  Path = wls_utils:path(Uri),
  {StartPos, _EndPos} = wls_utils:range(Range),
  {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(Path, true),
  case refac_fold_expression:pos_to_fun_clause(AnnAST, StartPos) of
    {ok, _} -> true;
    _ ->false
  end.



%%==============================================================================
%% Calculate regions for the Wrangler Form
%%==============================================================================

calculate_regions(Path, Pos) ->
  {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(Path, true, [wls_utils:root_folder()], wls_utils:tab_with()),
  case refac_fold_expression:pos_to_fun_clause(AnnAST, Pos) of
    {ok, {Mod, FunName, Arity, _FunClauseDef, ClauseIndex}} ->
        recalculate_regions(Path, {Mod, FunName, Arity, ClauseIndex});
    _ -> {error, "Could not find function clause"}
  end.  

recalculate_regions(Path, {Mod, FunName, Arity, ClauseIndex} = Data) ->
  try refac_fold_expression:fold_expr_by_name(
    Path, atom_to_list(Mod), atom_to_list(FunName), 
    integer_to_list(Arity), integer_to_list(ClauseIndex), 
    [wls_utils:root_folder()], wls, wls_utils:tab_with()) 
    of
      {ok, Candidates} -> 
        {ok, make_regions(Candidates), Data};
      {error, Msg} -> {error, Msg}
    catch 
      _:{error, Msg} -> {error, Msg}
  end.

make_regions([{SLine, SCol, ELine, ECol, _Expr, _NewExp, _FunClauseDef} | Rem]) -> 
  [{SLine, SCol, ELine, ECol}] ++ make_regions(Rem).


%%==============================================================================
%% Execute commands
%% - initiate the refactoring with Wrangler Form
%% - Execute the selected refactoring 
%%==============================================================================

-spec execute_command([any()]) -> [map()].
execute_command([#{ <<"range">> := Range
                    , <<"uri">> := Uri
                   }]) ->
  Path = wls_utils:path(Uri),
  {StartPos, _EndPos} = wls_utils:range(Range),             
  case wls_server:start_refactoring(Path, fold_expression, StartPos) of
    ok ->  wls_utils:send_info("Select the highlighted fold candidates you want to refactor.");
    {error, Msg} -> wls_utils:send_warning(Msg)
  end,
  [];
execute_command([#{ <<"uri">> := Uri
                  , <<"index">> := Num}]) ->
  Path = wls_utils:path(Uri),
  case wls_server:get_state(Path) of
    {under_refactoring, #{refactor := Refactor, regions := Regions}} ->
      case Refactor of
        fold_expression -> 
          {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(Path, true),
          try lists:nth(Num, Regions) of
            Region -> 
              try refac_fold_expression:fold_candidate(AnnAST, Region, Path, wls, 8, "") of
                {ok, [{OldPath, _NewPath, Text}]} -> 
                  file:write_file(OldPath, Text),
                  wls_server:refresh(OldPath);
                Err -> 
                  wls_utils:send_error("Unknown error occurred. See logs for details."),
                  ?LOG_INFO("Error while doing fold: ~p", [Err])
                catch
                   _:{error, Message} -> wls_utils:send_warning(Message);
                   _:E -> 
                    wls_utils:send_error("Unknown error occurred. See logs for details."),
                    ?LOG_INFO("Error while doing fold: ~p", [E])
                end
          catch _ ->
            wls_utils:send_info("The selected candidate is not valid.")
          end;
        _ -> ?LOG_WARNING("Unknown refactoring: ~p", [Refactor])
      end;
    State -> ?LOG_WARNING("Unknown state: ~p", [State])
  end.