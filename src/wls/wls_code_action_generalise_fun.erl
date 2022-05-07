-module(wls_code_action_generalise_fun).

-behaviour(wls_code_actions).

-export([ title/0
        , id/0
        , command_args/3
        , precondition/2
        , execute_command/1
        ]).

-include_lib("wls_core.hrl").
-include_lib("kernel/include/logger.hrl").

-spec title() -> binary().
title() -> <<"Generalise function">>.

-spec id() -> action_id().
id() -> <<"generalise-fun">>.

-spec command_args(els_core:uri(), els_core:range(), state()) -> map().
command_args(Uri, Range, _State) ->
  #{ 'range' => Range
   , 'uri' => Uri
   , 'user_input' => #{'type' => variable, 'text' => <<"New argument's name">>}
  }.

-spec precondition(els_core:uri(), els_core:range()) -> boolean().
precondition(Uri, Range) ->
  Path = wls_utils:path(Uri),
  {StartPos, EndPos} = wls_utils:range(Range),
  {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(Path, true),
  case api_interface:pos_to_expr(AnnAST, StartPos, EndPos) of
    {error, _} -> 
      false;
    _Exp ->
      true
  end.

-spec execute_command([any()]) -> [map()].
execute_command([#{ <<"range">> := Range
                  , <<"uri">>   := Uri 
                  , <<"user_input">> := #{<<"value">> := NewVar}}]) ->
  {StartPos, EndPos} = wls_utils:range(Range),
  Path = wls_utils:path(Uri),
  generalise(Path, StartPos, EndPos, binary_to_list(NewVar)),
  [];
execute_command([#{ <<"range">> := Range
                  , <<"uri">>   := Uri 
                }]) ->
  {StartPos, EndPos} = wls_utils:range(Range),
  Path = wls_utils:path(Uri),
  ?LOG_INFO("Using default variable name: NewVar"),
  generalise(Path, StartPos, EndPos, "NewVar"),
  [].

%%==============================================================================
%% Private Functions
%%==============================================================================

generalise(Path, StartPos, EndPos, NewVar) ->
  try refac_gen:generalise(Path, StartPos, EndPos, NewVar, [wls_utils:root_folder()], wls, 8) of
    {ok, Changes} -> 
        wls_utils:apply_edit(Changes);
    {unknown_side_effect, Details} -> wls_utils:send_warning("Unknown side effect."),
      ?LOG_INFO("Error generalising fun - unknown side effect: {ParName1, FunName, FunArity,
        FunDefPos, Exp1, NoOfClauses, DupsInFun, DupsInClause, Cmd} = ~p", [Details]);
    {more_than_one_clause, Details} -> wls_utils:send_warning("More than one clause. Wrangler does not support generalising functions with more than one clause."),
      ?LOG_INFO("Error generalising fun - more than one clause: {ParName1, FunName, FunArity, 
        FunDefPos, Exp1, SideEffect, DupsInFun, DupsInClause, Cmd} = ~p", [Details]);
    {multiple_instances, Details} -> wls_utils:send_warning("Multiple instances. Wrangler does not support generalising functions with multiple instances."),
      ?LOG_INFO("Error generalising fun - multiple instances: {ParName1, FunName, FunArity, 
        FunDefPos, Exp1, SideEffect, DupsInFun, Cmd} = ~p", [Details]);
    Err -> wls_utils:send_error("Unknown error occurred. See logs for details."),
       ?LOG_INFO("Error generalising fun: ~p", [Err])
  catch
    _:{error, Message} -> wls_utils:send_warning(Message);
    _:E -> 
      wls_utils:send_error("Unknown error occurred. See logs for details."),
      ?LOG_INFO("Error generalising fun: ~p", [E])
  end.
