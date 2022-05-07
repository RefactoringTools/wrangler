-module(wls_code_action_rename_fun2).
%% 2th in order to avoid name clash with ELS`s rename_fun.

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
title() -> <<"Rename Function">>.

-spec id() -> action_id().
id() -> <<"rename-fun2">>.

-spec command_args(els_core:uri(), els_core:range(), state()) -> map().
command_args(Uri, Range, _State) ->
  #{ 'range' => Range
   , 'uri' => Uri
   , 'user_input' => #{'type' => atom, 'text' => <<"New function name">>}
  }.

-spec precondition(els_core:uri(), els_core:range()) -> boolean().
precondition(Uri, Range) ->
  Path = wls_utils:path(Uri),
  {StartPos, _EndPos} = wls_utils:range(Range),
  case api_interface:pos_to_fun_name(Path, StartPos) of
    {ok, _} -> true;
    _ -> false
  end.

-spec execute_command([any()]) -> [map()].
execute_command([#{ <<"range">> := Range
                  , <<"uri">>   := Uri 
                  , <<"user_input">> := #{<<"value">> := NewFun}}]) ->
  {StartPos, _EndPos} = wls_utils:range(Range),
  Path = wls_utils:path(Uri),
  rename_fun(Path, StartPos, binary_to_list(NewFun)),
  [];
execute_command(_) ->
  wls_utils:send_warning("Your code editor does not support this refactoring."),
  [].


%%==============================================================================
%% Private Functions
%%==============================================================================

rename_fun(Path, {Line, Col}, NewName) ->
  try refac_rename_fun:rename_fun(Path, Line, Col,  NewName, [wls_utils:root_folder()], wls, 8) of
    {ok, Changes} -> 
        wls_utils:apply_edit(Changes);
    {error, Message} -> wls_utils:send_warning(Message);
    Err -> wls_utils:send_error("Unknown error occurred. See logs for details."),
       ?LOG_INFO("Error renaming fun: ~p", [Err])
  catch
    _:{error, Message} -> wls_utils:send_warning(Message);
    _:E -> 
      wls_utils:send_error("Unknown error occurred. See logs for details."),
      ?LOG_INFO("Error renaming fun: ~p", [E])
  end.
