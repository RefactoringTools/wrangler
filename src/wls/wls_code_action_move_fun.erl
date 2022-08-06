-module(wls_code_action_move_fun).

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
title() -> <<"Move function">>.

-spec id() -> action_id().
id() -> <<"move-fun">>.

-spec command_args(els_core:uri(), els_core:range(), state()) -> map().
command_args(Uri, Range, _State) ->
  #{ 'range' => Range
   , 'uri' => Uri
   , 'user_input' => #{'type' => file, 'text' => <<"File name">>}
  }.

-spec precondition(els_core:uri(), els_core:range()) -> boolean().
precondition(Uri, Range) ->
  Path = wls_utils:path(Uri),
  {StartPos, _EndPos} = wls_utils:range(Range),
  refac_move_fun:is_available_at(Path, StartPos).

-spec execute_command([any()]) -> [map()].
execute_command([#{ <<"range">> := Range
                  , <<"uri">>   := Uri
                  , <<"user_input">> := #{<<"value">> := TargetFile}}]) ->
  {StartPos, _EndPos} = wls_utils:range(Range),
  Path = wls_utils:path(Uri),
  move_fun(Path, StartPos, binary_to_list(TargetFile)),
  [];
execute_command(_) ->
  wls_utils:send_warning("File name not provided or this refactoring is not supported in this code editor"),
  [].


%%==============================================================================
%% Private Functions
%%==============================================================================

move_fun(Path, {Line, Col}, TargetFile) ->
  try refac_move_fun:move_fun(Path, Line, Col, TargetFile, wls_utils:search_paths(), wls, wls_utils:tab_width()) of
    {ok, Changes} -> 
        wls_utils:apply_edit(Changes);
    Err -> wls_utils:send_error("Unknown error occurred. See logs for details."),
       ?LOG_INFO("Error moving fun: ~p", [Err])
  catch
    _:{error, Message} -> wls_utils:send_warning(Message);
    _:E -> 
      wls_utils:send_error("Unknown error occurred. See logs for details."),
      ?LOG_INFO("Error moving fun: ~p", [E])
  end.