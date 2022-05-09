-module(wls_code_action_new_fun).

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
title() -> <<"Extract function">>.

-spec id() ->  action_id().
id() -> <<"new-fun">>.

-spec command_args(els_core:uri(), els_core:range(), state()) -> map().
command_args(Uri, Range, _State) ->
  #{ 'range' => Range
   , 'uri' => Uri
   , 'user_input' => #{'type' => atom, 'text' => <<"The new function`s name">>}
  }.

-spec precondition(els_core:uri(), els_core:range()) -> boolean().
precondition(Uri, Range) ->
  Path = wls_utils:path(Uri),
  {StartPos, EndPos} = wls_utils:range(Range),
  refac_new_fun:is_available_at(Path, StartPos, EndPos).

-spec execute_command([any()]) -> [map()].

execute_command([#{ <<"range">> := Range
                  , <<"uri">>   := Uri 
                  , <<"user_input">> := #{<<"value">> := NewFun}}]) ->
  {StartPos, EndPos} = wls_utils:range(Range),
  Path = wls_utils:path(Uri),
  new_fun(Path, StartPos, EndPos, binary_to_list(NewFun)),
  [];
execute_command([#{ <<"range">> := Range
                  , <<"uri">>   := Uri
                }]) ->
  {StartPos, EndPos} = wls_utils:range(Range),
  Path = wls_utils:path(Uri),
  ?LOG_INFO("Using default variable name: NewFun"),
  new_fun(Path, StartPos, EndPos, "NewFun"),
  [].


%%==============================================================================
%% Private Functions
%%==============================================================================

new_fun(Path, StartPos, EndPos, NewFun) ->
    try refac_new_fun:fun_extraction(Path, StartPos, EndPos, NewFun, wls, 8) of
      {ok, Changes} -> 
          wls_utils:apply_edit(Changes);
      Err -> wls_utils:send_error("Unknown error occurred. See logs for details."),
         ?LOG_INFO("Error extracting fun: ~p", [Err])
    catch
      _:{error, Message} -> wls_utils:send_warning(Message);
      _:E -> 
        wls_utils:send_error("Unknown error occurred. See logs for details."),
        ?LOG_INFO("Error extracting fun: ~p", [E])
    end.