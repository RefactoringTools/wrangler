-module(wls_code_action_new_var).

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
title() -> <<"Introduce new variable">>.

-spec id() ->  action_id().
id() -> <<"new-var">>.

-spec command_args(els_core:uri(), els_core:range(), state()) -> map().
command_args(Uri, Range, _State) ->
  #{ 'range' => Range
   , 'uri' => Uri
   , 'user_input' => #{'type' => variable, 'text' => <<"Variable name">>}
  }.

-spec precondition(els_core:uri(), els_core:range()) -> boolean().
precondition(Uri, Range) ->
  Path = wls_utils:path(Uri),
  {StartPos, EndPos} = wls_utils:range(Range),
  refac_intro_new_var:is_available_at(Path, StartPos, EndPos).

-spec execute_command([any()]) -> [map()].

execute_command([#{ <<"range">> := Range
                  , <<"uri">>   := Uri 
                  , <<"user_input">> := #{<<"value">> := NewVar}}]) ->
  {StartPos, EndPos} = wls_utils:range(Range),
  Path = wls_utils:path(Uri),
  new_var(Path, StartPos, EndPos, binary_to_list(NewVar)),
  [];
execute_command([#{ <<"range">> := Range
                  , <<"uri">>   := Uri
                }]) ->
  {StartPos, EndPos} = wls_utils:range(Range),
  Path = wls_utils:path(Uri),
  ?LOG_INFO("Using default variable name: NewVar"),
  new_var(Path, StartPos, EndPos, "NewVar"),
  [].


%%==============================================================================
%% Private Functions
%%==============================================================================

new_var(Path, StartPos, EndPos, NewVar) ->
  try refac_intro_new_var:intro_new_var(Path, StartPos, EndPos, NewVar, wls_utils:search_paths(), wls, wls_utils:tab_width()) of
    {ok, Changes} -> 
        wls_utils:apply_edit(Changes);
    Err -> wls_utils:send_error("Unknown error occurred. See logs for details."),
       ?LOG_INFO("Error introducing new variable: ~p", [Err])
  catch
    _:{error, Message} -> wls_utils:send_warning(Message);
    _:E -> 
      wls_utils:send_error("Unknown error occurred. See logs for details."),
      ?LOG_INFO("Error introducing new variable: ~p", [E])
  end.
