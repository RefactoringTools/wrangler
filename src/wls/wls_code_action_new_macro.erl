-module(wls_code_action_new_macro).

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
title() -> <<"Introduce new macro">>.

-spec id() -> action_id().
id() -> <<"new-macro">>.

-spec command_args(els_core:uri(), els_core:range(), state()) -> map().
command_args(Uri, Range, _State) ->
  #{ 'range' => Range
   , 'uri' => Uri
   , 'user_input' => #{'type' => macro, 'text' => <<"Macro name">>}
  }.

-spec precondition(els_core:uri(), els_core:range()) -> boolean().
precondition(Uri, Range) ->
  Path = wls_utils:path(Uri),
  {StartPos, EndPos} = wls_utils:range(Range),
  {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(Path, true),
  case api_interface:pos_to_expr_or_pat_list(AnnAST, StartPos, EndPos) of
    [] -> false;
    _ExpList ->
      true
  end.

-spec execute_command([any()]) -> [map()].
execute_command([#{ <<"range">> := Range,
                    <<"uri">>   := Uri,
                    <<"user_input">> := #{<<"value">> := NewMacro}}]) ->
  {StartPos, EndPos} = wls_utils:range(Range),
  Path = wls_utils:path(Uri),
  new_macro(Path, StartPos, EndPos, binary_to_list(NewMacro)),
  [];
execute_command([#{ <<"range">> := Range,
                    <<"uri">>   := Uri
                }]) ->
  {StartPos, EndPos} = wls_utils:range(Range),
  Path = wls_utils:path(Uri),
  ?LOG_INFO("Using default macro name: NewMacro"),
  new_macro(Path, StartPos, EndPos, "NewMacro"),
  [].



%%==============================================================================
%% Private Functions
%%==============================================================================

new_macro(Path, StartPos, EndPos, NewMacro) ->
  try refac_new_macro:new_macro(Path, StartPos, EndPos, NewMacro, wls_utils:search_paths(), wls, wls_utils:tab_width()) of
    {ok, Changes} -> 
        wls_utils:apply_edit(Changes);
    Err -> wls_utils:send_error("Unknown error occurred. See logs for details."),
       ?LOG_INFO("Error introducing new macro: ~p", [Err])
  catch
    _:{error, Message} -> wls_utils:send_warning(Message);
    _:E -> 
      wls_utils:send_error("Unknown error occurred. See logs for details."),
      ?LOG_INFO("Error introducing new macro: ~p", [E])
  end.
