
-module(wls_execute_command_provider).

-export([enabled_commands/0, execute_command/2]).
-include_lib("kernel/include/logger.hrl").

-spec enabled_commands() -> [els_command:command_id()].
enabled_commands() -> 
    Commands = wls_code_actions:enabled_actions() 
                ++ [ <<"form-exit">> 
                   , <<"comment-out-spec">> ],
    [<<"wrangler-", Cmd/binary>> || Cmd <- Commands].

-spec execute_command(els_command:command_id(), [any()]) -> [map()].
execute_command(<<"wrangler-form-exit">>, [#{ <<"uri">> := Uri }]) -> 
    Path = wls_utils:path(Uri),
    wls_server:exit_form(Path),
    [];
execute_command(<<"wrangler-comment-out-spec">>, [#{ <<"uri">> := Uri }]) ->
    refac_comment_out_spec:comment_out([wls_utils:path(Uri)]),
    [];
execute_command(Command, Arguments) ->
    Action = lists:nthtail(9, binary_to_list(Command)),
    ?LOG_INFO("Executing command: " ++ Action),
    case lists:member(list_to_binary(Action), wls_code_actions:enabled_actions()) of
        true -> wls_code_actions:execute_command(list_to_binary(Action), Arguments);
        false -> ?LOG_INFO("Unsupported command: ~p", [Command])
    end,
    [].