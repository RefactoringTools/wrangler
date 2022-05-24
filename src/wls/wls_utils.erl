-module(wls_utils).

-include_lib("wls_core.hrl").
-include_lib("../../include/wrangler_internal.hrl").
-include_lib("kernel/include/logger.hrl").


-export([ range/1
        , pos/1
        , path/1
        , search_paths/0
        , enabled_refactorings/0
        , tab_with/0
        , create_file/1
        , rename_file/2
        , apply_edit/1
        , text_document_edit/2
        , get_document_changes/1
        , text_edit/1
        , send_info/1
        , send_warning/1
        , send_error/1]).

%%==============================================================================
%% ELS to WLS representation
%%==============================================================================

-spec range(els_core:range()) -> range().
range(Range) ->
  #{<<"start">> := StartPos
  , <<"end">>   := EndPos} = Range,
  {pos(StartPos), pos(EndPos)}.


-spec pos(els_core:position()) -> pos().
pos(Pos) ->
  #{<<"character">> := Col, <<"line">> := Line} = Pos,
  {Line+1, Col+1}.


-spec path(els_core:uri()) -> string().
path(Uri) ->
  binary_to_list(els_uri:path(Uri)).

%%==============================================================================
%% Config
%%==============================================================================

-spec enabled_refactorings() -> [string()].
enabled_refactorings() ->
  Config = wrangler_handler:wrangler_config(),
  maps:get("enabled_refactorings", Config, []).

-spec tab_with() -> integer().
tab_with() ->
  Config = wrangler_handler:wrangler_config(),
  maps:get("tab_with", Config, 8).

-spec search_paths() -> [string()]. %default: the project directory
search_paths() ->
  Config = wrangler_handler:wrangler_config(),
  maps:get("search_paths", Config, [binary_to_list(els_uri:path(els_config:get(root_uri)))]).


%%==============================================================================
%% LSP workspace edit constructors
%%==============================================================================

-spec apply_edit([{path(), path(), binary()}]) -> ok.
apply_edit(Changes) ->
  Method = <<"workspace/applyEdit">>,
  Params = #{
    edit => #{
      documentChanges => wls_utils:get_document_changes(Changes)
    }
  },
  ?LOG_INFO("Processing changes: ~p", [Params]),
  els_server:send_request(Method, Params).

-spec get_document_changes([{path(), path(), binary()}]) -> [map()].
get_document_changes([]) -> [];
get_document_changes([{OldPath, OldPath, Text} | Remaining]) ->
    [wls_utils:text_document_edit(OldPath, Text)] ++ get_document_changes(Remaining);
get_document_changes([{OldPath, NewPath, Text} | Remaining]) ->
    [
        wls_utils:rename_file(OldPath, NewPath),
        wls_utils:text_document_edit(NewPath, Text)
    ]
    ++ get_document_changes(Remaining).

-spec create_file(path()) -> createFile().
create_file(Name) ->
  #{
    kind => <<"create">>,
    uri => els_uri:uri(list_to_binary(Name))
  }.

-spec rename_file(path(), path()) -> renameFile().
rename_file(OldName, NewName) ->
  #{
    kind => <<"rename">>,
    oldUri => els_uri:uri(list_to_binary(OldName)),
    newUri => els_uri:uri(list_to_binary(NewName))
  }.

-spec text_document_edit(path(), binary()) -> textDocumentEdit().
text_document_edit(Name, Text) ->
  #{
  textDocument =>
    #{
      uri => els_uri:uri(list_to_binary(Name)),
      version => null
    },
  edits =>
    [
      text_edit(Text)
    ]
  }.

-spec text_edit(binary()) ->  textEdit().
text_edit(Text) -> #{
  range =>
    #{ start => #{ line => 0, character => 0},
      'end' => #{ line => ?MAX_FILE_LENGTH, character => 0}
    },
  newText => els_utils:to_binary(Text)}.


%%==============================================================================
%% LSP notifications
%%==============================================================================

send_info(Message) ->
  els_server:send_notification(<<"window/showMessage">>,
    #{ type => ?LSP_MESSAGE_TYPE_INFO,
      message => els_utils:to_binary(Message++ " (Wrangler)")
    }).

send_warning(Message) ->
  els_server:send_notification(<<"window/showMessage">>,
    #{ type => ?LSP_MESSAGE_TYPE_WARNING,
      message => els_utils:to_binary(Message++ " (Wrangler)")
    }).

send_error(Message) -> 
  els_server:send_notification(<<"window/showMessage">>,
    #{ type => ?LSP_MESSAGE_TYPE_ERROR,
      message => els_utils:to_binary(Message ++ " (Wrangler)")
    }).