-ifndef(__ELS_CORE_HRL__).
-define(__ELS_CORE_HRL__, 1).



-define(CODE_ACTION_KIND_REFACTOR, <<"refactor">>).
-define(LSP_MESSAGE_TYPE_ERROR, 1).
-define(LSP_MESSAGE_TYPE_WARNING, 2).
-define(LSP_MESSAGE_TYPE_INFO, 3).
-define(LSP_MESSAGE_TYPE_LOG, 4).

-type description() :: binary().
-type syntax_type() :: 'atom' | 'variable' | 'macro'.
-type command_args() :: map().


%%==============================================================================
%% LSP Code Actions
%%==============================================================================
-type uri() :: binary().
-type action_id() :: binary().
-type state() :: any().
-type code_action_kind() :: binary().
-type code_action() :: #{ title       => binary()
                        , kind        => code_action_kind()
                        , diagnostics => [els_diagnostics:diagnostic()] % The diagnostics that this code action resolves.
                        , edit        => workspaceEdit()
                        , command     => els_command:command()
                        }.


%%==============================================================================
%% LSP document edits
%%==============================================================================

-define(MAX_FILE_LENGTH, 9999).


-type refactor_type() :: binary().

-type optionalVersionedTextDocumentIdentifier() :: #{ 'uri' := els_core:uri()
                                                    , 'version' := integer() | null 
                                                    }.
-type textEdit() :: #{ 'range'   := els_core:range()
                     , 'newText' := binary() 
                     }.

-type textDocumentEdit() :: #{ 'textDocument' := optionalVersionedTextDocumentIdentifier()
                             , 'edits'        := [textEdit()]
                             } 
                          | #{ 'changes'      := #{ els_core:uri() := [textEdit()] }
                             }.

-type workspaceEdit() :: #{ 'documentChanges' := textDocumentEdit() | renameFile() }.

-type createFile() :: #{ 'kind' :=refactor_type()
                       , 'uri'  :=els_core:uri()
                       }.

-type renameFile() :: #{ 'kind'   :=refactor_type()
                       , 'newUri' :=els_core:uri()
                       , 'oldUri' :=els_core:uri()
                       }.

-endif.