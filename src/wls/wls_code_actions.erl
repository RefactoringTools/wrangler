%%==============================================================================
%% Code Actions: Behaviour and API
%%==============================================================================

-module(wls_code_actions).

%%==============================================================================
%% Callback Functions
%%==============================================================================

%% Title which is shown to the user.
-callback title() -> binary().

%% Identifier of the action and the commands. 
%% Same as the file's name without the `wls_code_action_` prefix.
%% Use `-` instead of `_` in the identifier.
-callback id() -> action_id().

%% Initialize the action. 
%% Optional callback.
-callback init(els_core:uri(), els_core:range()) -> state().

%% Whether the action is offered based on the highlighted range. 
%% Optional callback, defaults to true.
-callback precondition(els_core:uri(), els_core:range()) -> boolean().

%% The command`s arguments.
-callback command_args(els_core:uri(), els_core:range(), state()) -> map().

%% Execute the command with the given arguments. 
%% The first element of the argument list is the one returned by command_args. 
-callback execute_command([any()]) -> [map()].

-optional_callbacks([ init/2
                    , precondition/2
                    ]).

%%==============================================================================
%% API
%%==============================================================================

-export([ enabled_actions/0
        , get_actions/2 
        , execute_command/2
        , cb_module/1
      ]).

%%==============================================================================
%% Includes
%%==============================================================================

-include("wls_core.hrl").
%-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% API
%%==============================================================================


-spec get_actions(els_core:uri(), els_core:range()) -> [code_action()].
get_actions(Doc, Range) ->
  Pred = fun (Action) -> Action /= null end,
  lists:filter(Pred, [actions(Id, Doc, Range) || Id <- enabled_actions()]).

-spec available_actions() -> [action_id()].
available_actions() ->
  [ <<"generalise-fun">>
  , <<"new-macro">>
  , <<"new-var">>
  , <<"rename-fun">>
  , <<"fold-expression">>
  , <<"move-fun">>
  , <<"new-fun">>
  , <<"rename-var">>
].

-spec enabled_actions() ->  [action_id()].
enabled_actions() ->
  Enabled = [els_utils:to_binary(Id) || Id <- wls_utils:enabled_refactorings()],
  lists:usort(lists:filter(fun is_valid/1, Enabled)).

-spec actions(action_id(), els_core:uri(), els_core:range()) ->  code_action() | null.
actions(Id, Uri, Range) ->
  CbModule = cb_module(Id),
  case precondition(CbModule, Uri, Range) of
    true ->
      State = case erlang:function_exported(CbModule, init, 1) of
                true ->
                  CbModule:init(Uri, Range);
                false ->
                  'state_not_initialized'
              end,
      make_action(CbModule, Uri, Range, State);
    false ->
      null
  end.

-spec execute_command(els_command:command_id(), [any()]) -> [map()].
execute_command(Command, Arguments) -> 
  CbModule = cb_module(Command),
  CbModule:execute_command(Arguments).


%%==============================================================================
%% Constructors
%%==============================================================================

-spec make_action(module(), els_core:uri(), els_core:range(), state()) -> code_action().
make_action(CbModule, Uri, Range, State) ->
  Title = CbModule:title(),
  Id = CbModule:id(),
  #{ title       => <<"Wrangler: ", Title/binary>>
   , kind        => ?CODE_ACTION_KIND_REFACTOR
   , command     => els_command:make_command(CbModule:title(), <<"wrangler-", Id/binary>>, [CbModule:command_args(Uri, Range, State)])
  }.


%% @doc Return the callback module for a given Code Action Identifier
-spec cb_module(action_id()) -> module().
cb_module(Id0) ->
  Id = re:replace(Id0, "-", "_", [global, {return, binary}]),
  binary_to_atom(<<"wls_code_action_", Id/binary>>, utf8).

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec is_valid(action_id()) -> boolean().
is_valid(Id) ->
  lists:member(Id, available_actions()).

-spec precondition(atom(), els_core:uri(), wls_utils:range()) -> boolean().
precondition(CbModule, Uri, Range) ->
  case erlang:function_exported(CbModule, precondition, 2) of
    true ->
      CbModule:precondition(Uri, Range);
    false ->
      true
  end.
