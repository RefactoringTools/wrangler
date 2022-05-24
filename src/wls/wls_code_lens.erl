%%==============================================================================
%% Code Lens: Behaviour and API
%%==============================================================================

-module(wls_code_lens).

%%==============================================================================
%% Callback Functions
%%==============================================================================

-callback init(els_dt_document:item()) -> state().
-callback command(els_dt_document:item(), els_core:poi(), state()) ->
  els_command:command(). % make sure to prefix the command with "wrangler-"
-callback pois(els_dt_document:item()) -> [els_core:poi()].
-callback precondition(els_dt_document:item()) -> boolean().
-optional_callbacks([ init/1
                    , precondition/1
                    ]).

%%==============================================================================
%% API
%%==============================================================================

-export([ available_lenses/0
        , enabled_lenses/0
        , lenses/2
        ]).


%%==============================================================================
%% Type Definitions
%%==============================================================================

-type lens() :: #{ range   := els_core:range()
                 , command => els_command:command()
                 , data    => any()
                 }.
-type lens_id() :: binary().
-type state() :: any().
-export_type([ lens/0
             , lens_id/0
             , state/0
             ]).

%%==============================================================================
%% API
%%==============================================================================

-spec available_lenses() -> [lens_id()].
available_lenses() -> [ <<"form-exit">>
                      , <<"form-refactor">>
                      , <<"comment-out-spec">> ].

-spec enabled_lenses() -> [lens_id()].
enabled_lenses() ->
  Enabled = [els_utils:to_binary(Id) || Id <- wls_utils:enabled_refactorings()],
  OtherLenses = [<<"form-exit">>, <<"form-refactor">>],
  lists:usort((OtherLenses ++ lists:filter(fun is_valid/1, Enabled))).

-spec lenses(lens_id(), els_dt_document:item()) -> [lens()].
lenses(Id, Document) ->
  CbModule = cb_module(Id),
  case precondition(CbModule, Document) of
    true ->
      State = case erlang:function_exported(CbModule, init, 1) of
                true ->
                  CbModule:init(Document);
                false ->
                  'state_not_initialized'
              end,
      [make_lens(CbModule, Document, POI, State) ||
        POI <- CbModule:pois(Document)];
    false ->
      []
  end.

%%==============================================================================
%% Constructors
%%==============================================================================

-spec make_lens(atom(), els_dt_document:item(), els_core:poi(), state()) -> lens().
make_lens(CbModule, Document, #{range := Range} = POI, State) ->
  #{ range   => els_protocol:range(Range)
   , command => CbModule:command(Document, POI, State)
   , data    => []
   }.

-spec cb_module(lens_id()) -> module().
cb_module(Id0) ->
  Id = re:replace(Id0, "-", "_", [global, {return, binary}]),
  binary_to_atom(<<"wls_code_lens_", Id/binary>>, utf8).

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec is_valid(lens_id()) -> boolean().
is_valid(Id) ->
  lists:member(Id, available_lenses()).

-spec precondition(atom(), els_dt_document:item()) -> boolean().
precondition(CbModule, Document) ->
  case erlang:function_exported(CbModule, precondition, 1) of
    true ->
      CbModule:precondition(Document);
    false ->
      true
  end.