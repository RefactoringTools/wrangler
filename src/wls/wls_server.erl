%%==============================================================================
%% The server handles Wrangler Forms.
%% Refactoring clients can use it`s API to create and refresh, exit forms.
%% 
%% The server uses ets to store the calculated regions where 
%% 'refactor_this_candidate' and 'exit' code lenses appear.
%% 
%%==============================================================================
-module(wls_server).

-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").
-include_lib("../../include/wrangler_internal.hrl").

-type data() :: #{'refactor' => atom(), 
                  'regions' => [region()],
                  'data' => any()}.

-type region() :: #{'range' => {pos(), pos()}, 'data' => any()}.

-export_type([data/0, region/0]).
%%==============================================================================
%% Api
%%==============================================================================

-export([start/0,
         get_state/1,
         start_refactoring/3,
         exit_form/1,
         refresh/1]).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%==============================================================================
%% Internal definitions
%%==============================================================================

-record(state, {ets_tab=none}).
-define(header,"%! Wrangler refactor form.\n%! Choose some of the highlighted refactoring candidates then exit the form.\n%! Before exiting, do not edit the file manually.\n").

%%==============================================================================
%% Api
%%==============================================================================

start() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, _Pid} -> ok;
        ignore -> ?LOG_WARNING("WLS server failed to start. Reason: ignore");
        {error,{already_started, _OtherPid}} -> ?LOG_INFO("WLS server already started.");
        {error, Reason} -> ?LOG_WARNING("WLS server failed to start. Reason: ~p", [Reason])
    end.


-spec get_state(path()) -> {'under_refactoring', data()} | 'not_exists'.
get_state(Path) ->
    try gen_server:call(wls_server, {get_state, Path}, 50000)
    catch _:{noproc, E} -> 
        wls_utils:send_info("Restarting WLS Server..."),
        ?LOG_INFO("Restarting WLS Server: ~p", [E]),
        start(),
        get_state(Path)
    end.

%% Initiate a wrangler form.
-spec start_refactoring(path(), atom(), pos()) -> 'ok' | {'error', any()} | 'unknown_refactoring'.
start_refactoring(Path, Refactor, Pos) ->
    gen_server:call(wls_server, {create_form, {Path, Refactor, Pos}}, 50000).

%% Refresh the wrangler form.
-spec refresh(path()) -> 'ok' | {'error', any()} | 'not_exists' | 'unknown_refactoring'.
refresh(Path) ->
    gen_server:call(wls_server, {recalculate_form, Path}, 50000).

exit_form(Path) ->
    gen_server:call(wls_server, {delete_form, Path}, 50000).

init(_Args) ->
    process_flag(trap_exit, true),
    EtsTab = ets:new(ast_tab, [set, protected,{read_concurrency, true}]),
    {ok, #state{ets_tab=EtsTab}}.

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

handle_call({get_state, Path}, _From, State = #state{ets_tab=EtsTab}) ->
    case ets:lookup(EtsTab, Path) of
        [{Path, Data}] -> 
            {reply, {under_refactoring, Data}, State};
        _ -> 
            {reply, not_exists, State}
    end;

handle_call({create_form, {Path, Refactor, {Line, Col}}}, _From, State = #state{ets_tab=EtsTab}) ->
    {ok, Text} = file:read_file(Path),
    delete_header(Path, Text),
    file:write_file(Path, erlang:iolist_to_binary([?header, Text])),
    case Refactor of
        fold_expression ->
            case wls_code_action_fold_expression:calculate_regions(Path, {Line+3, Col}) of
                {ok, [], _} -> 
                    delete_header(Path),
                    ets:delete(EtsTab, Path),
                    {reply, ok, State};
                {ok, Candidates, Data} ->
                    true=ets:insert(EtsTab, {Path, #{refactor => fold_expression, regions => Candidates, data => Data}}),
                    wls_utils:send_warning("Please do not modify the file manually."),
                    {reply, ok, State};
                {error, Msg} ->
                    delete_header(Path),
                    ets:delete(EtsTab, Path),
                    {reply, {error, Msg}, State}
            end;
        _ ->
            delete_header(Path),
            ?LOG_INFO("Unknown refactoring ~p", [Refactor]),
            {reply, unknown_refactoring, State}
    end;

handle_call({recalculate_form, Path}, _From, State = #state{ets_tab=EtsTab}) ->
    case ets:lookup(EtsTab, Path) of
        [{Path, #{refactor := Refactor, data := Data }}] -> 
            case Refactor of
                fold_expression ->
                    case wls_code_action_fold_expression:recalculate_regions(Path, Data) of
                        {ok, [], _} -> 
                            delete_header(Path),
                            ets:delete(EtsTab, Path),
                            {reply, ok, State};
                        {ok, Candidates, Data2} ->
                            true=ets:insert(EtsTab, {Path, #{refactor => fold_expression, regions => Candidates, data => Data2}}),
                            {reply, ok, State};
                        {error, Msg} ->
                            delete_header(Path),
                            ets:delete(EtsTab, Path),
                            {reply, {error, Msg}, State}
                    end;
                _ ->
                    delete_header(Path),
                    ets:delete(EtsTab, Path),
                    ?LOG_INFO("Unknown refactoring ~p", [Refactor]),
                    {reply, unknown_refactoring, State}
            end;
        _ -> 
            delete_header(Path),
            ?LOG_INFO("Entry does not exist"),
            {reply, not_exists, State}
    end;

handle_call({delete_form, Path}, _From, State = #state{ets_tab=EtsTab}) ->
    delete_header(Path),
    ets:delete(EtsTab, Path),
    {reply, ok, State}.
    

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State=#state{ets_tab=EtsTab}) ->
    ets:delete(EtsTab),
    ok.

-spec code_change(_, _, _) -> {'ok', _}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================


-spec delete_header([char()]) -> 'ok' | {'error', atom()}.
delete_header(Path) ->
  {ok, Text} = file:read_file(Path),
  delete_header(Path, Text).

-spec delete_header([char()], binary()) -> 'ok' | {'error', atom()}.
delete_header(Path, Text) ->
  case binary:split(Text, <<"\n">>) of
    [<<"%! Wrangler refactor form.">>, RemText1] ->
        [_, RemText2] = binary:split(RemText1, <<"\n">>),
        [_, RemText3] = binary:split(RemText2, <<"\n">>),
        file:write_file(Path, RemText3);
    _ -> ok
  end.