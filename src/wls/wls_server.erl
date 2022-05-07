-module(wls_server).

-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_state(Path) ->
    gen_server:call(wls_server, {get_state, Path}, 50000).

start_refactoring(Path, Refactor, Data) ->
    gen_server:call(wls_server, {create_form, {Path, Refactor, Data}}, 50000).

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

handle_call({create_form, {Path, Refactor, Data}}, _From, State = #state{ets_tab=EtsTab}) ->
    {ok, Text} = file:read_file(Path),
    delete_header(Path, Text),
    file:write_file(Path, erlang:iolist_to_binary([?header, Text])),
    case Refactor of
        fold_expression ->
            {Line, Col} = Data,
            {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(Path, true, [wls_utils:root_folder()], wls_utils:tab_with()),
            case refac_fold_expression:pos_to_fun_clause(AnnAST, {Line+3, Col}) of
                {ok, {Mod, FunName, Arity, _FunClauseDef, ClauseIndex}} ->
                    try refac_fold_expression:fold_expr_by_name(
                            Path, atom_to_list(Mod), atom_to_list(FunName), 
                            integer_to_list(Arity), integer_to_list(ClauseIndex), 
                            [wls_utils:root_folder()], wls, 8) 
                    of
                        {ok, []} -> 
                            exit_form(Path),
                            {reply, ok, State};
                        {ok, Regions} -> 
                            true=ets:insert(EtsTab, {Path, #{refactor => Refactor, regions => Regions, data => {Mod, FunName, Arity, ClauseIndex}}}),
                            {reply, ok, State};
                        {error, Msg} ->
                            ?LOG_INFO("Error: ~p", [Msg]),
                            delete_header(Path),
                            {reply, {error, Msg}, State}
                    catch 
                        _:{error, Msg} ->
                            delete_header(Path),
                            {reply, {error, Msg}, State}
                    end;
                _ -> 
                    delete_header(Path),
                    ?LOG_INFO("Could not find function clause"),
                    {reply, {error, "Could not find function clause"}, State}
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
                    {Mod, FunName, Arity, ClauseIndex} = Data,
                    try refac_fold_expression:fold_expr_by_name(
                        Path, atom_to_list(Mod), atom_to_list(FunName), 
                        integer_to_list(Arity), integer_to_list(ClauseIndex), 
                        [wls_utils:root_folder()], wls, 8) 
                    of
                        {ok, []} -> 
                            exit_form(Path),
                            {reply, ok, State};
                        {ok, Regions} -> 
                            true=ets:insert(EtsTab, {Path, #{refactor => Refactor, regions => Regions, data => {Mod, FunName, Arity, ClauseIndex}}}),
                            {reply, ok, State};
                        {error, Msg} ->
                            ?LOG_INFO("Error: ~p", [Msg]),
                            {reply, {error, Msg}, State}
                    catch 
                        _:{error, Msg} -> 
                            {reply, {error, Msg}, State}
                    end;
                _ -> 
                    ?LOG_INFO("Unknown refactoring ~p", [Refactor]),
                    {reply, unknown_refactoring, State}
            end;
        _ -> 
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