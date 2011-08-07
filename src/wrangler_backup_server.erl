%% Copyright (c) 2010, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
%%% File    : wrangler_undo_server.erl
%%% Author  :  <Huiqing>
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%%------------------------------------------------------------------- 
-module(wrangler_backup_server).

-behaviour(gen_server).

%% API
-export([start_backup_server/0,add_to_backups/1, recover_backups/0, reset_backups/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/wrangler_internal.hrl").

-record(state, {backups=[],
                new_files=[],
                preview_pairs=[]}).

%%====================================================================
%% API
%%====================================================================
start_backup_server() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, wrangler_backup_server}, ?MODULE, [], []).

add_to_backups(Files) ->
    gen_server:cast(wrangler_backup_server, {add_backups, Files}).

recover_backups() ->
    gen_server:call(wrangler_backup_server, recover_backups).

reset_backups()->
    gen_server:cast(wrangler_backup_server, reset_backups).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(recover_backups, _From, _State=#state{backups=BackUps,   
                                                 new_files=_NewFiles,
                                                 preview_pairs=PreviewPairs})->
    lists:foreach(fun({F, Content})->
                          SwpFileName = filename:rootname(F, ".erl")++ ".erl.swp",
                          file:copy(F, SwpFileName),
                          file:write_file(F, Content)
                  end, BackUps),
    {reply, {ok, PreviewPairs}, #state{}}.

handle_cast(reset_backups, _State) ->
    {noreply, #state{}};
handle_cast({add_backups,Files},State=#state{backups=BackUps, 
                                             new_files=NewFiles,
                                             preview_pairs=PreviewPairs}) ->
    {NewBackUps, NewFiles1, NewPreviewPairs} = update_backups(Files, {BackUps, NewFiles, PreviewPairs}),
    {noreply, State#state{backups=NewBackUps, new_files=NewFiles1, preview_pairs=NewPreviewPairs}}.

update_backups([], {BackUps, NewFiles, PreviewPairs}) ->
    {BackUps, NewFiles, PreviewPairs};
update_backups([{FileName, NewFileName}|Fs], {BackUps, NewFiles, PreviewPairs}) ->
    update_backups([{FileName, NewFileName, false}|Fs], {BackUps, NewFiles, PreviewPairs});
update_backups([{FileName, NewFileName, true}|Fs], {BackUps, NewFiles, PreviewPairs}) ->
    SwpFileName = filename:join([filename:rootname(NewFileName)++".erl.swp"]),
    NewFiles1 = [NewFileName|NewFiles],
    update_backups(Fs, {BackUps, NewFiles1,
                        [{{FileName, NewFileName, true}, SwpFileName}|PreviewPairs]});
update_backups([{FileName, NewFileName, false}|Fs], {BackUps, NewFiles, PreviewPairs}) ->
    NewPreviewPairs = update_preview_pairs(FileName, NewFileName, PreviewPairs),
    NewBackUps = do_update_backups(FileName,BackUps),
    update_backups(Fs, {NewBackUps, NewFiles,NewPreviewPairs}).

do_update_backups(OldFileName, BackUps) ->
    case lists:keyfind(OldFileName, 1, BackUps) of
        {OldFileName, _Content} ->
            BackUps;
        false ->
            {ok, Content} = file:read_file(OldFileName),
            [{OldFileName, Content}|BackUps]
    end.

update_preview_pairs(FileName, NewFileName,PreviewPairs) ->
    SwpFileName = filename:join([filename:rootname(NewFileName) ++ ".erl.swp"]),
    case [FName||{{_F, FName, _IsNew},_Swp}<-PreviewPairs, FName==FileName] of 
        [] -> [{{FileName, NewFileName, false}, SwpFileName}|PreviewPairs];
        _ ->
            lists:map(fun(Pair) ->
                              case Pair of
                                  {{F1, FileName, IsNew}, _Swp} ->
                                      {{F1, NewFileName, IsNew}, SwpFileName};
                                  _ ->
                                      Pair
                              end
                      end, PreviewPairs)
    end.
   
%%-------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

