% Copyright (c) 2010, Huiqing Li, Simon Thompson
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
%%@version 0.1 
%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%
%%
-module(wrangler_cmd_server).

-behaviour(gen_server).

-export([start_link/1, stop/0]).

-export([get_next_command/1]).

-export([init/1,handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile(export_all).

-record(state, {cmds=[], 
                changed_files=sets:new(), 
                atomic=true      ::boolean(),
                name_tracker_pid ::pid()
               }).

-include("../include/wrangler_internal.hrl").

start_link(Cmds)->
    gen_server:start({local, ?MODULE}, ?MODULE,[Cmds], []).


init([Cmds]) ->
    process_flag(trap_exit, true),
    NameTrackerPid = spawn_link(fun()->name_tracker_loop([]) end),
    CurrentPid = self(),
    Pid=spawn_link(fun()->cmd_server_loop(CurrentPid, 
                                          #state{cmds=make_list(Cmds),
                                                 name_tracker_pid=NameTrackerPid})
                   end),
    {ok, {Pid, NameTrackerPid}}.

get_next_command(PrevResult) ->
    gen_server:call(?MODULE, {get_next_command, PrevResult}).

stop() ->
    gen_server:cast(?MODULE, stop).

handle_call({get_next_command,PrevResult}, _From, State={CmdServerPid, NameTrackerPid}) ->
    wrangler_io:format("PrevResult:\n~p\n", [PrevResult]),
    PrevResult1=case PrevResult of 
                   [Result, {name_change, BeforeAfter}] ->
                       NameTrackerPid! {name_change, BeforeAfter},
                       Result;
                   _  ->
                       PrevResult
               end,
    wrangler_io:format("handlecall: get_next_command:~p\n",[PrevResult1]),
    Cmd = get_next_command(PrevResult1, {CmdServerPid, NameTrackerPid}),
    case Cmd of 
        {ok, none, _ChangedFiles, _Msg} ->
            {reply, Cmd, State};
        {ok, RefacCmd, {NextPid, NameTrackerPid}} when is_pid(NextPid) ->
            wrangler_io:format("RefacCmd:\n~p\n", [RefacCmd]),
            {reply, {ok, RefacCmd}, {NextPid, NameTrackerPid}}
    end.

get_next_command(PrevResult, {CmdServerPid, NameTrackerPid}) ->
    wrangler_io:format("CmdServerPid:\n~p\n", [CmdServerPid]),
    CmdServerPid ! {self(), get_next_command, PrevResult},
    Self = self(),
    receive 
        {get_next_command, Self, [error, Reason]} ->
            {ok, none, [], {error, Reason}};
        {get_next_command, Self, [ok, ChangedFiles]} ->
            {ok, none, ChangedFiles, "Refactoring finished successfullly."};
        {get_next_command, NextServerPid, Res} ->
            get_next_command(Res, {NextServerPid, NameTrackerPid});
        {get_next_command, NextServerPid} ->
            get_next_command(PrevResult, {NextServerPid, NameTrackerPid});
        {CmdServerPid, NextCmd, NextPid} ->
            case NextCmd of 
                {ok, none, _ChangedFiles, _Msg} ->
                    NextCmd;
                {ok, Cmd} when is_pid(NextPid)->
                    {ok, Cmd, {NextPid, NameTrackerPid}};
                _Cmd ->
                    error(format_msg("Unexpected refactoring command: ~p.\n", NextCmd))   
            end;
        Msg ->
            error(format_msg("Unexpected Message in get_next_command: ~p.\n",Msg))
    end.

handle_cast(stop, State={CmdServerPid, NameTrackerPid}) ->
    CmdServerPid ! stop,
    NameTrackerPid ! stop,
    {noreply, normal, State}.

handle_info(Info,State) ->
    {stop, Info, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cmd_server_loop(Parent, State=#state{cmds=Cmds0, changed_files=Changes}) ->
    receive 
        {From, get_next_command, PrevResult} ->
            Cmds = generate_cmds(Cmds0, State#state.name_tracker_pid),
            wrangler_io:format("Cmds in cmd_server_loop:\n~p\n",[Cmds]),
            wrangler_io:format("Cmdserver: changed files:\n~p\n", [sets:to_list(Changes)]),
            CurrentPid = self(),
            case Cmds of
                [] ->
                    case PrevResult of 
                        [error, Reason] ->
                            case State#state.atomic of 
                                true ->
                                    From ! {get_next_command, Parent, [error, Reason]};
                                false ->
                                    From ! {get_next_command, Parent, [ok, sets:to_list(Changes)]}
                            end;
                        [ok, _Modified] ->
                            ModifiedSoFar =update_modified_files(PrevResult,Changes),
                            From ! {get_next_command, Parent, [ok, sets:to_list(ModifiedSoFar)]}
                    end;
                [C|Cs] ->
                    wrangler_io:format("C in cmd server:\n~p\n",[C]),
                    wrangler_io:format("PrevResult:\n~p\n", [PrevResult]), 
                    case PrevResult of 
                        [error, Reason] when State#state.atomic->
                            From ! {get_next_command, Parent, [error, Reason]};
                        _ ->
                            ModifiedSoFar = update_modified_files(PrevResult,Changes),
                            CurrentPid = self(),
                            wrangler_io:format("ModifiedSoFar:\n~p\n", [ModifiedSoFar]),
                            process_flag(trap_exit, true),
                            case C of  
                                {non_atomic, CRs} ->
                                    NewState=State#state{cmds=CRs, atomic=false,
                                                         changed_files=sets:new()},
                                    Pid = spawn_link(fun()->
                                                             cmd_server_loop(CurrentPid, NewState)
                                                     end),
                                    From ! {get_next_command, Pid, [ok, []]};
                                {atomic, CRs} ->
                                    NewState=State#state{cmds=CRs, atomic=true,
                                                         changed_files=sets:new()},
                                    Pid = spawn_link(fun()->
                                                             cmd_server_loop(CurrentPid, NewState)
                                                     end),
                                    From ! {get_next_command, Pid, [ok, []]};
                                _ ->
                                    LoopName = make_loop_name(C),
                                    wrangler_io:format("LoopName:\n~p\n", [LoopName]),
                                    Pid = spawn_link(fun()->
                                                             ?MODULE:LoopName(CurrentPid,  
                                                                              State#state{cmds=C, 
                                                                                          atomic = true,
                                                                                          changed_files=sets:new()})
                                                     end),
                                    From ! {get_next_command, Pid, [ok, []]}
                            end,
                            cmd_server_loop(Parent, State#state{cmds=Cs, changed_files=ModifiedSoFar})
                    end
            end;
        {'EXIT', _ChildrenPid, normal} ->
            cmd_server_loop(Parent, State);
        {'EXIT', ChildrenPid, Reason} ->
            Parent!{'EXIT', ChildrenPid, Reason};
        stop -> 
            ok;
        Msg ->
            error(format_msg("Unexpected message in cmd_server_loop:~p\n", Msg))  
    end.

elementary_refac_loop(Parent, State=#state{cmds=ER}) ->
    receive 
        {From, get_next_command, PrevResult} ->
            case ER of
                {refactoring, Refac, _Question}->
                    NewState=update_state(State, PrevResult),
                    From ! {self(), {ok, {refactoring, Refac}}, self()},
                    elementary_refac_loop(Parent, NewState#state{cmds=none});
                none ->
                    From ! {get_next_command, Parent, PrevResult}
            end;
        Msg -> 
            error(format_msg("Unexpected message in elementary_refac_loop: ~p.\n", Msg))
    end.

interactive_refac_loop(Parent, State=#state{cmds={interactive, ER}}) 
  when not is_list(ER) ->
    interactive_refac_loop(Parent, State#state{cmds={interactive, make_list(ER)}});
interactive_refac_loop(Parent, State=#state{cmds={interactive, ER0s},
                                            changed_files=Changes}) ->
    receive 
        {From, get_next_command, PrevResult} ->
            wrangler_io:format("ER0s:\n~p\n", [ER0s]),
            ERs = generate_cmds(ER0s,State#state.name_tracker_pid),
            wrangler_io:format("ERs:\n~p\n", [ERs]),
            wrangler_io:format("PrevResult:\n~p\n", [PrevResult]),
            case PrevResult of
                [error, _Reason] ->
                    case State#state.atomic of 
                        true ->
                            From ! {get_next_command, Parent, PrevResult};
                        false ->
                            case ERs of
                                [{refactoring, Refac, Question}|Cs] ->
                                    From ! {self(), {ok, {interactive, Question, Refac}}, self()},
                                    interactive_refac_loop(Parent, State#state{cmds={interactive, Cs}});
                                [] ->
                                    From ! {get_next_command, Parent, [ok,Changes]}
                            end
                    end;
                [ok, _Modified] ->
                    case ERs of
                        [{refactoring, Refac, Question}|Cs] ->
                            NewState=update_state(State, PrevResult),
                            From ! {self(), {ok, {interactive, Question, Refac}}, self()},
                            interactive_refac_loop(Parent, NewState#state{cmds={interactive, Cs}});
                        [] ->
                            ModifiedSoFar =update_modified_files(PrevResult,Changes),
                            From ! {get_next_command, Parent, [ok, sets:to_list(ModifiedSoFar)]}
                    end
            end;
        Msg ->
            error(format_msg("Unexpected message in interactive_refac_loop: ~p.\n", Msg))
    end.

repeat_interactive_refac_loop(Parent, State=#state{cmds={repeat_interactive, ER}}) 
  when not is_list(ER) ->
    repeat_interactive_refac_loop(Parent, State#state{cmds={repeat_interactive, 
                                                            make_list(ER)}});
repeat_interactive_refac_loop(Parent, State=#state{cmds={repeat_interactive, ER0s},
                                                   changed_files=Changes}) ->
    receive 
        {From, get_next_command, PrevResult} ->
            ERs = generate_cmds(ER0s,State#state.name_tracker_pid),
            case ERs of
                [] -> 
                    ModifiedSoFar =update_modified_files(PrevResult,Changes),
                    From !{get_next_command, Parent, [ok, sets:to_list(ModifiedSoFar)]};
                _ ->
                    case PrevResult of
                        none ->
                            case ERs of 
                                [_C] ->
                                    From ! {get_next_command, Parent, [ok, sets:to_list(Changes)]};
                                [_C|Cs] ->
                                    From !{get_next_command, self(), [ok, []]},
                                    repeat_interactive_refac_loop(Parent, State#state{cmds={repeat_interactive, Cs}})
                            end;
                        [error, _Reason] ->
                            case State#state.atomic of 
                                true ->
                                    From ! {get_next_command, Parent, PrevResult};
                                false->
                                    [{refactoring, Refac, Question}|_Cs]=ERs,
                                    From ! {self(), {ok, {repeat_interactive, Question, Refac}}, self()},
                                    repeat_interactive_refac_loop(Parent,State)
                            end;
                        [ok, _Modified] ->
                            NewState=update_state(State, PrevResult),
                            [{refactoring, Refac, Question}|_Cs]=ERs,
                            From ! {self(), {ok, {repeat_interactive, Question, Refac}}, self()},
                            repeat_interactive_refac_loop(Parent, NewState)
                    end
            end;
        Msg ->
            error(format_msg("Unexpected message in repeat_interactive_refac_loop: ~p.\n", Msg))
    end.

while_refac_loop(Parent, State=#state{cmds={while, Cond, CmdGen},
                                      changed_files=Changes}) ->
    receive
        {From, get_next_command, PrevResult} ->
            case PrevResult of 
                [error, Reason] when State#state.atomic ->
                    From ! {get_next_command, Parent, [error, Reason]};
                _ ->
                    ModifiedSoFar = update_modified_files(PrevResult,Changes),
                    case Cond() of
                        true ->
                            Res = generate_cmds(CmdGen,State#state.name_tracker_pid),
                            case Res of 
                                [] ->
                                    From !{get_next_command, Parent, [ok, sets:to_list(ModifiedSoFar)]};
                                _ ->
                                    process_flag(trap_exit, true),
                                    Pid = spawn_link(fun() ->
                                                             cmd_server_loop(Parent, State#state{cmds=Res})
                                                     end),
                                    From ! {get_next_command, Pid, [ok, []]},
                                    while_refac_loop(Parent, State#state{cmds={while, Cond, CmdGen},
                                                                            changed_files=ModifiedSoFar})
                            end;
                        false ->
                            From !{get_next_command, Parent, [ok, sets:to_list(ModifiedSoFar)]}
                    end
            end;
        {'EXIT', ChildrenPid, Reason} ->
            wrangler_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
            while_refac_loop(Parent, State);
        Msg ->
            error(format_msg("Unexpected message in while_refac_loop:~p\n", Msg))    
    end.

%% not really a loop.
if_then_else_refac_loop(Parent, State=#state{cmds={if_then_else, Cond, CRs1, CRs2}}) ->
    receive 
        {From, get_next_command, _PrevResult} ->
            CRs = case Cond() of 
                      true ->
                          make_list(CRs1);
                      false ->
                          make_list(CRs2)
                  end,
            Pid = spawn_link(fun()->
                                     cmd_server_loop(Parent, State#state{cmds=CRs})
                             end),
            process_flag(trap_exit, true),
            From ! {get_next_command, Pid, [ok, []]};
        {'EXIT', ChildrenPid, Reason} ->
            wrangler_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
            if_then_else_refac_loop(Parent, State);
        Msg ->
            error(format_msg("Unexpected message in if_then_else_refac_loop:~p\n", Msg))  
    end.  

%% not really a loop.
if_then_refac_loop(Parent, State=#state{cmds={if_then, Cond, CRs}}) ->
    receive
        {From, get_next_command, _PrevResult} ->
            case Cond() of 
                true ->
                    CR1s = make_list(CRs),
                    Pid = spawn_link(fun()->
                                             cmd_server_loop(Parent, State#state{cmds=[CR1s]})
                                     end),
                    process_flag(trap_exit, true),
                    From ! {get_next_command, Pid, [ok,[]]};
                false ->
                    From !{get_next_command, Parent, [ok, []]}
            end;
        {'EXIT', ChildrenPid, Reason} ->
            wrangler_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
            if_then_refac_loop(Parent, State);
        Msg ->
            error(format_msg("Unexpected message in if_then_refac_loop:~p\n", Msg))  
    end.

       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%%              Refactoring command generator.                                %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This function takes the first command generator in the list, expands it
%% and appends the result to the front of the list of cmd generators.

generate_cmds(Cmd, NameTrackerPid) when not is_list(Cmd) ->
    generate_cmds([Cmd], NameTrackerPid);
generate_cmds([], _) -> [];
generate_cmds([Cmd|Cmds], NameTrackerPid) ->
    wrangler_io:format("Generate_cmds:Cmd:\n~p\n",[Cmd]), 
    Cmd1 = generate_a_cmd(Cmd, NameTrackerPid),
    wrangler_io:format("Cmd1:\n~p\n", [Cmd1]),
    case Cmd1 of 
        [] ->
            generate_cmds(Cmds, NameTrackerPid);
        {C, {generator, Gen}} ->
            [C, {generator, Gen}|Cmds];
        C when is_list(C) ->
            C++Cmds;
        C ->
            [C|Cmds]
    end. 

generate_a_cmd(Cmd={refactoring, _, _},_NameTrackerPid)->
    Cmd;
generate_a_cmd(Cmd={{refactoring,_, _}, {generator, _}}, _NameTrackerPid) ->
    Cmd;
generate_a_cmd({generator, Gen}, _NameTrackerPid) ->
    case Gen() of 
        [] ->
            [];
        {Cmd, {generator, Gen}} ->
            {Cmd, {generator, Gen}};
        Cmd ->
            Cmd
    end;
generate_a_cmd(Cmd={RefacName, Args}, NameTrackerPid) ->
    wrangler_io:format("NameTrackerPid:\n~p\n", [NameTrackerPid]),
    wrangler_io:format("Cmd_in_generate_a_cmd:\n~p\n", [Cmd]),
    wrangler_io:format("RefacName:\n~p\n", [RefacName]),
    case lists:member(RefacName, elementary_refacs()) andalso is_list(Args) of
        true ->
            NameTrackerPid ! {self(), update_args, Args},
            receive
                {NameTrackerPid, NewArgs} ->
                    try 
                        wrangler_io:format("RefacName:\n~p\n", [RefacName]),
                        Cmds=apply(wrangler_extended, RefacName, NewArgs),
                        wrangler_io:format("Cmdsdd:\n~p\n", [Cmds]),
                        generate_cmds(Cmds, NameTrackerPid)
                    catch
                        E1:E2 ->
                            throw({error, {E1,{E2, erlang:get_stacktrace()}}})
                    end                    
            end;
        false ->
            Cmd            
    end; 
generate_a_cmd(Cmd, _NameTrackerPid) ->
    Cmd.


update_state(State=#state{changed_files=Changes}, PrevResult) ->
    State#state{changed_files= update_modified_files(PrevResult,Changes)};
update_state(State, _) ->
    State.

make_loop_name(CR) ->
    case CR of 
        {refactoring, _, _} ->
            elementary_refac_loop;
        {{refactorng, _, _}, _Gen} ->
            elementary_refac_loop;
        {interactive, _} ->
            interactive_refac_loop;
        {repeat_interactive, _} ->
            repeat_interactive_refac_loop;
        {while, _, _} ->
            while_refac_loop;
        {if_then_else, _Cond, _CR1, _CR2}  ->
            if_then_else_refac_loop;
        {if_then, _Cond, _CR} ->
            if_then_refac_loop;
        _ ->
            throw({error, format_msg("Illegal composite refactoring cmd:\n~p\n", CR)})      
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%%              Name tracker in refactoring cmds.                             %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Node: the user should be able to state both the old enity and the new entity.
%% BY default, we refer to the new entity; but if the new name is unknown from 
%% the script, it is possible to refer to the old entity name, but that must be 
%% flagged!!!
name_tracker_loop(State) ->
    receive
        {name_change, BeforeAfter} ->
            NewState=update_name_history(State, BeforeAfter),
            name_tracker_loop(NewState);
        {From, update_args, Args} ->
            NewArgs = rename_in_args(Args, State),
            From ! {self(), NewArgs},
            name_tracker_loop(State);
        stop ->
            ok
    end.

update_name_history(H, BeforeAfter) ->
    lists:foldl(fun({Before, After}, CurH) ->
                        update_name_history_1(CurH, {Before, After})
                end, H, BeforeAfter).
update_name_history_1(H, {Before, After}) ->
    Before1 = normalise(Before),
    update_name_history_1(H, {Before1, After}, []).

update_name_history_1([], {Before, After}, Acc) ->
    [{Before, [After]}|Acc];
update_name_history_1([{Key, [Before|Bs]}|Ls], {Before, After}, Acc) ->
    [{Key, [After,Before|Bs]}|Ls]++Acc;
update_name_history_1([L|Ls], {Before, After}, Acc) ->
    update_name_history_1(Ls, {Before, After}, [L|Acc]).


rename_in_args(Args, State) ->
    [case Arg of
         {old, A} ->
             Arg1=normalise(A),
             case lists:keyfind(Arg1, 1,State) of
                 {Arg1, NewNames} ->
                     hd(NewNames);
                 false -> 
                     A    %% should throw error here!!!
             end;
         _ -> 
             Arg
     end||Arg <- Args].
    
normalise(Entry) ->
    case Entry of
        {M, F, A} ->
            M1 = case is_list(M) of 
                     true when M/=[]->
                         list_to_atom(M);
                     _ ->
                         M
                 end,
            F1 = case is_list(F) of 
                     true  when M/=[]->
                         list_to_atom(F);
                     _ -> 
                         F
                 end,
            A1 = case is_list(A) of 
                     true  when M/=[]->
                         list_to_integer(A);
                     _ -> A
                 end,
            {M1, F1, A1};
        _ -> Entry
    end.
                      
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%              Some utility functions                              %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The names of the elementary refactorings supported.
elementary_refacs() ->
    [rename_var, rename_fun, swap_args, fold_expr, gen_fun, move_fun, unfold_fun_app].
                
format_msg(Format, Data) ->
    lists:flatten(io_lib:format(Format, [Data])).

update_modified_files([error, _], ExistingChanges) ->
    ExistingChanges;
update_modified_files([ok, NewChanges], ExistingChanges) -> 
    sets:union(ExistingChanges, sets:from_list(NewChanges)).

make_list(CRs) when is_list(CRs)->
    CRs;
make_list(CRs) ->
    [CRs].
  
