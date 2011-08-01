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
%%@version 0.1 
%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%
%%
-module(wrangler_cmd_server).

-export([start_name_tracker_server/0, start_cmd_server/2,
         stop_name_tracker_server/1, stop_cmd_server/1,
         get_next_command/2]).

-compile(export_all).

-record(state, {cmds=[], 
                changed_files=sets:new(), 
                atomic=true      ::boolean(),
                success_times=0  ::integer(),
                name_tracker_pid ::pid()
               }).

-include("../include/wrangler_internal.hrl").

start_name_tracker_server()->
    process_flag(trap_exit, true),
    Pid=spawn_link(fun()->name_tracker_loop([]) end),
    %%TODO: need to trap the abnormal exit of Pid.
    Pid.

start_cmd_server(Cmds, NameTrackerPid)->
    process_flag(trap_exit, true),
    Pid=spawn_link(fun()->cmd_server_loop(none, 
                                          #state{cmds=make_list(Cmds),
                                                 name_tracker_pid=NameTrackerPid})
                   end),
    %%TODO: need to trap the abnormal exit of Pid.
    Pid.

stop_cmd_server(Pid) ->
    Pid!stop.

stop_name_tracker_server(Pid) ->
    Pid!stop.
  
get_next_command({CmdServerPid, NameTrackerPid}, PrevResult) ->
    ?debug("Next command Pid:\n~p\n", [CmdServerPid]),
    ?debug("RevResult:\n~p\n", [PrevResult]),
    PrevResult1 =case PrevResult of 
                     [Result, {name_change, BeforeAfter}] ->
                          ?debug("Before:\n~p\n", [BeforeAfter]),
                         NameTrackerPid! {name_change, BeforeAfter},
                         Result;
                     Result ->
                         Result
                 end,
    ?debug("PreResult1:\n~p\n", [PrevResult1]),
    ?debug("CmdSeverPid:\n~p\n", [CmdServerPid]),
    CmdServerPid ! {self(), get_next_command, PrevResult1},
    receive 
        {get_next_command, NextServerPid} ->
            get_next_command({NextServerPid, NameTrackerPid}, PrevResult1);
        {get_next_command, NextServerPid, Res} ->
            get_next_command({NextServerPid, NameTrackerPid}, Res);
        {CmdServerPid, NextCmd, NextPid} ->
            case NextCmd of 
                {ok, none, ChangedFiles, Msg} ->
                    case Msg of 
                        [error, Reason] ->
                            refac_io:format("ChangedFiles:\n~p\n", [ChangedFiles]),
                            _PreviewPairs = stop_servers_and_recover_backups(CmdServerPid,NameTrackerPid),
                            {ok, none, ChangedFiles, {error, Reason}};
                        _ ->
                            PreviewPairs = stop_servers_and_recover_backups(CmdServerPid,NameTrackerPid),
                            wrangler_preview_server:add_files({PreviewPairs, ""}),
                            {ok, none, ChangedFiles, Msg}
                    end;
                {ok, Cmd} ->
                    ?debug("NextPid:\n~p\n", [NextPid]),
                    {ok, Cmd, {NextPid, NameTrackerPid}};
                Cmd ->
                    throw({error, format_msg("Unexpected refactoring command: ~p.\n", Cmd)})   
            end;
        Msg ->
            throw({error, format_msg("Unexpected Message in get_next_command: ~p.\n",Msg)})
    end.

stop_servers_and_recover_backups(CmdServerPid,NameTrackerPid) ->
    stop_cmd_server(CmdServerPid),
    stop_name_tracker_server(NameTrackerPid),
    {ok, PreviewPairs}=wrangler_backup_server:recover_backups(),
    PreviewPairs.

cmd_server_loop(Parent, State=#state{cmds=Cmds0, changed_files=Changes}) ->
    Cmds = generate_cmds(Cmds0, State#state.name_tracker_pid),
    refac_io:format("Cmdserver: changed files:\n~p\n", [sets:to_list(Changes)]),
    refac_io:format("Cmds in cmd_server_loop:\n~p\n",[Cmds]),
    receive 
        {From, get_next_command, PrevResult} ->
            CurrentPid = self(),
            case Cmds of
                [] ->
                    case PrevResult of 
                        [error, Reason] ->
                            From ! {self(),{ok, none, sets:to_list(Changes), [error, Reason]}, self()};
                        [ok, _Modified] ->
                            ModifiedSoFar =update_modified_files(PrevResult,Changes),
                            From ! {self(), {ok, none, sets:to_list(ModifiedSoFar), "Refactoring finished."}, self()}
                    end;
                [C|Cs] ->
                    refac_io:format("C in cmd server:\n~p\n",[C]),
                    refac_io:format("PrevResult:\n~p\n", [PrevResult]), 
                    case PrevResult of 
                        [error, Reason] when State#state.atomic->
                            From !{self(),{ok, none, sets:to_list(Changes), [error, Reason]}, self()};
                        _ ->
                            ModifiedSoFar = update_modified_files(PrevResult,Changes),
                            CurrentPid = self(),
                            LoopName = make_loop_name(C),
                            refac_io:format("LoopName:\n~p\n", [LoopName]),
                            process_flag(trap_exit, true),
                            Pid = spawn_link(fun()->
                                                     NewState=case LoopName of 
                                                                  non_atomic_refac_loop ->
                                                                      State#state{cmds=C, atomic=false,
                                                                                  changed_files=ModifiedSoFar};
                                                                  _ ->
                                                                      State#state{cmds=C, changed_files=ModifiedSoFar}
                                                              end,
                                                     ?MODULE:LoopName(CurrentPid, NewState)
                                             end),
                            From ! {get_next_command, Pid, [ok, []]},
                            refac_io:format("Pid:\n~p\n", [Pid]),
                            cmd_server_loop(Parent, State#state{cmds=Cs, changed_files=ModifiedSoFar})
                    end
            end;
        {'EXIT', ChildrenPid, Reason} ->
            refac_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
            cmd_server_loop(Parent, State);
        Msg ->
            refac_io:format("Unexpected message in cmd_server_loop:~p\n", [Msg]),                
            error(format_msg("Unexpected message in cmd_server_loop:~p\n", Msg))  
    end.


atomic_refac_loop(Parent, State=#state{cmds={atomic, CRs},changed_files=Changes}) ->
    refac_io:format("atomic loop:\n~p\n", [sets:to_list(Changes)]),
    receive 
        {From, get_next_command, PrevResult} ->
            case PrevResult of 
                [error, Reason] -> 
                    refac_io:format("Changes:\n~p\n", [Changes]),
                    From ! {get_next_command, Parent, [error, Reason]};
                [ok, _Modified] ->
                    ModifiedSoFar =update_modified_files(PrevResult, Changes),
                    Cmds = generate_cmds(CRs, State#state.name_tracker_pid),
                    refac_io:format("Cmds in atomic_refac_loop:\n~p\n", [Cmds]),
                    case Cmds of
                        []->
                            From ! {get_next_command, Parent, [ok, sets:to_list(ModifiedSoFar)]};
                        [C|Cs] ->
                            refac_io:format("C:\n~p\n",[C]),
                            CurrentPid = self(),
                            LoopName = make_loop_name(C),
                            process_flag(trap_exit, true),
                            refac_io:format("LoopName:\n~p\n", [LoopName]),
                            Pid = spawn_link(fun()->
                                                     ?MODULE:LoopName(CurrentPid, State#state{
                                                                                    cmds=C,
                                                                                    changed_files=ModifiedSoFar })
                                             end),
                            From!{get_next_command, Pid, [ok, []]},
                            atomic_refac_loop(Parent, State#state{cmds={atomic, Cs}, changed_files=ModifiedSoFar})
                    end
            end;
        {'EXIT', ChildrenPid, Reason} ->
            refac_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
            cmd_server_loop(Parent, State);
        Msg ->
            refac_io:format("Unexpected message in function atomic_refac_loop:~p\n", [Msg]),
            error(format_msg("Unexpected message in function atomic_refac_loop:~p\n", Msg)) 
    end.

non_atomic_refac_loop(Parent, State=#state{cmds={non_atomic, CRs},
                                           changed_files=Changes, 
                                           success_times=Count}) ->
    receive 
        {From, get_next_command, PrevResult} ->
            case PrevResult of 
                [error, Reason] ->   
                    case CRs of
                        [] when Count>1 ->
                            From ! {get_next_command, Parent, [ok, Changes]};
                        [] ->
                            From ! {get_next_command, Parent, [error, Reason]};
                        [C|Cs] ->
                            CurrentPid = self(),
                            LoopName = make_loop_name(C),
                            Pid = spawn_link(fun()->
                                                     LoopName(CurrentPid, State#state{cmds=C})
                                             end),
                            Pid ! {get_next_command, Pid, [ok, []]},
                            non_atomic_refac_loop(Parent, State#state{cmds={non_atomic, Cs}})
                    end;
                [ok, _Modified] ->
                    ModifiedSoFar =update_modified_files(PrevResult, Changes),
                    case CRs of
                        []->
                            From ! {get_next_command, Parent, [ok, ModifiedSoFar]};
                        [C|Cs] ->
                            CurrentPid = self(),
                            LoopName = make_loop_name(C),
                            Pid = spawn_link(fun()->
                                                     ?MODULE:LoopName(CurrentPid, State#state{cmds=C})
                                             end),
                            Pid ! {get_next_command, Pid, [ok, []]},
                            non_atomic_refac_loop(Parent, State#state{cmds={non_atomic, Cs}, 
                                                                      changed_files=ModifiedSoFar})
                    end
            end;
        {'EXIT', ChildrenPid, Reason} ->
            refac_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
            non_atomic_refac_loop(Parent, State);
        Msg ->
            ?debug("Unexpected message in function non_atomic_refac_loop:~p\n", [Msg]),
            error(format_msg("Unexpected message in function non_atomic_refac_loop", Msg))
    end.
                
elementary_refac_loop(Parent, State=#state{cmds=Cmds}) ->
    receive 
        {From, get_next_command, PrevResult} ->
            case Cmds of 
                {refactoring, Refac, _Question}->
                    NewState=update_state(State, PrevResult),
                    From ! {self(), {ok, {refactoring, Refac}}, self()},
                    elementary_refac_loop(Parent, NewState#state{cmds=[]});
                [] ->
                    From ! {get_next_command, Parent, PrevResult}
            end;
        Msg ->
            error(format_msg("Unexpected message in repeat_question_refac_loop: ~p.\n", Msg))
    end.
question_refac_loop(Parent, State=#state{cmds={question, ER0s},changed_files=Changes}) ->
    receive 
        {From, get_next_command, PrevResult} ->
            ERs = generate_cmds(ER0s,State#state.name_tracker_pid),
            refac_io:format("ERs:\n~p\n", [ERs]),
            refac_io:format("PrevResult:\n~p\n", [PrevResult]),
            case PrevResult of
                [error, _Reason] ->
                    case State#state.atomic of 
                        true ->
                            From ! {get_next_command, Parent, PrevResult};
                        false ->
                            case ERs of
                                [{refactoring, Refac, Question}|Cs] ->
                                    NewState=update_state(State, PrevResult),
                                    From ! {self(), {ok, {question, Question, Refac}}, self()},
                                    question_refac_loop(Parent, NewState#state{cmds={question, Cs}});
                                [] ->
                                    From ! {get_next_command, Parent, [ok,Changes]}
                            end
                    end;
                [ok, _Modified] ->
                    case ERs of
                        [{refactoring, Refac, Question}|Cs] ->
                            NewState=update_state(State, PrevResult),
                            From ! {self(), {ok, {question, Question, Refac}}, self()},
                            question_refac_loop(Parent, NewState#state{cmds={question, Cs}});
                        [] ->
                            ModifiedSoFar =update_modified_files(PrevResult,Changes),
                            From ! {get_next_command, Parent, [ok, sets:to_list(ModifiedSoFar)]}
                    end
            end;
        {'EXIT', ChildrenPid, Reason} ->
            refac_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
            cmd_server_loop(Parent, State);
        Msg ->
            refac_io:format("Unexpected message in question_refac_loop: ~p.\n", [Msg]),
            error(format_msg("Unexpected message in question_refac_loop: ~p.\n", Msg))
    end.

repeat_question_refac_loop(Parent, State=#state{cmds={question, ER0s},changed_files=Changes}) ->
    receive 
        {From, get_next_command, PrevResult} ->
            ERs = generate_cmds(ER0s,State#state.name_tracker_pid),
            case ERs of
                [{refactoring, Refac, Question}|_Cs] ->
                    case PrevResult of
                        none ->
                            From ! {get_next_command, Parent, [ok, Changes]};
                        [ok, _Modified] ->
                            NewState=update_state(State, PrevResult),
                            From ! {self(), {ok, {repeat_question, Question, Refac}}, self()},
                            repeat_question_refac_loop(Parent, NewState#state{cmds=ERs});
                        [error, _Reason] ->
                            Parent! {get_next_command, Parent, PrevResult}
                    end;
                [] ->
                    From ! {get_next_command, Parent, [ok, Changes]}
            end;
        {'EXIT', ChildrenPid, Reason} ->
            refac_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
            cmd_server_loop(Parent, State);
        Msg ->
            refac_io:format("Unexpected message in repeat_question_refac_loop: ~p.\n", [Msg]),
            error(format_msg("Unexpected message in repeat_question_refac_loop: ~p.\n", Msg))
    end.

while_refac_loop(Parent, State=#state{cmds={while, Cond, CRs}, 
                                      changed_files=Changes}) ->
    receive
    {From, get_next_command, _PrevResult} ->
        case Cond() of
            true ->
                CurrentPid = self(),
                CR1s = make_list(CRs),
                Pid = spawn_link(fun()->
                                         cmd_server_loop(CurrentPid, State#state{cmds=CR1s})
                                 end),
                process_flag(trap_exit, true),
                From ! {get_next_command, Pid, [ok, []]},
                while_refac_loop(Parent, State);
            false ->
                From !{get_next_command, Parent, [ok, Changes]}
        end;
        {'EXIT', ChildrenPid, Reason} ->
            refac_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
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
            refac_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
            if_then_else_refac_loop(Parent, State);
        Msg ->
            ?debug("Unexpected message in function if_then_else_refac_loop:~p\n", [Msg]),
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
            refac_io:format("~p died with:~p~n", [ChildrenPid, Reason]),
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
    refac_io:format("Cmd:\n~p\n",[Cmd]), 
    Cmd1 = generate_a_cmd(Cmd, NameTrackerPid),
    refac_io:format("Cmd1:\n~p\n", [Cmd1]),
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
    refac_io:format("DDDD\n"),
    refac_io:format("NameTrackerPid:\n~p\n", [NameTrackerPid]),
    case lists:member(RefacName, elementary_refacs()) andalso is_list(Args) of
        true ->
            NameTrackerPid ! {self(), update_args, Args},
            receive
                {NameTrackerPid, NewArgs} ->
                    try 
                        refac_io:format("ddddd\n"),
                        Cmds=apply(wrangler_extended, RefacName, NewArgs),
                        refac_io:format("Cmdsdd:\n~p\n", [Cmds]),
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
        {atomic, _} ->
            atomic_refac_loop;
        {non_atomic, _} ->
            non_atomic_refac_loop;
        {refactoring, _, _} ->
            elementary_refac_loop;
        {{refactorng, _, _}, _Gen} ->
            elementary_refac_loop;
        {question, _} ->
            question_refac_loop;
        {repeat_question, _} ->
            repeat_question_refac_loop;
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
            ?debug("Args1:\n~p\n", [Args]),
            NewArgs = rename_in_args(Args, State),
            ?debug("NewArgs:\n~p\n", [NewArgs]),
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
    ?debug("State:\n~p\n", [State]),
    [case Arg of
         {old, A} ->
             Arg1=normalise(A),
             ?debug("Arg1:\n~p\n", [Arg1]),
             case lists:keyfind(Arg1, 1,State) of
                 {Arg1, NewNames} ->
                     ?debug("NewNames:\n~p\n", [NewNames]),
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
                     true ->
                         list_to_atom(M);
                     _ ->
                         M
                 end,
            F1 = case is_list(F) of 
                     true ->
                         list_to_atom(F);
                     _ -> 
                         F
                 end,
            A1 = case is_list(A) of 
                     true ->
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
    [rename_var, rename_fun, swap_args, fold_expr, gen_fun].
                
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
  
