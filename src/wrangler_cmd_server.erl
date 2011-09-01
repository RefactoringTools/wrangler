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
%%@hidden
%%@private
-module(wrangler_cmd_server).

-behaviour(gen_server).

-export([start_link/1, stop/0]).

-export([init/1,handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([elementary_refac_loop/2, interactive_refac_loop/2, 
         repeat_interactive_refac_loop/2, while_refac_loop/2,
         if_then_refac_loop/2]).

-export([get_next_command/1,update_entity/1]).

-record(state, {cmds=[], 
                changed_files=sets:new(), 
                atomic=true      ::boolean(),
                name_tracker_pid ::pid()
               }).

-include("../include/wrangler_internal.hrl").

-define(ENABLE_DEBUG, true).
 
-ifdef(ENABLE_DEBUG).
-define(wrangler_debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.
-define(wrangler_debug(__String, __Args), ok).
-endif.

start_link(Cmds)->
    gen_server:start({local, ?MODULE}, ?MODULE,[Cmds], []).

init([Cmds]) ->
    ?wrangler_debug("Cmds:\n~p\n", [Cmds]),
    process_flag(trap_exit, true),
    NameTrackerPid = spawn_link(fun()->name_tracker_loop([]) end),
    register(wrangler_name_tracker, NameTrackerPid),
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
    ?wrangler_debug("PrevResult:\n~p\n", [PrevResult]),
    PrevResult1=case PrevResult of 
                    [Result, {name_change, BeforeAfter}] ->
                        NameTrackerPid! {name_change, BeforeAfter},
                        Result;
                    _  ->
                        PrevResult
                end,
    ?wrangler_debug("handlecall: get_next_command:~p\n",[PrevResult1]),
    Cmd = get_next_command(PrevResult1, {CmdServerPid, NameTrackerPid}),
    case Cmd of 
        {ok, none, _ChangedFiles, _Msg} ->
            {reply, Cmd, State};
        {ok, RefacCmd, {NextPid, NameTrackerPid}} when is_pid(NextPid) ->
            ?wrangler_debug("RefacCmd:\n~p\n", [RefacCmd]),
            {reply, {ok, RefacCmd}, {NextPid, NameTrackerPid}}
    end.

get_next_command(PrevResult, {CmdServerPid, NameTrackerPid}) ->
    ?wrangler_debug("CmdServerPid:\n~p\n", [CmdServerPid]),
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
                    Msg =format_msg("Unexpected refactoring command: ~p.\n", 
                                    NextCmd),
                    {ok, none, [], {error, Msg}}
            end;
        Others ->
            Msg=format_msg("Unexpected Message in get_next_command: ~p.\n",
                           Others),
            {ok, none, [], {error, Msg}}            
    end.

handle_cast(stop, State={CmdServerPid, NameTrackerPid}) ->
    CmdServerPid ! stop,
    NameTrackerPid ! stop,
    {noreply, normal, State}.

handle_info(Info,State=State={CmdServerPid, NameTrackerPid}) ->
    CmdServerPid ! stop,
    NameTrackerPid ! stop,
    {stop, Info, State}.

update_entity(Entity) ->
    wrangler_name_tracker ! {self(), update_entity, Entity},
    receive 
        {wrangler_name_tracker, NewEntity}->
            NewEntity
    end.
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
            ?wrangler_debug("Cmds0:\n~p\n",[Cmds0]),
            Cmds = generate_cmds(Cmds0),
            ?wrangler_debug("Cmds in cmd_server_loop:\n~p\n",[Cmds]),
            ?wrangler_debug("Cmdserver: changed files:\n~p\n", [sets:to_list(Changes)]),
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
                    ?wrangler_debug("C in cmd server:\n~p\n",[C]),
                    ?wrangler_debug("PrevResult:\n~p\n", [PrevResult]), 
                    case PrevResult of 
                        [error, Reason] when State#state.atomic->
                            From ! {get_next_command, Parent, [error, Reason]};
                        _ ->
                            ModifiedSoFar = update_modified_files(PrevResult,Changes),
                            ?wrangler_debug("ModifiedSoFar:\n~p\n", [ModifiedSoFar]),
                            process_flag(trap_exit, true),
                            case C of  
                                {try_refac, CRs} ->
                                    NewState=State#state{cmds=CRs, atomic=false,
                                                         changed_files=sets:new()},
                                    Pid = spawn_link(fun()->
                                                             cmd_server_loop(CurrentPid, NewState)
                                                     end),
                                    From ! {get_next_command, Pid, [ok, []]};
                                _ ->
                                    LoopName = make_loop_name(C),
                                    ?wrangler_debug("LoopName:\n~p\n", [LoopName]),
                                    ?wrangler_debug("C:\~p\n", [C]),
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
                {refactoring, Refac, Args}->
                    NewState=update_state(State, PrevResult),
                    From ! {self(), {ok, {refactoring, Refac, Args}}, self()},
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
            ?wrangler_debug("ER0s:\n~p\n", [ER0s]),
            ERs = generate_cmds(ER0s),
            ?wrangler_debug("ERs:\n~p\n", [ERs]),
            ?wrangler_debug("PrevResult:\n~p\n", [PrevResult]),
            case PrevResult of
                [error, _Reason] ->
                    case State#state.atomic of 
                        true ->
                            From ! {get_next_command, Parent, PrevResult};
                        false ->
                            case ERs of
                                [{refactoring, Refac={_, RefacName}, Args}|Cs] ->
                                    Question =apply(wrangler_extended, gen_question, [RefacName, Args]),
                                    From ! {self(), {ok, {interactive, Question, Refac, Args}}, self()},
                                    interactive_refac_loop(Parent, State#state{cmds={interactive, Cs}});
                                [] ->
                                    From ! {get_next_command, Parent, [ok,Changes]}
                            end
                    end;
                [ok, _Modified] ->
                    case ERs of
                        [{refactoring, Refac={_, RefacName}, Args}|Cs] ->
                            Question=apply(wrangler_extended, gen_question, [RefacName,Args]),
                            NewState=update_state(State, PrevResult),
                            From ! {self(), {ok, {interactive, Question, Refac, Args}}, self()},
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
            ERs = generate_cmds(ER0s),
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
                                    From ! {get_next_command, Parent, [ok, sets:to_list(Changes)]}; %%TODO:test here again!
                                false->
                                    [{refactoring, Refac={_, RefacName}, Args}|_Cs]=ERs,
                                    Question=apply(wrangler_extended, gen_question, [RefacName, Args]),
                                    From ! {self(), {ok, {repeat_interactive, Question, Refac, Args}}, self()},
                                    repeat_interactive_refac_loop(Parent,State)
                            end;
                        [ok, _Modified] ->
                            NewState=update_state(State, PrevResult),
                            [{refactoring, Refac={_, RefacName}, Args}|_Cs]=ERs,
                            Question=apply(wrangler_extended, gen_question, [RefacName, Args]),
                            From ! {self(), {ok, {repeat_interactive, Question, Refac, Args}}, self()},
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
                        false ->
                            From !{get_next_command, Parent, [ok, sets:to_list(ModifiedSoFar)]};
                        {false, _} ->
                            From !{get_next_command, Parent, [ok, sets:to_list(ModifiedSoFar)]};
                        Others ->
                            CmdGen1=case Others of 
                                        true ->
                                            CmdGen;
                                        {true, Data} when is_function(CmdGen, 1) ->
                                            CmdGen(Data);
                                        {true, _} ->
                                            CmdGen
                                    end,
                            Res = generate_cmds(CmdGen1),
                            case Res of 
                                [] ->
                                    From !{get_next_command, Parent, [ok, sets:to_list(ModifiedSoFar)]};
                                _ ->
                                    process_flag(trap_exit, true),
                                    CurrentPid = self(),
                                    Pid = spawn_link(fun() ->
                                                             cmd_server_loop(CurrentPid, State#state{cmds=Res})
                                                     end),
                                    From ! {get_next_command, Pid, [ok, []]},
                                    while_refac_loop(Parent, State#state{cmds={while, Cond, CmdGen},
                                                                         changed_files=ModifiedSoFar})
                            end
                    end
            end;
        {'EXIT', ChildrenPid, Reason} ->
            case Reason of 
                normal ->
                    while_refac_loop(Parent, State);
                _ ->
                    error(format_msg("~p died with:~p~n", [ChildrenPid, Reason]))
            end;
        Msg ->
            error(format_msg("Unexpected message in while_refac_loop:~p\n", Msg))    
    end.

%% not really a loop.
if_then_refac_loop(Parent, State=#state{cmds={if_then, Cond, CRs}}) ->
    receive
        {From, get_next_command, _PrevResult} ->
            R = Cond(),
            ?wrangler_debug("R:\n~p\n", [R]),
            case R of 
                false ->
                    From !{get_next_command, Parent, [ok, []]};
                {false, _} ->
                    From !{get_next_command, Parent, [ok, []]};
                Others ->
                    CR1s =case Others of 
                              true ->
                                  CRs;
                              {true, Data} when is_function(CRs,1)->
                                  CRs(Data);
                              {true, _} ->
                                  CRs
                          end,
                    Pid = spawn_link(fun()->
                                             cmd_server_loop(
                                               Parent, 
                                               State#state{cmds=make_list(CR1s)})
                                     end),
                    process_flag(trap_exit, true),
                    From ! {get_next_command, Pid, [ok,[]]}
            end;
        {'EXIT', ChildrenPid, Reason} ->
            case Reason of 
                normal ->
                    if_then_refac_loop(Parent, State);
                _ ->
                    error(format_msg("~p died with:~p~n", [ChildrenPid, Reason]))
            end;
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

generate_cmds(Cmd) when not is_list(Cmd) ->
    generate_cmds([Cmd]); 
generate_cmds([]) -> [];
generate_cmds([Cmd|Cmds]) when is_function(Cmd) ->
    generate_cmds(make_list(Cmd())++Cmds);
generate_cmds([Cmd|Cmds]) ->
    ?wrangler_debug("Generate_cmds:Cmd:\n~p\n",[Cmd]), 
    Cmd1 = generate_a_cmd(Cmd), 
    ?wrangler_debug("Cmd1:\n~p\n", [Cmd1]),
    case Cmd1 of 
        [] ->
            generate_cmds(Cmds);
        {C, {generator, Gen}} ->
            [C, {generator, Gen}|Cmds];
        C when is_list(C) ->
            C++Cmds;
        C ->
            [C|Cmds]
    end. 

generate_a_cmd(Cmd={refactoring, _, _})->
    Cmd;
generate_a_cmd(Cmd={{refactoring,_, _}, {generator, _}}) ->
    Cmd;
generate_a_cmd({generator, Gen}) ->
    case Gen() of 
        [] ->
            [];
        {Cmd, {generator, Gen}} ->
            {Cmd, {generator, Gen}};
        Cmd ->
            Cmd
    end;
generate_a_cmd(Cmd={refac_,RefacName, Args0}) ->
    ?wrangler_debug("Cmd_in_generate_a_cmd:\n~p\n", [Cmd]),
    ?wrangler_debug("RefacName:\n~p\n", [RefacName]),
    Args = Args0(),
    case lists:member(RefacName, elementary_refacs()) andalso is_list(Args) of
        true ->
            ?wrangler_debug("Args:\n~p\n", [Args]),
            Cmds=apply(wrangler_extended, RefacName,Args),
            ?wrangler_debug("Cmds:\n~p\n", [Cmds]),
            generate_cmds(Cmds);
        false ->
            throw({error, format_msg("Illegal command generator:\n~p\n", Cmd)})   
    end; 
generate_a_cmd(Cmd) ->
    throw({error, format_msg("Illegal command generator:\n~p\n", Cmd)}).


update_state(State=#state{changed_files=Changes}, PrevResult) ->
    State#state{changed_files= update_modified_files(PrevResult,Changes)}.

make_loop_name(CR) ->
    case CR of 
        {refactoring, {_Mod, _Name},_Args} ->
            elementary_refac_loop;
        {{refactoring, {_Mod, _Name} ,_Args}, _Gen} ->
            elementary_refac_loop;
        {interactive, _ERs} ->
            interactive_refac_loop;
        {repeat_interactive, _ERs} ->
            repeat_interactive_refac_loop;
        {while, _Cond, _CR} ->
            while_refac_loop;
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
    ?wrangler_debug("State:\n~p\n", [State]),
    receive
        {name_change, BeforeAfter} ->
            NewState=update_name_history(State, BeforeAfter),
            name_tracker_loop(NewState);
        {From, update_entity, Entity} ->
            NormalisedEntity=normalise(Entity),
            case lists:keyfind(NormalisedEntity, 1,State) of
                {NormalisedEntity, NewNames} ->
                    From ! {wrangler_name_tracker, hd(NewNames)},
                    name_tracker_loop(State);
                false -> 
                    From ! {wrangler_name_tracker, Entity}, 
                    name_tracker_loop(State)
            end;
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
    [rename_var, rename_fun, swap_args, fold_expr, gen_fun, move_fun, unfold_fun_app, tuple_args].
                
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
  
