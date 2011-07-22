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
%%@hidden
%%@private
-module(gen_composite_refac).

-export([init_composite_refac/2,get_next_command/1, input_par_prompts/1]).

-export([behaviour_info/1]).

-include("../include/wrangler.hrl").

-record(state, {cmds=[], changed_files=[]}).

-spec behaviour_info(atom()) ->[{atom(), arity()}].
behaviour_info(callbacks) ->
    [{is_atomic, 0}, {composite_refac,0}, {input_par_prompts, 0}].


input_par_prompts(CallBackMod) ->
    Res =input_pars_1(CallBackMod),
    {ok, Res}.

input_pars_1(CallBackMod) when is_atom(CallBackMod)->
    erlang:apply(CallBackMod, input_par_prompts, []);
input_pars_1(CallBackMod) when is_list(CallBackMod)->
    erlang:apply(list_to_atom(CallBackMod), input_par_prompts, []);
input_pars_1(_) ->
    throw:error(badarg).

init_composite_refac(ModName, Args)->
    ?wrangler_io("\nCMD: ~p:init_composite_refac(~p,~p).\n",
		 [?MODULE, ModName, Args]),
    Module = if is_list(ModName) ->
                     list_to_atom(ModName);
                true ->
                     ModName
             end,
    case apply(Module, composite_refac, []) of
        {error, Reason} ->
            {error, Reason};
        Cmds ->
            start_composite_refac_server(lists:append(Cmds))
    end.

start_composite_refac_server(Cmds)->
    spawn_link(fun()->composite_refac_server_loop(#state{cmds=Cmds}) end).

stop_composite_refac_server(Pid) ->
    Pid!stop.

get_next_command(Pid) ->
    wrangler_io:format("Pid:\n~p\n", [Pid]),
    Pid!{self(), get_next_command},
    receive 
        {Pid, NextCmd} ->
            wrangler_io:format("NextCmd:\n~p\n", [NextCmd]),
            case NextCmd of 
                {ok, none} ->
                    ChangedFiles = get_changed_files(Pid),
                    stop_composite_refac_server(Pid),
                    wrangler_io:format("Res:\n~p\n", [{ok, none, ChangedFiles}]),
                    {ok, none, ChangedFiles};
                {ok, Cmd} ->
                    wrangler_io:format("Res:\n~p\n", [{ok, Cmd}]),
                    {ok, Cmd}
            end
    end.
        
get_changed_files(Pid) ->    
    Pid ! {self(), get_changed_files},
    receive 
        {Pid, ChangedFiles} ->
            ChangedFiles
    end.

composite_refac_server_loop(State=#state{cmds=Cmds, changed_files=Files}) ->
    receive
        {From, get_next_command} ->
            case Cmds of 
                 [] ->
                     From ! {self(), {ok, none}},
                     composite_refac_server_loop(State);
                 [C|Cs] ->
                     %% need to do more work here!!!
                     From ! {self(), {ok, C}},
                    composite_refac_server_loop(State#state{cmds=Cs})
             end;
        {From, get_changed_files} ->
            From ! {self(), lists:reverse(Files)},
            composite_refac_server_loop(State);
        stop ->
            ok;
        _Msg ->
            composite_refac_server_loop(State)
    end.
