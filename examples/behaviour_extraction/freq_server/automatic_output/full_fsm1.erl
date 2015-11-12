%%% File    : full_fsm1.erl
%%% Author  : Simon  Thompson <>
%%% Description : 
%%% Created : 22 Dec 2009 by Simon  Thompson <>
%%% Revised : 8 Jan 2010 by Simon  Thompson after comments from Thomas Arts and Huiqing Li

-module(full_fsm1).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).

-define(NUM_FREQ,3).

-behaviour(full_fsm).

%% Definition of the states. Each state is represented by a function, 
%% listing the transitions from that state, together with generators 
%% for the calls to make each transition.

freq() -> full_fsm:freq().

state(N, Data) -> full_fsm:state(?MODULE, N, Data).

off(X1) -> full_fsm:off(X1).

initial_state() -> full_fsm:initial_state().

get_frequencies() -> full_fsm:get_frequencies().

initial_state_data() -> full_fsm:initial_state_data(?MODULE).

%% Next state transformation for state data.
%% S is the current state, From and To are state names

next_state_data(From, To, S, V, Call) ->
    full_fsm:next_state_data(?MODULE, From, To, S, V, Call).

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence

precondition(From, To, S, Op) ->
    full_fsm:precondition(?MODULE, From, To, S, Op).

precondition(From, Op) -> full_fsm:precondition(?MODULE, From, Op).

normalRes(Res) -> full_fsm:normalRes(Res).

postcondition(From, To, S, Call, Res) ->
    full_fsm:postcondition(?MODULE, From, To, S, Call, Res).

prop_Frequency(Cmds) -> full_fsm:prop_Frequency(?MODULE, Cmds).

prop_Frequencies() -> full_fsm:prop_Frequencies(?MODULE).

command(S) -> full_fsm:command(?MODULE, S).

bugs() -> full_fsm:bugs(?MODULE).

bugs(N) -> full_fsm:bugs(?MODULE, N).

callback_1(_Alloc, _Freq) ->
    %SJT: changed the precondition here
    %   not lists:member(Freq,Alloc);
    false.