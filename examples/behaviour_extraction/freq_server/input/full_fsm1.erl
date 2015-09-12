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

%% Definition of the states. Each state is represented by a function, 
%% listing the transitions from that state, together with generators 
%% for the calls to make each transition.

freq() ->
    ?SUCHTHAT(X,int(),((X > 9) and (X < 15))).

state(N,_Data) ->
    [ {{state,N+1},{call,frequency,allocate,[]}} || N < ?NUM_FREQ] ++
    [ {{state,N},{call,frequency,allocate,[]}} || N =< ?NUM_FREQ] ++
    [ {{state,N-1},{call,frequency,deallocate,[freq()]}} ||  N > 0] ++
    [ {{state,N},{call,frequency,deallocate,[freq()]}} || N >= 0] ++
    [ {off,{call,frequency,stop,[]}} ].

off(_) ->
    [ {{state,0},{call,frequency,start,[]}} ].

%% Identify the initial state
initial_state() ->
    off.

get_frequencies() ->
     lists:seq(10, 9+?NUM_FREQ).

%% Initialize the state data
initial_state_data() ->
    get_frequencies().

%% Next state transformation for state data.
%% S is the current state, From and To are state names

next_state_data(off,_,_,_,{call,frequency,start,[]}) ->
    {get_frequencies(),[]};
next_state_data({state,_N},off,_,_,{call,frequency,stop,[]}) ->
    {get_frequencies(),[]};
next_state_data({state,_N},{state,_M},{[],A},_,{call,frequency,allocate,[]}) ->
    {[],A};
next_state_data({state,_N},{state,_M},{Free,Alloc},_,{call,frequency,allocate,[]}) ->
    {tl(Free),[hd(Free)|Alloc]};
next_state_data({state,N},{state,M},{Free,Alloc},_,{call,frequency,deallocate,[Freq]}) 
  when M < N ->
    case lists:member(Freq,Alloc) of
	true ->
	    {[Freq|Free],lists:filter(fun(A) -> A /= Freq end,Alloc)};
	_ -> 
	    {Free,Alloc}
    end;

next_state_data(_From,_To,S,_V,{call,_,_,_}=_Call) ->
    %%io:format("_From,_To,S,_V  Call = ~w ~w ~w ~w   ~w~n",[_From,_To,S,_V,_Call]),
    S.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence

precondition({state,_N},_,_,{call,frequency,start,_}) ->
    false;
precondition({state,N},{state,M},_S,{call,frequency,deallocate,_Freq}) when N == 0 ->
    M == N;
precondition({state,N},{state,M},{_Free,Alloc},{call,frequency,deallocate,[Freq]}) when M < N ->
    lists:member(Freq,Alloc);
precondition({state,_N},{state,_M},{_Free,Alloc},{call,frequency,deallocate,[Freq]}) ->
%SJT: changed the precondition here
%   not lists:member(Freq,Alloc);
    false;
precondition({state,N},{state,M},{[],_Alloc},{call,frequency,allocate,[]}) when M > N ->
    false;
precondition({state,N},{state,M},{Free,_Alloc},{call,frequency,allocate,[]}) when M == N ->
    Free == [];
precondition({state,_N},_,_,_) ->
    true;

precondition(off,_To,_S,{call,frequency,start,_}) ->
    true;
precondition(off,_To,_S,_) ->
    false;
precondition(_From,_To,_S,_Op) ->
    io:format("Er, whut? ~p ~n",[{_From,_To,_S,_Op}]),
    false.

precondition(From,Op) ->
    precondition(From,From,[],Op).

normalRes(Res) ->
    case Res of                      % normalise +ve results to 'ok'
	true   -> ok;
	ok     -> ok;
	{ok,_} -> ok;
	precondition_failed -> ok;
	X      -> X
    end.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>) 
postcondition({state,N},_To,_S,{call,_,allocate,_},Res) when N >= ?NUM_FREQ ->
    Res == {error,no_frequencies};
postcondition(_From,_To,_S,{call,_,_,_}=_Call,Res) ->
    normalRes(Res) == ok.

prop_Frequency(Cmds) ->
    begin
	{H,S,Res} = run_commands(?MODULE,Cmds),
	case S of                                 % tidy up after running commands
	    {{state,_},_} -> catch(frequency:stop());
	    {off,_}       -> ok
	end,
	Result = normalRes(Res),
	?WHENFAIL(
	   io:format("History: ~p\nState: ~p\nResult: ~p <~p>\n",[H,S,Result,Res]),
	   Result == ok)
    end.

prop_Frequencies() ->
    ?FORALL(Cmds,commands(?MODULE),
	    prop_Frequency(Cmds)).

command(off) ->
    {call,frequency,start,[]};
command(_S) ->
    oneof([{call,frequency,stop,[]},
	  {call,frequency,allocate,[]},
	  {call,frequency,deallocate,[freq()]}]).

bugs() -> bugs(100).
bugs(N) ->
    Spec = eqc_fsm_callbacks:new(?MODULE),
    Bugs = bugs:bugs_cmds(Spec, 20, N, fun prop_Frequency/1),
    bugs:print_bugs(Bugs).
