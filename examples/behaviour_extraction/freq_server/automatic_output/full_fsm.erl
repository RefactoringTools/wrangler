

-module(full_fsm).

-export([behaviour_info/1, bugs/1, bugs/2, command/2, freq/0,
         get_frequencies/0, initial_state/0, initial_state_data/1,
         next_state_data/6, normalRes/1, off/1, postcondition/6, precondition/3,
         precondition/5, prop_Frequencies/1, prop_Frequency/2, state/3]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-define(NUM_FREQ,3).

behaviour_info(callbacks) ->
    [{bugs, 1}, {callback_1, 2}, {freq, 0}, {get_frequencies, 0},
     {normalRes, 1}, {precondition, 4}, {prop_Frequency, 1}];
behaviour_info(_Other) -> undefined.

freq() ->
    ?SUCHTHAT(X,int(),((X > 9) and (X < 15))).

state(Module,N,_Data) ->
    [ {{state,N+1},{call,frequency,allocate,[]}} || N < ?NUM_FREQ] ++
    [ {{state,N},{call,frequency,allocate,[]}} || N =< ?NUM_FREQ] ++
    [{{state,N - 1},{call,frequency,deallocate,[Module:freq()]}} ||  N > 0] ++
    [{{state,N},{call,frequency,deallocate,[Module:freq()]}} || N >= 0] ++
    [ {off,{call,frequency,stop,[]}} ].

off(_) ->
    [ {{state,0},{call,frequency,start,[]}} ].

%% Identify the initial state
initial_state() ->
    off.

get_frequencies() ->
     lists:seq(10, 9+?NUM_FREQ).

%% Initialize the state data
initial_state_data(Module) ->
    Module:get_frequencies().

next_state_data(Module,off,_,_,_,{call,frequency,start,[]}) ->
    {Module:get_frequencies(),[]};
next_state_data(Module,{state,_N},off,_,_,{call,frequency,stop,[]}) ->
    {Module:get_frequencies(),[]};
next_state_data(_Module,{state,_N},{state,_M},{[],A},_,{call,frequency,allocate,[]}) ->
    {[],A};
next_state_data(_Module,{state,_N},{state,_M},{Free,Alloc},_,{call,frequency,allocate,[]}) ->
    {tl(Free),[hd(Free)|Alloc]};
next_state_data(_Module,{state,N},{state,M},{Free,Alloc},_,{call,frequency,deallocate,[Freq]})
  when M < N ->
    case lists:member(Freq,Alloc) of
	true ->
	    {[Freq|Free],lists:filter(fun(A) -> A /= Freq end,Alloc)};
	_ -> 
	    {Free,Alloc}
    end;

next_state_data(_Module,_From,_To,S,_V,{call,_,_,_}=_Call) ->
    %%io:format("_From,_To,S,_V  Call = ~w ~w ~w ~w   ~w~n",[_From,_To,S,_V,_Call]),
    S.

precondition(_Module,{state,_N},_,_,{call,frequency,start,_}) ->
    false;
precondition(_Module,{state,N},{state,M},_S,{call,frequency,deallocate,_Freq}) when N == 0 ->
    M == N;
precondition(_Module,{state,N},{state,M},{_Free,Alloc},{call,frequency,deallocate,[Freq]}) when M < N ->
    lists:member(Freq,Alloc);
precondition(Module,{state,_N},{state,_M},{_Free,Alloc},{call,frequency,deallocate,[Freq]}) ->
    Module:callback_1(Alloc, Freq);
precondition(_Module,{state,N},{state,M},{[],_Alloc},{call,frequency,allocate,[]}) when M > N ->
    false;
precondition(_Module,{state,N},{state,M},{Free,_Alloc},{call,frequency,allocate,[]}) when M == N ->
    Free == [];
precondition(_Module,{state,_N},_,_,_) ->
    true;

precondition(_Module,off,_To,_S,{call,frequency,start,_}) ->
    true;
precondition(_Module,off,_To,_S,_) ->
    false;
precondition(_Module,_From,_To,_S,_Op) ->
    io:format("Er, whut? ~p ~n",[{_From,_To,_S,_Op}]),
    false.

precondition(Module,From,Op) ->
    Module:precondition(From,From,[],Op).

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
postcondition(_Module,{state,N},_To,_S,{call,_,allocate,_},Res) when N >= ?NUM_FREQ ->
    Res == {error,no_frequencies};
postcondition(Module,_From,_To,_S,{call,_,_,_}=_Call,Res) ->
    Module:normalRes(Res) == ok.

prop_Frequency(Module,Cmds) ->
    begin
	{H,S,Res} = run_commands(Module,Cmds),
	case S of                                 % tidy up after running commands
	    {{state,_},_} -> catch(frequency:stop());
	    {off,_}       -> ok
	end,
	Result = Module:normalRes(Res),
	?WHENFAIL(
	   (io:format("History: ~p\nState: ~p\nResult: ~p <~p>\n",[H,S,Result,Res])),
	   (Result == ok))
    end.

prop_Frequencies(Module) ->
    ?FORALL(Cmds, (commands(Module)),
	    (Module:prop_Frequency(Cmds))).

command(_Module,off) ->
    {call,frequency,start,[]};
command(Module,_S) ->
    oneof([{call,frequency,stop,[]},
	  {call,frequency,allocate,[]},
	  {call,frequency,deallocate,[Module:freq()]}]).

bugs(Module) -> Module:bugs(100).
bugs(Module,N) ->
    Spec = eqc_fsm_callbacks:new(Module),
    Bugs = bugs:bugs_cmds(Spec, 20, N, fun Module:prop_Frequency/1),
    bugs:print_bugs(Bugs).
