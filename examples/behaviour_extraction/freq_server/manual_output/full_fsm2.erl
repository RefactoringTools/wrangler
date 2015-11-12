-module(full_fsm2).

-behaviour(full_fsm).

-export([callback_1/2]).

callback_1(Alloc, Freq) -> not lists:member(Freq,Alloc).