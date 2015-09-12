-module(full_fsm1).

-behaviour(full_fsm).

-export([callback_1/2]).

callback_1(_Alloc, _Freq) -> false.
