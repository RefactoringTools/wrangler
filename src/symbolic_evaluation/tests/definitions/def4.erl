%% @private
-module(def4).
-export([is_even/1]).

is_even(N) when is_number(N) ->
    N rem 2 == 0.
