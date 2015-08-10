-module(test_inline_variable).
-export([test/0]).

f([]) -> 0;
f( [X | Xs] ) -> X + f(Xs).

test() ->
    1 + 2 + 3 + f(1).
