-module(test_inline_variable).
-export([test/0]).

f([]) -> 0;
f( [X | Xs] ) -> X + f(Xs).

test() ->
    B = 2,
    C = 3,
    D = f(1),
    1 + B + C + D.



