-module(test_inline_variable).
-export([test/0]).

f([]) -> 0;
f( [X | Xs] ) -> X + f(Xs).

test() ->
    A = 1,
    B = 2,
    C = 3,
    D = f(1),
    A + B + C + D.




