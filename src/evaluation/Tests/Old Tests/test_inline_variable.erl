-module(test_inline_variable).

-export([test/0]).
test() ->
    A = 1,
    B = 2,
    C,
    C = 3,
    D,
    f(A),
    C = 1 + B + C.


