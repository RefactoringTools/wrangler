-module(test_arithmetics).
-compile([export_all]).

test1() -> 2.

test2(X) -> 3 * X.

test3(X) -> (2 * X) + 5.

testVariablesPlusAndMinus(X,Y) -> X.

test4(X, Y) ->
      2 * (X + Y).

test5(X) ->
     Z = X,
     H = 3 * Z,
     3 * H.


