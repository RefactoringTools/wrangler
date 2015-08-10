%% @private
-module(test_arithmetics).
-compile([export_all]).

test1() -> 1 + 1.

test2(X) -> ((2 * X + X) + 0) * 1.

test3(X) -> (1 * X + X) + (2 + 3).

testVariablesPlusAndMinus(X,Y) -> (2 * X - X) + (Y - Y).

test4(X, Y) ->
   ((X + Y) + (X + Y)) div 1.

test5(X) ->
    Z = X + 0,
    H = 2 * Z + Z,
    ((1 * H) + (2 * H)) div (8 - 7).


