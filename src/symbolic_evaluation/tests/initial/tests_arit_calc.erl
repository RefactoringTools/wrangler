-module(tests_arit_calc).
-compile([export_all]).

testXPlusX(X) -> X + X.

test1Plus2() -> 1 + 2.

testPlus1To6() -> 1 + 2 + 3 + 4 + 5 + 6.

testPlusAndTimes() -> 1 * 5 + 7 * 3 * 2 + 1.

testPlusTimesAndMinus() -> 1 + 2 * 4 - 5 + 6 * 7.

testMinusItself(Y) ->
    Y - Y.

testMinusItself2(X,Y) -> 
    (X + Y) - (X + Y).

testMinusItself3(X,Y) -> (X + Y + 1) - (X + Y + 1).

testDivBy1() -> 5 div 1.

test2Divs() -> 5 div 1 div (2 - 1).

test2divsBy0() -> (5 div 1) div (1 - 1).

testVarsArit(X) -> X + 2 * X.

testVarsArit2(X) -> 1 * X + 2 * X.

testVarsArit3(X) -> 2 * X + 3 * X.

testPlusAndMinus(X) ->
      1 + (X - X).

testVariablesPlusAndMinus(X,Y) -> (2 * X - X) + (Y - Y).

testArbitrary(Y) ->
   (foo(3) + Y) +  2 * (foo(3) + Y).

testExp(X, Y, Z) ->
    (X + Y + Z) + 2 * (X + Y + Z).

%% NOT REFACTORED

testDivBy0() -> 5 div 0.

testAlternate(X) -> X + 1 + X.

testAlternate2(X,Y,Z) -> X + Y + Z + X.

testAlternate3(X) -> 1 + X + 2.

foo(X) -> X.
