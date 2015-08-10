-module(tests_arit_calc).
-compile([export_all]).

testXPlusX(X) -> 2 * X.

test1Plus2() -> 3.

testPlus1To6() -> 21.

testPlusAndTimes() -> 48.

testPlusTimesAndMinus() -> 46.

testMinusItself(Y) ->
     0.

testMinusItself2(X,Y) -> 
      0.

testMinusItself3(X,Y) -> 0.

testDivBy1() -> 5.

test2Divs() -> 5.

test2divsBy0() -> 5 div 0.

testVarsArit(X) -> 3 * X.

testVarsArit2(X) -> 3 * X.

testVarsArit3(X) -> 5 * X.

testPlusAndMinus(X) ->
       1.

testVariablesPlusAndMinus(X,Y) -> (1 * X) + 0.

testArbitrary(Y) ->
     3 * (foo(3) + Y).

testExp(X, Y, Z) ->
      3 * (X + Y + Z).

%% NOT REFACTORED

testDivBy0() -> 5 div 0.

testAlternate(X) -> X + 1 + X.

testAlternate2(X,Y,Z) -> X + Y + Z + X.

testAlternate3(X) -> 1 + X + 2.

foo(X) -> X.
