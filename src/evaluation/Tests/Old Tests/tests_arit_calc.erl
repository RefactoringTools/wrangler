-module(tests_arit_calc).
-compile([export_all]).

testXPlusX(X) -> X + X.

%%1 + 2
test1Plus2() -> 1 + 2.

%% 1 + 2 + 3 + 4 + 5 + 6
testPlus1To6() -> 1 + 2 + 3 + 4 + 5 + 6.

%% 1 * 5 + 7 * 3 * 2 + 1
testPlusAndTimes() -> 1 * 5 + 7 * 3 * 2 + 1.

%%1 + 2 * 4 - 5 + 6 * 7
testPlusTimesAndMinus() -> 1 + 2 * 4 - 5 + 6 * 7.

%% Y - Y
testMinusItself(Y) ->
    Y - Y.

%% (X + Y) - (X + Y)
testMinusItself_2(X,Y) -> (X + Y) - (X + Y).

%% (X + Y + 1) - (X + Y + 1)
testMinusItself_3(X,Y) -> (X + Y + 1) - (X + Y + 1).

%%5 div 1
testDivBy1() -> 5 div 1.

%%5 div 0
testDivBy0() -> 5 div 0.

%%5 div 1 div (2 - 1)
test2Divs() -> 5 div 1 div (2 - 1).

%%5 div 1 div (1 - 1)
test2divsBy0() -> 5 div 1 div (1 - 1).

%% X + 2 * X
testVarsArit(X) -> X + 2 * X.

%% 1 * X + 2 * X
testVarsArit2() -> 1 * X + 2 * X.

%% 2 * X + 3 * X
testVarsArit3(X) -> 2 * X + 3 * X.

%%1 + (X - X)
testPlusAndMinus(X) ->
      1 + (X - X).

%% TESTS THAT FAIL

%% X + 1 + X
testAlternate(X) -> X + 1 + X.

%% X + Y + Z + X
testAlternate2(X,Y,Z) -> X + Y + Z + X.

%% 1 + X + 2 
testAlternate3(X,Y,Z) -> 1 + X + 2.

%% (2 * X - X) + (Y - Y)
testBug(X,Y) -> (2 * X - X) + (Y - Y).


