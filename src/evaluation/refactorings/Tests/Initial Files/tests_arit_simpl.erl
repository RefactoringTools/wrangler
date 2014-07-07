-module(tests_arit_simpl).
-compile([export_all]).

testPlusZero(X) -> X + 0.

testPlusZero2(X,Y) -> 0 + (X + Y).

testExpTimesOne(X,Y) -> (X + Y) * 1.
    
testExpTimesOne2(X,Y) -> 1 * (X + Y + 1).

testPlusZero3(X,Y) -> 
    Z = X + Y + 1, 
    Z + 0.

testMinusZero(X) -> 
    Z = X - 0,
    Z.

testTimes0AndPlus0(X) -> X + X * 0 + 0 * X.

testDivByOne(X,Y) -> (X + Y) div 1. 

testZeroMinus(X) -> 0 - X.

testSeqOfOps(X,Y,Z) ->
    I = X + 0 + Y * 1,
    J = I * 1 - 0,
    L = 0 - J,
    (Z + L - 0) div 1.








