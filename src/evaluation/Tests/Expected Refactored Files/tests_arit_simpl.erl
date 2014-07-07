-module(tests_arit_simpl).
-compile([export_all]).

testPlusZero(X) -> X.

testPlusZero2(X,Y) -> X + Y.

testExpTimesOne(X,Y) -> X + Y.
    
testExpTimesOne2(X,Y) -> X + Y + 1.

testPlusZero3(X,Y) -> 
    Z = X + Y + 1, 
    Z.

testMinusZero(X) -> 
     Z = X,
     Z.

testTimes0AndPlus0(X) -> X.

testDivByOne(X,Y) -> X + Y. 

testZeroMinus(X) -> -X.

testSeqOfOps(X,Y,Z) ->
     I = X + Y,
     J = I,
     L = -J,
     Z + L.
