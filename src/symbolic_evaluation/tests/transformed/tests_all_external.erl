-module(tests_all_external).
-compile([export_all]).

testTwiceX(X) -> def:twice(X).

testTwice0() -> 0.

testTwice1() -> 2.

testH(X) -> def:g(X).

testH() -> 3.

testTripleX(X) -> 3 * (X).

testTwiceTimesTriple(X,Y) ->
     def:twice(X) * (3 * (Y)).

testTwiceOfTriple(X) ->
    def:twice(3 * (X)).

testOfTwice() -> 16.

testNestedApps() -> def:add(12,12).

testLists2(X) -> X + 14.

testFac10() -> 3628800.

testMap() ->
    [0,2,4,6].
    
testMap2() -> [].

testMap3() ->
    [0,2,4,6].

testMap4() -> [].
    
testFilter() -> [E || E <- [2,4,6,10,15,19],E > 10].

testWrongFilter() -> [E || E <- [2,4,6,10,15,19],E + 10].

testBar() -> [0, 1, 3, 6].

testCase() -> even.


    


