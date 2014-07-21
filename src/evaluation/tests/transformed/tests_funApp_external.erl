-module(tests_funApp_external).
%%-compile([export_all]).
-export([internalCall2/1]).

testTwiceX(X) -> def:twice(X).

testTwice0() -> 0.

testTwice1() -> 2 * 1.

testH(X) -> def:g(X).

testH() -> 2 + 1.

testTripleX(X) -> 3 * (X).

testTwiceTimesTriple(X,Y) ->
     def:twice(X) * (3 * (Y)).

testTwiceOfTriple(X) ->
    def:twice(3 * (X)).

testTwiceOfTwice(X)->
    def:twice(def:twice(X)).

testOfTwice2() -> def:twice(2 * 4).

testNestedApps(X) -> def:add(def:twice(2 * 3),3 * (X)).

testLists2(X) -> X + (2 + (3 + (4 + (5 + 0)))).

testFac10() -> 10 * def:fac(10 - 1).
    
testMap() ->
    [2 * 1,2 * 2,2 * 3].

testMap2() -> [].

testMap3() ->
    [2 * 1,2 * 2,2 * 3].

testMap4() ->
    [].

testFunAppAssign() ->
    X = 3,
    def:testAssignFun().

testBar() ->
    [0, 1 + def2:bar(1 - 1), 2 + def2:bar(2 - 1), 3 + def2:bar(3 - 1)].

testExternalCall() -> 3 + tests_funApp_external:internalCall(3 - 1).

testExternalCall2() -> 3 + internalCall2(3 - 1).

testInternalCall()->
    tests_funApp_external:internalCall(3).

internalCall(X) when X > 0 -> 2 * X + internalCall(X - 1); 
internalCall(0) -> 10. 

internalCall2(X) when X > 0 -> 2 * X + internalCall(X - 1); 
internalCall2(0) -> 10. 
