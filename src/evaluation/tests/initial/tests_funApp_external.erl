-module(tests_funApp_external).
%%-compile([export_all]).
-export([internalCall2/1]).

testTwiceX(X) -> def:twice(X).

testTwice0() -> def:twice(0).

testTwice1() -> def:twice(1).

testH(X) -> def:h(X).

testH() -> def:h(2).

testTripleX(X) -> def:triple(X).

testTwiceTimesTriple(X,Y) ->
    def:twice(X) * def:triple(Y).

testTwiceOfTriple(X) ->
   def:twice(def:triple(X)).

testTwiceOfTwice(X)->
    def:twice(def:twice(X)).

testOfTwice2() ->
    def:twice(def:twice(4)).

testNestedApps(X) ->  def:add(def:twice(def:twice(3)),def:triple(X)).

testLists2(X) -> def:f([X,2,3,4,5]).

testFac10() ->
    def:fac(10).
    
testMap() -> def:map(fun(X) -> 2 * X end,[1,2,3]).

testMap2() -> def:map(fun(X) -> 2 * X end,[]).

testMap3() -> def:map(fun def:twice/1,[1,2,3]).

testMap4() -> def:map(fun def:twice/1,[]).

testFunAppAssign() ->
    X = 3,
    def:testAssignFun().

testBar() ->
    def:map(fun(X) -> def2:bar(X) end, [0,1,2,3]).

testExternalCall()->
    def:externalCall(3).

testExternalCall2()->
    def:externalCall2(3).

testInternalCall()->
    tests_funApp_external:internalCall(3).

internalCall(X) when X > 0 -> 2 * X + internalCall(X - 1); 
internalCall(0) -> 10. 

internalCall2(X) when X > 0 -> 2 * X + internalCall(X - 1); 
internalCall2(0) -> 10. 
    
