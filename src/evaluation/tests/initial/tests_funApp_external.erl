-module(tests_funApp_external).
-compile([export_all]).

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
