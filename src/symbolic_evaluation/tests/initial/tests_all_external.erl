%% @private
-module(tests_all_external).
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

testOfTwice() ->
    def:twice(def:twice(4)).

testNestedApps() ->  def:add(def:twice(def:twice(3)),def:triple(4)).

testLists2(X) -> def:f([X,2,3,4,5]).

testFac10() ->
    def:fac(10).

testMap() -> def:map(fun(X) -> 2 * X end,[0,1,2,3]).

testMap2() -> def:map(fun(X) -> 2 * X end,[]).

testMap3() -> def:map(fun def:twice/1,[0,1,2,3]).

testMap4() -> def:map(fun def:twice/1,[]).
    
testFilter() -> def:filter(fun(X) -> X > 10 end, [2,4,6,10,15,19]).

testWrongFilter() -> def:filter(fun(X) -> X + 10 end, [2,4,6,10,15,19]).

testBar() ->
    def:map(fun(X) -> def2:bar(X) end, [0,1,2,3]).

testCase() ->
    def3:case_refac(4).


    


