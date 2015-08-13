%% @private
-module(tests_all_internal).
-compile([export_all]).

%%INTERNAL DEFINITIONS
add(X,Y) -> X + Y.

addTupleElems({A,B}) -> A + B. 

f([]) -> 0;
f( [X | Xs] ) -> (2 * X) + f(Xs).

footle(0,Y) -> Y;
footle(X,Y) ->
    X + footle(X - 1, Y).

infinite_loop(X) -> infinite_loop(X + 1).

is_even(N) when is_number(N) ->
    N rem 2 == 0.

%%TESTS
testTupleAndApp1() -> 3.

testTupleAndApp2(X) -> 2 * X.

testTupleAndApp3(X) -> X + 7.

testIdentityAndApp(X) -> 2 * X.

testLists1() -> 30.

testLists2() -> 110.

testHugeList() -> 2550.

testFootle(Y) -> 3 + (2 + (1 + Y)).

testFootle2(X) ->
    footle(X,3).

testFootle3() -> 18.

testFootle4() -> 10.

testFootle5(Y) ->
    footle(3,Y + 1).

testInfiniteLoop() ->
    infinite_loop(1000).
    
testAssignFun() ->
    X = true,
    X.

testFunAppAssign() ->
    X = true,
    X.

testFunAppAssign2() ->
    X = true,
    X.

testFunAppAssign3() ->
    1 + testAssignFun().

testFunAppAssign4() ->
    X = 3,
    Z = testAssignFun(),
    X + 2 + Z.

testFunAppAssign5() ->
    X = 3,
    testAssignFun(),
    X + 2.

testFunAppAssign6(X) ->
    testAssignFun().

testEvaluator() -> 
    X = true,
    X.
