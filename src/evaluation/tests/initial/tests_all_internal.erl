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

%%TESTS
testTupleAndApp1() -> addTupleElems({1,2}).

testTupleAndApp2(X) -> addTupleElems({X,X}).

testTupleAndApp3(X) -> addTupleElems({X, addTupleElems({3,4})}).

testIdentityAndApp(X) ->	       
     add(X,X) * 1 + 0.

testLists1() -> f([1,2,3,4,5]).

testLists2() -> f([1,2,3,4,5,6,7,8,9,10]).

testHugeList() ->
    f([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50]).

testFootle(Y) ->
    footle(3,Y).

testFootle2(X) ->
    footle(X,3).

testFootle3() ->
    testFootle2(5).

testFootle4()->
    footle(3, 4).

testFootle5(Y) ->
    footle(3,Y + 1).

testInfiniteLoop() -> infinite_loop(1000).
    


