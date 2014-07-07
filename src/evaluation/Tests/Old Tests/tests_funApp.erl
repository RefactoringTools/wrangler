-module(tests_funApp).
-record(robot, {name,
		type=industrial,
		hobbies,
		details=[]}).
-export([h/1]).

first_robot() ->
    #robot{name="Mechatron",
    type=handmade,
    details=["Moved by a small man inside"]}.

first_robot2()-> {name,age}.

first_robot3() ->
     [1,2].

first_robot4() ->
     1 + 2.

test() -> first_robot().

f([]) -> 0;
f( [X | Xs] ) -> (2 * X) + f(Xs).

add(X,Y) -> X + Y.
    
%%TESTS

test2() -> 2 + 1.

%%twice(X)
testTwiceX(X) -> definitions:twice(X).

%%twice(0)
testTwice0() -> definitions:twice(0).

%%twice(1)
testTwice1() -> definitions:twice(1).

testH(X) -> definitions:h(X).

testH() -> definitions:h(2).

testExternal() -> definitions:externalCall(10).

externalCall(0) -> 0;
externalCall(X) -> X + definitions:externalCall(X - 1).

%%triple(X)
testTripleX(X) -> definitions:triple(X).

%%twice(X) * triple(Y)
testTwiceTimesTriple(X,Y) ->
    definitions:twice(X) * definitions:triple(Y).

%%add(W,Z)
testAdd(W,Z) ->
    add(W,Z).

%%add(Z,W)
testAdd2(Z,W) ->
    add(Z,W).

%%twice(triple(X))
testTwiceOfTriple(X) ->
   definitions:twice(definitions:triple(X)).

%%twice(twice(X))
testTwiceOfTwice(X)->
    definitions:twice(definitions:twice(X)).

testOfTwice2() ->
    defitions:twice(definitions:twice(1)).

testOfTwice3() ->
    definitions:twice(definitions:twice(4)).

%% add(twice(twice(X)),triple(Y+1)).
testNestedApps(X,Y) ->  definitions:add(definitions:twice(definitions:twice(X)),definitions:triple(Y+1)).

%% f([1,2,3,4,5])
testLists1() ->f([1,2,3,4,5]).

%% f([X,2,3,4,5])
testLists2(X) -> definitions:f([X,2,3,4,5]).

testHugeList() ->
    f([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50]).

footle(0,Y) -> Y;
footle(X,Y) ->
    X + footle(X - 1, Y).

%%footle(3, Y)
testFootle(Y) ->
    footle(3,Y).

%%footle(X,3)
testFootle2(X) ->
    footle(X,3).

%%testFootle2(5)
%%== 5 + footle(4,3)
%%== 5 + (4 + footle (3,3))
%%== 5 + (4 + (3 + (footle(2,3))))
%%== 5 + (4 + (3 + (2 + footle(1,3))))
%%== 5 + (4 + (3 + (2 + (1 + (footle(0,3))))))
%%== 5 + (4 + (3 + (2 + (1 + (3)))))
testFootle3() ->
    testFootle2(5).

testFootle4()->
    footle(3, 4).

%%footle(3, Y)
testFootle5(Y) ->
    footle(3,Y + 1).

infinite_loop(X) -> X + infinite_loop(X - 1).

testInfiniteLoop() ->
    infinite_loop(1000).
g(X) when X == 0 -> 0;
g(X) when X < 0 -> error;
g(X) when X > 0 andalso not(X == 1) andalso X < 3 -> X + 1 ;
g(X) when (X + X == 3 * X - X andalso X + 1 < 4)-> X + 2;
g(X) when ((X > 0 andalso is_integer(X)) andalso 1 == 1) -> X + 3;
g(X) when is_float(X) andalso X > 0 -> float;
g(X) -> lastClause.

h([]) -> 0;
h(X) when length(X) > 1 -> 1;
h(X) when X > 2 -> 2;
h(X) -> last. 

testH1() -> h([]).
testH2() -> h([1]).
testH3() -> h([1,2]).

guardsFun(X) when X > X -> 1;
guardsFun(_) -> last. 

testGuardsFun(X) -> guardsFun(X).

testG1() -> g(-1).

testG2() -> g(2). 

testG3(X) when X > 0 ->
    g(1).

testG4() -> g(3).

testG5() -> g(4.1).

testG6(X) -> g(X).

bar(N) when N > 0 -> N + bar(N-1);
bar(0) -> 0.

strangeGuard(N) when [] -> N + 2;
strangeGuard(N) -> N.

testLength() ->
    length([1,2,3]).

testBar() -> bar(3).

testLength2() -> length(1).

testMap() -> lists:map(fun(X) -> 2 * X end,[1,2,3]).

testMap2() -> lists:map(fun(X) -> 2 * X end,[]).

testMap3() -> defi:map(fun defi:twice/1,[1,2,3]).

testMap4() -> defi:map(fun defi:twice/1,[1,2,3]).


