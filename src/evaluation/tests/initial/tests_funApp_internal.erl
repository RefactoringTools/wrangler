-module(tests_funApp_internal).
-record(book, {name,
		authors,
		year,
		publisher}).
-compile(export_all).

%%INTERNAL DEFINITIONS
record_book() ->
    #book{name="Erlang Programming",
    authors=["Francesco Cesarini", "Simon Thompson"],
    year=2009,
    publisher="O'Reilly Media, Inc."}.

f([]) -> 0;
f( [X | Xs] ) -> (2 * X) + f(Xs).

g(X) when X == 0 -> 0;
g(X) when X < 0 -> error;
g(X) when X > 0 andalso not(X == 1) andalso X < 3 -> X + 1 ;
g(X) when is_float(X) andalso X > 0 -> float;
g(X) when (X + X == 3 * X - X andalso X + 1 < 4)-> X + 2;
g(X) when ((X > 0 andalso is_integer(X)) andalso 1 == 1) -> X + 3;
g(_) -> lastClause.

h([]) -> 0;
h(X) when length(X) > 1 -> 1;
h(X) when X > 2 -> 2;
h(_) -> last. 

add(X,Y) -> X + Y.

footle(0,Y) -> Y;
footle(X,Y) ->
    X + footle(X - 1, Y).

infinite_loop(X) -> X + infinite_loop(X - 1).

bar(N) when N > 0 -> N + bar(N-1);
bar(0) -> 0.

guardsFun(X) when X > X -> 1;
guardsFun(_) -> last. 

strangeGuard(N) when [] -> N + 2;
strangeGuard(N) -> N.

is_even(N) when is_number(N) ->
    N rem 2 == 0.
    
addTuple({A,B}) -> A + B.

firstOfTuple({A,B}) ->
     A.

firstOfTuple2({hello,world}) ->
     hello.

f2([X]) -> X;
f2([X | Xs]) -> X + f2(Xs). 
%%TESTS

test_funAppRecord() -> record_book().

testAdd(W,Z) ->
    add(W,Z).

testAdd2(Z,W) ->
    add(Z,W).

testLists1() ->f([1,2,3,4,5]).

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

testInfiniteLoop() ->
    infinite_loop(1000).

testH1() -> h([]).
testH2() -> h([1]).
testH3() -> h([1,2]).

testGuardsFun(X) -> guardsFun(X).

testG1() -> g(-1).

testG2() -> g(2). 

testG3(X) when X > 0 ->
    g(1).

testG4() -> g(3).

testG5() -> g(4.1).

testG6(X) -> g(X).

testLength() ->
    length([1,2,3]).

testBar() -> bar(3).

testBar(X) -> bar(X).

testLength2() -> length(1).

testStrangeGuard() -> strangeGuard(hello).

testAssignFun() ->
    X = is_even(2),
    X.

testIsEven() ->
    X = 2,
    is_even(X).
    
testAddTuple() ->
    addTuple({1,2}).

testFirstOfTuple() ->
    firstOfTuple({hello,world}).

testFirstOfTuple2() ->
    firstOfTuple2({hello,world}).

testf2() -> f2([1,2,3]).

test2f2() -> f2([]).

%%Not WORKING PROPERLY
testFunAppAssign() ->
    X = 3,
    testAssignFun().
    

