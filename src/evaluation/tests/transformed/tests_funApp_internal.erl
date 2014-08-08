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
g(X) when (X + X == 3 * X - X andalso X + 1 < 4)-> X + 2;
g(X) when ((X > 0 andalso is_integer(X)) andalso 1 == 1) -> X + 3;
g(X) when is_float(X) andalso X > 0 -> float;
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

strangeGuard2() when false -> false;
strangeGuard2() when atom -> false2;
strangeGuard2() when true -> true; 
strangeGuard2() -> other. 

is_even(N) when is_number(N) ->
    N rem 2 == 0.

firstOfTuple({A,B}) ->
     A.

firstOfTuple2({hello,world}) ->
     hello.

f2([X]) -> X;
f2([X | Xs]) -> X + f2(Xs). 
    
%%TESTS
test_funAppRecord() ->
    #book{name = "Erlang Programming",
    authors = ["Francesco Cesarini","Simon Thompson"],
    year = 2009,
    publisher = "O'Reilly Media, Inc."}.

testAdd(W,Z) ->
     W + Z.

testAdd2(Z,W) ->
     Z + W.

testLists1() -> 2 * 1 + (2 * 2 + (2 * 3 + (2 * 4 + (2 * 5 + 0)))).

testLists2() ->
    2 * 1 + (2 * 2 + (2 * 3 + (2 * 4 + (2 * 5 + (2 * 6 + (2 * 7 + (2 * 8 + (2 * 9 + (2 * 10 + 0))))))))).

testHugeList() ->
    2 * 0 + (2 * 1 + (2 * 2 + (2 * 3 + (2 * 4 + (2 * 5 + (2 * 6 + (2 * 7 + (2 * 8 + (2 * 9 + (2 * 10 + (2 * 11 + (2 * 12 + (2 * 13 + (2 * 14 + (2 * 15 + (2 * 16 + (2 * 17 + (2 * 18 + (2 * 19 + (2 * 20 + (2 * 21 + (2 * 22 + (2 * 23 + (2 * 24 + (2 * 25 + (2 * 26 + (2 * 27 + (2 * 28 + (2 * 29 + (2 * 30 + (2 * 31 + (2 * 32 + (2 * 33 + (2 * 34 + (2 * 35 + (2 * 36 + (2 * 37 + (2 * 38 + (2 * 39 + (2 * 40 + (2 * 41 + (2 * 42 + (2 * 43 + (2 * 44 + (2 * 45 + (2 * 46 + (2 * 47 + (2 * 48 + (2 * 49 + (2 * 50 + 0)))))))))))))))))))))))))))))))))))))))))))))))))).

testFootle(Y) ->
     3 + footle(3 - 1,Y).

testFootle2(X) ->
    footle(X,3).

testFootle3() ->
    5 + footle(5 - 1, 3).

testFootle4() -> 3 + footle(3 - 1, 4).

testFootle5(Y) -> 3 + footle(3 - 1,Y + 1).

testInfiniteLoop() -> infinite_loop(1000).

testH1() -> 0.
testH2() -> 2.
testH3() -> 1.

testGuardsFun(X) -> last.

testG1() -> error.

testG2() -> 2 + 1. 

testG3(X) when X > 0 -> 1 + 2.

testG4() -> 3 + 3.

testG5() -> float.

testG6(X) -> g(X).

testLength() -> 3.

testBar() -> 3 + (3 - 1 + (3 - 1 - 1 + bar(3 - 1 - 1 - 1))).

testBar(X) -> bar(X).

testLength2() -> length(1).

testStrangeGuard() -> hello.
    
testAssignFun() ->
    X = 2 rem 2 == 0,
    X.

testIsEven() ->
    X = 2,
    is_even(X).

testAddTuple() ->
    1 + 2.

testFirstOfTuple() -> hello.

testFirstOfTuple2() -> hello.
    
testf2() -> 1 + 2 + 3.
    
test2f2() -> f2([]).

testFunAppAssign() ->
    X = 3,
    testAssignFun().

testFunAppAssign2() ->
    X = 3,
    Y = X + 1,
    testAssignFun().

testFunAppAssign3() ->
    X = 3,
    Y = X + 1,
    1 + testAssignFun().

testFunAppAssign4() ->
    X = 3,
    Y = X + 1,
    Z = testAssignFun(),
    X + 2 + Z.

testFunAppAssign5() ->
    X = 3,
    testAssignFun(),
    X + 2.

testFunAppAssign6(X) ->
    testAssignFun().

testEvaluator() ->
    X = 3,
    testAssignFun().

testStrangeGuard2() -> true.
