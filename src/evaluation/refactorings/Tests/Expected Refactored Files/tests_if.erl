-module(tests_if).
-compile([export_all]).

test1() ->
    [2].

test2() ->
	A = 1,
	A.

test3() -> [].

test4() ->
    X = true,
    [2].

test5() ->
    X = false,
    [].

test6() ->
    X = false,
    Y = true,
    [3].

test7() ->
    X = 4,
    [2].

test8() ->
    Y = 4,
    X = Y > 2,
    [2].

test9()->
    [3].

test10() ->
    Y = 1,
    X = Y > 2,
    [3].

test11(X) ->
    if
	X -> [2];
	true -> []
    end.

test12() ->
    Aux = 0 == 0,
    [2].

test13() ->
    Aux = 1 == 0,
    [].

funIf(X) ->
    if
	X > 2 -> [2];
	true -> [3]
    end.

testFunIf() ->
    [2].
 	    	     
is_even(N) when is_number(N) ->
    N rem 2 == 0.




