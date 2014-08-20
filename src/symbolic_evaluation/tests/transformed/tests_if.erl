-module(tests_if).
-compile([export_all]).

is_even(N) when is_number(N) ->
    N rem 2 == 0.

test1() -> [2].

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

test9() -> [3].

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
    Aux = is_even(2),
    if
	Aux -> [2];
	true -> []
    end.

test13() ->
    Aux = is_even(1),
    if
	Aux -> [2];
	true -> []
    end.

test14()->
    if
	2 < 1 -> 42;
	false -> 37
    end.

test15() -> 37.

test16() -> 37.

test17() -> 37.

test18() -> 37.
	    
funIf(X) ->
    if
	X > 2 -> [2];
	true -> [3]
    end.

testFunIf() ->
    funIf(3).
 	    	     
%%can be done in 2 stages
testCallTest4() ->
    test4().




