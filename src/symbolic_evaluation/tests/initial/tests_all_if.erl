-module(tests_all_if).
-compile([export_all]).

is_even(N) when is_number(N) ->
    N rem 2 == 0.

test1() ->
    if
	true -> [2];
	true -> []
    end.

test2() ->
    if
	true -> 
	    A = 1,
	    A;
	true -> 2
    end.

test3() ->
    if
	false -> [2];
	true -> []
    end.

test4() ->
    X = true,
    if
	X -> [2];
	true -> []
    end.

test5() ->
    X = false,
    if
	X -> [2];
	true -> []
    end.

test6() ->
    X = false,
    Y = true,
    if
	X -> [2];
	Y -> [3];
	true -> []
    end.

test7() ->
    X = 4,
    if
	X > 2 -> [2];
	true -> [3]
    end.

test8() ->
    Y = 4,
    X = Y > 2,
    if
	X -> [2];
	true -> [3]
    end.

test9()->
    if
	is_integer(2) -> [3];
	true -> []
    end.

test10() ->
    Y = 1,
    X = Y > 2,
    if
	X -> [2];
	true -> [3]
    end.

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




