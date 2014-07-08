-module(tests_if).
-export([test1/0,test2/0,test3/0,test4/0,test5/0,test6/0,test7/0,test8/0,test9/0,test10/0,test11/1,test12/0,test13/0,funIf/1,testFunIf/0]).

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

funIf(X) ->
    if
	X > 2 -> [2];
	true -> [3]
    end.

testFunIf() ->
    funIf(3).
 	    	     
is_even(N) when is_number(N) ->
    N rem 2 == 0.




