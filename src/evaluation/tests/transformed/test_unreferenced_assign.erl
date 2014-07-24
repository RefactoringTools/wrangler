-module(test_unreferenced_assign).
-compile([export_all]).

test1() -> 2 + 3.

test2() ->
    A = 10,
    A.

test3() -> 1 + 2.

test4() ->
    A = 3,
    B = A + 1,
    B + 3.

test5() -> 1.

test6(X) ->
    if
	X -> 
	    A = 3,
	    B = A + 1,
	    B + 3;
	true ->
	    1
    end.

test7(X) ->
    if
	X -> 
	    2 + 3;
	true ->
	    A = 10,
	    A
    end.

test8(X) ->
    if
	X -> 
	    A = 3,
	    B = A + 1,
	    B + 3;
	true ->
	    1
    end.

test9(0) -> 1;
test9(1) ->
    X = 1,
    X.

%%ALL THE ASSIGNMENTS ARE VALID
validTest() ->
    A = 1,
    B = A + 2,
    B.





