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

test6(X) ->
    if
	X -> 
	    2 + 3;
	_ ->
	    1
    end.

%%ALL THE ASSIGNMENTS ARE VALID
test7() ->
    A = 1,
    B = A + 2,
    B.


