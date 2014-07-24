-module(test_unreferenced_assign).
-compile([export_all]).

test1() -> 
    A = 1,
    2 + 3.

test2() ->
    A = 10,
    B = 20,
    C = 30,
    E = A + B + C,
    A.

test3() ->
    A = 100,
    B = 200,
    C = 300,
    E = A + C,
    1 + 2.

test4() ->
    A = 3,
    B = A + 1,
    C = B + 3.

test5() ->
    A = 1.

test6(X) ->
    if
	X -> 
	    A = 1,
	    2 + 3;
	_ ->
	    B = 1
    end.

%%ALL THE ASSIGNMENTS ARE VALID
test7() ->
    A = 1,
    B = A + 2,
    B.




