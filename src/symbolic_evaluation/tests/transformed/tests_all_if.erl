%% @private
-module(tests_all_if).
-compile([export_all]).

is_even(N) when is_number(N) ->
    N rem 2 == 0.

test1() -> [2].

test2() ->
    A = 1,
    A.

test3() -> [].

test4() ->
    [2].

test5() ->
    [].

test6() ->
    [3].

test7() ->
    [2].

test8() ->
    [2].

test9() -> [3].

test10() -> [3].

test11(X) ->
    if
	X -> [2];
	true -> []
    end.

test12() -> [2].

test13() -> [].

test14()->
    if
	false -> 42;
	false -> 37
    end.
	    
funIf(X) ->
    if
	X > 2 -> [2];
	true -> [3]
    end.

testFunIf() -> [2].
 	    	     
%%can be done in 2 stages
testCallTest4() ->
    X = true,
    if X -> [2];
       true -> []
    end.




