-module(tests_funApp_arit_boolean_case).
-compile([export_all]).

test1() -> hello.

test2() -> hello.

test3() -> bye.

test4() -> bye.

test5() -> hello.

%%NOT WORKING -- extend boolean operators refactoring
test6() ->
    case is_atom(hello) of
	true -> hello;
	_ -> bye
    end.

test7() -> hello.

test8() -> helloWorld.

test9() -> [1,2].

test10() -> [1,2,3].

test11() ->
    X = true,
    hello.

test12() ->
    X = true,
    X.

test13() -> hello.

test14() -> bye.

test15() ->
    X = false,
    bye.

test16() ->
    X = true,
    hello.

test17(X) ->
   case X of 
	true -> hello;
	_ -> bye
   end.

test18() -> hello.

test19() -> bye.

is_even(N) when is_number(N) ->
    N rem 2 == 0.
	    
