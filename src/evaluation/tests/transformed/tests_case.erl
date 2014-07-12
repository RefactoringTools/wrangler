-module(tests_case).
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

test13() ->
    case is_even(2) of 
	true -> hello;
	_ -> bye
   end.

test14() ->
    case is_even(1) of 
	true -> hello;
	_ -> bye
   end.

test15() ->
    X = is_even(1),
     case X of 
	true -> hello;
	_ -> bye
   end.

test16() ->
    X = is_even(4),
     case X of 
	true -> hello;
	_ -> bye
   end.

test17(X) ->
   case X of 
	true -> hello;
	_ -> bye
   end.

test18() ->
    test17(true).

test19() ->
    test17(false).

is_even(N) when is_number(N) ->
    N rem 2 == 0.
	    
