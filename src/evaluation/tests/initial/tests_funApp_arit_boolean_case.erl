-module(tests_funApp_arit_boolean_case).
-compile([export_all]).

test1() ->
    case [1,2] of
    	 Var -> hello;
	 _ -> bye
    end.

test2() ->
    case [1,2] of
    	 [H | T] -> hello;
	 _ -> bye
    end.

test3() ->
    case [1,2] of
    	 [] -> hello;
	 _ -> bye
    end.

test4() ->
    case [1,2] of
    	 [] -> hello;
	 _A -> bye
    end.

test5() ->
    case hello of
	Var -> hello;
	_ -> bye
    end.

test6() ->
    case is_atom(hello) of
	true -> hello;
	_ -> bye
    end.

test7() ->
    case {hello, world} of
	{A,B} -> hello;
	_ -> bye
    end.

test8() ->
    case {hello,world} of
	{hello,world} -> helloWorld;
	_ -> bye
    end.

test9() ->
    case [1,2] of
    	 Var -> Var;
	 _ -> bye
    end.

test10() ->
    case [1,2,3] of
    	 [H | T] -> [H | T];
	 _ -> bye
    end.

test11() ->
    X = true,
    case X of 
	true -> hello;
	_ -> bye
   end.

test12() ->
    X = true,
     case X of 
	true -> X;
	_ -> bye
   end.

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

test20() ->
    X = is_even(3),
    if
	X -> test17(true);
	true -> test17(false) 
    end.
	    
is_even(N) when is_number(N) ->
    N rem 2 == 0.
	    
