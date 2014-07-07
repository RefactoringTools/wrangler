-module(tests_boolean_operators).

test() ->
    1 > 2.

test2() ->
    2 > 1.

test3() ->
    3 < 2.

test4() ->
    3 =< 2.

test5() ->
    2 =< 2.

test6() ->
    5 =:= 5.

test7() ->
    atom == atom.

test8() ->
    true /= false.

test9() ->
    true =/= false.

test10() ->
    {a,b} == {a,b}.

test20() ->
       X = 10,
       X > 5.

test30(Y) ->
	 Y > 3.
