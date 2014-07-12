-module(tests_boolean_operators).
-compile([export_all]).

test() -> false.

test2() -> true.

test3() -> false.

test4() -> false.

test5() -> true.

test6() -> true.

test7() -> true.

test8() -> true.

test9() -> true.

test10() -> true.

test20() ->
       X = 10,
       X > 5.

test30(Y) ->
	 Y > 3.
