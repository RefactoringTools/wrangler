-module(tests_boolean_operators).
-compile([export_all]).

test_gt_1() -> false.

test_gt_2() -> true.

test_gt_3() -> false.

test_lt_1() -> false.

test_lt_2() -> true.

test_lt_3() -> false.

test_lte_1() -> false.

testlte_2() -> true.

testlte_3() -> true.

test_eequal_1() -> true.

test_eequal_2() -> false.

test_eequal_3() -> false.

test_equal_1() -> true.

test_equal_2() -> false.

test_equal_3() -> true.

test_diff_1() -> true.

test_diff_2() -> false.

test_ediff_1() -> true.

test_ediff_2() -> false.

test_ediff_3() -> true.

test_and_1() -> false.

test_and_2() -> false.

test_and_3() -> false.

test_and_4() -> true.

test_andalso_1() -> false.

test_andalso_2() -> false.

test_andalso_3() -> false.

test_andalso_4() -> true.

test_or_1() -> true.

test_or_2() -> false.

test_or_3() -> true.

test_or_4() -> true.

test_orelse_1() -> true.

test_orelse_2() -> false.

test_orelse_3() -> true.

test_orelse_4() -> true.

test_xor_1() -> true.

test_xor_2() -> false.

test_xor_3() -> true.

test_xor_4() -> false.

test_xor_5() ->
    maybe xor true.

test_not_1() -> true.

test_not_2() -> false.

test_not_3() ->
    not(maybe).

test_is_list_1() -> true.

test_is_list_2() -> true.

test_is_list_3() -> false.

test_is_tuple_1() -> true.

test_is_tuple_2() -> true.

test_is_tuple_3() -> false.

test_is_integer_1() -> true.

test_is_integer_2() -> false.

test_is_integer_3() -> false.

test_is_float_1() -> false.

test_is_float_2() -> true.

test_is_float_3() -> false.

test_is_number_1() -> true.

test_is_number_2() -> true.

test_is_number_3() -> false.

test_is_boolean_1() -> false.

test_is_boolean_2() -> true.

test_is_boolean_3() -> true.

test_is_atom_1() -> true.

test_is_atom_2() -> true.

test_is_atom_3() -> true.

test_is_atom_4() -> false.

test_is_function_1() ->
    is_function(hello).

test_is_function_2() ->
    is_function(fun(X) -> 1 + X end).

test_is_function_3() ->
    is_function(fun(X) -> 1 + X end(3)).

test_is_function_4() ->
    is_function(fun def:test_is_function_1/0).

test200() ->
       X = 10,
       X > 5.

test300(Y) ->
	 Y > 3.
