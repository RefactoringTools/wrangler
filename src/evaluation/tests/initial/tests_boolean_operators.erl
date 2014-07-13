-module(tests_boolean_operators).
-compile([export_all]).

test_gt_1() ->
    1 > 2.

test_gt_2() ->
    2 > 1.

test_gt_3() ->
    2 > 2.

test_lt_1() ->
    3 < 2.

test_lt_2() ->
    2 < 3.

test_lt_3() ->
    3 < 3.

test_lte_1() ->
    3 =< 2.

testlte_2() ->
    2 =< 2.

testlte_3() ->
    2 =< 3.

test_eequal_1() ->
    5 =:= 5.

test_eequal_2() ->
    5 =:= 6.

test_eequal_3() ->
    1 =:= 1.0.

test_equal_1() ->
    atom == atom.

test_equal_2() ->
    atom == true.

test_equal_3() ->
    {a,b} == {a,b}.

test_diff_1() ->
    true /= false.

test_diff_2() ->
    true /= true.

test_ediff_1() ->
    true =/= false.

test_ediff_2() ->
    true =/= true.

test_ediff_3() ->
    1 =/= 1.0.

test_and_1() ->
    true and false.

test_and_2() ->
    false and false.

test_and_3() ->
    false and true.

test_and_4() ->
    true and true.

test_andalso_1() ->
    true andalso false.

test_andalso_2() ->
    false andalso false.

test_andalso_3() ->
    false andalso true.

test_andalso_4() ->
    true andalso true.

test_or_1() ->
    true or false.

test_or_2() ->
    false or false.

test_or_3() ->
    false or true.

test_or_4() ->
    true or true.

test_orelse_1() ->
    true orelse false.

test_orelse_2() ->
    false orelse false.

test_orelse_3() ->
    false orelse true.

test_orelse_4() ->
    true orelse true.

test_xor_1() ->
    true xor false.

test_xor_2() ->
    false xor false.

test_xor_3() ->
    false xor true.

test_xor_4() ->
    true xor true.

test_xor_5() ->
    maybe xor true.

test_not_1() ->
    not(false).

test_not_2() ->
    not(true).

test_not_3() ->
    not(maybe).

test200() ->
       X = 10,
       X > 5.

test300(Y) ->
	 Y > 3.
