%% @private
-module(tests_lists_concat).

test() ->
    [1,2] ++ [3,4].

test2() ->
    "ab" ++ "cd".

test3() ->
   [1,2] ++ "a,b".

test4() ->
   [1,2] ++ "ab".

test5() ->
    "abc" ++ [1,2].

test6() ->
    [] ++ [].

test7() ->
    [] ++ [1,2].

test8() ->
    [1,2,3] ++ [].

test9() ->
    [] ++ "".

test10() ->
    "" ++ [].

test11() ->
    [] ++ "abc".

test12() ->
    "Hello World!" ++ [].

test13() ->
    [a,b,c] ++ "".

test14() ->
    "" ++ [{a,b,c},d].

test15() ->
    "" ++ "".

test16() ->
    "Hello" ++ " " ++ "World!".

test17() ->
    [1,2,3,4,5,6,7,8,9,10] ++ [{a,b,c}, {d,e,f}].

test18() ->
    [3 | [4,7]] ++ [].

test19() ->
    [3 | [4,7]] ++ [9,10].

test20() ->
    [ "Hello" ++ " " ++ "World!"]++[ "Hello" ++ " " ++ "World!"].

test21() ->
    [test16()] ++ [test16()].


