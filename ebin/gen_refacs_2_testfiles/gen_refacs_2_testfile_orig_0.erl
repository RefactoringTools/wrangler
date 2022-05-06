-module(gen_refacs_2_testfile_orig_0).

-export([orig_fun_appl/0,
         orig_fun_appl_2/0,
         orig_fun_appl_3/0,orig_fun/2]).

-include_lib("eunit/include/eunit.hrl").


orig_fun(X, Y) -> X + Y.

orig_fun_appl() -> 
    orig_fun(1,2).

orig_fun_appl_2() -> 
    gen_refacs_2_testfile_orig_0:orig_fun(1,2).

orig_fun_appl_3() -> 
    apply(gen_refacs_2_testfile_orig_0,orig_fun,[1,2]).


basic_test() ->
    [?assert(orig_fun(1, 2) =:= 3),
     ?assert(orig_fun_appl() =:= 3),
     ?assert(orig_fun_appl_2() =:= 3),
     ?assert(orig_fun_appl_3() =:= 3)
    ].
