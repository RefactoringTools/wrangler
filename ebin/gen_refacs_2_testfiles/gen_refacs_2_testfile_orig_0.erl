-module(gen_refacs_2_testfile_orig_0).

-export([orig_fun_appl/0,
         orig_fun_appl_2/0,
         orig_fun_appl_3/0,orig_fun/1]).

-include_lib("eunit/include/eunit.hrl").


orig_fun(X) -> X + 5.

orig_fun_appl() -> 
    orig_fun(10).

orig_fun_appl_2() -> 
    gen_refacs_2_testfile_orig_0:orig_fun(10).

orig_fun_appl_3() -> 
    apply(gen_refacs_2_testfile_orig_0,orig_fun,[10]).


basic_test() ->
    [?assert(orig_fun(1) =:= 6),
     ?assert(orig_fun_appl() =:= 15),
     ?assert(orig_fun_appl_2() =:= 15),
     ?assert(orig_fun_appl_3() =:= 15)
    ].
