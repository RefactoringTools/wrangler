-module(gen_refacs_2_testfile_ext_1).

-export([orig_fun_appl_ext/0,
         orig_fun_appl_ext_2/0]).

-include_lib("eunit/include/eunit.hrl").


orig_fun_appl_ext() -> 
    gen_refacs_2_testfile_orig_1:orig_fun(10).

orig_fun_appl_ext_2() -> 
    apply(gen_refacs_2_testfile_orig_1, orig_fun, [10]).


basic_test() ->
    [?assert(orig_fun_appl_ext() =:= 15),
     ?assert(orig_fun_appl_ext_2() =:= 15)
    ].
