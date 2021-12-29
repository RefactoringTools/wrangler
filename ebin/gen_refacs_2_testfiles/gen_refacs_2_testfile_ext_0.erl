-module(gen_refacs_2_testfile_ext_0).

-export([orig_fun_appl_ext/0,
         orig_fun_appl_ext_2/0]).

-include_lib("eunit/include/eunit.hrl").


orig_fun_appl_ext() -> 
    gen_refacs_2_testfile_orig_0:orig_fun(1, 2).

orig_fun_appl_ext_2() -> 
    apply(gen_refacs_2_testfile_orig_0,orig_fun,[1, 2]).


basic_test() ->
    [?assert(orig_fun_appl_ext() =:= 3),
     ?assert(orig_fun_appl_ext_2() =:= 3)
    ].
