-module(eqc_clone).

-compile(export_all).

test_clone_detection(Dirs) ->
    application:start(wrangler_app),
    Res= api_wrangler:similar_code(Dirs, 5, 40, 2, 4, 0.8, Dirs),
    wrangler_io:format("Res:\n~p\n",[Res]),
    application:stop(wrangler_app).
  
test_clone_detection0()->
    test_clone_detection(["c:/cygwin/home/hl/test_codebase/ibrowse"]).
    
test_clone_detection1() ->
    test_clone_detection(["c:/cygwin/home/hl/test_codebase/lampera" ]).

test_clone_detection2() ->
    test_clone_detection(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_clone_detection3() ->
    test_clone_detection(["c:/cygwin/home/hl/test_codebase/refactorerl"]).

test_clone_detection4() ->
    test_clone_detection(["c:/cygwin/home/hl/test_codebase/suites"]).

test_clone_detection5() ->
    test_clone_detection(["c:/cygwin/home/hl/test_codebase/wrangler"]).

test_clone_detection6() ->
    test_clone_detection(["c:/cygwin/home/hl/test_codebase/stdlib"]).

test_clone_detection7() ->
    test_clone_detection(["c:/cygwin/home/hl/test_codebase/yxa"]).

test_clone_detection8() ->
    test_clone_detection(["c:/cygwin/home/hl/test_codebase/dialyzer"]).

test_clone_detection9() ->
    test_clone_detection(["c:/cygwin/home/hl/test_codebase/syntax_tools"]).

run_test_clone_detection() ->
    test_clone_detection1(),
    test_clone_detection2(),
    test_clone_detection3(),
    test_clone_detection4(),
    test_clone_detection5(),
    test_clone_detection6(),
    test_clone_detection7(),
    test_clone_detection8(),
    test_clone_detection9().
    
