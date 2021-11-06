-module(gen_refacs_2_test_runner).

-export([run_test_correctness/0,
         run_test_comparison/0]).

-include("wrangler.hrl").


run_test_correctness() ->
    % Checking correctness of 1 refactoring (orig <-> refactored)
    % steps of testing:
    % Run refactoring -> file1
    % Compare functions orig <-> file1
    % List differences (where calling with same parameter has different result)
    % Analyze

    {ok, CurrentDirectory} = file:get_cwd(), % tests stored under 'ebin' folder for now
    api_wrangler:start(),

    TestFileOrig0 = "gen_refacs_2_testfiles/gen_refacs_2_testfile_orig_0.erl",
    TestFileExt0 = "gen_refacs_2_testfiles/gen_refacs_2_testfile_ext_0.erl",
    
    % The eunit tests are passing by default
    % they need to stay passed, after the refactoring.
    % Any problem encountered should be highlighted in this process,
    % not only semantic changes, but also compile errors. 

    gen_refac_2_utils:create_backups([TestFileOrig0,TestFileExt0], "_testrefac.txt"),

    api_wrangler:new_dummy_old("gen_refacs_2_testfiles/gen_refacs_2_testfile_orig_0.erl",["gen_refacs_2_testfile_orig_0","orig_fun","1","Y"], CurrentDirectory),

    case compile:file(TestFileOrig0) of 
        {ok, CompiledOrig0} -> 
            ?wrangler_io("\n- Orig file tests:\n", []),
            eunit:test(CompiledOrig0);
        _ ->
            ?wrangler_io("\n- Orig file does not compile after refactoring!\n", [])
    end,
            
    case compile:file(TestFileExt0) of 
        {ok, CompiledExt0} -> 
            ?wrangler_io("\n- Ext file tests:\n", []),
            eunit:test(CompiledExt0);
        _ ->
            ?wrangler_io("\n- Ext file does not compile after refactoring!\n", [])
    end,
    gen_refac_2_utils:restore_backups([TestFileOrig0,TestFileExt0], "_testrefac.txt"),

    api_wrangler:stop().


run_test_comparison() ->
    % Comparing 2 refactorings (old <-> new)
    % steps of testing:
    % Refactor fun with #1 (file1)
    % Refactor fun with #2 (file2)
    % Compare functions of file1 <-> file2
    % List differences
    % Analyze log

    {ok, CurrentDirectory} = file:get_cwd(),
    TestFileOrig1 = "gen_refacs_2_testfiles/gen_refacs_2_testfile_orig_1.erl",
    ModuleOrig1 = gen_refacs_2_testfile_orig_1,
    TestFileExt1 = "gen_refacs_2_testfiles/gen_refacs_2_testfile_ext_1.erl",
    % ModuleExt1 = gen_refacs_2_testfile_ext_1,

    TestFileOrig2 = "gen_refacs_2_testfiles/gen_refacs_2_testfile_orig_2.erl",
    ModuleOrig2 = gen_refacs_2_testfile_orig_2,
    TestFileExt2 = "gen_refacs_2_testfiles/gen_refacs_2_testfile_ext_2.erl",
    % ModuleExt2 = gen_refacs_2_testfile_ext_2,

    gen_refac_2_utils:create_backups([TestFileOrig1,TestFileExt1,TestFileOrig2,TestFileExt2], "_testrefac.txt"),
    
    api_wrangler:start(),
    api_wrangler:new_dummy_new(TestFileOrig1,[atom_to_list(ModuleOrig1),"orig_fun","1","Y"], CurrentDirectory),
    api_wrangler:new_dummy_old(TestFileOrig2,[atom_to_list(ModuleOrig2),"orig_fun","1","Y"], CurrentDirectory),

    case compile:file(TestFileOrig1) of 
        {ok, CompiledOrig1} -> 
            case compile:file(TestFileOrig2) of 
            {ok, _} ->
                ?wrangler_io("\n- Comparison tests:\n", []),
                CompiledOrig1:common_test();
            _ ->
                ?wrangler_io("\n- Orig file does not compile after refactoring!\n", [])
            end;
        _ ->
            ?wrangler_io("\n- Orig file does not compile after refactoring!\n", [])
    end,

    gen_refac_2_utils:restore_backups([TestFileOrig1,TestFileExt1,TestFileOrig2,TestFileExt2], "_testrefac.txt"),

    api_wrangler:stop().

