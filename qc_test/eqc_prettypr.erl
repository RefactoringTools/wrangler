-module(eqc_prettypr).

-compile(export_all).

get_original_toks(AST) ->
    Fs = refac_syntax:form_list_elements(AST),
    FormToks = [refac_misc:get_toks(F)|| F <- Fs],
    lists:append(FormToks).


group_toks_by_line(Toks) ->
    Toks1 =[case T  of 
		{K, {L,_C}, V} -> {K, L, V};
		{K, {L,_C}} -> {K, L}
	    end || T <-Toks],
    group_toks_by_line_1(Toks1,[]).

group_toks_by_line_1([],Acc) -> lists:reverse(Acc);
group_toks_by_line_1(Toks = [T| _Ts],Acc) ->
    L = element(2,T),
    {Toks1, Toks2} = 
	lists:partition(fun (T1) ->
				element(2,T1) == L
			end,
			Toks),
    group_toks_by_line_1(Toks2,[Toks1|Acc]).
			

prop_prettypr(FileName) ->
    refac_io:format("Check: ~p.\n", [FileName]),
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, []),
    Fs = refac_syntax:form_list_elements(AST),
    lists:foreach(fun(F) ->
                          OrigToks=refac_misc:get_toks(F),
                          {NewStr,_}=refac_prettypr:print_a_form(F, 'unix', [], 8),
                          {ok, NewToks, _} = refac_scan_with_layout:string(NewStr, {1, 1}, 8, 'unix'),
                          OrigLines = group_toks_by_line(OrigToks),
                          NewLines = group_toks_by_line(NewToks),
                          DiffByLine=refac_prettypr:levenshtein_dist(OrigLines, NewLines, 8),
                          output_diff(DiffByLine)
                  end,
                  Fs).

prop_prettypr_1(FileName) ->
    refac_io:format("Check: ~p.\n", [FileName]),
    {ok, {AST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, []),
    Fs = refac_syntax:form_list_elements(AST),
    lists:foreach(fun(F) ->
                          F1 = reset_attrs(F),
                          NewStr=refac_prettypr:format(F1),
                          {ok, NewToks, _} = refac_scan_with_layout:string(NewStr, {1, 1}, 8, 'unix'),
                          %%refac_io:format("Newtoks:\n~p\n", [NewToks]),
                          NewStr1 = erl_prettypr:format(F1),
                          {ok, NewToks1, _} = refac_scan_with_layout:string(NewStr1, {1, 1}, 8, 'unix'),
                          %%refac_io:format("Newtoks1:\n~p\n", [NewToks1]),
                          NewToks11 = refac_prettypr:expand_tab_keys(NewToks1, 8),
                          NewLinesbyWrangler = group_toks_by_line(NewToks),
                          NewLinesbySyntaxTools = group_toks_by_line(NewToks11),
                          DiffByLine=refac_prettypr:levenshtein_dist(
                                       NewLinesbyWrangler, NewLinesbySyntaxTools, 8),
                          output_diff(DiffByLine)
                  end,
                  Fs).
   
output_diff(DiffsByLine) ->
    lists:foreach(fun(D) -> output_diff_1(D) end, DiffsByLine).

output_diff_1({'d', Toks}) ->
    refac_io:format("\n~p\n", [{'d', refac_misc:concat_toks(Toks)}]);
output_diff_1({'i', Toks}) ->
    refac_io:format("\n~p\n", [{'i', refac_misc:concat_toks(Toks)}]);
output_diff_1({'s', OldToks, NewToks}) ->
    refac_io:format("\n~p\n", [{'s', refac_misc:concat_toks(OldToks), refac_misc:concat_toks(NewToks)}]);
output_diff_1(_) -> ok.

%%c("../wrangler-0.9.2/qc_test/eqc_prettypr.erl").
%%eqc_prettypr:test_prettypr1().

reset_attrs(Node) ->
    ast_traverse_api:full_buTP(fun (T, _Others) -> 
				       T1=refac_syntax:set_ann(T, []),
                                       refac_syntax:set_pos(T1, {0,0})
                               end, Node, {}).

    

test_prettypr(Dirs) ->
    AllErlFiles = refac_misc:expand_files(Dirs, ".erl"),
    lists:foreach(fun(F) ->prop_prettypr(F) end, AllErlFiles).
   
test_prettypr0()->
    test_prettypr(["c:/cygwin/home/hl/test_codebase/ibrowse"]).
    
test_prettypr1() ->
    test_prettypr(["c:/cygwin/home/hl/test_codebase/lampera"]).

test_prettypr2() ->
    test_prettypr(["c:/cygwin/home/hl/test_codebase/eunit"]).

test_prettypr3() ->
    test_prettypr(["c:/cygwin/home/hl/test_codebase/refactorerl"]).

test_prettypr4() ->
    test_prettypr(["c:/cygwin/home/hl/test_codebase/suites"]).

test_prettypr5() ->
    test_prettypr(["c:/cygwin/home/hl/test_codebase/wrangler"]).

test_prettypr6() ->
    test_prettypr(["c:/cygwin/home/hl/test_codebase/stdlib"]).

test_prettypr7() ->
    test_prettypr(["c:/cygwin/home/hl/test_codebase/yxa"]).

test_prettypr8() ->
    test_prettypr(["c:/cygwin/home/hl/test_codebase/dialyzer"]).

run_test_prettypr() ->
    test_prettypr1(),
    test_prettypr2(),
    test_prettypr3(),
    test_prettypr4(),
    test_prettypr5(),
    test_prettypr6(),
    test_prettypr7(),
    test_prettypr8().
    
