%% @hidden
%% @private
-module(refac_bug_cond).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../../include/wrangler.hrl").

-spec (input_par_prompts/0::() -> [string()]).
input_par_prompts() -> [].
   
-spec (select_focus/1::(#args{}) -> 
                             {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) -> 
    {ok, none}.

-spec (check_pre_cond/1::(#args{}) -> ok).
check_pre_cond(_Args) ->
    ok.

-spec (selective/0::()-> true). 
selective() ->
    false.

-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}).
transform(_Args=#args{search_paths=SearchPaths})->
    ?FULL_TD_TP([bug_cond_rule_1(),
                bug_cond_rule_2(),
                bug_cond_rule_3(),
                bug_cond_rule_4(),
                bug_cond_rule_5(),
                bug_cond_rule_6(),
                bug_cond_rule_7(),
                bug_cond_rule_8(),
                bug_cond_rule_9(),
                bug_cond_rule_10(),
                bug_cond_rule_11(),
                bug_cond_rule_12(),
                bug_cond_rule_13()
                ],
                SearchPaths).
                  
bug_cond_rule_1() ->                
    ?RULE(?T("case Expr@ of 
                  true ->
                    Body1@@;
                  false ->
                   Body2@@
              end"),
             Body2@@,
             is_bug_cond_macro(Expr@)).

bug_cond_rule_2() ->                
    ?RULE(?T("case Expr@ of 
                  false ->
                    Body1@@;
                 true ->
                    Body2@@
              end"),
             Body1@@,
             is_bug_cond_macro(Expr@)).

bug_cond_rule_3() ->
    ?RULE(?T("[Expr@||Es@@,not Cond@]"),
          if Es@@==[] ->
                  ?TO_AST("[Expr@]");
             true ->?TO_AST("[Expr@||Es@@]")
          end,
          is_bug_cond_macro(Cond@)).

bug_cond_rule_4() ->
    ?RULE(?T("[Expr@||Cond@]"),
          ?TO_AST("[]"),
          is_bug_cond_macro(Cond@)).

bug_cond_rule_5() ->
    ?RULE(?T("if Cond@,Conds@@ -> Body1@@;
                 true -> Body2@@
              end"), Body2@@, is_bug_cond_macro(Cond@)).

bug_cond_rule_6()->
    ?RULE(?T("Expr1@ orelse Expr2@"),
          Expr2@, is_bug_cond_macro(Expr1@)).

bug_cond_rule_7()->
    ?RULE(?T("Expr1@ orelse Expr2@"),
          Expr1@, is_bug_cond_macro(Expr2@)).

bug_cond_rule_8()->
    ?RULE(?T("not Expr1@ orelse Expr2@"),
          ?TO_AST("true"), is_bug_cond_macro(Expr1@)).

bug_cond_rule_9()->
    ?RULE(?T("Expr1@ orelse not Expr2@"),
          ?TO_AST("true"), is_bug_cond_macro(Expr2@)).

bug_cond_rule_10()->
    ?RULE(?T("Expr1@ andalso Expr2@"),
          ?TO_AST("false"), 
          is_bug_cond_macro(Expr1@) orelse 
          is_bug_cond_macro(Expr2@)).

bug_cond_rule_11()->
    ?RULE(?T("f@(Args@@) when not Cond@ -> Body@@;"),
          ?TO_AST("f@(Args@@) -> Body@@;"), is_bug_cond_macro(Cond@)).

bug_cond_rule_12()->
    ?RULE(?T("?IMPL(Cond@, Expr@)"), 
          ?TO_AST("true"),
          is_bug_cond_macro(Cond@)).

bug_cond_rule_13() ->
    ?RULE(?T("case Expr@ of 
                 Pats@@@ when Guard@@@ -> Body@@@;
                 Pats@@ when Cond@ -> Body@@;
                 Pats1@@@ when Guard1@@@ -> Body1@@@
             end"),
            ?TO_AST(
               "case Expr@ of 
                 Pats@@@ when Guard@@@ -> Body@@@;
                 Pats1@@@ when Guard1@@@ -> Body1@@@
             end"),
           is_bug_cond_macro(Cond@)).

is_bug_cond_macro(Expr) ->
    api_refac:type(Expr) == macro andalso 
        wrangler_syntax:macro_arguments(Expr) == none andalso
        is_bug_cond_name(?PP(Expr)).

is_bug_cond_name(Str) ->
   re:run(Str, "[a-z]+_bug_[0-9]+")/=nomatch.
