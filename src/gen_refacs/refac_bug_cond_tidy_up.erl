%% @hidden
%% @private
-module(refac_bug_cond_tidy_up).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../../include/wrangler.hrl").

-spec (input_par_prompts/0::() -> [string()]).
input_par_prompts() -> [].
   
-spec (select_focus/1::(#args{}) ->{ok, syntaxTree()}|{ok, none}).  
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
    ?FULL_TD_TP([tidy_up_rule_1(),
                 tidy_up_rule_2(),
                 tidy_up_rule_3(),
                 tidy_up_rule_4(),
                 tidy_up_rule_5(),
                 tidy_up_rule_6(),
                 tidy_up_rule_7(),
                 tidy_up_rule_8(),
                 tidy_up_rule_9()
                ],
                SearchPaths).
                  
tidy_up_rule_1() -> 
    ?RULE(?T("Expr@ andalso true"),
          Expr@, true).

tidy_up_rule_2() -> 
    ?RULE(?T("true andalso Expr@"),
          Expr@, true).

tidy_up_rule_3() -> 
    ?RULE(?T("case true of 
                 Pats1@@@ when Guards1@@@ ->
                   Body1@@@;
                 true ->
                    Body1@@;
                 Pats2@@@ when Guards2@@@ ->
                   Body2@@@
              end"),
          Body1@@, true).

tidy_up_rule_4() -> 
    ?RULE(?T("case false of 
                 Pats1@@@ when Guards1@@@ ->
                   Body1@@@;
                 false ->
                    Body1@@;
                 Pats2@@@ when Guards2@@@ ->
                   Body2@@@
              end"),
          Body1@@, true).

tidy_up_rule_5() ->
    ?RULE(?T("Expr@++[]"), Expr@, true).

tidy_up_rule_6() ->
    ?RULE(?T("[]++Expr@"), Expr@, true).

tidy_up_rule_7()->
    ?RULE(?T("Body@@, V@=Expr@, V@"), 
             Body@@++[Expr@],
          [_]=api_refac:var_refs(V@) andalso
          api_refac:type(V@) == variable).

tidy_up_rule_8() ->
    ?RULE(?T("not Expr@"),
          ?TO_AST("true"),
          is_bug_cond_macro(Expr@)).

tidy_up_rule_9() ->
    ?RULE(?T("Expr@"),
          ?TO_AST("false"),
          is_bug_cond_macro(Expr@)).

is_bug_cond_macro(Expr) ->
    api_refac:type(Expr) == macro andalso 
        wrangler_syntax:macro_arguments(Expr) == none andalso
        is_bug_cond_name(?PP(Expr)).

is_bug_cond_name(Str) ->
   re:run(Str, "[a-z]+_bug_[0-9]+")/=nomatch.

