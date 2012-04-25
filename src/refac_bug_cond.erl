%% @hidden
%% @private
-module(refac_bug_cond).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1, refac_bug_cond/3]).

-include("../include/wrangler.hrl").

-spec (input_par_prompts/0::() -> [string()]).
input_par_prompts() -> [].
   
-spec (select_focus/1::(#args{}) -> 
                             {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) -> 
    {ok, none}.

-spec (check_pre_cond/1::(#args{}) -> ok).
check_pre_cond(_Args) ->
    ok.

-spec (selective/0::()-> boolean()). 
selective() ->
    false.

-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}).
transform(_Args=#args{current_file_name=File})->
    ?FULL_BU_TP(rules(),[File]).

rules() ->
    [replace_bug_cond_macro_rule(),
     logic_rule_1(),
     logic_rule_2(),
     logic_rule_3(),
     list_rule_1(),
     list_rule_2(),
     list_rule_3(),
     list_rule_4(),
     imply_rule_1(),
     if_rule_1(),
     case_rule_1(),
     case_rule_2(),
     case_rule_3(),
     guard_rule_1(),
     guard_rule_2()
    ].
    
replace_bug_cond_macro_rule() ->
    ?RULE(?T("Expr@"),
          ?TO_AST("false"),
          is_bug_cond_macro(Expr@)).


logic_rule_1() ->
    ?RULE(?T("not false"),?TO_AST("true"),true).

logic_rule_2() ->
    ?RULE(?T("Expr1@ orelse Expr2@"),
          eval_expr('orelse', Expr1@, Expr2@),
          is_bool_literal(Expr1@) orelse is_bool_literal(Expr2@)).

logic_rule_3() ->
    ?RULE(?T("Expr1@ andalso Expr2@"),
          eval_expr('andalso', Expr1@, Expr2@),
          is_bool_literal(Expr1@) orelse is_bool_literal(Expr2@)).
         
list_rule_1() ->
    ?RULE(?T("[Expr@||Es@@,true]"),
          if Es@@==[] ->
                  ?TO_AST("[Expr@]");
             true ->?TO_AST("[Expr@||Es@@]")
          end,
          true).

list_rule_2() ->
    ?RULE(?T("[Expr@||Es@@,false]"),
          ?TO_AST("[]"), true).

list_rule_3() ->
    ?RULE(?T("Expr@++[]"), Expr@, true).

list_rule_4() ->
    ?RULE(?T("[]++Expr@"), Expr@, true).

        
if_rule_1() ->
    ?RULE(?T("if false,Conds@@ -> Body1@@;
                 true -> Body2@@
              end"), Body2@@, true).

if_rule_2() ->
   ?RULE(?T("if Pats1@@@ -> Body1@@@;
                false, Cond@@ -> Body2@@;
                Pats3@@@  -> Body3@@@
             end"),
         ?TO_AST("if Pats1@@@ -> Body1@@@;
                     Pats3@@@ -> Body3@@@
                  end"),
         true).

guard_rule_1()->
    ?RULE(?T("f@(Args@@) when true, Guards@@ -> Body@@;"),
          ?TO_AST("f@(Args@@) when Guards@@ -> Body@@;"), true).

guard_rule_2()->
    ?RULE(?T("f@(Args@@) when false, Guards@@ -> Body@@;"),
          ?TO_AST(""), true).

imply_rule_1() ->
    ?RULE(?T("?IMPL(false, Expr@)"), 
          ?TO_AST("true"), true).

case_rule_1() ->
    ?RULE(?T("case true of 
                 Pats1@@@ when Guards1@@@ ->
                   Body1@@@;
                 true ->
                    Body1@@;
                 Pats2@@@ when Guards2@@@ ->
                   Body2@@@
              end"),
          Body1@@, true).

case_rule_2() ->
    ?RULE(?T("case false of 
                 Pats1@@@ when Guards1@@@ ->
                   Body1@@@;
                 false ->
                    Body1@@;
                 Pats2@@@ when Guards2@@@ ->
                   Body2@@@
              end"),
          Body1@@, true).

case_rule_3() ->
    ?RULE(?T("case Expr@ of 
                 Pats@@@ when Guard@@@ -> Body@@@;
                 Pats@@ when false -> Body@@;
                 Pats1@@@ when Guard1@@@ -> Body1@@@
             end"),
            ?TO_AST(
               "case Expr@ of 
                 Pats@@@ when Guard@@@ -> Body@@@;
                 Pats1@@@ when Guard1@@@ -> Body1@@@
             end"),
          true).
                   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_bug_cond_macro(Expr) ->
    api_refac:type(Expr) == macro andalso 
        is_bug_cond_name(?PP(wrangler_misc:reset_attrs(Expr))).
    

is_bug_cond_name(Str) ->
    Len = length(Str), 
    {match, [{0, Len}]} ==re:run(Str, "\\?[a-z]+_bug_[0-9]+").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_bool_literal(E) ->
    %% only use ?PP when E is a small literal.
    %% alternatively, you can write Str=wrangler_syntax:atom_value(E)
    Str = ?PP(E), 
    Str == "true" orelse Str=="false".
 
eval_expr(Op, E1, E2) ->
    eval_expr_1(Op, {E1, ?PP(E1)}, {E2, ?PP(E2)}).
   
eval_expr_1('orelse', {E1, "true"}, _) -> E1;
eval_expr_1('orelse', {_, "false"}, {E2, _}) -> E2;
eval_expr_1('orelse', _, {E2, "true"}) -> E2;
eval_expr_1('orelse', {E1, _}, {_, "false"}) -> E1;
eval_expr_1('andalso', {_, "true"}, {E2, _}) -> E2;
eval_expr_1('andalso', {E1, "false"}, _) -> E1;
eval_expr_1('andalso', {E1,_}, {_, "true"}) -> E1;
eval_expr_1('andalso', _, {E2, "false"}) -> E2.


-spec refac_bug_cond/3::([filename()|dir()], editor(), integer())->{ok, string()}.
refac_bug_cond(FileOrDirs, Editor, TabWidth) ->
    Files = wrangler_misc:expand_files(FileOrDirs, ".erl"),
    {ok, Res}=?FULL_BU_TP(rules(), Files),
    wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"").
