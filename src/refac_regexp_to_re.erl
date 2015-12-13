%%@private
-module(refac_regexp_to_re). 

-include("../include/wrangler.hrl"). 

-export([meta_rule_set/0, simple_rule_set/0, old_apis/0]).

old_apis() ->
    [{regexp, match, 2}, {regexp, first_match, 2}, {regexp, matches, 2},
     {regexp, sub, 3}, {regexp, gsub, 3}, {regexp, split, 2},
     {regexp, parse, 1}].

meta_rule_set() ->
    [match_meta_rule(), match_match_rule(), first_match_meta_rule(),
     first_match_match_rule(), matches_meta_rule(), matches_match_rule(),
     sub_meta_rule(), sub_match_rule(), gsub_meta_rule(), gsub_match_rule(),
     split_meta_rule()].
  
simple_rule_set() ->
    [match_rule(), first_match_rule(), matches_rule(), sub_rule(),
     gsub_rule(), split_rule(), parse_rule()].

match_meta_rule() ->
    ?META_RULE(?T("case regexp:match(String@,RegExp@) of
                      {match,Start@,Len@} when Guard1@@ -> Body1@@;
                      nomatch when Guard2@@ -> Body2@@
                  end"),
               begin
                   Start0 = refac_api_migration:mk_new_var("Start0", _File@),
                   Error = refac_api_migration:mk_new_var("Error", _File@),
                   Match = refac_api_migration:mk_new_var("Match", _File@),
                   api_refac:anti_quote(refac_api_migration:mk_str("try
                                                                       re:run(String@,RegExp@,[global])
                                                                   of
                                                                       {match,~s}
                                                                           when
                                                                           Guard1@@ ->
                                                                           {~s,Len@} = lists:last(lists:ukeysort(2,lists:append(~s))),
                                                                           Start@ = ~s
                                                                                      + 1,
                                                                           Body1@@;
                                                                       nomatch
                                                                           when
                                                                           Guard2@@ ->
                                                                           Body2@@
                                                                   catch
                                                                       error:_ ->
                                                                           {error,~s} = re:compile(RegExp@),
                                                                           {error,~s}
                                                                   end",
                                                                   [Match,
                                                                    Start0,
                                                                    Match,
                                                                    Start0,
                                                                    Error,
                                                                    Error]))
               end,
               api_refac:free_vars(Guard1@@) --
                 (api_refac:bound_vars(Len@) ++ api_refac:bound_vars(Start@))
                 ==
                 api_refac:free_vars(Guard1@@)).

match_match_rule() ->
    ?META_MATCH_RULE(?T("MatchLeftExpr@ = regexp:match(String@,RegExp@)"),
                     begin
                         Start0 = refac_api_migration:mk_new_var("Start0",
                                                                 _File@),
                         Error = refac_api_migration:mk_new_var("Error",
                                                                _File@),
                         Start = refac_api_migration:mk_new_var("Start",
                                                                _File@),
                         Match = refac_api_migration:mk_new_var("Match",
                                                                _File@),
                         Len = refac_api_migration:mk_new_var("Len", _File@),
                         ?TO_AST(refac_api_migration:mk_str("MatchLeftExpr@ = try
                                                                                 re:run(String@,RegExp@,[global])
                                                                             of
                                                                                 {match,~s} ->
                                                                                              {~s,~s} = lists:last(lists:ukeysort(2,lists:append(~s))),
                                                                                              ~s = ~s
                                                                                                     + 1,
                                                                                              {match,~s,~s};
                                                                                 nomatch -> nomatch
                                                                             catch
                                                                                 error:_ ->
                                                                                     {error,~s} = re:compile(RegExp@),
                                                                                     {error,~s}
                                                                             end",
                                                            [Match, Start0, Len,
                                                             Match, Start,
                                                             Start0, Start, Len,
                                                             Error, Error]))
                     end,
                     true).

first_match_meta_rule() ->
    ?META_RULE(?T("case regexp:first_match(String@,RegExp@) of
                      {match,Start@,Len@} when Guard1@@ -> Body1@@;
                      nomatch when Guard2@@ -> Body2@@
                  end"),
               begin
                   Start0 = refac_api_migration:mk_new_var("Start0", _File@),
                   api_refac:anti_quote(refac_api_migration:mk_str("try
                                                                       re:run(String@,RegExp@)
                                                                   of
                                                                       {match,[{~s,Len@}]}
                                                                           when
                                                                           Guard1@@ ->
                                                                           Start@ = ~s
                                                                                      + 1,
                                                                           Body1@@;
                                                                       nomatch
                                                                           when
                                                                           Guard2@@ ->
                                                                           Body2@@
                                                                   catch
                                                                       error:_ ->
                                                                           re:compile(RegExp@)
                                                                   end",
                                                                   [Start0,
                                                                    Start0]))
               end,
               api_refac:free_vars(Guard1@@) --
                 (api_refac:bound_vars(Len@) ++ api_refac:bound_vars(Start@))
                 ==
                 api_refac:free_vars(Guard1@@)).

first_match_match_rule() ->
    ?META_MATCH_RULE(?T("MatchLeftExpr@ = regexp:first_match(String@,RegExp@)"),
                     begin
                         Start0 = refac_api_migration:mk_new_var("Start0",
                                                                 _File@),
                         Start = refac_api_migration:mk_new_var("Start",
                                                                _File@),
                         Len = refac_api_migration:mk_new_var("Len", _File@),
                         ?TO_AST(refac_api_migration:mk_str("MatchLeftExpr@ = try
                                                                                 re:run(String@,RegExp@)
                                                                             of
                                                                                 {match,[{~s,~s}]} ->
                                                                                              ~s = ~s
                                                                                                     + 1,
                                                                                              {match,~s,~s};
                                                                                 nomatch -> nomatch
                                                                             catch
                                                                                 error:_ ->
                                                                                     re:compile(RegExp@)
                                                                             end",
                                                            [Start0, Len, Start,
                                                             Start0, Start,
                                                             Len]))
                     end,
                     true).

matches_meta_rule() ->
    ?META_RULE(?T("case regexp:matches(String@,RegExp@) of
                      {match,Matches@} when Guard1@@ -> Body1@@;
                      {match,[]} when Guard2@@ -> Body2@@
                  end"),
               begin
                   Res = refac_api_migration:mk_new_var("Res", _File@),
                   Start = refac_api_migration:mk_new_var("Start", _File@),
                   Len = refac_api_migration:mk_new_var("Len", _File@),
                   api_refac:anti_quote(refac_api_migration:mk_str("try
                                                                       re:run(String@,RegExp@,[global])
                                                                   of
                                                                       {match,~s}
                                                                           when
                                                                           Guard1@@ ->
                                                                           Matches@ = [{~s
                                                                                          + 1,~s}
                                                                                       || {~s,~s} <- lists:append(~s)],
                                                                           Body1@@;
                                                                       nomatch
                                                                           when
                                                                           Guard2@@ ->
                                                                           Body2@@
                                                                   catch
                                                                       error:_ ->
                                                                           re:compile(RegExp@)
                                                                   end",
                                                                   [Res, Start,
                                                                    Len, Start,
                                                                    Len, Res]))
               end,
               api_refac:free_vars(Guard1@@) -- (api_refac:bound_vars(Matches@))
                 ==
                 api_refac:free_vars(Guard1@@)).

matches_match_rule() ->
    ?META_MATCH_RULE(?T("MatchLeftExpr@ = regexp:matches(String@,RegExp@)"),
                     begin
                         Res = refac_api_migration:mk_new_var("Res", _File@),
                         Matches = refac_api_migration:mk_new_var("Matches",
                                                                  _File@),
                         Start = refac_api_migration:mk_new_var("Start",
                                                                _File@),
                         Len = refac_api_migration:mk_new_var("Len", _File@),
                         ?TO_AST(refac_api_migration:mk_str("MatchLeftExpr@ = try
                                                                                 re:run(String@,RegExp@,[global])
                                                                             of
                                                                                 {match,~s} ->
                                                                                              ~s = [{~s
                                                                                                       + 1,~s}
                                                                                                    || {~s,~s} <- lists:append(~s)],
                                                                                              {match,~s};
                                                                                 nomatch ->
                                                                                              {match,[]}
                                                                             catch
                                                                                 error:_ ->
                                                                                     re:compile(RegExp@)
                                                                             end",
                                                            [Res, Matches,
                                                             Start, Len, Start,
                                                             Len, Res,
                                                             Matches]))
                     end,
                     true).

sub_meta_rule() ->
    ?META_RULE(?T("case regexp:sub(String@,RegExp@,New@) of
                      {ok,NewString@,Count@} when Guard1@@ -> Body1@@
                  end"),
               begin
                   api_refac:anti_quote(refac_api_migration:mk_str("try
                                                                       re:replace(String@,RegExp@,New@,[{return,list}])
                                                                   of
                                                                       NewString@
                                                                           when
                                                                           Guard1@@ ->
                                                                           Count@ = case re:run(String@,RegExp@) of
                                                                                        nomatch -> 0;
                                                                                        _ -> 1
                                                                                    end,
                                                                           Body1@@
                                                                   catch
                                                                       error:_ ->
                                                                           re:compile(RegExp@)
                                                                   end",
                                                                   []))
               end,
               api_refac:free_vars(Guard1@@) --
                 (api_refac:bound_vars(Count@) ++
                    api_refac:bound_vars(NewString@))
                 ==
                 api_refac:free_vars(Guard1@@)).

sub_match_rule() ->
    ?META_MATCH_RULE(?T("MatchLeftExpr@ = regexp:sub(String@,RegExp@,New@)"),
                     begin
                         Count = refac_api_migration:mk_new_var("Count",
                                                                _File@),
                         NewString = refac_api_migration:mk_new_var("NewString",
                                                                    _File@),
                         ?TO_AST(refac_api_migration:mk_str("MatchLeftExpr@ = try
                                                                                 re:replace(String@,RegExp@,New@,[{return,list}])
                                                                             of
                                                                                 ~s ->
                                                                                     ~s = case re:run(String@,RegExp@) of
                                                                                              nomatch -> 0;
                                                                                              _ -> 1
                                                                                          end,
                                                                                     {ok,~s,~s}
                                                                             catch
                                                                                 error:_ ->
                                                                                     re:compile(RegExp@)
                                                                             end",
                                                            [NewString, Count,
                                                             NewString, Count]))
                     end,
                     true).

gsub_meta_rule() ->
    ?META_RULE(?T("case regexp:gsub(String@,RegExp@,New@) of
                      {ok,NewString@,Count@} when Guard1@@ -> Body1@@
                  end"),
               begin
                   Matches = refac_api_migration:mk_new_var("Matches", _File@),
                   api_refac:anti_quote(refac_api_migration:mk_str("try
                                                                       re:replace(String@,RegExp@,New@,[{return,list}])
                                                                   of
                                                                       NewString@
                                                                           when
                                                                           Guard1@@ ->
                                                                           Count@ = case re:run(String@,RegExp@,[global]) of
                                                                                        nomatch -> 0;
                                                                                        {match,~s} ->
                                                                                            length(lists:append(~s))
                                                                                    end,
                                                                           Body1@@
                                                                   catch
                                                                       error:_ ->
                                                                           re:compile(RegExp@)
                                                                   end",
                                                                   [Matches,
                                                                    Matches]))
               end,
               api_refac:free_vars(Guard1@@) --
                 (api_refac:bound_vars(Count@) ++
                    api_refac:bound_vars(NewString@))
                 ==
                 api_refac:free_vars(Guard1@@)).

gsub_match_rule() ->
    ?META_MATCH_RULE(?T("MatchLeftExpr@ = regexp:gsub(String@,RegExp@,New@)"),
                     begin
                         Matches = refac_api_migration:mk_new_var("Matches",
                                                                  _File@),
                         Count = refac_api_migration:mk_new_var("Count",
                                                                _File@),
                         NewString = refac_api_migration:mk_new_var("NewString",
                                                                    _File@),
                         ?TO_AST(refac_api_migration:mk_str("MatchLeftExpr@ = try
                                                                                 re:replace(String@,RegExp@,New@,[{return,list}])
                                                                             of
                                                                                 ~s ->
                                                                                     ~s = case re:run(String@,RegExp@,[global]) of
                                                                                              nomatch -> 0;
                                                                                              {match,~s} ->
                                                                                                  length(lists:append(~s))
                                                                                          end,
                                                                                     {ok,~s,~s}
                                                                             catch
                                                                                 error:_ ->
                                                                                     re:compile(RegExp@)
                                                                             end",
                                                            [NewString, Count,
                                                             Matches, Matches,
                                                             NewString, Count]))
                     end,
                     true).

split_meta_rule() ->
    ?META_RULE(?T("case regexp:split(String@,RegExp@) of
                      {ok,SplitList@} when Guard1@@ -> Body1@@
                  end"),
               begin
                   api_refac:anti_quote(refac_api_migration:mk_str("try
                                                                       re:split(String@,RegExp@,[{return,list}])
                                                                   of
                                                                       SplitList@
                                                                           when
                                                                           Guard1@@ ->
                                                                           Body1@@
                                                                   catch
                                                                       error:_ ->
                                                                           re:compile(RegExp@)
                                                                   end",
                                                                   []))
               end,
               api_refac:free_vars(Guard1@@) --
                 (api_refac:bound_vars(SplitList@))
                 ==
                 api_refac:free_vars(Guard1@@)).

match_rule() ->
    ?RULE(?T("regexp:match(String@,RegExp@)"),
          begin
              Start0 = refac_api_migration:mk_new_var("Start0", _File@),
              Error = refac_api_migration:mk_new_var("Error", _File@),
              Start = refac_api_migration:mk_new_var("Start", _File@),
              Match = refac_api_migration:mk_new_var("Match", _File@),
              Len = refac_api_migration:mk_new_var("Len", _File@),
              ?TO_AST(refac_api_migration:mk_str("try
                                                     re:run(String@,RegExp@,[global])
                                                 of
                                                     {match,~s} ->
                                                                  {~s,~s} = lists:last(lists:ukeysort(2,lists:append(~s))),
                                                                  ~s = ~s + 1,
                                                                  {match,~s,~s};
                                                     nomatch -> nomatch
                                                 catch
                                                     error:_ ->
                                                         {error,~s} = re:compile(RegExp@),
                                                         {error,~s}
                                                 end",
                                                 [Match, Start0, Len, Match,
                                                  Start, Start0, Start, Len,
                                                  Error, Error]))
          end,
          true).

first_match_rule() ->
    ?RULE(?T("regexp:first_match(String@,RegExp@)"),
          begin
              Start0 = refac_api_migration:mk_new_var("Start0", _File@),
              Start = refac_api_migration:mk_new_var("Start", _File@),
              Len = refac_api_migration:mk_new_var("Len", _File@),
              ?TO_AST(refac_api_migration:mk_str("try re:run(String@,RegExp@) of
                                                     {match,[{~s,~s}]} ->
                                                                  ~s = ~s + 1,
                                                                  {match,~s,~s};
                                                     nomatch -> nomatch
                                                 catch
                                                     error:_ ->
                                                         re:compile(RegExp@)
                                                 end",
                                                 [Start0, Len, Start, Start0,
                                                  Start, Len]))
          end,
          true).

matches_rule() ->
    ?RULE(?T("regexp:matches(String@,RegExp@)"),
          begin
              Res = refac_api_migration:mk_new_var("Res", _File@),
              Matches = refac_api_migration:mk_new_var("Matches", _File@),
              Start = refac_api_migration:mk_new_var("Start", _File@),
              Len = refac_api_migration:mk_new_var("Len", _File@),
              ?TO_AST(refac_api_migration:mk_str("try
                                                     re:run(String@,RegExp@,[global])
                                                 of
                                                     {match,~s} ->
                                                                  ~s = [{~s
                                                                           + 1,~s}
                                                                        || {~s,~s} <- lists:append(~s)],
                                                                  {match,~s};
                                                     nomatch ->
                                                                  {match,[]}
                                                 catch
                                                     error:_ ->
                                                         re:compile(RegExp@)
                                                 end",
                                                 [Res, Matches, Start, Len,
                                                  Start, Len, Res, Matches]))
          end,
          true).

sub_rule() ->
    ?RULE(?T("regexp:sub(String@,RegExp@,New@)"),
          begin
              Count = refac_api_migration:mk_new_var("Count", _File@),
              NewString = refac_api_migration:mk_new_var("NewString", _File@),
              ?TO_AST(refac_api_migration:mk_str("try
                                                     re:replace(String@,RegExp@,New@,[{return,list}])
                                                 of
                                                     ~s ->
                                                         ~s = case re:run(String@,RegExp@) of
                                                                  nomatch -> 0;
                                                                  _ -> 1
                                                              end,
                                                         {ok,~s,~s}
                                                 catch
                                                     error:_ ->
                                                         re:compile(RegExp@)
                                                 end",
                                                 [NewString, Count, NewString,
                                                  Count]))
          end,
          true).

gsub_rule() ->
    ?RULE(?T("regexp:gsub(String@,RegExp@,New@)"),
          begin
              Matches = refac_api_migration:mk_new_var("Matches", _File@),
              Count = refac_api_migration:mk_new_var("Count", _File@),
              NewString = refac_api_migration:mk_new_var("NewString", _File@),
              ?TO_AST(refac_api_migration:mk_str("try
                                                     re:replace(String@,RegExp@,New@,[{return,list}])
                                                 of
                                                     ~s ->
                                                         ~s = case re:run(String@,RegExp@,[global]) of
                                                                  nomatch -> 0;
                                                                  {match,~s} ->
                                                                      length(lists:append(~s))
                                                              end,
                                                         {ok,~s,~s}
                                                 catch
                                                     error:_ ->
                                                         re:compile(RegExp@)
                                                 end",
                                                 [NewString, Count, Matches,
                                                  Matches, NewString, Count]))
          end,
          true).

split_rule() ->
    ?RULE(?T("regexp:split(String@,RegExp@)"),
          begin
              SplitList = refac_api_migration:mk_new_var("SplitList", _File@),
              ?TO_AST(refac_api_migration:mk_str("try
                                                     re:split(String@,RegExp@,[{return,list}])
                                                 of
                                                     ~s -> {ok,~s}
                                                 catch
                                                     error:_ ->
                                                         re:compile(RegExp@)
                                                 end",
                                                 [SplitList, SplitList]))
          end,
          true).

parse_rule() ->
    ?RULE(?T("regexp:parse(RegExp@)"), ?TO_AST("re:compile(RegExp@)"),
          true).


