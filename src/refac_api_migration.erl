%% @hidden
%% @private
-module(refac_api_migration).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

%% -include_lib("wrangler/include/wrangler.hrl").

-include("../include/wrangler.hrl").

input_par_prompts() ->
    [].
  
select_focus(_Args) ->
    {ok, none}.

check_pre_cond(_Args) ->
    ok.
         
selective() ->
    false.
 

transform(#args{current_file_name=File, search_paths=_SearchPaths}) ->
          ?FULL_TD_TP([rule_case(),
                       rule_match()],[File]).
                     

rule_case()->
     ?META_RULE(?T("case regexp:match(String@, RegExp@) of 
                {match, Start@, Len@} when Guard1@@ ->
                   Body1@@;
                nomatch when Guard2@@->
                   Body2@@
               end"), 
                begin
                    Match=mk_newvar("Match", _File@, _This@),
                    Start0=mk_newvar("Start0", _File@,  _This@),
                    api_refac:anti_quote(
                      mk_str("case re:run(String@, RegExp@, [global]) of
                                                   {match, ~s} when Guard1@@->
                                                         {~s, Len@} = lists:last(
                                                            lists:ukeysort(
                                                                 2, lists:append(~s))),
                                                         Start@ = ~s + 1,
                                                         Body1@@;
                                                   nomatch when Guard2@@->
                                                      Body2@@
                                                end", [Match, Start0, Match, Start0]))
                end,
                true).

rule_match()->
     ?RULE(?T("Pat@=regexp:match(String@, RegExp@)"),
           begin
               Match=mk_newvar("Match", _File@, _This@),
               Start0=mk_newvar("Start0", _File@,  _This@),
               Len=mk_newvar("Len", _File@,  _This@),
               ?TO_AST(
                  mk_str("Pat@=case re:run(String@, RegExp@, [global]) of
                                                   {match, ~s}->
                                                     {~s, ~s} = lists:last(
                                                         lists:ukeysort(
                                                                 2, lists:append(~s))),
                                                     {match, ~s+1, ~s};
                                                   nomatch->
                                                      nomatch
                                                end", [Match, Start0, Len,Match, Start0, Len]))
           end,
           true).


mk_str(Str, Pars) ->
    lists:flatten(io_lib:format(Str, Pars)).

mk_newvar(BaseName, _File, _Node)->
    BaseName.
 
 
%%Things to do:
%% 1). infer transformation rules from the api change module;
%%     each rule for a different application senario.
