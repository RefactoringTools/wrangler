-module(refac_dict_to_map). 

-include_lib("wrangler/include/wrangler.hrl"). 

-export([meta_rule_set/0, simple_rule_set/0, old_apis/0]).

old_apis() ->
    [{dict, new, 0}, {dict, is_key, 2}, {dict, to_list, 1},
     {dict, from_list, 1}, {dict, size, 1}, {dict, is_empty, 1},
     {dict, fetch, 2}, {dict, fetch_keys, 1}, {dict, erase, 2},
     {dict, keys, 1}, {dict, store, 3}, {dict, fold, 3}, {dict, map, 2},
     {dict, update, 3}].

meta_rule_set() -> [].

simple_rule_set() ->
    [new_rule(), is_key_rule(), to_list_rule(), from_list_rule(),
     size_rule(), is_empty_rule(), fetch_rule(), fetch_keys_rule(),
     erase_rule(), keys_rule(), store_rule(), fold_rule(), map_rule(),
     update_rule()].

new_rule() -> ?RULE(?T("dict:new()"), ?TO_AST("maps:new()"), true).

is_key_rule() ->
    ?RULE(?T("dict:is_key(Key@,Dict@)"), ?TO_AST("maps:is_key(Key@,Dict@)"),
          true).

to_list_rule() ->
    ?RULE(?T("dict:to_list(Dict@)"), ?TO_AST("maps:to_list(Dict@)"), true).

from_list_rule() ->
    ?RULE(?T("dict:from_list(List@)"), ?TO_AST("maps:from_list(List@)"),
          true).

size_rule() ->
    ?RULE(?T("dict:size(Dict@)"), ?TO_AST("maps:size(Dict@)"), true).

is_empty_rule() ->
    ?RULE(?T("dict:is_empty(Dict@)"), ?TO_AST("maps:size(Dict@) =:= 0"),
          true).

fetch_rule() ->
    ?RULE(?T("dict:fetch(Key@,Dict@)"), ?TO_AST("maps:get(Key@,Dict@)"),
          true).

fetch_keys_rule() ->
    ?RULE(?T("dict:fetch_keys(Dict@)"), ?TO_AST("maps:keys(Dict@)"), true).

erase_rule() ->
    ?RULE(?T("dict:erase(Key@,Dict@)"), ?TO_AST("maps:remove(Key@,Dict@)"),
          true).

keys_rule() ->
    ?RULE(?T("dict:keys(Dict@)"), ?TO_AST("maps:keys(Dict@)"), true).

store_rule() ->
    ?RULE(?T("dict:store(Key@,Value@,Dict@)"),
          ?TO_AST("maps:put(Key@,Value@,Dict@)"), true).

fold_rule() ->
    ?RULE(?T("dict:fold(Fun@,Acc@,Dict@)"),
          ?TO_AST("maps:fold(Fun@,Acc@,Dict@)"), true).

map_rule() ->
    ?RULE(?T("dict:map(Fun@,Dict@)"), ?TO_AST("maps:map(Fun@,Dict@)"),
          true).

update_rule() ->
    ?RULE(?T("dict:update(Key@,Fun@,Dict1@)"),
          ?TO_AST("maps:update(Key@,Fun@(maps:get(Key@,Dict1@)),Dict1@)"),
          true).