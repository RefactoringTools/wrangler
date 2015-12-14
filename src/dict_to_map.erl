%%@private
-module(dict_to_map). 

-include("../include/wrangler.hrl"). 

-export([new/0, 
         is_key/2,
         to_list/1,
         from_list/1,
         size/1,
         is_empty/1,
         fetch/2,
         fetch_keys/1,
         erase/2,
         keys/1,
         store/3,
         fold/3,
         map/2]).

-export([update/3]).

-export([old_api_module_name/0]).

old_api_module_name() ->
    dict.

new() ->
    maps:new().

is_key(Key, Dict) ->
    maps:is_key(Key, Dict).

to_list(Dict) ->
    maps:to_list(Dict).

from_list(List) ->
    maps:from_list(List).

size(Dict) ->
    maps:size(Dict).

is_empty(Dict) ->
    maps:size(Dict)=:=0.
 
fetch(Key, Dict) ->
    maps:get(Key, Dict).

fetch_keys(Dict) ->
    maps:keys(Dict).

erase(Key, Dict) ->
    maps:remove(Key, Dict).

keys(Dict) ->
    maps:keys(Dict).

store(Key, Value, Dict) ->
    maps:put(Key, Value, Dict).

fold(Fun, Acc, Dict) ->
    maps:fold(Fun, Acc, Dict).

map(Fun,Dict) ->
    maps:map(Fun, Dict).

update(Key, Fun, Dict1) ->
    maps:update(Key, Fun(maps:get(Key, Dict1)), Dict1).
    
 
%% append(Key, Value, Dict) ->
%%     dict:append(Key, Value, Dict).

%% append_list(Key, ValueList, Dist) ->
%%     dict:append_list(Key, ValueList, Dist).

%% update(Key, Fun, Dict1) ->
%%     dict:update(Key, Fun, Dict1).

%% update(Key, Fun, Initial, Dict)->
%%     dict:update(Key, Fun, Initial, Dict).

%% update_counter(Key, Increment, Dict)->
%%     dict:update_counter(Key, Increment, Dict).

%% filter(Pred, Dict) ->
%%     dict:filter(Pred, Dict).

%% merge(Fun, Dict1, Dict2)->
%%     dict:merge(Fun, Dict1, Dict2).
