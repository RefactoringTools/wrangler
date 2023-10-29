%%@hidden
%%@private
-module(keysearch_keyfind).

-export([old_api_module_name/0]).

-compile(export_all).

old_api_module_name() ->
    lists.  

keysearch(Key, N, TupleList) ->
    case lists:keyfind(Key, N, TupleList) of
        Tuple when is_tuple(Tuple)->
            {value, Tuple};
        false ->
            false
    end.
      
