%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% It implements a double associative map. It is a key-value
%%% storage that can be queried through both the key and the
%%% value.
%%% @end
%%% Created :  3 Jun 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(da_map).

-compile({no_auto_import,[size/1]}).

-export([new/0, has_key/2, find_key/2, has_value/2, find_value/2, has_pair/3,
	 put/3, delete_by_key/2, delete_by_value/2, fold/3, size/1, union/2]).


-record(da_map, {key_map, value_map}).

-export_type([da_map/2]).

-opaque da_map(Key, Value) :: #da_map{key_map::dict:dict(Key, Value),
				      value_map::dict:dict(Value, Key)}.

%%--------------------------------------------------------------------
%% @doc
%% It creates an empty da_map.
%% @end
%%--------------------------------------------------------------------
-spec new() -> da_map(any(), any()).
new() -> #da_map{key_map = dict:new(),
		 value_map = dict:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Returns true if there is an entry with Key as key in the da_map,
%% returns false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec has_key(Key, DaMap :: da_map(Key, any())) -> boolean() when
      Key :: any().
has_key(Key, DaMap) -> dict:is_key(Key, DaMap#da_map.key_map).

%%--------------------------------------------------------------------
%% @doc
%% Returns `{ok, Key}' if there is an entry with Value as value
%% in the da_map, returns `error' otherwise.
%% @end
%%--------------------------------------------------------------------
-spec find_key(Value, DaMap :: da_map(Key, Value)) ->
		       {ok, Key} | error when
      Key :: any(), Value :: any().
find_key(Value, DaMap) -> dict:find(Value, DaMap#da_map.value_map).

%%--------------------------------------------------------------------
%% @doc
%% Returns true if there is an entry with Value as value in the
%% DaMap, returns false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec has_value(Value, DaMap :: da_map(any(), Value)) -> boolean() when
      Value :: any().
has_value(Value, DaMap) -> dict:is_key(Value, DaMap#da_map.value_map).

%%--------------------------------------------------------------------
%% @doc
%% Returns `{ok, Value}' if there is an entry with Key as key in
%% the da_map, returns `error' otherwise.
%% @end
%%--------------------------------------------------------------------
-spec find_value(Key, DaMap :: da_map(Key, Value)) ->
		       {ok, Value} | error when
      Key :: any(), Value :: any().
find_value(Key, DaMap) -> dict:find(Key, DaMap#da_map.key_map).

%%--------------------------------------------------------------------
%% @doc
%% Returns true if there is an entry with Key as key and Value as
%% value in the DaMap, returns false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec has_pair(Key, Value, DaMap :: da_map(Key, Value)) -> boolean() when
      Key :: any(), Value :: any().
has_pair(Key, Value, DaMap) ->
    case dict:find(Key, DaMap#da_map.key_map) of
	{ok, V} when V =:= Value -> true;
	_ -> false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stores an entry with Key as key and Value as value in the DaMap.
%% It removes all existing entries with Key as key or Value as value.
%% @end
%%--------------------------------------------------------------------
-spec put(Key, Value, DaMap :: da_map(Key, Value)) -> da_map(Key, Value) when
      Key :: any(), Value :: any().
put(Key, Value, #da_map{key_map = KM, value_map = VM} = OriDaMap) ->
    TmpDaMap = delete_by_key(Key, OriDaMap),
    DaMap = delete_by_value(Value, TmpDaMap),
    DaMap#da_map{
      key_map = dict:store(Key, Value, KM),
      value_map = dict:store(Value, Key, VM)
     }.

%%--------------------------------------------------------------------
%% @doc
%% Remove the entry with Key as key from the DaMap if it exists.
%% @end
%%--------------------------------------------------------------------
-spec delete_by_key(Key, DaMap :: da_map(Key, Value)) -> da_map(Key, Value) when
      Key :: any(), Value :: any().
delete_by_key(Key, #da_map{key_map = KM, value_map = VM} = OriDaMap) ->
    case dict:find(Key, KM) of
	{ok, Value} -> #da_map{key_map = dict:erase(Key, KM),
			       value_map = dict:erase(Value, VM)};
	error -> OriDaMap
    end.

%%--------------------------------------------------------------------
%% @doc
%% Remove the entry with Value as value from the DaMap if it exists.
%% @end
%%--------------------------------------------------------------------
-spec delete_by_value(Value, DaMap :: da_map(Key, Value)) -> da_map(Key, Value) when
      Key :: any(), Value :: any().
delete_by_value(Value, #da_map{key_map = KM, value_map = VM} = OriDaMap) ->
    case dict:find(Value, VM) of
	{ok, Key} -> #da_map{key_map = dict:erase(Key, KM),
			     value_map = dict:erase(Value, VM)};
	error -> OriDaMap
    end.

%%--------------------------------------------------------------------
%% @doc
%% Folds the function Fun through all the entries of the DaMap.
%% @see dict:fold/3
%% @end
%%--------------------------------------------------------------------
-spec fold(Fun, AccIn :: Acc, DaMap :: da_map(Key, Value)) ->
					    AccOut :: Acc when
      Fun :: fun((Key, Value, Acc) -> Acc),
      Acc :: term(), Key :: any(), Value :: any().
fold(Fun, Acc, DaMap) -> dict:fold(Fun, Acc, DaMap#da_map.key_map).

%%--------------------------------------------------------------------
%% @doc
%% Returns the number of entries of the DaMap.
%% @end
%%--------------------------------------------------------------------
-spec size(DaMap :: da_map(any(), any())) -> non_neg_integer().
size(DaMap) -> dict:size(DaMap#da_map.key_map).

%%--------------------------------------------------------------------
%% @doc
%% Joins DaMap1 and DaMap2 by inserting all the entries of DaMap2 in
%% DaMap1. It assumes DaMap1 and DaMap2 are disjoint in terms of both
%% their keys and their values. It throws da_maps_not_disjoint if
%% DaMap1 and DaMap2 are not disjoint in terms of keys or values.
%% @throws da_maps_not_disjoint
%% @end
%%--------------------------------------------------------------------
-spec union(DaMap1 :: da_map(Key, Value), DaMap2 ::da_map(Key, Value)) ->
		   da_map(Key, Value) when
      Key :: any(), Value :: any().
union(DaMap1, DaMap2) ->
    DestDaMap = fold(fun put/3, DaMap2, DaMap1),
    case {size(DestDaMap), size(DaMap1) + size(DaMap2)} of
	{Expected, Expected} -> DestDaMap;
	_ -> throw({error, "Double-associative maps are not disjoint"})
    end.
