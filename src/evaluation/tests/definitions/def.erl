-module(def).
-export([testAll2/0,testMap/0,triple/1, fac/1, map/2]).

triple(X) ->
     3 * X.

fac(1) -> 1;
fac(X) when X > 1 -> X * fac(X - 1). 

map(F, [H|T]) ->
    [F(H)|map(F, T)];
map(F, []) when is_function(F, 1) -> [].

testMap() -> map(fun(X) -> 2 * X end,[0,1,2,3]).

testAll2() -> 
    X = is_atom("atom"),
    Y = if
           X -> is_integer(triple(1 + 2));
           true -> false
        end,
    Z = case X of
        true -> is_list(fac(3));
        _ -> true
    end,
   X andalso Y orelse Z. 


