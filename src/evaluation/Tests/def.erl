-module(def).
-export([triple/1, twice/1, f/1, g/1, h/1, fac/1, externalCall/1, map/2,filter/2]).

triple(X) ->
     3 * X.

twice(0) -> 0;
twice(X) ->
     2 * X.

f([]) -> 0;
f( [X | Xs] ) -> X + f(Xs).

g(X) when X > 0 -> X + 1;
g(0) -> 0. 

h(X) -> g(X).

fac(1) -> 1;
fac(X) when X > 1 -> X * fac(X - 1).  

externalCall(0) -> 0;
externalCall(X) -> X + tests_funApp:externalCall(X - 1).

map(F, [H|T]) ->
    [F(H)|map(F, T)];
map(F, []) when is_function(F, 1) -> [].

-spec filter(Pred, List1) -> List2 when
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().
filter(Pred, List) when is_function(Pred, 1) ->
    [ E || E <- List, Pred(E) ].
