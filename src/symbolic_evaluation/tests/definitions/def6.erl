%% @private
-module(def6).
-export([sumList/1, fac/1]).

sumList([]) -> 0;
sumList( [X | Xs] ) -> X + sumList(Xs).

fac(1) -> 1;
fac(X) when X > 1 -> X * fac(X - 1).  

