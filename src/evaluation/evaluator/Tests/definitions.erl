-module(definitions).
-export([f/1,fac/1,double_all/1,test_timeout/1,testArgs/2]).

fac(X) when X == 0 ->
    1;
fac(X) when X > 0 -> X * fac(X - 1).

f([]) -> 0;
f( [X | Xs] ) -> X + f(Xs).

double_all([]) -> 0;
double_all( [X | Xs] ) -> (2 * X) + f(Xs).

test_timeout(N) ->
     test_timeout(N+1).

testArgs(X,Y) ->
     X + Y.
