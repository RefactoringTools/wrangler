-module(definitions2).
-export([g/1]).

g(0) -> 0;
g(N) when N > 0 -> N + g(N-1).
