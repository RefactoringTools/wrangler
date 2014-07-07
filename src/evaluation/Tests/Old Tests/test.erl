-module(test).
-export([g/1]).

g(X) when X == 0 -> 0;
g(X) when X < 0 -> error;
g(X) when X > 0 andalso not(X == 1) andalso X < 3 -> X + 1 ;
g(X) when (X + X == 3 * X - X andalso X + 1 < 4)-> X + 2;
g(X) when ((X > 0 andalso is_integer(X)) andalso 1 == 1) -> X + 3;
g(X) when is_float(X) -> float;
g(X) -> lastClause.

h([]) -> 0;
h(X) when length(X) > 1 -> 1 ;
h(X) -> last. 


testG1() -> g(-1).

testG2() -> g(2). 

testG3(X) when X > 0 ->
    g(1).

testG4() -> g(3).

testG6() -> g(4.1).

testG7(X) -> g(X).

testLength() ->
    h(1).

bar(N) when N > 0 -> N + bar(N-1);
bar(0) -> 0.
