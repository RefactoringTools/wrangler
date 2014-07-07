-module(test_arithmetics).
-compile([export_all]).

test1() -> 1 + 1;
test1(X) -> (2 * X + X + 0) * 1.

test2(X) -> (1 * X + X) + (2 + 3).



