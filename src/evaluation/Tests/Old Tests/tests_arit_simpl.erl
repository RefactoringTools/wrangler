-module(tests_arit_simpl).
-compile([export_all]).

% X + 0
testPlusZero() -> X + 0.

%% (X + Y) * 1
testExpTimes1(X,Y) -> (X + Y) * 1.
    
%% (X + Y + 1) * 1
testExpTimes1_2(X,Y) -> (X + Y + 1) * 1.

%% Z = X + 1, Z + 0
testPlus0(X,Y) -> Z = X + 1, Z + 0.

%%Z = X - 0,Z
testMinus0(X) -> Z = X - 0,Z.

%% X + X * 0 + 0 * X.
testTimes0AndPlus0(X) -> X + X * 0 + 0 * X.

newTest(X,Y,Z) -> ((X + 2.1 + Y - Z + 1) + 0).







