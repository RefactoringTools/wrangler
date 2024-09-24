%% @private
-module(def5).
-compile([export_all]).
-compile(nowarn_export_all).

add(X,Y) -> X + Y.

addTupleElems({A,B}) -> A + B.                                                                             
f([]) -> 0;
f( [X | Xs] ) -> X + f(Xs).

callAddTupleElems() -> addTupleElems({1,2}).

callAddTupleElemsNested(X) -> addTupleElems({X, addTupleElems({3,4})}).

callF(X) -> f([X,2,3,4,5]).

callAddTimes1(X) -> add(X,X) * 1 + 0.

doArithmetics(X,Z) -> (X + X - X) + (Z - Z).





















