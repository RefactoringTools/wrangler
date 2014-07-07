-module(tests_funAppANDarit).
-compile([export_all]).

add(X,Y) -> X + Y.

addTupleElems({A,B}) -> A + B. 

%%TESTS

%%addTupleElems({1,2})
testTupleAndApp1() -> addTupleElems({1,2}).

%%addTupleElems({X,X})
testTupleAndApp2(X) -> addTupleElems({X,X}).

%%addTupleElems({X, addTupleElems({3,4})})
testTupleAndApp3(X) -> addTupleElems({X, addTupleElems({3,4})}).

%%add(X,X) * 1 + 0.
testIdentityAndApp(X) ->	       
     add(X,X) * 1 + 0.

%% (X + X - X) + (Z - Z)
testAritAndIdentity(X,Z) -> (X + X - X) + (Z - Z).

testIf(X) ->
    if X > 0 -> 2 * X;
       true -> X
    end.    

testFac5() -> definitions:fac(5).
    


