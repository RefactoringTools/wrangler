

-module(tests_all).
-compile([export_all]).

testIfAndCase() ->
    def7:ifAndCase(4).

testFlatten() ->
    def7:listsFlatten([[1,2,3],[4,5,6],[7,8,9]]).

testFunAll() -> def7:all(3,1).

testAll2() -> 
    X = is_atom("atom"),
   ((if
           X -> is_integer(def:triple(1 + 2));
           true -> false
    end) orelse 
    (
      case X of
        true -> is_list(def:fac(3));
        _ -> true
      end
    )
   ) xor 
   (length(testFlatten()) =< 3).





