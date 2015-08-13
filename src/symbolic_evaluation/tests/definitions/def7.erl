%% @private
-module(def7).
-compile([export_all]).

listsFlatten([]) -> [];
listsFlatten([H | T]) when is_list(H) ->
    H ++ listsFlatten(T);
listsFlatten(List) when is_list(List) -> List.

caseFun(X) ->
   case X of 
	{even, N} -> 2 * N;
        {odd, N} -> 3 * N;
	Other -> Other
   end.
    
ifAndCase(N) when is_number(N)->
    X = def4:is_even(N),
    Y = if
	X -> {even, N};
	true -> {odd, N} 
    end,
    caseFun(Y).

all(Num,Num2) when is_integer(Num) ->
     case def:twice(def:triple(Num)) of
	N when is_integer(N) ->
	    X = is_integer(Num2),
	    if
		X -> 
		    def:fac(Num + Num2) + N;
		Num > Num2 -> def:twice(N);
		true -> def:triple(N)
	    end;
	_ -> 0
    end.
