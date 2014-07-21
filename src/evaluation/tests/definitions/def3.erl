-module(def3).
-export([case_refac/1]).

case_refac(X) ->
   case def4:is_even(X) of 
	true -> even;
	_ -> odd
   end.
