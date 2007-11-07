%% -*- erlang-indent-level: 2 -*-
%% =====================================================================
%% Basic representation of Erlang types.
%%
%% Copyright (C) 2000-2003 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% Original author: Richard Carlsson. Rewritten in whole based on the 
%%                                    original implementation 
%%                                    by Tobias Lindahl
%%
%% Author contact: tobiasl@it.uu.se, richardc@it.uu.se
%%
%% $Id: refac_erl_types.erl,v 1.1.1.1 2007-11-07 21:56:10 hl Exp $
%%
%% Modified: 17 Jan 2007 by  Huiqing Li <hl@kent.ac.uk>

-module(refac_erl_types).

-export([
	 t_any/0,
	 t_atom/0,
	 t_atom/1,
	 %% added by HL;begin.
	 t_atom/2,
         t_module_atom/0,
	 t_module_atom/1,
	 t_module_atom/2,
	 t_function_atom/0,
	 t_function_atom/1,
	 t_function_atom/2,
	 t_process_atom/0,
	 t_process_atom/1,
	 t_process_atom/2,
	 t_is_module_atom/1,
	 t_is_function_atom/1,
	 t_is_process_atom/1,
	 %% added by HL; end.
	 t_atom_vals/1,
	 t_binary/0,
	 t_bool/0,
	 t_byte/0,
	 t_char/0,
	 t_components/1,
	 t_cons/0,
	 t_cons/2,
	 t_cons_hd/1,
	 t_cons_tl/1,
	 t_constant/0,
	 t_float/0,
	 t_from_range/2,
	 t_from_term/1,
	 t_fun/0,
	 t_fun/1,
	 t_fun/2,
	 t_fun_args/1,
	 t_fun_arity/1,
	 t_fun_range/1,
	 t_has_var/1,
	 t_identifier/0,
	 t_pos_improper_list/0,
	 t_inf/2,
	 t_inf_lists/2,
	 t_integer/0,
	 t_integer/1,
	 t_integers/1,
	 t_is_any/1,
	 t_is_atom/1,
	 t_is_atom/2,
	 t_is_binary/1,
	 t_is_bool/1,
	 t_is_byte/1,
	 t_is_char/1,
	 t_is_cons/1,
	 t_is_constant/1,
	 t_is_equal/2,
	 t_is_float/1,
	 t_is_fun/1,
	 t_is_pos_improper_list/1,
	 t_is_integer/1,
	 t_is_list/1,
	 t_is_nil/1,
	 t_is_none/1,
	 t_is_number/1,	 
	 t_is_pid/1,
	 t_is_port/1,
	 t_is_ref/1,
	 t_is_subtype/2,
	 t_is_tuple/1,
	 t_is_var/1,
	 t_limit/2,
	 t_list/0,
	 t_list/1,
	 t_list_elements/1,
	 t_nil/0,	 
	 t_none/0,
	 t_nonempty_list/0,
	 t_nonempty_list/1,
	 t_number/0,
	 t_number/1,
	 t_number_vals/1,
	 t_pid/0,
	 t_port/0,
	 t_product/1,
	 t_ref/0,
	 t_string/0,
	 t_subst/2,
	 t_subtract/2,
	 t_sup/1,
	 t_sup/2,
	 t_to_string/1,
	 t_to_string/2,
	 t_tuple/0,
	 t_tuple/1,
	 t_tuple_args/1,
	 t_tuple_arities/1,
	 t_tuple_arity/1,
	 t_tuple_subtypes/1,
	 t_unify/2,
	 t_var/1,
	 t_var_name/1
	]).

%-define(ENABLE_TEST, true).

-ifdef(ENABLE_TEST).
-export([test/0]).
-else.
-define(NO_UNUSED, true).
-endif.


-ifndef(NO_UNUSED).
-export([t_is_identifier/1]).
-endif.

%%============================================================================
%% 
%% Definition of the type structure
%%
%%============================================================================

%%-----------------------------------------------------------------------------
%% Limits
%%

-define(SET_LIMIT, 7).
-define(DICT_LIMIT, 5).
-define(MAX_BYTE, 255).
-define(MAX_CHAR, 16#10ffff).

%%-----------------------------------------------------------------------------
%% Tags
%%

-define(atom_tag,       atom).
-define(binary_tag,     binary).
-define(byte_tag,       byte).
-define(char_tag,       char).
-define(float_tag,      float).
-define(function_tag,   function).
-define(identifier_tag, identifier).
-define(int_tag,        int).
-define(list_tag,       list).
-define(nil_tag,        nil).
-define(nonempty_tag,   nonempty).
-define(number_tag,     number).
-define(pid_tag,        pid).
-define(port_tag,       port).
-define(product_tag,    product).
-define(ref_tag,        ref).
-define(tuple_set_tag,  tuple_set).
-define(tuple_tag,      tuple).
-define(record_tag,     record).
-define(union_tag,      union).
-define(var_tag,        var).

%% Qualifiers for atoms added by HL. Begin.
-define(module_atom, module_atom).
-define(function_atom,  function_atom).
-define(process_atom, process_atom).
-define(normal_atom, normal_atom).
%% Qualifiers for atoms added by HL. End.

%%-----------------------------------------------------------------------------
%% Primitive types
%%

-define(any, any).
-define(none, none).
-define(empty, []).

-record(c, {tag, elements=[], qualifier=?any, ann=?empty}). %% Generic constructor.
-record(int_set, {set}).
-record(int_range, {from, to}).


%%-define(atom(Set),                 #c{tag=?atom_tag, elements=Set, qualifier=?normal_atom}). %% Modified by HL
%%-define(atom(Set, Category),       #c{tag=?atom_tag, elements=Set, qualifier=Category}). %% Modified by HL
-define(atom(Set, Category, Ann),    #c{tag=?atom_tag, elements=Set, qualifier=Category, ann=Ann}). %% Modified by HL

-define(binary,                    #c{tag=?binary_tag}).
-define(float,                     ?number(?any,  ?float_tag)).
-define(function(Domain,Range),    #c{tag=?function_tag, 
				      elements=[Domain, Range]}).
-define(identifier(Types),         #c{tag=?identifier_tag, elements=Types}).
-define(integer(Types),            ?number(Types, ?int_tag)).
-define(int_range(From, To),       ?integer(#int_range{from=From, to=To})).
-define(int_set(Set),              ?integer(#int_set{set=Set})).
-define(list(Types, Term, Size),   #c{tag=?list_tag, elements=[Types,Term],
				      qualifier=Size}).
-define(nil,                       #c{tag=?nil_tag}).
-define(nonempty_list(Types, Term),?list(Types, Term, ?nonempty_tag)).
-define(number(Set, Tag),          #c{tag=?number_tag, elements=Set, 
				      qualifier=Tag}.
-define(product(Types),            #c{tag=?product_tag, elements=Types}).
-define(tuple(Types, Arity, Tag),  #c{tag=?tuple_tag, elements=Types, 
				      qualifier={Arity, Tag}}).
-define(tuple_set(Tuples),         #c{tag=?tuple_set_tag, elements=Tuples}).
-define(var(Id),                   #c{tag=?var_tag, elements=Id}.


-define(byte,                      ?int_range(0, ?MAX_BYTE)).
-define(char,                      ?int_range(0, ?MAX_CHAR)).


%%-----------------------------------------------------------------------------
%% Unions
%%

-define(UNION_SIZE, 5).
-define(union(List),        #c{tag=?union_tag, elements=List}).

-define(atom_union(T),        ?union([T,?none,?none,?none,?none,?none,?none])).
-define(binary_union(T),      ?union([?none,T,?none,?none,?none,?none,?none])).
-define(function_union(T),    ?union([?none,?none,T,?none,?none,?none,?none])).
-define(identifier_union(T),  ?union([?none,?none,?none,T,?none,?none,?none])).
-define(list_union(T),        ?union([?none,?none,?none,?none,T,?none,?none])).
-define(number_union(T),      ?union([?none,?none,?none,?none,?none,T,?none])).
-define(tuple_union(T),       ?union([?none,?none,?none,?none,?none,?none,T])).


-define(integer_union(T),     ?number_union(T)).
-define(float_union(T),       ?number_union(T)).
-define(nil_union(T),         ?list_union(T)).


%%============================================================================
%% 
%% Primitive operations such as type construction and type tests.
%%
%%============================================================================

%%-----------------------------------------------------------------------------
%% Top and bottom
%%

t_any() ->
  ?any.

t_is_any(?any) -> true;
t_is_any(_) -> false.

t_none() ->
  ?none.

t_is_none(?none) -> true;
t_is_none(_) -> false.


%%-----------------------------------------------------------------------------
%% Atoms and the derived type bool.
%%
t_atom() ->
  ?atom(?any,?normal_atom, ?empty).
t_module_atom() ->
    ?atom(?any, ?module_atom,?empty).
t_function_atom() ->
    ?atom(?any, ?function_atom,?empty).
t_process_atom() ->
    ?atom(?any, ?process_atom,?empty).

t_atom(A) when is_atom(A) ->
  ?atom(set_singleton(A), ?normal_atom,?empty).
t_module_atom(A) when is_atom(A) ->
    ?atom(set_singleton(A), ?module_atom,?empty).
t_function_atom(A) when is_atom(A) -> 
    ?atom(set_singleton(A), ?function_atom, ?empty).
t_process_atom(A) when is_atom(A) ->
    ?atom(set_singleton(A), ?process_atom, ?empty).

t_atom(A, Ann) when is_atom(A) ->
  ?atom(set_singleton(A), ?normal_atom,Ann).
t_module_atom(A, Ann) when is_atom(A) ->
    ?atom(set_singleton(A), ?module_atom,Ann).
t_function_atom(A, Ann) when is_atom(A) -> 
    ?atom(set_singleton(A), ?function_atom, Ann).
t_process_atom(A, Ann) when is_atom(A) ->
    ?atom(set_singleton(A), ?process_atom, Ann).


t_atom_vals(?atom(Set,_, _)) ->
  set_to_list(Set);
t_atom_vals(Other) ->
  case t_inf(t_atom(), Other) of
    ?atom(Set,_, _) -> set_to_list(Set);
    ?none -> ?none
  end.
      

t_is_atom(?atom(_,_,_)) -> true;
t_is_atom(_) -> false.

t_is_atom(Atom, ?atom(Set,_,_)) when is_atom(Atom) ->
  (set_size(Set) =:= 1) andalso (set_is_element(Atom, Set));
t_is_atom(Atom, _) when is_atom(Atom) ->
  false.

t_is_module_atom(?atom(_, ?module_atom, Ann)) ->
			Ann /= [];
t_is_module_atom(_) -> false.

t_is_function_atom(?atom(_, ?function_atom, Ann))->
    Ann /=[];
t_is_function_atom(_) -> false.

t_is_process_atom(?atom(_, ?process_atom, Ann)) ->
    Ann /=[];
t_is_process_atom(_) -> false.
     
%%------------------------------------

t_bool() ->
  ?atom(set_from_list([false, true]),?normal_atom, ?empty).

t_is_bool(?atom(?any,_, _)) -> false;
t_is_bool(?atom(Set,_, _)) ->  %%  or t_is_bool(?atom(Set, ?normal_atom)???
  case set_size(Set) of
    1 -> set_is_element(true, Set) orelse set_is_element(false, Set);
    2 -> set_is_element(true, Set) andalso set_is_element(false, Set);
    N when N > 2 -> false
  end;
t_is_bool(_) -> false.

%%-----------------------------------------------------------------------------
%% Binaries
%%

t_binary() ->
  ?binary.

t_is_binary(?binary) -> true;
t_is_binary(_) -> false.

%%-----------------------------------------------------------------------------
%% Functions
%%

t_fun() ->
  ?function(?any, ?any).

t_fun(Range) ->
  ?function(?any, Range).

t_fun(Domain, Range) when is_list(Domain) ->
  ?function(?product(Domain), Range);
t_fun(Arity, Range) when is_integer(Arity) ->
  ?function(?product(duplicate(Arity, ?any)), Range).

t_fun_args(?function(?product(Domain), _)) -> 
  Domain;
t_fun_args(?function(?any, _)) ->
  ?any.

t_fun_arity(?function(?any, _)) ->
  ?any;
t_fun_arity(?function(Domain, _)) ->
  length(Domain).

t_fun_range(?function(_, Range)) ->
  Range.

t_is_fun(?function(_, _)) -> true;
t_is_fun(_) -> false.

%%-----------------------------------------------------------------------------
%% Identifiers. Includes ports, pids and refs.
%% 

t_identifier() ->
  ?identifier(?any).

-ifdef(ENABLE_TEST).
t_is_identifier(?identifier(_)) -> true;
t_is_identifier(_) -> false.
-endif.

%%------------------------------------

t_port() ->
  ?identifier(set_singleton(?port_tag)).

t_is_port(?identifier(Set)) -> 
  (set_size(Set) =:= 1) andalso set_is_element(?port_tag, Set);
t_is_port(_) -> false.

%%------------------------------------

t_pid() ->
  ?identifier(set_singleton(?pid_tag)).

t_is_pid(?identifier(Set)) -> 
  (set_size(Set) =:= 1) andalso set_is_element(?pid_tag, Set);
t_is_pid(_) -> false.

%%------------------------------------

t_ref() ->
  ?identifier(set_singleton(?ref_tag)).

t_is_ref(?identifier(Set)) -> 
  (set_size(Set) =:= 1) andalso set_is_element(?ref_tag, Set);
t_is_ref(_) -> false.

%%-----------------------------------------------------------------------------
%% Numbers are divided into floats, integers, chars and byts.
%%

t_number() ->
  ?number(?any, ?number_tag).

t_number(X) when is_integer(X) ->
  t_integer(X).

t_is_number(?number(_, _)) -> true;
t_is_number(_) -> false.

t_number_vals(?int_set(Set)) -> set_to_list(Set);
t_number_vals(?number(_, _)) -> ?any;
t_number_vals(Other) ->
  Inf = t_inf(Other, t_number()),
  case t_is_none(Inf) of
    true -> [];
    false -> t_number_vals(Inf)
  end.      

%%------------------------------------

t_float() ->
  ?float.

t_is_float(?float) -> true;
t_is_float(_) -> false.

%%------------------------------------

t_integer() ->
  ?integer(?any).

t_integer(I) when is_integer(I) ->
  ?int_set(set_singleton(I)).

t_integers(List) when is_list(List) ->
  t_sup([t_integer(I) || I <- List]).

t_is_integer(?integer(_)) -> true;
t_is_integer(_) -> false.

%%------------------------------------

t_byte() ->
  ?byte.

t_is_byte(?int_range(From, To)) when From >= 0, To =< ?MAX_BYTE -> true;
t_is_byte(?int_set(Set)) -> lists:all(fun(X) -> 
					  (X >= 0) andalso (X =< ?MAX_BYTE)
				      end, set_to_list(Set));
t_is_byte(_) -> false.

%%------------------------------------

t_char() ->
  ?char.

t_is_char(?int_range(From, To)) when From >= 0, To =< ?MAX_CHAR -> true;
t_is_char(?int_set(Set)) -> lists:all(fun(X) -> 
					  (X >= 0) andalso (X =< ?MAX_CHAR)
				      end, set_to_list(Set));
t_is_char(_) -> false.

%%-----------------------------------------------------------------------------
%% Lists
%%

t_cons() ->
  ?nonempty_list(?any, ?any).

t_cons(?none,  _) -> ?none;
t_cons(_, ?none) -> ?none;
t_cons(Hd, ?nil) ->
  ?nonempty_list(Hd, ?nil);
t_cons(Hd, ?list(Contents, Termination, _)) ->
  ?nonempty_list(t_sup(Contents, Hd), Termination);
t_cons(Hd, Tail) ->
  case t_inf(Tail, t_pos_improper_list()) of
    ?list(Contents, _Termination, _Size) -> 
      ?nonempty_list(t_sup(Hd, Contents), Tail);
    _Other ->
      ?nonempty_list(Hd, Tail)
  end.
  
t_is_cons(?nonempty_list(_, _)) -> true;
t_is_cons(_) -> false.  

t_cons_hd(?nonempty_list(Contents, _Termination)) -> Contents.

t_cons_tl(T = ?nonempty_list(_Contents, Termination)) ->
  t_sup(Termination, T).

t_nil() ->
  ?nil.

t_is_nil(?nil) -> true;
t_is_nil(_) -> false.

t_list() ->  
  ?list(?any, ?nil, ?any).

t_list(?none) -> ?none;
t_list(Contents) ->
  ?list(Contents, ?nil, ?any).

t_list_elements(?list(Contents, ?nil, _)) -> Contents;
t_list_elements(?list(Contents, Termination, _)) -> 
  t_sup(Contents, Termination);
t_list_elements(?nil) -> ?none.

t_is_list(?list(_Contents, ?nil, _)) -> true;
t_is_list(?nil) -> true;
t_is_list(_) -> false.

t_nonempty_list() ->
  t_cons(?any, ?nil).

t_nonempty_list(Type) ->
  t_cons(Type, ?nil).

t_string() ->
  t_list(t_char()).

t_pos_improper_list() ->
  ?list(?any, ?any, ?any).

t_is_pos_improper_list(?list(_, _, _)) -> true;
t_is_pos_improper_list(?nil) -> true;
t_is_pos_improper_list(_) -> false.

  
%%-----------------------------------------------------------------------------
%% Tuples
%%

t_tuple() ->
  ?tuple(?any, ?any, ?any).

t_tuple(N) when is_integer(N) ->
  ?tuple(duplicate(N, ?any), N, ?any);
t_tuple(List) ->
  case any_none(List) of
    true -> t_none();
    false ->
      Arity = length(List),
      Tag = get_tuple_tag(List),
      ?tuple(List, Arity, Tag)
  end.

get_tuple_tag([T = ?atom(Set,_, _)|_]) ->
  case set_size(Set) of
    1 -> T;
    _ -> ?any
  end;
get_tuple_tag(_) ->
  ?any.
      
t_tuple_args(?tuple(?any, ?any, ?any)) ->
  ?any;
t_tuple_args(?tuple(List, _, _)) ->
  List.

t_tuple_arity(?tuple(?any, ?any, ?any)) ->
  ?any;
t_tuple_arity(?tuple(_, Arity, _)) ->
  Arity.

t_tuple_arities(?tuple(?any, ?any, ?any)) ->
  ?any;
t_tuple_arities(?tuple(_, Arity, _)) ->
  [Arity];
t_tuple_arities(?tuple_set(List)) ->
  [Arity || ?tuple(_, Arity, _) <- List].

t_tuple_subtypes(?tuple(?any, ?any, ?any)) -> ?any;
t_tuple_subtypes(T = ?tuple(_, _, _)) -> [T];
t_tuple_subtypes(?tuple_set(List)) -> List.

t_is_tuple(?tuple(_, _, _)) -> true;
t_is_tuple(?tuple_set(_)) -> true;
t_is_tuple(_) -> false.

tuple_set(List) ->
  Length = length(List),
  if Length =:= 1 -> hd(List);
     Length =< ?SET_LIMIT -> ?tuple_set(List);
     true -> reduce_tuple_set(List)
  end.

tuple_set(T1 = ?tuple(_Elements1, Arity1, Tag1),
	  T2 = ?tuple(_Elements2, Arity2, Tag2)) ->
  if Arity1 < Arity2 -> ?tuple_set([T1, T2]);
     Arity1 > Arity2 -> ?tuple_set([T2, T1]);
     Tag1 < Tag2     -> ?tuple_set([T1, T2]);
     Tag1 > Tag2     -> ?tuple_set([T2, T1])
  end.

reduce_tuple_set(List) ->
  List1 = reduce_tuple_tags(List),
  Length = length(List1),
  if Length =:= 1 -> hd(List1);
     Length =< ?SET_LIMIT -> ?tuple_set(List1);
     true -> t_tuple()
  end.

reduce_tuple_tags([H|T]) ->
  reduce_tuple_tags(T, H, []). 

reduce_tuple_tags([?tuple(Elements1, Arity, _Tag1)|Left], 
		  ?tuple(Elements2, Arity, _Tag2), Acc) ->
  NewTuple = ?tuple(t_sup_lists(Elements1, Elements2), Arity, ?any),
  reduce_tuple_tags(Left, NewTuple, Acc);
reduce_tuple_tags([T1 = ?tuple(_, Arity1, _)|Left], 
		  T2 = ?tuple(_, Arity2, _), Acc) when Arity1 > Arity2 ->
  reduce_tuple_tags(Left, T1, [T2|Acc]);
reduce_tuple_tags([], T, Acc) ->
  lists:reverse([T|Acc]).

			  


%%-----------------------------------------------------------------------------
%% Non-primitive types
%%

t_constant() ->
  t_sup([t_number(), t_identifier(), t_atom(), t_fun(), t_binary()]).   %% changed t_atom() to ....? HL

t_is_constant(X) ->
  not t_is_none(t_inf(t_constant(), X)).

%%------------------------------------

%% ?none is allowed in products. Products of size 1 is not a product.

t_product([T]) -> T;
t_product(Types) when is_list(Types) ->
  ?product(Types).

t_components(?product(Types)) -> Types;
t_components(?any) -> ?any;
t_components(?none) -> ?none;
t_components(T) -> [T].
  
%%------------------------------------

t_var(Atom) when is_atom(Atom) -> ?var(Atom);
t_var(Int) when is_integer(Int)-> ?var(Int).

t_is_var(?var(_)) -> true;
t_is_var(_) -> false.

t_var_name(?var(Id)) -> Id.

t_has_var(?var(_)) -> true;
t_has_var(?function(Domain, Range)) -> 
  t_has_var(Domain) orelse t_has_var(Range);
t_has_var(?list(Contents, Termination, _)) ->
  t_has_var(Contents) orelse t_has_var(Termination);
t_has_var(?product(Types)) -> t_has_var_list(Types);
t_has_var(?tuple(?any, ?any, ?any)) -> false;
t_has_var(?tuple(Elements, _, _)) ->
  t_has_var_list(Elements);
t_has_var(?tuple_set(List)) ->
  t_has_var_list(List);
t_has_var(_) -> false.

t_has_var_list([T|Left]) ->
  t_has_var(T) orelse t_has_var_list(Left);
t_has_var_list([]) -> false.


%%============================================================================
%% 
%% Type construction from Erlang terms.
%%
%%============================================================================

%%-----------------------------------------------------------------------------
%% Make a type from a term. No type depth is enforced.
%%

t_from_term([H|T]) ->                  t_cons(t_from_term(H), t_from_term(T));
t_from_term([]) ->                     t_nil();
t_from_term(T) when is_atom(T) ->      t_atom(T);
t_from_term(T) when is_binary(T) ->    t_binary();
t_from_term(T) when is_float(T) ->     t_float();
t_from_term(T) when is_function(T) ->  t_fun();
t_from_term(T) when is_integer(T) ->   t_integer(T);
t_from_term(T) when is_pid(T) ->       t_pid();
t_from_term(T) when is_port(T) ->      t_port();
t_from_term(T) when is_reference(T) -> t_ref();
t_from_term(T) when is_tuple(T) ->     t_tuple([t_from_term(E) 
						|| E <- tuple_to_list(T)]).


%%-----------------------------------------------------------------------------
%% Integer types from a range.
%%

t_from_range(X, Y) when is_integer(X), is_integer(Y), X =< Y ->
  if (Y - X) < ?SET_LIMIT -> t_integers(lists:seq(X, Y));
     true -> ?int_range(X, Y)
  end.

int_range(neg_inf, pos_inf)         -> t_integer();
int_range(neg_inf, To)              -> ?int_range(neg_inf, To);
int_range(From, pos_inf)            -> ?int_range(From, pos_inf);
int_range(From, To) when From =< To -> t_from_range(From, To).

in_range(_, ?int_range(neg_inf, pos_inf)) -> true;
in_range(X, ?int_range(From, pos_inf))    -> X >= From;
in_range(X, ?int_range(neg_inf, To))      -> X =< To;
in_range(X, ?int_range(From, To))         -> (X >= From) andalso (X =< To).

range_includes_set(Range, Set) ->
  lists:all(fun(X) -> in_range(X, Range) end, set_to_list(Set)).

max(neg_inf, Y) -> Y;
max(X, neg_inf) -> X;
max(pos_inf, _) -> pos_inf;
max(_, pos_inf) -> pos_inf;
max(X, Y) when X =< Y -> Y;
max(X, _) -> X.

min(neg_inf, _) -> neg_inf;
min(_, neg_inf) -> neg_inf;
min(pos_inf, Y) -> Y;
min(X, pos_inf) -> X;
min(X, Y) when X =< Y -> X;
min(_, Y) -> Y.
  
expand_range_list(?int_range(From, To), List) ->
  expand_range_limits_list(From, To, List).

expand_range_limits_list(From, To, [H|T]) ->
  Min = min(From, H),
  Max = max(To, H),
  if Min =:= neg_inf, Max =:= pos_inf -> t_integer();
     true -> expand_range_limits_list(Min, Max, T)
  end;
expand_range_limits_list(From, To, []) ->
  ?int_range(From, To).
  
%%============================================================================
%% 
%% Lattice operations
%%
%%============================================================================

%%-----------------------------------------------------------------------------
%% Supremum
%%

t_sup([?any|_]) ->
  ?any;
t_sup([H1, H2|T]) ->
  t_sup([t_sup(H1, H2)|T]);
t_sup([H]) ->
  subst_all_vars_to_any(H);
t_sup([]) ->
  ?none.  

t_sup(?any, _) -> ?any;
t_sup(_, ?any) -> ?any;
t_sup(?none, T) -> T;
t_sup(T, ?none) -> T;
t_sup(T, T) -> subst_all_vars_to_any(T);
t_sup(?var(_), _) -> ?any;
t_sup(_, ?var(_)) -> ?any;
t_sup(?atom(Set1,_, _), ?atom(Set2,_, _)) ->                   %% IS THIS CORRECT???  HL
  ?atom(set_union(Set1, Set2),?normal_atom, ?empty);
t_sup(?binary, ?binary) ->
  ?binary;
t_sup(?function(Domain1, Range1), ?function(Domain2, Range2)) ->
  %% The domain is either a product or any.
  ?function(t_sup(Domain1, Domain2), t_sup(Range1, Range2));
t_sup(?identifier(Set1), ?identifier(Set2)) ->
  ?identifier(set_union(Set1, Set2));
t_sup(?nil, ?nil) -> ?nil;
t_sup(?nil, ?list(Contents, Termination, _)) ->
  ?list(Contents, t_sup(?nil, Termination), ?any);
t_sup(?list(Contents, Termination, _), ?nil) ->
  ?list(Contents, t_sup(?nil, Termination), ?any);
t_sup(?list(Contents1, Termination1, Size1), 
      ?list(Contents2, Termination2, Size2)) ->
  NewSize =
    case {Size1, Size2} of
      {?any, ?any} -> ?any;
      {?any, ?nonempty_tag} -> ?any;
      {?nonempty_tag, ?any} -> ?any;
      {?nonempty_tag, ?nonempty_tag} -> ?nonempty_tag
    end,
  NewContents = t_sup(Contents1, Contents2),
  NewTermination = t_sup(Termination1, Termination2),
  ?list(NewContents, NewTermination, NewSize);
t_sup(?number(_, _), T = ?number(?any, ?number_tag)) -> T;  
t_sup(T = ?number(?any, ?number_tag), ?number(_, _)) -> T;
t_sup(?float, ?float) -> ?float;
t_sup(?float, ?integer(_)) -> t_number();
t_sup(?integer(_), ?float) -> t_number();
t_sup(T = ?integer(?any), ?integer(_)) -> T;
t_sup(?integer(_), T = ?integer(?any)) -> T;
t_sup(?int_set(Set1), ?int_set(Set2)) ->
  case set_union(Set1, Set2) of
    ?any ->
      L1 = set_to_list(Set1),
      L2 = set_to_list(Set2),
      ByteFun = fun(X) -> (X >= 0) andalso (X =< ?MAX_BYTE) end,
      case lists:all(ByteFun, L1) andalso lists:all(ByteFun, L2) of
	true -> t_byte();
	false ->
	  CharFun = fun(X) -> (X >= 0) andalso (X =< ?MAX_CHAR) end,
	  case lists:all(CharFun, L1) andalso lists:all(CharFun, L2) of
	    true -> t_char();
	    false -> t_integer()
	  end
      end;
    Set -> ?int_set(Set)
  end;
t_sup(?int_range(From1, To1), ?int_range(From2, To2)) ->
  int_range(min(From1, From2), max(To1, To2));
t_sup(Range = ?int_range(_, _), ?int_set(Set)) ->
  case range_includes_set(Range, Set) of
    true -> Range;
    false -> expand_range_list(Range, set_to_list(Set))
  end;
t_sup(?int_set(Set), Range = ?int_range(_, _)) ->
  case range_includes_set(Range, Set) of
    true -> Range;
    false -> expand_range_list(Range, set_to_list(Set))
  end;
t_sup(?product(Types1), ?product(Types2)) ->
  L1 = length(Types1),
  L2 = length(Types2),
  if L1 =:= L2 -> ?product(t_sup_lists(Types1, Types2));
     true -> ?any
  end;
t_sup(?product(_), _) ->
  ?any;
t_sup(_, ?product(_)) ->
  ?any;
t_sup(T = ?tuple(?any, ?any, ?any), ?tuple(_, _, _)) -> T;
t_sup(?tuple(_, _, _), T = ?tuple(?any, ?any, ?any)) -> T;
t_sup(T = ?tuple(?any, ?any, ?any), ?tuple_set(_)) -> T;
t_sup(?tuple_set(_), T = ?tuple(?any, ?any, ?any)) -> T;
t_sup(T1 = ?tuple(Elements1, Arity, Tag1), 
      T2 = ?tuple(Elements2, Arity, Tag2)) ->
  if Tag1 =:= Tag2 -> ?tuple(t_sup_lists(Elements1, Elements2), Arity, Tag1);
     Tag1 =:= ?any -> ?tuple(t_sup_lists(Elements1, Elements2), Arity, Tag1);
     Tag2 =:= ?any -> ?tuple(t_sup_lists(Elements1, Elements2), Arity, Tag2);
     Tag1 =/= Tag2 -> tuple_set(T1, T2)
  end;
t_sup(T1 = ?tuple(_, _, _), T2 = ?tuple(_, _, _)) ->
  tuple_set(T1, T2);
t_sup(?tuple_set(List1), ?tuple_set(List2)) ->
  sup_tuple_sets(List1, List2);
t_sup(?tuple_set(List1), T2 = ?tuple(_, _, _)) ->
  sup_tuple_sets(List1, [T2]);
t_sup(T1 = ?tuple(_, _, _), ?tuple_set(List2)) ->
  sup_tuple_sets([T1], List2);
t_sup(?union(U1), ?union(U2)) ->
  %% We know that the end result must be a union too.
  ?union(t_sup_lists(U1, U2));
t_sup(T1, T2) ->
  ?union(U1) = force_union(T1),
  ?union(U2) = force_union(T2),
  sup_union(U1, U2).

t_sup_lists([T1|Left1], [T2|Left2]) ->
  [t_sup(T1, T2)|t_sup_lists(Left1, Left2)];
t_sup_lists([], []) ->
  [].

sup_tuple_sets(L1, L2) ->
  sup_tuple_sets(L1, L2, []).

sup_tuple_sets(L1 = [T1 = ?tuple(_Elements1, Arity1, Tag1)|Left1],
	       L2 = [T2 = ?tuple(_Elements2, Arity2, Tag2)|Left2],
	       Acc) ->
  if Arity1 < Arity2 -> sup_tuple_sets(Left1, L2, [T1|Acc]);
     Arity1 > Arity2 -> sup_tuple_sets(L1, Left2, [T2|Acc]);
     Tag1 =:= Tag2   -> sup_tuple_sets(Left1, Left2, [t_sup(T1, T2)|Acc]);
     Tag1 =:= ?any   -> sup_tuple_sets([t_sup(T1, T2)|Left1], Left2, Acc);
     Tag2 =:= ?any   -> sup_tuple_sets(Left1, [t_sup(T1, T2)|Left2], Acc);
     Tag1 < Tag2     -> sup_tuple_sets(Left1, L2, [T1|Acc]);
     Tag1 > Tag2     -> sup_tuple_sets(L1, Left2, [T2|Acc])
  end;
sup_tuple_sets([], L2, Acc) -> tuple_set(lists:reverse(Acc, L2));
sup_tuple_sets(L1, [], Acc) -> tuple_set(lists:reverse(Acc, L1)).

sup_union(U1, U2) ->
  sup_union(U1, U2, 0, []).

sup_union([?none|Left1], [?none|Left2], N, Acc) ->
  sup_union(Left1, Left2, N, [?none|Acc]);
sup_union([T1|Left1], [T2|Left2], N, Acc) ->
  sup_union(Left1, Left2, N+1, [t_sup(T1, T2)|Acc]);
sup_union([], [], N, Acc) ->
  if N =:= 0 -> ?none;
     N =:= 1 -> 
      [Type] = [T || T <- Acc, T=/=?none],
      Type;
     N =:= length(Acc)  -> ?any;
     true -> ?union(lists:reverse(Acc))
  end.

force_union(T = ?atom(_,_,_)) ->        ?atom_union(T);
force_union(T = ?binary) ->         ?binary_union(T); 
force_union(T = ?function(_, _)) -> ?function_union(T);
force_union(T = ?identifier(_)) ->  ?identifier_union(T);
force_union(T = ?list(_, _, _)) ->  ?list_union(T);
force_union(T = ?nil) ->            ?list_union(T);
force_union(T = ?number(_,_)) ->    ?number_union(T);
force_union(T = ?tuple(_, _, _)) -> ?tuple_union(T);
force_union(T = ?tuple_set(_)) ->   ?tuple_union(T);
force_union(T = ?union(_)) ->       T.

%%-----------------------------------------------------------------------------
%% Infimum
%%

t_inf(?var(_), ?var(_)) -> ?any;
t_inf(?var(_), T) -> T;
t_inf(T, ?var(_)) -> T;
t_inf(?any, T) -> T;
t_inf(T, ?any) -> T;
t_inf(?none, _) -> ?none;
t_inf(_, ?none) -> ?none;
t_inf(T, T) -> subst_all_vars_to_any(T);
t_inf(?atom(Set1,?normal_atom, A1), ?atom(Set2,C,A2)) ->
  case set_intersection(Set1, Set2) of
    ?none ->  ?none;
    NewSet -> case A1 of 
		  [] -> ?atom(NewSet, C, A2) ;
		  _  -> case A2 of 
			    [] -> ?atom(NewSet, C, A1);
			    _  -> if A1 == A2 ->
					  ?atom(NewSet, C, A1);
				     true -> ?none
				  end
			end
	      end
  end;
t_inf(?atom(Set1,C, A1), ?atom(Set2,?normal_atom, A2)) ->
  case set_intersection(Set1, Set2) of
    ?none ->  ?none;
    NewSet -> case A1 of 
		  [] -> ?atom(NewSet, C, A2);
		  _  -> case A2 of 
			    [] -> ?atom(NewSet, C, A1);
			    _  -> if A1 == A2 ->
					  ?atom(NewSet, C, A1);
				     true  -> ?none
				  end
			end
	      end
  end;

t_inf(?atom(Set1,C1, A1), ?atom(Set2,C2,A2)) ->
  case set_intersection(Set1, Set2) of
    ?none ->  ?none;
    NewSet -> if C1==C2 -> case A1 of 
			       [] -> ?atom(NewSet, C1,A2);
			       _ -> case A2 of 
					[] -> ?atom(NewSet, C1, A1);
					_ -> if A1 == A2 ->
						     ?atom(NewSet, C1, A1);
						true -> ?none
					     end
				    end
			   end;
		 true -> ?none
	      end
  end;


t_inf(?binary, ?binary) ->
  ?binary;
t_inf(?function(Domain1, Range1), ?function(Domain2, Range2)) ->
  case t_inf(Domain1, Domain2) of
    ?none -> ?none;
    Domain ->
      ?function(Domain, t_inf(Range1, Range2))
  end;
t_inf(?identifier(Set1), ?identifier(Set2)) ->
  case set_intersection(Set1, Set2) of
    ?none -> ?none;
    Set -> ?identifier(Set)
  end;
t_inf(?nil, ?nil) -> ?nil;
t_inf(?nil, ?nonempty_list(_, _)) ->
  ?none;
t_inf(?nonempty_list(_, _), ?nil) ->
  ?none;
t_inf(?nil, ?list(_Contents, Termination, _)) ->
  t_inf(?nil, Termination);
t_inf(?list(_Contents, Termination, _), ?nil) ->
  t_inf(?nil, Termination);
t_inf(?list(Contents1, Termination1, Size1), 
      ?list(Contents2, Termination2, Size2)) ->
  case t_inf(Termination1, Termination2) of
    ?none -> ?none;
    Termination ->
      case t_inf(Contents1, Contents2) of
	?none -> ?none;
	Contents -> 
	  Size =
	    case {Size1, Size2} of
	      {?any, ?any} -> ?any;
	      {?any, ?nonempty_tag} -> ?nonempty_tag;
	      {?nonempty_tag, ?any} -> ?nonempty_tag;
	      {?nonempty_tag, ?nonempty_tag} -> ?nonempty_tag
	    end,
	  ?list(Contents, Termination, Size)
      end
  end;
t_inf(T1 = ?number(_, _), T2 = ?number(_, _)) ->
  case {T1, T2} of
    {T, T}                           -> T;
    {_, ?number(?any, ?number_tag)}  -> T1;
    {?number(?any, ?number_tag), _}  -> T2;
    {?float, ?integer(_)}            -> ?none;
    {?integer(_), ?float}            -> ?none;
    {?integer(?any), ?integer(_)}    -> T2;
    {?integer(_), ?integer(?any)}    -> T1;
    {?int_set(Set1), ?int_set(Set2)} -> 
      case set_intersection(Set1, Set2) of
	?none -> ?none;
	Set -> ?int_set(Set)
      end;
    {?int_range(From1, To1), ?int_range(From2, To2)} -> 
      int_range(max(From1, From2), min(To1, To2));
    {Range = ?int_range(_, _), ?int_set(Set)} ->
      case set_filter(fun(X) -> in_range(X, Range) end, Set) of
	?none -> ?none;
	NewSet -> ?int_set(NewSet)
      end;
    {?int_set(Set), Range = ?int_range(_, _)} ->
      case set_filter(fun(X) -> in_range(X, Range) end, Set) of
	?none -> ?none;
	NewSet -> ?int_set(NewSet)
      end
  end;
t_inf(?product(Types1), ?product(Types2)) ->
  L1 = length(Types1),
  L2 = length(Types2),
  if L1 =:= L2 -> ?product(t_inf_lists(Types1, Types2));
     true -> ?none
  end;
t_inf(?tuple(?any, ?any, ?any), T = ?tuple(_, _, _)) -> T;
t_inf(T = ?tuple(_, _, _), ?tuple(?any, ?any, ?any)) -> T;
t_inf(?tuple(?any, ?any, ?any), T = ?tuple_set(_)) -> T;
t_inf(T = ?tuple_set(_), ?tuple(?any, ?any, ?any)) -> T;
t_inf(?tuple(Elements1, Arity, Tag1), ?tuple(Elements2, Arity, Tag2)) ->
  case t_inf(Tag1, Tag2) of
    ?none -> ?none;
    NewTag ->
      case t_inf_lists_strict(Elements1, Elements2) of
	?none -> ?none;
	NewElements -> ?tuple(NewElements, Arity, NewTag)
      end
  end;
t_inf(?tuple_set(List1), ?tuple_set(List2)) ->
  inf_tuple_sets(List1, List2);
t_inf(?tuple_set(List), T = ?tuple(_, _, _)) ->
  inf_tuple_sets(List, [T]);
t_inf(T = ?tuple(_, _, _), ?tuple_set(List)) ->
  inf_tuple_sets(List, [T]);
t_inf(?union(U1), T) ->
  ?union(U2) = force_union(T),
  inf_union(U1, U2);
t_inf(T, ?union(U2)) ->
  ?union(U1) = force_union(T),
  inf_union(U1, U2);
t_inf(#c{}, #c{}) ->
  ?none;
t_inf(_, _) ->   %% Dirty adding by HL
    ?none.

t_inf_lists(L1, L2) ->
  t_inf_lists(L1, L2, []).

t_inf_lists([T1|Left1], [T2|Left2], Acc) ->
  t_inf_lists(Left1, Left2, [t_inf(T1, T2)|Acc]);
t_inf_lists([], [], Acc) ->
  lists:reverse(Acc).

%% Infimum of lists with strictness. If any element is none, the whole
%% type is none.

t_inf_lists_strict(L1, L2) ->
  t_inf_lists_strict(L1, L2, []).

t_inf_lists_strict([T1|Left1], [T2|Left2], Acc) ->
  case t_inf(T1, T2) of
    ?none -> ?none;
    T -> t_inf_lists_strict(Left1, Left2, [T|Acc])
  end;
t_inf_lists_strict([], [], Acc) ->
  lists:reverse(Acc).


inf_tuple_sets(L1, L2) ->
  inf_tuple_sets(L1, L2, []).

inf_tuple_sets(L1 = [T1 = ?tuple(_Elements1, Arity1, Tag1)|Left1],
	       L2 = [T2 = ?tuple(_Elements2, Arity2, Tag2)|Left2],
	       Acc) ->
  if Arity1 < Arity2 -> inf_tuple_sets(Left1, L2, Acc);
     Arity1 > Arity2 -> inf_tuple_sets(L1, Left2, Acc);
     Tag1 =:= Tag2   -> case t_inf(T1, T2) of
			  ?none -> inf_tuple_sets(Left1, Left2, Acc);
			  NewTuple -> 
			    inf_tuple_sets(Left1, Left2, [NewTuple|Acc])
			end;
     Tag1 =:= ?any   -> case t_inf(T1, T2) of
			  ?none -> inf_tuple_sets(L1, Left2, Acc);
			  NewTuple -> inf_tuple_sets(L1, Left2, [NewTuple|Acc])
			end;
     Tag2 =:= ?any   -> case t_inf(T1, T2) of
			  ?none -> inf_tuple_sets(Left1, L2, Acc);
			  NewTuple -> inf_tuple_sets(Left1, L2, [NewTuple|Acc])
			end;
     Tag1 < Tag2     -> inf_tuple_sets(Left1, L2, Acc);
     Tag1 > Tag2     -> inf_tuple_sets(L1, Left2, Acc)
  end;
inf_tuple_sets(_, _, []) -> ?none;
inf_tuple_sets([], _L2, Acc) -> tuple_set(lists:reverse(Acc));
inf_tuple_sets(_L1, [], Acc) -> tuple_set(lists:reverse(Acc)).


inf_union(U1, U2) ->
  inf_union(U1, U2, 0, []).

inf_union([?none|Left1], [?none|Left2], N, Acc) ->
  inf_union(Left1, Left2, N, [?none|Acc]);
inf_union([T1|Left1], [T2|Left2], N, Acc) ->
  case t_inf(T1, T2) of
    ?none -> inf_union(Left1, Left2, N, [?none|Acc]);
    T     -> inf_union(Left1, Left2, N+1, [T|Acc])
  end;
inf_union([], [], N, Acc) ->
  if N =:= 0 -> ?none;
     N =:= 1 ->
      [Type] = [T || T <- Acc, T=/=?none],
      Type;
     N >= 2  -> ?union(lists:reverse(Acc))
  end.

%%-----------------------------------------------------------------------------
%% Substituting variables.
%%

t_subst(T, Dict) ->
  case t_has_var(T) of
    true -> t_subst(T, Dict, fun(X) -> X end);
    false -> T
  end.

subst_all_vars_to_any(T) ->
  case t_has_var(T) of
    true -> t_subst(T, dict:new(), fun(_) -> ?any end);
    false -> T
  end.

t_subst(T = ?var(Id), Dict, Fun) ->
  case dict:find(Id, Dict) of
    error -> Fun(T);
    {ok, Type} -> Type
  end;
t_subst(?list(Contents, Termination, Size), Dict, Fun) ->
  ?list(t_subst(Contents, Dict, Fun), t_subst(Termination, Dict, Fun), Size);
t_subst(?function(Domain, Range), Dict, Fun) ->
  ?function(t_subst(Domain, Dict, Fun), t_subst(Range, Dict, Fun));
t_subst(?product(Types), Dict, Fun) -> 
  ?product([t_subst(T, Dict, Fun) || T <- Types]);
t_subst(T = ?tuple(?any, ?any, ?any), _Dict, _Fun) ->
  T;
t_subst(?tuple(Elements, Arity, Tag), Dict, Fun) ->
  ?tuple([t_subst(E, Dict, Fun) || E <- Elements], Arity, Tag);
t_subst(?tuple_set(List), Dict, Fun) ->
  ?tuple_set([t_subst(T, Dict, Fun) || T <- List]);
t_subst(T, _Dict, _Fun) -> 
  T.


		      
%%-----------------------------------------------------------------------------
%% Unification
%%

t_unify(T1, T2) ->
  {T, Dict} = t_unify(T1, T2, dict:new()),
  {t_subst(T, Dict), lists:keysort(1,dict:to_list(Dict))}.

t_unify(T = ?var(Id), ?var(Id), Dict) ->
  {T, Dict};
t_unify(T = ?var(Id1), ?var(Id2), Dict) ->
  case dict:find(Id1, Dict) of
    error -> 
      case dict:find(Id2, Dict) of
	error -> {T, dict:store(Id2, T, Dict)};
	{ok, Type} -> {Type, t_unify(T, Type, Dict)}
      end;
    {ok, Type1} ->
      case dict:find(Id2, Dict) of
	error -> {Type1, dict:store(Id2, T, Dict)};
	{ok, Type2} -> t_unify(Type1, Type2, Dict)
      end
  end;
t_unify(?var(Id), Type, Dict) ->
  case dict:find(Id, Dict) of
    error -> {Type, dict:store(Id, Type, Dict)};
    {ok, VarType} -> t_unify(VarType, Type, Dict)
  end;
t_unify(Type, ?var(Id), Dict) ->
  case dict:find(Id, Dict) of
    error -> {Type, dict:store(Id, Type, Dict)};
    {ok, VarType} -> t_unify(VarType, Type, Dict)
  end;
t_unify(?function(Domain1, Range1), ?function(Domain2, Range2), Dict) ->
  {Domain, Dict1} = t_unify(Domain1, Domain2, Dict),
  {Range, Dict2} = t_unify(Range1, Range2, Dict1),
  {?function(Domain, Range), Dict2};
t_unify(?list(Contents1, Termination1, Size), 
	?list(Contents2, Termination2, Size), Dict) ->
  {Contents, Dict1} = t_unify(Contents1, Contents2, Dict),
  {Termination, Dict2} = t_unify(Termination1, Termination2, Dict1),
  {?list(Contents, Termination, Size), Dict2};
t_unify(?product(Types1), ?product(Types2), Dict) -> 
  {Types, Dict1} = unify_lists(Types1, Types2, Dict),
  {?product(Types), Dict1};
t_unify(T = ?tuple(?any, ?any, ?any), ?tuple(?any, ?any, ?any), Dict) ->
  {T, Dict};
t_unify(?tuple(Elements1, Arity, _), 
	?tuple(Elements2, Arity, _), Dict) when Arity =/= ?any ->
  {NewElements, Dict1} = unify_lists(Elements1, Elements2, Dict),
  {t_tuple(NewElements), Dict1};
t_unify(?tuple_set(List1), ?tuple_set(List2), Dict) ->
  unify_lists(List1, List2, Dict);
t_unify(T1 = ?tuple(_, Arity, _), T2 = ?tuple_set(List), Dict) ->
  case reduce_tuple_tags(List) of
    [Tuple = ?tuple(_, Arity, _)] -> t_unify(T1, Tuple, Dict);
    _ -> throw({mismatch, T1, T2})
  end;
t_unify(T1 = ?tuple_set(List), T2 = ?tuple(_, Arity, _), Dict) ->
  case reduce_tuple_tags(List) of
    [Tuple = ?tuple(_, Arity, _)] -> t_unify(Tuple, T2, Dict);
    _ -> throw({mismatch, T1, T2})
  end;
t_unify(T, T, Dict) ->
  {T, Dict};

t_unify(T1=?atom(Set,_, _), _T2=?atom(Set,_,_), Dict) -> {T1, Dict};   %% Added by HL, MAY NOT CORRECT!!!

t_unify(T1, T2, _) ->
  throw({mismatch, T1, T2}).

unify_lists(L1, L2, Dict) ->
  unify_lists(L1, L2, Dict, []).

unify_lists([T1|Left1], [T2|Left2], Dict, Acc) ->
  {NewT, NewDict} = t_unify(T1, T2, Dict),
  unify_lists(Left1, Left2, NewDict, [NewT|Acc]);
unify_lists([], [], Dict, Acc) ->
  {lists:reverse(Acc), Dict}.


%%-----------------------------------------------------------------------------
%% Subtraction. 
%%
%% Note that the subtraction is an approximation since we do not have
%% negative types. Also, tuples should be handled using the cartesian
%% product of the elements, but this is not feasible to do.
%% 
%% Example: {a|b,c|d}\{a,d} = {a,c}|{a,d}|{b,c}|{b,d} \ {a,d} = 
%%                          = {a,c}|{b,c}|{b,d} = {a|b,c|d}
%%

t_subtract(_, ?any) -> ?none;
t_subtract(?any, _) -> ?any;
t_subtract(?none, _) -> ?none;
t_subtract(T, ?none) -> T;
t_subtract(?atom(Set1,_,_), ?atom(Set2,_, _)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none; 
    Set -> ?atom(Set,?normal_atom, ?empty)             %% IS THIS CORRECT??? HL
  end;
t_subtract(?binary, ?binary) ->
  ?none;
t_subtract(T1 = ?function(_, _), T2 = ?function(_, _)) ->
  case t_is_subtype(T1, T2) of
    true -> ?none;
    false -> T1
  end;
t_subtract(?identifier(Set1), ?identifier(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?identifier(Set)
  end;
t_subtract(?nil, ?nil) ->
  ?none;
t_subtract(?nil, ?nonempty_list(_, _)) ->
  ?nil;
t_subtract(?nil, ?list(_, _, _)) ->
  ?none;
t_subtract(T = ?list(Contents, Termination, _Size), ?nil) ->
  case Termination =:= ?nil of
    true -> ?nonempty_list(Contents, Termination);
    false -> T
  end;
t_subtract(T = ?list(Contents1, Termination1, Size1), 
	   ?list(Contents2, Termination2, Size2)) ->
  case t_is_subtype(Contents1, Contents2) of
    true ->
      case t_is_subtype(Termination1, Termination2) of
	true ->
	  case {Size1, Size2} of
	    {?nonempty_tag, ?any} -> ?none;
	    {?any, ?nonempty_tag} -> Termination1;
	    {S, S} -> ?none
	  end;
	false ->
	  %% If the termination is not covered by the subtracted type
	  %% we cannot really say anything about the result.
	  T
      end;
    false ->
      %% All contents must be covered if there is going to be any
      %% change to the list.
      T
  end;
t_subtract(?float, ?float) -> ?none;
t_subtract(T1 = ?number(_, _), ?float) -> t_inf(T1, t_integer());
t_subtract(?float, ?number(_Set, Tag)) ->
  case Tag of
    ?any -> ?none;
    _ -> ?float
  end;
t_subtract(?number(_, _), ?number(?any, ?number_tag)) -> ?none;
t_subtract(T1 = ?number(?any, ?number_tag), ?number(_, _)) -> T1;
t_subtract(?int_set(Set1), ?int_set(Set2)) ->
  case set_subtract(Set1, Set2) of
    ?none -> ?none;
    Set -> ?int_set(Set)
  end;
t_subtract(T1 = ?int_range(From1, To1), T2 = ?int_range(_, _)) ->
  case t_inf(T1, T2) of
    ?none -> T1;
    ?int_range(From1, To1) -> ?none;
    ?int_range(neg_inf, To) -> int_range(To + 1, To1);
    ?int_range(From, pos_inf) -> int_range(From1, From - 1);
    ?int_range(From, To) -> t_sup(int_range(From1, From - 1), 
				  int_range(To + 1, To))
  end;
t_subtract(T1 = ?int_range(From, To), T2 = ?int_set(Set)) ->
  NewFrom = case set_is_element(From, Set) of
	      true -> From + 1;
	      false -> From
	    end,
  NewTo = case set_is_element(To, Set) of
	    true -> To - 1;
	    false -> To
	  end,
  if (NewFrom =:= From) and (NewTo =:= To) -> T1;
     true -> t_subtract(t_from_range(NewFrom, NewTo), T2)
  end;
t_subtract(?int_set(Set), ?int_range(From, To)) ->
  case set_filter(fun(X) -> not ((X =< From) orelse (X >= To))end, Set) of
    ?none -> ?none;
    NewSet -> ?int_set(NewSet)
  end;
t_subtract(?integer(_), ?integer(?any)) -> ?none;
t_subtract(T1 = ?integer(?any), ?integer(_)) -> T1;
t_subtract(?tuple(_, _, _), ?tuple(?any, ?any, ?any)) -> ?none;
t_subtract(?tuple_set(_), ?tuple(?any, ?any, ?any)) -> ?none;
t_subtract(T1 = ?tuple(?any, ?any, ?any), ?tuple_set(_)) -> T1;
t_subtract(T1 = ?tuple(Elements1, Arity1, _Tag1), 
	   ?tuple(Elements2, Arity2, _Tag2)) ->
  if Arity1 =/= Arity2 -> T1;
     Arity1 =:= Arity2 ->
      NewElements = t_subtract_lists(Elements1, Elements2),
      case [E || E <- NewElements, E =/= ?none] of
	[] -> ?none;
	[_] -> t_tuple(replace_one_tuple_element(Elements1, NewElements));
	_ -> T1
      end
  end;
t_subtract(T1 = ?tuple_set(List1), T2 = ?tuple(_, Arity, _)) ->
  case [T || T = ?tuple(_, Arity1, _) <- List1, Arity1 =:= Arity] of
    [] -> T1;
    List2 ->
      Left = [T || T = ?tuple(_, Arity1, _) <- List1, Arity1 =/= Arity],
      t_sup([t_subtract(L, T2) || L <- List2]++Left)
  end;
t_subtract(T1 = ?tuple(_, Arity, _), ?tuple_set(List1)) ->
  case [T || T = ?tuple(_, Arity1, _) <- List1, Arity1 =:= Arity] of
    [] -> T1;
    List2 -> t_sup([t_subtract(T1, L) || L <- List2])
  end;
t_subtract(?tuple_set(List1), T2 = ?tuple_set(_)) ->
  case [T || T <- [t_subtract(Tuple, T2) || Tuple <- List1], T =/= ?none] of
    [] -> ?none;
    [Tuple] -> Tuple;
    NewList1 -> t_sup(NewList1)
  end;  
t_subtract(?union(U1), ?union(U2)) ->
  subtract_union(U1, U2);
t_subtract(T1, T2) ->  
  ?union(U1) = force_union(T1),
  ?union(U2) = force_union(T2),
  subtract_union(U1, U2).


t_subtract_lists(L1, L2) ->
  t_subtract_lists(L1, L2, []).

t_subtract_lists([T1|Left1], [T2|Left2], Acc) ->
  t_subtract_lists(Left1, Left2, [t_subtract(T1, T2)|Acc]);
t_subtract_lists([], [], Acc) ->
  lists:reverse(Acc).


subtract_union(U1, U2) ->
  subtract_union(U1, U2, 0, []).

subtract_union([T1|Left1], [T2|Left2], N, Acc) ->
  case t_subtract(T1, T2) of
    ?none -> subtract_union(Left1, Left2, N, [?none|Acc]);
    T ->     subtract_union(Left1, Left2, N+1, [T|Acc])
  end;
subtract_union([], [], 0, _Acc) ->
  ?none;
subtract_union([], [], 1, Acc) ->
  [T] = [X || X <- Acc, X =/= ?none],
  T;
subtract_union([], [], N, Acc) when is_integer(N), N > 1 ->
  ?union(lists:reverse(Acc)).

replace_one_tuple_element(El1, El2) ->
  replace_one_tuple_element(El1, El2, []).

replace_one_tuple_element([T1|Left1], [?none|Left2], Acc) ->
  replace_one_tuple_element(Left1, Left2, [T1|Acc]);
replace_one_tuple_element([_|Left1], [T2|_], Acc) ->
  lists:reverse(Acc) ++ [T2|Left1].


%%-----------------------------------------------------------------------------
%% Relations
%%

t_is_equal(T, T)  -> true;
t_is_equal(_, _) -> false.
  
t_is_subtype(T1, T2) ->
  Inf = t_inf(T1, T2),
  t_is_equal(T1, Inf).

%%-----------------------------------------------------------------------------
%% K-depth abstraction.
%%

t_limit(_, K) when K =< 0 -> ?any;
t_limit(T = ?tuple(?any, ?any, ?any), _K) -> T;
t_limit(?tuple(Elements, Arity, _), K) ->
  if K =:= 1 -> t_tuple(Arity);
     true -> t_tuple([t_limit(E, K-1) || E <- Elements])
  end;
t_limit(?tuple_set(List), K) ->
  if K =:= 1 -> t_sup([t_limit(L, K) || L <- List]);
     true -> ?tuple_set([t_limit(L, K) || L <- List])
  end;
t_limit(?list(Elements, Termination, Size), K) ->
  if K =:= 1 ->
      %% We do not want to lose the termination information.
      ?list(t_limit(Elements, K - 1), t_limit(Termination, K), Size);
     true ->
      ?list(t_limit(Elements, K - 1), t_limit(Termination, K - 1), Size)
  end;
t_limit(?function(Domain, Range), K) ->
  %% The domain is either a product or any() so we do not decrease the K.
  ?function(t_limit(Domain, K), t_limit(Range, K-1));
t_limit(?product(Elements), K) ->
  ?product([t_limit(X, K - 1) || X <- Elements]);
t_limit(?union(Elements), K) ->
  Elements1 = [t_limit(X, K - 1) || X <- Elements],
  case all_any(Elements1) of
    true -> ?any;
    false -> ?union([t_limit(X, K) || X <- Elements])
  end;
t_limit(T, _K) -> T.


%%============================================================================
%% 
%% Prettyprinter
%%
%%============================================================================

t_to_string(T) ->
  t_to_string(T, dict:new()).

t_to_string(?any, _RecDict) -> 
  "any()";
t_to_string(?none, _RecDict) ->
  "none()";
t_to_string(?atom(?any,?module_atom,_), _RecDict) -> 
  "module()";
t_to_string(?atom(?any,?function_atom,_),_RecDict) ->
   "function()";
t_to_string(?atom(?any,?process_atom,_),_RecDict) ->
   "process()";
t_to_string(?atom(?any,?normal_atom,_),_RecDict) ->
   "atom()";
t_to_string(?atom(Set,_,_), _RecDict)  ->
  case set_size(Set) of
    2 ->
      case set_is_element(true, Set) andalso set_is_element(false, Set) of
	true -> "bool()";
	false -> set_to_string(Set)
      end;
    _ ->
      set_to_string(Set)
  end;
t_to_string(?binary, _RecDict) -> 
  "binary()";
t_to_string(?function(Domain, Range), RecDict) ->
  case Domain of
    ?any -> 
      case Range =:= ?any of
	true -> "function()";
	false -> "((...)->"++t_to_string(Range, RecDict) ++ ")"
      end;
    ?product(List) -> 
      "((" ++ comma_sequence(List, RecDict) ++ ") -> " 
	++ t_to_string(Range, RecDict) ++ ")"
  end;
t_to_string(?identifier(Set), _RecDict) -> 
  if Set =:= ?any -> "identifier()";
     true -> sequence([io_lib:format("~w()", [T]) 
		       || T <- set_to_list(Set)], [], " | ")
  end;
t_to_string(?nil, _RecDict) ->
  "[]";
t_to_string(?nonempty_list(Contents, Termination), RecDict) ->
  ContentString = t_to_string(Contents, RecDict),
  case Termination of
    ?nil -> "["++ContentString++",...]";
    ?any -> 
      if Contents =:= ?any ->
	  "nonempty_possibly_improper_list()";
	 true ->
	  "nonempty_possibly_improper_list("++ContentString++")"
      end;
    _ -> "nonempty_improper_list("++ContentString++","
	   ++t_to_string(Termination, RecDict)++")"
  end;
t_to_string(?list(Contents, Termination, ?any), RecDict) ->
  ContentString = t_to_string(Contents, RecDict),
  case Termination of
    ?nil ->
      case t_is_char(Contents) andalso (not t_is_byte(Contents)) of
	true -> "string()";
	false -> "["++ContentString++"]"
      end;
    ?any ->
      if Contents =:= ?any ->
	  "possibly_improper_list()";
	 true ->
	  "possibly_improper_list("++ContentString++")"
      end;
    _ -> 
      case t_is_subtype(t_nil(), Termination) of
	true ->
	  "possibly_improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")";
	false ->
	  "improper_list("++ContentString++","
	    ++t_to_string(Termination, RecDict)++")"
      end
  end;
t_to_string(?int_set(Set), _RecDict) ->
  set_to_string(Set);  
t_to_string(?byte, _RecDict) -> "byte()";
t_to_string(?char, _RecDict) -> "char()";
t_to_string(?int_range(From, To), _RecDict) ->
  io_lib:format("range(~w, ~w)", [From, To]);
t_to_string(?integer(?any), _RecDict) -> "integer()";
t_to_string(?float, _RecDict) -> "float()";
t_to_string(?number(?any, ?number_tag), _RecDict) -> "number()";
t_to_string(?product(List), RecDict) -> 
  "<" ++ comma_sequence(List, RecDict) ++ ">";
t_to_string(?tuple(?any, ?any, ?any), _RecDict) -> "tuple()";
t_to_string(?tuple(Elements, _Arity, ?any), RecDict) ->   
  "{" ++ comma_sequence(Elements, RecDict) ++ "}";
t_to_string(?tuple(Elements, Arity, Tag), RecDict) ->
  [TagAtom] = t_atom_vals(Tag),
  case dict:find({TagAtom, Arity-1}, RecDict) of
    error ->   "{" ++ comma_sequence(Elements, RecDict) ++ "}";
    {ok, FieldNames} -> record_to_string(TagAtom, Elements, FieldNames, RecDict)
  end;
t_to_string(?tuple_set(List), RecDict) ->
  union_sequence(List, RecDict);
t_to_string(?union(Types), RecDict) ->
  union_sequence([T || T <- Types, T =/= ?none], RecDict);
t_to_string(?var(Id), _RecDict) ->
  io_lib:format("Var(~w)", [Id]).

record_to_string(Tag, [_|Fields], FieldNames, RecDict) ->
  FieldStrings = record_fields_to_string(Fields, FieldNames, RecDict, []),
  "#" ++ atom_to_list(Tag)++"{" ++ sequence(FieldStrings, [], ",") ++ "}".

record_fields_to_string([?any|Left1], [_|Left2], RecDict, Acc) ->
  record_fields_to_string(Left1, Left2, RecDict, Acc);
record_fields_to_string([Field|Left1], [FieldName|Left2], RecDict, Acc) ->
  case t_is_atom(undefined, Field) of
    true -> record_fields_to_string(Left1, Left2, RecDict, Acc);
    false ->
      String = atom_to_list(FieldName) ++ "=" ++ t_to_string(Field, RecDict),
      record_fields_to_string(Left1, Left2, RecDict, [String|Acc])
  end;
record_fields_to_string([], [], _RecDict, Acc) ->
  lists:reverse(Acc).

comma_sequence(Types, RecDict) ->
  List = [case T =:= ?any of
	    true -> "_";
	    false -> t_to_string(T, RecDict)
	  end || T <- Types], 
  sequence(List, [], ",").

union_sequence(Types, RecDict) ->
  List = [t_to_string(T, RecDict) || T <- Types], 
  sequence(List, [], " | ").

sequence([], [], _Delimiter) ->
  [];
sequence([T], Acc, _Delimiter) ->
  lists:flatten(lists:reverse([T|Acc]));
sequence([T|Left], Acc, Delimiter) -> 
  sequence(Left, [T ++ Delimiter|Acc], Delimiter).


%%============================================================================
%% 
%% Utilities
%%
%%============================================================================

duplicate(0, _) -> [];
duplicate(N, T) -> [T|duplicate(N-1, T)].

any_none([?none|_]) -> true;
any_none([_|Left]) -> any_none(Left);
any_none([]) -> false.

all_any([?any|Left]) -> all_any(Left);
all_any([]) -> true;
all_any([_|_]) -> false.
  
  
%% -----------------------------------
%% Set
%%

set_singleton(Element) ->
  [Element].

set_is_element(Element, Set) ->
  ordsets:is_element(Element, Set).

set_union(?any, _) -> ?any;
set_union(_, ?any) -> ?any;
set_union(S1, S2)  -> 
  case ordsets:union(S1, S2) of
    S when length(S) =< ?SET_LIMIT -> S;
    _ -> ?any
  end.

%% The intersection and subtraction can return ?none. 
%% This should always be handled right away since ?none is not a valid set.
%% However, ?any is considered a valid set.

set_intersection(?any, S) -> S;
set_intersection(S, ?any) -> S;
set_intersection(S1, S2)  -> 
  case ordsets:intersection(S1, S2) of
    [] -> ?none;
    S -> S
  end.      

set_subtract(_, ?any) -> ?none;
set_subtract(?any, _) -> ?any;
set_subtract(S1, S2) ->
  case ordsets:subtract(S1, S2) of
    [] -> ?none;
    S -> S
  end.

set_from_list(List)  ->
  case length(List) of
    L when L =< ?SET_LIMIT -> ordsets:from_list(List);
    L when L > ?SET_LIMIT -> ?any
  end.

set_to_list(Set) ->
  Set.

set_filter(Fun, Set) ->
  case lists:filter(Fun, Set) of
    [] -> ?none;
    NewSet -> NewSet
  end.

set_size(?any) ->
  ?any;
set_size(Set) ->
  length(Set).

set_to_string(Set) ->
  io:format("Set:\n~p\n", [Set]),
  List = set_to_list(Set),
  List1 = 
    [case is_atom(X) of
       true -> io_lib:write_string(atom_to_list(X), $'); % This is not a quote '
       false -> io_lib:format("~w", [X])
     end|| X <- List],
  sequence(List1, [], " | ").


%%============================================================================
%% 
%% Test
%%
%%============================================================================

-ifdef(ENABLE_TEST).		  

test() ->
  Atom1  = t_atom(),
  Atom2  = t_atom(foo),
  Atom3  = t_atom(bar),
  true   = t_is_atom(Atom2),

  True   = t_atom(true),
  False  = t_atom(false),
  Bool   = t_bool(),
  true   = t_is_bool(True),
  true   = t_is_bool(Bool),
  false  = t_is_bool(Atom1),

  Binary = t_binary(),
  true   = t_is_binary(Binary),

  Int1   = t_integer(),
  Int2   = t_integer(1),
  Int3   = t_integer(16#ffffffff),
  true   = t_is_integer(Int2),
  true   = t_is_byte(Int2),
  false  = t_is_byte(Int3),
  false  = t_is_byte(t_from_range(-1, 1)),
  true   = t_is_byte(t_from_range(1, ?MAX_BYTE)),
  
  Tuple1 = t_tuple(),
  Tuple2 = t_tuple(3),
  Tuple3 = t_tuple([Atom1, Int1]),
  Tuple4 = t_tuple([Tuple1, Tuple2]),
  Tuple5 = t_tuple([Tuple3, Tuple4]),
  Tuple6 = t_limit(Tuple5, 2),
  Tuple7 = t_limit(Tuple5, 3),
  true   = t_is_tuple(Tuple1),  
  
  Port   = t_port(),
  Pid    = t_pid(),
  Ref    = t_ref(),
  Identifier = t_identifier(),
  false  = t_is_ref(Port),
  true   = t_is_identifier(Port),

  Function1 = t_fun(),
  Function2 = t_fun(Pid),
  Function3 = t_fun([], Pid),
  Function4 = t_fun([Port, Pid], Pid),
  Function5 = t_fun([Pid, Atom1], Int2),
  true      = t_is_fun(Function3),  

  List1 = t_list(),
  List2 = t_list(t_bool()),
  List3 = t_cons(t_bool(), List2),
  List4 = t_cons(t_bool(), t_atom()),
  List5 = t_cons(t_bool(), t_nil()),
  List6 = t_cons_tl(List5),
  List7 = t_sup(List4, List5),
  List8 = t_inf(List7, t_list()),
  List9 = t_cons(),
  List10 = t_cons_tl(List9),
  true  = t_is_bool(t_cons_hd(List5)),
  true  = t_is_list(List5),
  false = t_is_list(List4),
  


  Product1 = t_product([Atom1, Atom2]),
  Product2 = t_product([Atom3, Atom1]),
  Product3 = t_product([Atom3, Atom2]),

  Union1 = t_sup(Atom2, Atom3),
  Union2 = t_sup(Tuple2, Tuple3),
  Union3 = t_sup(Int2, Atom3),
  Union4 = t_sup(Port, Pid),
  Union5 = t_sup(Union4, Int1),
  Union6 = t_sup(Function1, Function2),
  Union7 = t_sup(Function4, Function5),
  Union8 = t_sup(True, False),
  true   = t_is_bool(Union8),
  Union9 = t_sup(Int2, t_integer(2)),
  true   = t_is_byte(Union9),
  
  ?any   = t_sup(Product3, Function5),

  Atom3  = t_inf(Union3, Atom1),
  Union2 = t_inf(Union2, Tuple1),
  Int2   = t_inf(Int1, Union3),
  Union4 = t_inf(Union4, Identifier),
  Port   = t_inf(Union5, Port),
  Function4 = t_inf(Union7, Function4),
  ?none  = t_inf(Product2, Atom1),
  Product3 = t_inf(Product1, Product2),
  Function5 = t_inf(Union7, Function5),
  true   = t_is_byte(t_inf(Union9, t_number())),
  true   = t_is_char(t_inf(Union9, t_number())),

  RecDict = dict:store({foo, 2}, [bar, baz], dict:new()),
  Record1 = t_from_term({foo, [1,2], {1,2,3}}),
    
  
  Types = [
	   Atom1,
	   Atom2,
	   Atom3,
	   Binary,
	   Int1,
	   Int2,
	   Tuple1,
	   Tuple2,
	   Tuple3,
	   Tuple4,
	   Tuple5,
	   Tuple6,
	   Tuple7,
	   Ref,
	   Port,
	   Pid,
	   Identifier,
	   List1,
	   List2,
	   List3,
	   List4,
	   List5,
	   List6,
	   List7,
	   List8,
	   List9,
	   List10,
	   Function1,
	   Function2,
	   Function3,
	   Function4,
	   Function5,
	   Product1,
	   Product2,
	   Record1,
	   Union1,
	   Union2,
	   Union3,
	   Union4,
	   Union5,
	   Union6,
	   Union7,
	   Union8
	  ],
  io:format("~p\n", [[t_to_string(X, RecDict) || X <- Types]]).
		       
-endif.
