%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% Copyright 2006, Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id: refac_atom_info.erl,v 1.1.1.1 2007-11-07 21:56:10 hl Exp $
%%

%%% -*- erlang -*-
%%%-------------------------------------------------------------------
%%% File    : dialyzer_typesig.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 25 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%% Modified: 17 Jan 2007 by  Huiqing Li <hl@kent.ac.uk>
%%%-------------------------------------------------------------------
-module(refac_atom_info).

-export([get_atom_info/1]).	
	
-export([analyze_scc/4]).
-import(refac_erl_types, 
	[t_any/0, t_atom/0, 
         %% Begin of added by HL
	 t_module_atom/0, t_function_atom/0, t_process_atom/0,
	 
	 t_module_atom/1, t_function_atom/1, t_process_atom/1,
	 t_module_atom/2, t_function_atom/2, t_process_atom/2,
         t_is_module_atom/1, t_is_function_atom/1, t_is_process_atom/1,
         %% End of added by HL
         t_binary/0, t_bool/0, t_cons/2, t_cons/0,
	 t_cons_hd/1, t_cons_tl/1, t_components/1, t_float/0,
	 t_has_var/1,
	 t_is_float/1, t_from_range/2, t_fun/0, t_fun/2, t_fun_args/1,
	 t_fun_range/1, t_is_fun/1, t_pos_improper_list/0,
	 t_inf/2, t_inf_lists/2, t_integer/0, t_is_integer/1,
	 t_integers/1, 
	 t_is_atom/1, %% Added by HL;
	 t_is_atom/2, t_is_cons/1, t_is_equal/2,
	 t_is_tuple/1, t_is_none/1,
	 t_is_any/1, t_is_subtype/2, t_limit/2,
	 t_number/0, t_number_vals/1, t_pid/0, t_port/0,
	 t_product/1, t_ref/0, t_to_string/1, t_subst/2,
	 t_tuple/0, t_tuple_subtypes/1,
	 t_tuple/1, t_tuple_args/1, t_sup/1, t_sup/2,
	 t_unify/2, t_is_var/1, t_var/1, t_var_name/1, t_from_term/1, 
	 t_none/0]).

-import(cerl, 
	[ann_c_apply/3, alias_pat/1, alias_var/1,
	 apply_args/1, apply_op/1, atom_val/1, bitstr_size/1,
	 bitstr_val/1, bitstr_type/1, bitstr_flags/1, bitstr_unit/1,
	 binary_segments/1, c_atom/1, c_letrec/2,
	 call_args/1, call_module/1, call_name/1, case_arg/1,
	 case_clauses/1, clause_body/1, clause_guard/1,
	 clause_pats/1, concrete/1, cons_hd/1, cons_tl/1, 
	 fname_id/1, fname_arity/1,
	 fun_arity/1, fun_body/1, fun_vars/1, get_ann/1, int_val/1, is_c_atom/1,
	 is_c_fun/1, is_c_int/1, is_c_var/1, is_c_values/1, is_literal/1,
	 let_arg/1, let_body/1, let_vars/1, letrec_body/1,
	 letrec_defs/1, module_defs/1, module_name/1,
	 primop_name/1, receive_action/1,
	 receive_clauses/1, receive_timeout/1, seq_arg/1, seq_body/1,
	 set_ann/2, try_arg/1, try_body/1, try_evars/1, try_handler/1,
	 try_vars/1, tuple_es/1, type/1, values_es/1]).

-record(state, {callgraph=none, cs, cmap, fun_map, fun_arities, 
		name_map, next_label, plt, prop_args, prototypes, sccs, atoms}).
-record(constraint, {lhs, op, rhs, deps}).
-record(constraint_list, {type, list, deps, id}).
-record(constraint_ref, {id, deps, type}).
-record(fun_var, {'fun', deps}).
-record(c, {tag, elements=[], qualifier=any, ann=[]}). %% Generic constructor.
-record(literal, {ann = [], val}).
-record(call, {ann = [], module, name, args}).

-define(TYPE_LIMIT, 4).
-define(INTERNAL_TYPE_LIMIT, 5).


%% -define(DEBUG, true).
%% -define(DEBUG_PP, true).
%% -define(DEBUG_CONSTRAINTS, true).
%% -define(DEBUG_NAME_MAP, true).
%% -define(DOT, true).
-define(NO_DATAFLOW, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), io:format(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.

%%% ============================================================================
%%%
%%%  The analysis.
%%%
%%% ============================================================================

%%% --------------------------------------------------------
%%% Analysis of strongly connected components.
%%
%%% analyze_scc(SCC, NextLabel, CallGraph, Plt) -> [{MFA, Type}]
%%%
%%% SCC       - [{MFA, Def}]
%%%             where Def = {Var, Fun} as in the Core Erlang module definitions.
%%%
%%% NextLabel - An integer that is higher than any label in the code.
%%%
%%% CallGraph - A callgraph as produced by dialyzer_callgraph.erl 
%%%             Note: The callgraph must have been built with all the 
%%%                   code that the scc is a part of.
%%% Plt       - A dialyzer plt. This plt should contain available information
%%%             about functions that can be called by this SCC.
%%%

analyze_scc(SCC, NextLabel, CallGraph, Plt) when is_integer(NextLabel) ->
  assert_format_of_scc(SCC),
  analyze_scc(SCC, NextLabel, CallGraph, Plt, dict:new()).

assert_format_of_scc([{_MFA, {_Var, _Fun}}|Left]) ->
  assert_format_of_scc(Left);
assert_format_of_scc([]) ->
  ok;
assert_format_of_scc(Other) ->
  exit({?MODULE, illegal_scc, Other}).

analyze_scc(SCC, NextLabel, CallGraph, Plt, PropArgs) ->
  {TopMap, _MapDicts} = analyze_scc_1(SCC, NextLabel, CallGraph, Plt, PropArgs),
  [{MFA, lookup_type(mk_var(Fun), TopMap)} || {MFA, {_Var, Fun}} <- SCC].

analyze_scc_1(SCC, NextLabel, CallGraph, Plt, PropArgs) ->
  Defs = [Def || {_MFA, Def} <- SCC],
  State1 = new_state(SCC, NextLabel, CallGraph, Plt, PropArgs),
  {State2, _} = traverse(c_letrec(Defs, c_atom(foo)), sets:new(), State1),
  State3 = state__finalize(State2),
  Funs = state__sccs(State3),
  [pp_constrs_scc(X, State3) || X <- Funs],
  analyze_loop(Funs, State3, dict:new()).


%%% --------------------------------------------------------
%%% Analysis of one module.
%%%

analyze_module(LabeledTree, NextLabel, Plt) ->
  debug_pp(LabeledTree, dict:new()),
  Callgraph1 = dialyzer_callgraph:new(),
  Callgraph2 = dialyzer_callgraph:scan_core_tree(LabeledTree, Callgraph1),
  {Callgraph3, _Ext} = dialyzer_callgraph:remove_external(Callgraph2),
  Callgraph4 = dialyzer_callgraph:finalize(Callgraph3),
  {SCCs, Callgraph5} = get_sccs_from_callgraph(Callgraph4),
  CodeSCCs = insert_code_in_sccs(SCCs, LabeledTree),
  FunTypes = analyze_all_sccs(CodeSCCs, NextLabel, Callgraph5, Plt, dict:new()),  
  analyze_module_loop(LabeledTree, CodeSCCs, NextLabel,
		      Callgraph5, Plt, FunTypes).

-ifdef(NO_DATAFLOW).
analyze_module_loop(_LabeledTree, _CodeSCCs, _NextLabel, 
		    _Callgraph, _Plt, FunTypes) ->
  FunTypes.

-else.
analyze_module_loop(LabeledTree, CodeSCCs, NextLabel, 
		    Callgraph, Plt, FunTypes) ->
%  M = atom_val(module_name(LabeledTree)),
%  Sigs = [{{M, fname_id(V), fname_arity(V)}, 
%	   dict:fetch(get_label(F), FunTypes)} 
%	  || {V, F} <- module_defs(LabeledTree)],
%  Sigs1 = ordsets:from_list(Sigs),
%  io:format("\n\n----------------------------------------\n\n", []),
%  io:format("Types into dataflow: \n", []),  
%  pp_signatures(Sigs1),
  {T1, _} = statistics(runtime),
  ?debug("Start of dataflow\n", []),
  PropTypes = dialyzer_dataflow:get_fun_types(LabeledTree, FunTypes, Plt),
  {T2, _} = statistics(runtime),  
  ?debug("End of dataflow in ~.2f secs\n", [(T2-T1)/1000]),
  MapFun = fun(_Key, Type) -> 
	       case t_is_none(t_fun_range(Type)) of
		 true -> failed_fun;
		 false -> t_fun_args(Type)
	       end
	   end,
  OldArgs = dict:map(MapFun, FunTypes),
  MapFun2 = fun(_Key, Type) -> t_fun_args(Type)end,
  NewArgs = dict:map(MapFun2, PropTypes),
  case compare_arg_types(OldArgs, NewArgs) of
    fixpoint -> FunTypes;
    not_fixpoint ->      
      %MapFun2 = fun(_Key, Type) -> t_fun_args(Type)end,
      NewPropTypes = dict:map(MapFun2, PropTypes),
      NewFunTypes = analyze_all_sccs(CodeSCCs, NextLabel, Callgraph, 
				     Plt, NewPropTypes),
      ?debug("Merry go around\n", []),
      analyze_module_loop(LabeledTree, CodeSCCs, NextLabel, Callgraph, 
			  Plt, NewFunTypes)
  end.

compare_arg_types(Dict1, Dict2) ->  
  List1 = lists:keysort(1, dict:to_list(Dict1)),
  List2 = lists:keysort(1, dict:to_list(Dict2)),
  compare_arg_types_1(List1, List2).

compare_arg_types_1([{X, failed_fun}|Left1], [{X, _Types2}|Left2]) ->
  compare_arg_types_1(Left1, Left2);  
%compare_arg_types_1([{X, _Types1}|Left1], [{X, failed_fun}|Left2]) ->
%  compare_arg_types_1(Left1, Left2);  
compare_arg_types_1([{X, Types1}|Left1], [{X, Types2}|Left2]) ->
  case lists:all(fun({T1, T2})-> t_is_equal(t_limit(T1, ?TYPE_LIMIT), 
					    t_limit(T2, ?TYPE_LIMIT))
		 end,
		 lists:zip(Types1, Types2)) of
    true -> compare_arg_types_1(Left1, Left2);
    false -> ?debug("Failed fixpoint for ~w\n~s=/=\n~s\n", 
		    [X, t_to_string(t_product(Types1)),
		     t_to_string(t_product(Types2))]), 
	     not_fixpoint
  end;
compare_arg_types_1([_|Left1], List2) ->
  %% If the function was not called.
  compare_arg_types_1(Left1, List2);
compare_arg_types_1([], []) ->
  fixpoint.
-endif.

get_sccs_from_callgraph(CallGraph) ->
  get_sccs_from_callgraph(CallGraph, []).

get_sccs_from_callgraph(CallGraph, Acc) ->
  case dialyzer_callgraph:take_scc(CallGraph) of
    none -> 
      {lists:reverse(Acc), CallGraph};
    {ok, SCC0, NewCallGraph} ->
      %% Remove all functions that are not top functions since all
      %% other functions need to be analyzed in their environment.
      SCC1 = [Fun || Fun <- SCC0, not is_integer(Fun)],
      if SCC1 =:= [] -> get_sccs_from_callgraph(NewCallGraph, Acc);
	 true ->        get_sccs_from_callgraph(NewCallGraph, [SCC1|Acc])
      end
  end.

insert_code_in_sccs(SCCs, Tree) ->
  M = atom_val(module_name(Tree)),
  List = [{{M, fname_id(V), fname_arity(V)}, {V, F}} 
	  || {V, F} <- module_defs(Tree)],
  CodeMap = dict:from_list(List),
  lists:map(fun(SCC) ->
		[{MFA, dict:fetch(MFA, CodeMap)} || MFA <- SCC]
	    end, SCCs).  

analyze_all_sccs(SCCs, NextLabel, Callgraph, Plt, PropTypes) ->
  %{T1, _} = statistics(runtime),
  ?debug("Start of typesig\n", []),
  Res = analyze_all_sccs(SCCs, NextLabel, Callgraph, Plt, PropTypes, dict:new()),
  %{T2, _} = statistics(runtime),  
  %?debug("End of typesig in ~.2f secs\n", [(T2-T1)/1000]),
  ?debug("End of typesig\n", []),
  Res.

analyze_all_sccs([SCC|Left], NextLabel, CallGraph, Plt, PropTypes, FunTypes) ->
  {TopMap, MapDicts} = analyze_scc_1(SCC, NextLabel, CallGraph, Plt, 
				     PropTypes),
  MapDict = merge_mapdicts(MapDicts, TopMap),
  NewFunTypes = get_fun_types_in_scc(SCC, MapDict, FunTypes),
  NewPlt = update_plt(SCC, NewFunTypes, Plt),
  analyze_all_sccs(Left, NextLabel, CallGraph, NewPlt, PropTypes, NewFunTypes);
analyze_all_sccs([], _NextLabel, _Callgraph, _Plt, _PropTypes, FunTypes) ->
  FunTypes.

get_fun_types_in_scc([{_MFA, {_Var, Tree}}|Left], MapDict, Acc) ->
  FoldFun = 
    fun(SubTree, Dict) ->
	case is_c_fun(SubTree) of
	  true ->
	    Label = get_label(SubTree),
	    Type = 
	      case dict:find(t_var(Label), MapDict) of
		error -> 
		  Arity = fun_arity(SubTree),
		  t_fun(duplicate(Arity, t_none()), t_none());
		{ok, Map} -> 
		  lookup_type(t_var(Label), Map)
	      end,
	    dict:store(Label, Type, Dict);
	  false -> Dict
	end
    end,
  NewAcc = cerl_trees:fold(FoldFun, Acc, Tree),
  get_fun_types_in_scc(Left, MapDict, NewAcc);
get_fun_types_in_scc([], _MapDict, Acc) ->
  Acc.

update_plt([{MFA, {_Var, Fun}}|Left], FunTypes, Plt) ->
  Type = dict:fetch(get_label(Fun), FunTypes),
  Update =
    case t_is_fun(Type) of
      true ->
	Args = t_fun_args(Type),
	Return = t_fun_range(Type),
	{MFA, Return, Args};
      false ->
	{_M, _F, Arity} = MFA,
	Args = duplicate(Arity, t_any()),
	{MFA, t_none(), Args}
    end,
  dialyzer_plt:insert(Plt, Update),
  update_plt(Left, FunTypes, Plt);
update_plt([], _FunTypes, Plt) ->
  Plt.

analyze_loop(Funs, State, Map) ->
  analyze_loop(Funs, State, Map, []).

analyze_loop([[Fun]|Left], State, Map, Acc) ->
  ?debug("============ Analyzing Fun: ~w ===========\n", 
	  [debug_lookup_name(Fun)]),
  {Map1, MapDict} = solve_fun(Fun, Map, State),
  analyze_loop(Left, State, Map1, [MapDict|Acc]);
analyze_loop([SCC|Left], State, Map, Acc) ->
  ?debug("============ Analyzing SCC: ~w ===========\n", 
	  [[debug_lookup_name(F) || F <- SCC]]),
  {Map1, MapDicts} = solve_scc(SCC, Map, State),
  case maps_are_equal(Map1, Map, SCC) of
    true ->       
      analyze_loop(Left, State, Map1, [MapDicts|Acc]);
    false -> 
      ?debug("SCC ~w did not reach fixpoint\n", [SCC]),
      analyze_loop([SCC|Left], State, Map1, Acc)
  end;
analyze_loop([], _State, Map, Acc) ->
  {Map, lists:flatten(Acc)}.



%%% ============================================================================
%%%
%%%  Getting the constraints by traversing the code.
%%%
%%% ============================================================================

traverse(Tree, DefinedVars, State) ->
  ?debug("Handling ~p\n", [type(Tree)]),
  case type(Tree) of
    alias ->
      Var = alias_var(Tree),
      Pat = alias_pat(Tree),
      DefinedVars1 = add_def(Var, DefinedVars),
      {State1, PatVar} = traverse(Pat, DefinedVars1, State),
      State2 = state__store_conj(mk_var(Var), eq, PatVar, State1),
      {State2, PatVar};
    apply ->
      Args = apply_args(Tree),
      Arity = length(Args),
      Op = apply_op(Tree),
      {State0, OpType} = traverse(Op, DefinedVars, State),
      {State1, FunType} = state__get_fun_prototype(OpType, Arity, State0),
      State2 = state__store_conj(FunType, eq, OpType, State1),
      State3 = state__store_conj(mk_var(Tree),sub,t_fun_range(FunType), State2),
      {State4, ArgTypes} = traverse_list(Args, DefinedVars, State3),      
      State5 = state__store_conj_lists(ArgTypes,sub,t_fun_args(FunType),State4),
      {State5, mk_var(Tree)};
    binary ->
      {State1, _} = traverse_list(binary_segments(Tree), DefinedVars, State),
      State2 = state__store_conj(mk_var(Tree), eq, t_binary(), State1),
      {State2, t_binary()};
    bitstr ->
      %% Only care about Size and Value since the other fields are
      %% constant literals. Size must be an integer - NO, it can be
      %% 'all' but this has to be a literal.
      Size = bitstr_size(Tree),
      Unit = bitstr_unit(Tree),
      Val = bitstr_val(Tree),            
      {State1, [SizeType, ValType]} = 
	traverse_list([Size, Val], DefinedVars, State),
      State2 =
	case type(Size) of
	  literal ->
	    %% Safety check
	    case concrete(Size) of
	      all -> State1;
	      N when is_integer(N) -> State1
	    end;
	  var ->
	    state__store_conj(SizeType, sub, t_integer(), State1)
	end,
      
      {case concrete(bitstr_type(Tree)) of
	 float -> state__store_conj(ValType, sub, t_float(), State2);
	 binary -> state__store_conj(ValType, sub, t_binary(), State2);
	 integer ->	  
	   case is_c_int(Size) of
	     true ->
	       SizeVal = int_val(Size),
	       UnitVal = int_val(Unit),
	       TotalSizeVal = SizeVal * UnitVal,
	       Flags = concrete(bitstr_flags(Tree)),
	       Type = 
		case lists:member(signed, Flags) of
		  true -> 
		    t_from_range(-(1 bsl (TotalSizeVal - 1)),
				 1 bsl (TotalSizeVal - 1) - 1);
		  false -> 
		    t_from_range(0,1 bsl TotalSizeVal - 1)
		end,
	       state__store_conj(ValType, sub, Type, State2);
	     false -> 
	       state__store_conj(ValType, sub, t_integer(), State2)
	   end
       end, t_any()};
    'case' ->
      Arg = case_arg(Tree),
      Clauses = filter_match_fail(case_clauses(Tree)),
      {State1, ArgVar} = traverse(Arg, DefinedVars, State),
      handle_clauses(Clauses, mk_var(Tree), ArgVar, DefinedVars, State1);
    call ->
      handle_call(Tree, DefinedVars, State);
    'catch' ->
      %% XXX: Perhaps there is something to say about this.
      {State, mk_var(Tree)};
    cons ->
      Hd = cons_hd(Tree),
      Tl = cons_tl(Tree),

      {State1, [HdVar, TlVar]} = traverse_list([Hd, Tl], DefinedVars, State),
      ConsVar = mk_var(Tree),
      ConsType = mk_fun_var(fun(Map)->
				t_cons(lookup_type(HdVar, Map), 
				       lookup_type(TlVar, Map))
			    end, [HdVar, TlVar]),
      
      HdType = mk_fun_var(fun(Map)->
			      Cons = lookup_type(ConsVar, Map),
			      case t_is_cons(Cons) of
				false -> t_any();
				true -> t_cons_hd(Cons)
			      end
			  end, [ConsVar]),
      TlType = mk_fun_var(fun(Map)->
			      Cons = lookup_type(ConsVar, Map),
			      case t_is_cons(Cons) of
				false -> t_any();
				true -> t_cons_tl(Cons)
			      end
			  end, [ConsVar]),
  
      State2 = state__store_conj_lists([HdVar, TlVar, ConsVar], sub, 
				       [HdType, TlType, ConsType], 
				       State1),
      {State2, ConsVar};
    'fun' ->
      Body = fun_body(Tree),
      Vars = fun_vars(Tree),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      State0 = state__new_constraint_context(State),
      FunFailType = t_fun(duplicate(length(Vars), t_none()), t_none()),
      State2 = 
	try	
	  case state__add_prop_constrs(Tree, State0) of
	    not_called -> 
	      state__store_conj(mk_var(Tree), eq, FunFailType, State0);
	    State1 ->
	      {BodyState, BodyVar} = traverse(Body, DefinedVars1, State1),
	      state__store_conj(mk_var(Tree), eq, 
				t_fun(mk_var_list(Vars), BodyVar), BodyState)
	  end
	catch
	  throw:error -> 
	    state__store_conj(mk_var(Tree), eq, FunFailType, State0)
	end,
      Cs = state__cs(State2),
      State3 = state__store_constrs(mk_var(Tree), Cs, State2),
      Ref = mk_constraint_ref(mk_var(Tree), get_deps(Cs), 'fun'),
      OldCs = state__cs(State),
      State4 = state__new_constraint_context(State3),
      State5 = state__store_conj_list([OldCs, Ref], State4),
      State6 = state__store_fun_arity(Tree, State5),
      {State6, mk_var(Tree)};
    'let' ->
      Vars = let_vars(Tree),
      Arg = let_arg(Tree),
      Body = let_body(Tree),
      {State1, ArgVars} = traverse(Arg, DefinedVars, State),
      State2 = state__store_conj(t_product(mk_var_list(Vars)), eq, 
				 ArgVars, State1),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      traverse(Body, DefinedVars1, State2);
    letrec ->
      Defs = letrec_defs(Tree),
      Body = letrec_body(Tree),
      Funs = [Fun || {_Var, Fun} <- Defs],
      Vars = [Var || {Var, _Fun} <- Defs],
      
      State1 = state__store_funs(Vars, Funs, State),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      {State2, _} = traverse_list(Funs, DefinedVars1, State1),
      traverse(Body, DefinedVars1, State2);
    literal ->      
       C = t_from_term(concrete(Tree)),
       case t_is_atom(C) of 
	   true -> {State, C#c{ann=literal_ann(Tree)}};
	   false -> {State, C}
       end;       
     module ->
      Defs = module_defs(Tree),
      Funs = [Fun || {_Var, Fun} <- Defs],
      Vars = [Var || {Var, _Fun} <- Defs],
      DefinedVars1 = add_def_list(Vars, DefinedVars),      
      State1 = state__store_funs(Vars, Funs, State),
      FoldFun = fun(Fun, AccState) ->
		    {S, _} = traverse(Fun, DefinedVars1,
				      state__new_constraint_context(AccState)),
		    S
		end,
      lists:foldl(FoldFun, State1, Funs);
    primop ->
      case atom_val(primop_name(Tree)) of
	match_fail -> throw(error);
	raise -> throw(error);
	Other -> erlang:fault({'Unsupported primop', Other})
      end;
    'receive' ->
      Clauses = filter_match_fail(receive_clauses(Tree)),
      Timeout = receive_timeout(Tree),
      case is_c_atom(Timeout) andalso atom_val(Timeout) =:= infinity of
	false ->
	  Action = receive_action(Tree),
	  {State1, TimeoutVar} = traverse(Timeout, DefinedVars, State),
	  TimeoutType = t_sup(t_from_term(infinity), t_integer()),
	  State2 = state__store_conj(TimeoutVar, sub, TimeoutType, State1),
	  handle_clauses(Clauses, mk_var(Tree), [], Action, DefinedVars,State2);
	true ->
	  handle_clauses(Clauses, mk_var(Tree), [], DefinedVars, State)
      end;
    seq ->
      Body = seq_body(Tree),
      Arg = seq_arg(Tree),
      {State1, _} = traverse(Arg, DefinedVars, State),
      traverse(Body, DefinedVars, State1);
    'try' ->
      Arg = try_arg(Tree),
      Vars = try_vars(Tree),
      EVars = try_evars(Tree),
      Body = try_body(Tree),
      Handler = try_handler(Tree),

      State1 = state__new_constraint_context(State),
      {ArgBodyState, BodyVar} =
	try 
	  {State2, ArgVar} = traverse(Arg, DefinedVars, State1),
	  DefinedVars1 = add_def_list(Vars, DefinedVars),
	  {State3, BodyVar1} = traverse(Body, DefinedVars1, State2),
	  State4 = state__store_conj(t_product(mk_var_list(Vars)), eq, ArgVar,
				     State3),
	  {state__store_conj(mk_var(Tree), eq, BodyVar1, State4), BodyVar1}
	catch
	  throw:error -> {State1, t_none()}
	end,
      State6 = state__new_constraint_context(ArgBodyState),
      {HandlerState, HandlerVar} =
	try
	  DefinedVars2 = add_def_list([X || X <- EVars, is_c_var(X)], 
				      DefinedVars),
	  {State7, HandlerVar1} = traverse(Handler, DefinedVars2, State6),
	  {state__store_conj(mk_var(Tree), eq, HandlerVar1, State7), 
	   HandlerVar1}
	catch
	  throw:error -> {State6, t_none()}	    
	end,
      ArgBodyCs = state__cs(ArgBodyState),
      HandlerCs = state__cs(HandlerState),
      {NewCs, ReturnVar} =
	case {state__has_constraints(ArgBodyState), 
	      state__has_constraints(HandlerState)} of
	  {true, true} ->
	    Disj = mk_disj_constraint_list([ArgBodyCs, HandlerCs]),
	    {Disj, mk_var(Tree)};
	  {true, false} ->
	    {ArgBodyCs, BodyVar};
	  {false, true} ->
	    {HandlerCs, HandlerVar};
	  {false, false} ->
	    ?debug("Throw failed\n", []),
	    throw(error)
	end,
      OldCs = state__cs(State),
      Conj = mk_conj_constraint_list([OldCs, NewCs]),
      NewState1 = state__new_constraint_context(HandlerState),
      NewState2 = state__store_conj(Conj, NewState1),
      {NewState2, ReturnVar};
    tuple ->
      Elements = tuple_es(Tree),
      {State1, EVars} = traverse_list(Elements, DefinedVars, State),
      %% We have the same basic problem as in products, but we want to
      %% make sure that everything that can be used as tags for the
      %% disjoint unions stays in the tuple.
      {NewEvars, State2} = 
	lists:mapfoldl(fun(Var, AccState) ->
			   case t_has_var(Var) of
			     true ->
			       {AccState1, NewVar} = state__mk_var(AccState),
			       AccState2 = state__store_conj(Var, eq, NewVar,
							     AccState1),
			       {NewVar, AccState2};
			     false ->
			       {Var, AccState}
			   end
		       end, State1, EVars),
      {State2, t_tuple(NewEvars)};
    values ->
      %% We can get into trouble when unifying products that have the
      %% same element appearing several times. Handle these cases by
      %% introducing fresh variables and constraining them to be equal
      %% to the original ones. This is similar to what happens in
      %% pattern matching where the matching is done on fresh
      %% variables and guards assert that the matching is correct.
      Elements = values_es(Tree),
      {State1, EVars} = traverse_list(Elements, DefinedVars, State),
      Arity = length(EVars),
      Unique = length(ordsets:from_list(EVars)),
      case Arity =:= Unique of
	true -> {State1, t_product(EVars)};
	false ->
	  {State2, Vars} = state__mk_vars(Arity, State1),
	  State3 = state__store_conj_lists(Vars, eq, EVars, State2),
	  {State3, t_product(Vars)}
      end;
    var ->
      case is_def(Tree, DefinedVars) of
	true -> {State, mk_var(Tree)};
	false ->
	  %% If we are analyzing sccs this can be a function variable.
	  case state__lookup_undef_var(Tree, State) of
	    error -> erlang:fault({'Undefined variable', Tree});
	    {ok, Type} -> {State, Type}
	  end
      end;
    Other ->
      erlang:fault({'Unsupported type', Other})
  end.

traverse_list(Trees, DefinedVars, State) ->
  traverse_list(Trees, DefinedVars, State, []).

traverse_list([Tree|Tail], DefinedVars, State, Acc) ->
  {State1, Var} = traverse(Tree, DefinedVars, State),
  traverse_list(Tail, DefinedVars, State1, [Var|Acc]);
traverse_list([], _DefinedVars, State, Acc) ->
  {State, lists:reverse(Acc)}.

add_def(Var, Set) ->
  sets:add_element(get_label(Var), Set).

add_def_list([H|T], Set) ->
  add_def_list(T, add_def(H, Set));
add_def_list([], Set) ->
  Set.

add_def_from_tree(T, DefinedVars) ->
  Vars = cerl_trees:fold(fun(X, Acc) ->
			     case is_c_var(X) of
			       true -> [X|Acc];
			       false -> Acc
			     end
			 end, [], T),
  add_def_list(Vars, DefinedVars).

add_def_from_tree_list([H|T], DefinedVars) ->
  add_def_from_tree_list(T, add_def_from_tree(H, DefinedVars));
add_def_from_tree_list([], DefinedVars) ->
  DefinedVars.

is_def(Var, Set) ->
  sets:is_element(get_label(Var), Set).
  
%%________________________________________
%%
%% Call
%%

handle_call(Call, DefinedVars, State) ->   
  Args = call_args(Call),
  Mod = call_module(Call),
  Fun = call_name(Call),
  Dst = mk_var(Call),
  case is_c_atom(Mod) andalso is_c_atom(Fun) of
    true ->
      M = atom_val(Mod),
      F = atom_val(Fun),
      M1 = t_module_atom(M,literal_ann(Mod)),
      F1 = t_function_atom(F, literal_ann(Fun)),
     Call_Loc = case call_ann(Call) of 
 		     [_H,H0|_T] -> H0;
 		     _ -> 0
 		 end,
      M_Loc = case literal_ann(Mod) of 
		  [H1|_T1] -> H1;
		  _      -> 0
	      end,
      F_Loc = case literal_ann(Fun) of
		   [H2|_T2] -> H2;
		  _  ->0
	      end,
      case {M_Loc, Call_Loc} of
	  {{L1, C1}, {L2, C2}} ->
		  if (L1 < L2) or ((L1==L2) and ((C2-C1) > length(M)))
				   -> atom_collector ! {self(), add, M1};  
		     true -> ok
		  end;
	  _ -> ok
      end,
      case {F_Loc, Call_Loc} of
	  {{_L3, _C3}, {_L4, _C4}} ->
		  if F_Loc < Call_Loc -> atom_collector ! {self(), add, F1};
		     true -> ok
		  end;
	  _ -> ok
      end,
      A = length(Args),      
      {State1, ArgVars} = traverse_list(Args, DefinedVars, State),
      case state__lookup_name({M, F, A}, State) of
	error ->
	  case get_bif_constr({M, F, A}, Dst, ArgVars) of
	    none -> 
	      get_plt_constr({M, F, A}, Dst, ArgVars, State1);
	    C -> 
	      {state__store_conj(C, State1), Dst}
	  end;
	{ok, Var} ->
	  %% This is part of the scc currently analyzed.
	  %% Intercept and change this to an apply instead.
	  ?debug("Found the call to ~w\n", [{M, F, A}]),
	  Label = get_label(Call),
	  Apply = ann_c_apply([{label, Label}], Var, Args),
	  get_label(Apply),
	  traverse(Apply, DefinedVars, State)
      end;
    false ->
      {State1, MF} = traverse_list([Mod, Fun], DefinedVars, State),
      {state__store_conj_lists(MF, sub, [t_module_atom(), t_function_atom()], State1), Dst}
  end.


get_bif_constr({erlang, Op, 2}, Dst, [Arg1, Arg2]) when ((Op =:= '+') or 
							 (Op =:= '-') or 
							 (Op =:= '*')) ->
  DstFun = 
    fun(Map)->
	Arg1Type = lookup_type(Arg1, Map),
	Arg2Type = lookup_type(Arg2, Map),
	case t_is_integer(Arg1Type) andalso t_is_integer(Arg2Type) of
	  true ->
	    Vals1 = t_number_vals(Arg1Type),
	    Vals2 = t_number_vals(Arg2Type),
	    case (Vals1 =:= any) orelse (Vals2 =:= any) of
	      true -> t_integer();
	      false ->
		AllCombs = [{X, Y} || X <- Vals1, Y <- Vals2],
		AllRes = [eval_arith(Op, X, Y) || {X, Y} <- AllCombs],
		t_integers(AllRes)
	    end;
	  false ->
	    case t_is_float(Arg1Type) orelse t_is_float(Arg2Type) of
	      true -> t_float();
	      false -> t_number()
	    end
	end
    end,
  DstFunVar = mk_fun_var(DstFun, [Arg1, Arg2]),
  
  ArgFun = 
    fun(A, Pos) ->
	F = 
	  fun(Map)->
	      DstType = lookup_type(Dst, Map),
	      AType = lookup_type(A, Map),
	      case t_is_integer(DstType) of
		true ->
		  case t_is_integer(AType) of
		    true -> 
		      DstVals = t_number_vals(DstType),
		      AVals = t_number_vals(AType),
		      case (DstVals =:= any) orelse (AVals =:= any) of
			true -> t_integer();
			false ->
			  AllCombs = [{X, Y} || X <- DstVals, Y <- AVals],
			  AllRes0 = [eval_inv_arith(Op, X, Y, Pos) 
				     || {X, Y} <- AllCombs],
			  case lists:flatten(AllRes0) of
			    [] -> t_none();
			    AllRes ->
			      case lists:any(fun(any)->true;
						(_) -> false
					     end, AllRes) of
				true -> t_integer();
				false -> t_integers(AllRes)
			      end
			  end
		      end;
		    false  ->
		      t_integer()
		  end;
		false ->
		  case t_is_integer(DstType) of
		    true -> t_integer();
		    false -> t_number()
		  end
	      end
	  end,
	mk_fun_var(F, [Dst, A])
    end,
  Arg1FunVar = ArgFun(Arg2, 2),
  Arg2FunVar = ArgFun(Arg1, 1),
  mk_conj_constraint_list([mk_constraint(Arg1, sub, Arg1FunVar),
			   mk_constraint(Arg2, sub, Arg2FunVar),
			   mk_constraint(Dst, sub, DstFunVar)]);
get_bif_constr({erlang, is_atom, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_atom());
get_bif_constr({erlang, is_binary, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_binary());
get_bif_constr({erlang, is_boolean, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_bool());
get_bif_constr({erlang, is_float, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_float());
get_bif_constr({erlang, is_function, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_fun());
get_bif_constr({erlang, is_function, 2}, Dst, [Fun, Arity]) ->
  ArgFun = fun(Map) ->
	       DstType = lookup_type(Dst, Map),
	       case t_is_atom(true, DstType) of
		 true -> 
		   ArityType = lookup_type(Arity, Map),
		   case t_number_vals(ArityType) of
		     any -> t_fun();
		     Vals -> t_sup([t_fun(X, t_any())
				    || X <- Vals])
		   end;
		 false -> t_any()
	       end
	   end,
  ArgV = mk_fun_var(ArgFun, [Dst, Arity]),
  mk_conj_constraint_list([mk_constraint(Dst, sub, t_bool()),
			   mk_constraint(Arity, sub, t_integer()),
			   mk_constraint(Fun, sub, ArgV)]);
get_bif_constr({erlang, is_integer, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_integer());
get_bif_constr({erlang, is_list, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_cons());
get_bif_constr({erlang, is_number, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_number());
get_bif_constr({erlang, is_pid, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_pid());
get_bif_constr({erlang, is_port, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_port());
get_bif_constr({erlang, is_reference, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_ref());
get_bif_constr({erlang, is_tuple, 1}, Dst, [Arg]) ->
  get_bif_test_constr(Dst, Arg, t_tuple());
get_bif_constr({M, F, A}, Dst, Args) ->
  GenType = refac_erl_bif_types:type(M, F, A),
  case t_is_none(GenType) of
    true -> ?debug("Bif: ~w failed\n", [{M, F, A}]), throw(error);
    false ->
      ArgTypes = refac_erl_bif_types:arg_types(M, F, A),
      ReturnType = mk_fun_var(fun(Map)-> 
				  TmpArgTypes = lookup_type_list(Args, Map),
				  refac_erl_bif_types:type(M, F, A, TmpArgTypes)
			      end, Args),
      case ArgTypes =:= any of
	true -> 
	  case t_is_any(GenType) of
	    true -> 
	      none;
	    false ->
	      mk_constraint(Dst, sub, ReturnType)
	  end;
	false -> 
	  Cs = mk_constraints(Args, sub, ArgTypes),
	  mk_conj_constraint_list([mk_constraint(Dst, sub, ReturnType)|Cs])
      end
  end.

eval_arith('+', X, Y) -> X + Y;
eval_arith('*', X, Y) -> X * Y;
eval_arith('-', X, Y) -> X - Y.

eval_inv_arith('+', Dst, A, _Pos) -> Dst - A;
eval_inv_arith('*', 0, 0, _Pos) -> any;
eval_inv_arith('*', _Dst, 0, _Pos) -> [];
eval_inv_arith('*', Dst, A, _Pos) -> Dst div A;
eval_inv_arith('-', Dst, A, 1) -> A - Dst;
eval_inv_arith('-', Dst, A, 2) -> A + Dst.

get_bif_test_constr(Dst, Arg, Type) ->
  ArgFun = fun(Map) ->
	       DstType = lookup_type(Dst, Map),
	       case t_is_atom(true, DstType) of
		 true -> Type;
		 false -> t_any()
	       end
	   end,
  ArgV = mk_fun_var(ArgFun, [Dst]),
  mk_conj_constraint_list([mk_constraint(Dst, sub, t_bool()),
			   mk_constraint(Arg, sub, ArgV)]).


get_plt_constr(MFA, Dst, ArgVars, State) ->
  Plt = state__plt(State),
  case dialyzer_plt:lookup(Plt, MFA) of
    none -> {State, Dst};
    {value, {RetType, ArgTypes}} ->
      {state__store_conj_lists([Dst|ArgVars], sub, [RetType|ArgTypes], State), 
       Dst}
  end.

filter_match_fail([Clause]) ->
  Body = clause_body(Clause),
  case type(Body) of
    primop ->
      case atom_val(primop_name(Body)) of
	match_fail -> [];
	raise -> [];
	_ -> [Clause]
      end;
    _ -> [Clause]
  end;
filter_match_fail([H|T]) ->
  [H|filter_match_fail(T)];
filter_match_fail([]) ->
  %% This can actually happen, for example in 
  %%      receive after 1 -> ok end
  [].


handle_clauses(Clauses, TopVar, Arg, DefinedVars, State) ->
  handle_clauses(Clauses, TopVar, Arg, none, DefinedVars, State).

handle_clauses([], _, _, Action, DefinedVars, State) when Action =/= none ->
  %% Can happen when a receive has no clauses, see filter_match_fail.
  traverse(Action, DefinedVars, State);
handle_clauses(Clauses, TopVar, Arg, Action, DefinedVars, State) ->
  {State1, CList} = 
    handle_clauses_1(Clauses, TopVar, Arg, DefinedVars, State, []),
  State2 = state__store_constrs_list(CList, State1),
  CRefs = [mk_constraint_ref(Id, get_deps(Cs), clause) || {Id, Cs} <- CList],
  {NewCs, NewState} =
    case Action of
      none -> 
	if CList =:= [] -> throw(error);
	   true -> {CRefs, State2}
	end;
      _ -> 
	try 
	  {State3, ActionVar} = traverse(Action, DefinedVars, State2),
	  TmpC = mk_constraint(TopVar, eq, ActionVar),
	  ActionCs = mk_conj_constraint_list([state__cs(State3),TmpC]),
	  {[ActionCs|CRefs], State3}
	catch
	  throw:error ->
	    if CList =:= [] -> throw(error);
	       true -> {CRefs, State2}
	    end
	end
    end,
  OldCs = state__cs(State),
  NewCList = mk_disj_constraint_list(NewCs),
  FinalState = state__new_constraint_context(NewState),
  {state__store_conj_list([OldCs, NewCList], FinalState), TopVar}.

handle_clauses_1([Clause|Tail], TopVar, Arg, DefinedVars, State, Acc) ->
  State1 = state__new_constraint_context(State),
  Pats = clause_pats(Clause),
  Guard = clause_guard(Clause),
  Body = clause_body(Clause),
  try 
    DefinedVars1 = add_def_from_tree_list(Pats, DefinedVars),
    {State2, PatVars} = traverse_list(Pats, DefinedVars1, State1),
    if Arg =:= [] -> State3 = State2;
       true -> State3 = state__store_conj(Arg, eq, t_product(PatVars), State2)
    end,
    State4 = handle_guard(Guard, DefinedVars1, State3),
    {State5, BodyVar} = traverse(Body, DefinedVars1, State4),
    State6 = state__store_conj(TopVar, eq, BodyVar, State5),
    Cs = state__cs(State6),
    Id = mk_var(Clause),
    handle_clauses_1(Tail, TopVar, Arg, DefinedVars, State6, [{Id, Cs}|Acc])
  catch
    throw:error -> handle_clauses_1(Tail, TopVar, Arg, DefinedVars, State, Acc)
  end;
handle_clauses_1([], _TopVar, _Arg, _DefinedVars, State, Acc) ->
  {state__new_constraint_context(State), Acc}.



%%________________________________________
%%
%% Guards
%%

handle_guard(Guard, DefinedVars, State) ->  
  try 
    handle_guard(Guard, State, DefinedVars, dict:new())
  catch
    throw:dont_know -> State
  end.


%% TODO: This has to be rewritten in a more general way.
handle_guard(Guard, State, DefinedVars, Env) ->
  %%  ?debug("Handling: ~w\n", [type(Guard)]),
  case type(Guard) of
    binary -> 
      State;    
    'case' ->      
      %% This is so far only possible in strict record tests and
      %% orelse-guards. This might have to be extended if general case
      %% statements are allowed here.
      Arg = case_arg(Guard),
      [] = values_es(Arg),
      [C1, C2] = case_clauses(Guard),
      B1 = clause_body(C1),
      B2 = clause_body(C2),
      G1 = clause_guard(C1),
      G2 = clause_guard(C2),
      case is_literal(B2) of
	true ->
	  %% Just an assert.
	  fail = concrete(B2),
	  State1 = handle_guard(G1, State, DefinedVars, Env),
	  handle_guard(B1, State1, DefinedVars, Env);
	false ->
	  %% This is handled analogous to an or.
	  State1 = state__new_constraint_context(State),
	  State2 = handle_guard(G1, State1, DefinedVars, Env),
	  State3 = handle_guard(B1, State2, DefinedVars, Env),

	  State4 = state__new_constraint_context(State3),
	  State5 = handle_guard(G2, State4, DefinedVars, Env),
	  State6 = handle_guard(B2, State5, DefinedVars, Env),

	  OldCs = state__cs(State),
	  Cs1 = state__cs(State3),
	  Cs2 = state__cs(State6),

	  Disj = mk_disj_constraint_list([Cs1, Cs2]),
	  Conj = mk_conj_constraint_list([OldCs, Disj]),
	  State7 = state__new_constraint_context(State6),
	  state__store_conj(Conj, State7)
      end;
    cons ->
      %% XXX: Might want to handle the elements
      State;
    literal ->
      State;
    'try' ->
      State1 = handle_guard(try_arg(Guard), State, DefinedVars, Env),
      handle_guard(try_body(Guard), State1, DefinedVars, Env);
    tuple ->
      %% XXX: Might want to handle the elements
      State;
    'let' ->
      Arg = let_arg(Guard),
      [Var] = let_vars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      NewEnv = dict:store(get_label(Var), Arg, Env),      
      handle_guard(let_body(Guard), State, add_def(Var, DefinedVars), NewEnv);
    var ->
      %%?debug("Looking for: ~w...", [Guard]),
      case dict:find(get_label(Guard), Env) of
	error -> 
	  %?debug("Did not find it\n", []),
	  State;
	{ok, Tree} -> 
	  %?debug("Found it\n", []),
	  handle_guard(Tree, State, DefinedVars, Env)
      end;
    call ->
      Args = call_args(Guard),
      M = atom_val(call_module(Guard)),
      F = atom_val(call_name(Guard)),
      A = length(Args),      
      case {M, F, A} of
	{erlang, is_atom, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_atom(), State);
	{erlang, is_boolean, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_bool(), State);
	{erlang, is_binary, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_binary(), State);
	{erlang, is_float, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_float(), State);
	{erlang, is_function, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_fun(), State);
	{erlang, is_function, 2} ->
	  [Fun, Arity] = Args,
	  State1 = state__store_conj(mk_var(Arity), sub, t_integer(), State),
	  ArityVar = mk_var(Arity),
	  ArgFun = fun(Map) ->
		       ArityType = lookup_type(ArityVar, Map),
		       case t_number_vals(ArityType) of
			 any -> t_fun();
			 Vals -> t_sup([t_fun(X, t_any())
					|| X <- Vals])
		       end
		   end,
	  ArgV = mk_fun_var(ArgFun, [ArityVar]),
	  state__store_conj(mk_var(Fun), sub, ArgV, State1);
	{erlang, is_integer, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_integer(), State);
	{erlang, is_list, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_pos_improper_list(), State);
	{erlang, is_number, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_number(), State);
	{erlang, is_pid, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_pid(), State);
	{erlang, is_port, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_port(), State);
	{erlang, internal_is_record, 3} ->
	  [Rec, Tag, Arity] = Args,
	  TagType = t_from_term(atom_val(Tag)),
	  OtherEs = duplicate(int_val(Arity) - 1, t_any()),
	  TupleType = t_tuple([TagType|OtherEs]),
	  state__store_conj(mk_var(Rec), sub, TupleType, State);
	{erlang, is_record, 3} ->
	  [Rec, Tag, Arity] = Args,
	  TagType = t_from_term(atom_val(Tag)),
	  OtherEs = duplicate(int_val(Arity) - 1, t_any()),
	  TupleType = t_tuple([TagType|OtherEs]),
	  state__store_conj(mk_var(Rec), sub, TupleType, State);
	{erlang, is_reference, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_ref(), State);
	{erlang, is_tuple, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_tuple(), State);
	{erlang, '=:=', 2} ->
	  [Arg1, Arg2] = Args,	  
	  State1 = handle_guard(Arg1, State, DefinedVars, Env),
	  State2 = handle_guard(Arg2, State1, DefinedVars, Env),
	  state__store_conj(mk_var(Arg1), eq, mk_var(Arg2), State2);
%%%	{erlang, '==', 2} ->
	{erlang, 'and', 2} ->
	  [Arg1, Arg2] = Args,
	  State1 = handle_guard(Arg1, State, DefinedVars, Env),
	  handle_guard(Arg2, State1, DefinedVars, Env);
	{erlang, 'or', 2} ->
	  [Arg1, Arg2] = Args,
	  State1 = state__new_constraint_context(State),
	  State2 = handle_guard(Arg1, State1, DefinedVars, Env),
	  State3 = state__new_constraint_context(State2),
	  State4 = handle_guard(Arg2, State3, DefinedVars, Env),
	  State5 = state__new_constraint_context(State4),
	  
	  OldCs = state__cs(State),
	  Cs1 = state__cs(State2),
	  Cs2 = state__cs(State4),

	  Disj = mk_disj_constraint_list([Cs1, Cs2]),
	  Conj = mk_conj_constraint_list([OldCs, Disj]),
	  state__store_conj(Conj, State5);
	{erlang, 'not', 1} ->
	  throw(dont_know);
	_Other ->
	  State1 = 
	    lists:foldl(fun(X, AccMap)->
			    handle_guard(X, AccMap, DefinedVars, Env)
			end,
			State, Args),
	  {State2, _} = handle_call(Guard, DefinedVars, State1),
	  State2
      end
  end.

%%=============================================================================
%%
%%  Constraint solver.
%%
%%=============================================================================

solve_fun(Fun, FunMap, State) ->
  Cs = state__get_cs(Fun, State),
  Deps = get_deps(Cs),
  Ref = mk_constraint_ref(Fun, Deps, 'fun'),
  %% Note that functions are always considered to succeed.
  {ok, MapDict, NewMap} = solve_ref_or_list(Ref, FunMap, State),
  NewType = lookup_type(Fun, NewMap),
  case state__get_rec_var(Fun, State) of
    error ->     NewFunMap1 = FunMap;
    {ok, Var} -> NewFunMap1 = enter_type(Var, NewType, FunMap)
  end,
  NewFunMap = enter_type(Fun, NewType, NewFunMap1),
  {NewFunMap, MapDict}.

solve_scc(Scc, FunMap, State) ->
  Vars0 = [{Fun, state__get_rec_var(Fun, State)}||Fun <- Scc],  
  Vars = [Var || {_, {ok, Var}} <- Vars0],
  Funs = [Fun || {Fun, {ok, _}} <- Vars0],
  Types = unsafe_lookup_type_list(Funs, FunMap),
  RecTypes = [t_limit(Type, ?TYPE_LIMIT) || Type <- Types],
  CleanFunMap = lists:foldl(fun(Fun, AccFunMap)->
			     dict:erase(t_var_name(Fun), AccFunMap)
			 end, FunMap, Scc),
  FunMap1 = enter_type_lists(Vars, RecTypes, CleanFunMap),
  SolveFun = fun(X, Y)-> scc_fold_fun(X, Y, State)end,
  lists:foldl(SolveFun, {FunMap1, []}, Scc).

scc_fold_fun(F, {FunMap, Acc}, State) ->
  Deps = get_deps(state__get_cs(F, State)),
  Cs = mk_constraint_ref(F, Deps, 'fun'),
  %% Note that functions are always considered to succeed.
  {ok, MapDict, Map} = solve_ref_or_list(Cs, FunMap, State),
  NewType0 = unsafe_lookup_type(F, Map),
  NewType = t_limit(NewType0, ?TYPE_LIMIT),
  case state__get_rec_var(F, State) of
    {ok, R} ->
      NewFunMap = enter_type(R, NewType, enter_type(F, NewType, FunMap));
    error ->
      NewFunMap = enter_type(F, NewType, FunMap)
  end,
  ?debug("Done solving for function ~w\n", [debug_lookup_name(F)]),
  {NewFunMap, [MapDict|Acc]}.

solve_ref_or_list(C, Map, State) ->
  case solve_ref_or_list(C, Map, dict:new(), State) of
    {error, _} -> 
      %% Cannot happen for functions.
      error;
    {ok, MapDict, Map1} -> 
      {ok, MapDict, Map1}
  end.

solve_ref_or_list(#constraint_ref{id=Id, deps=Deps, type='fun'}, 
		  Map, MapDict, State) ->
  {OldLocalMap, Check} = 
    case dict:find(Id, MapDict) of
      error -> {dict:new(), false};
      {ok, M} -> {M, true}
    end,
  ?debug("Checking ref to fun: ~w\n", [debug_lookup_name(Id)]),
  case Check andalso maps_are_equal(OldLocalMap, Map, 
				    ordsets:del_element(Id, Deps)) of
    true -> 
      ?debug("Equal\n", []),
      {ok, MapDict, Map};
    false ->
      ?debug("Not equal. Solving\n", []),
      Cs = state__get_cs(Id, State),
      Res = 
	case state__is_self_rec(Id, State) of
	  true -> solve_self_recursive(Cs, Map, MapDict, Id, t_none(), State);
	  false -> solve_ref_or_list(Cs, Map, MapDict, State)
	end,
      case Res of
	{error, NewMapDict} ->	  
	  ?debug("Error solving for function ~p\n", [debug_lookup_name(Id)]),
	  Arity = state__fun_arity(Id, State),
	  %% HERE
	  FunType = t_fun(duplicate(Arity, t_none()), t_none()),
	  %%FunType = t_fun(Arity, t_none()),
	  NewMap1 = enter_type(Id, FunType, Map),
	  NewMap2 =
	    case state__get_rec_var(Id, State) of
	      {ok, Var} -> enter_type(Var, FunType, NewMap1);
	      error -> NewMap1
	    end,
	  {ok, dict:store(Id, NewMap2, NewMapDict), NewMap2};
	{ok, NewMapDict, NewMap} ->
	  ?debug("Done solving fun: ~p\n", [debug_lookup_name(Id)]),
	  FunType = lookup_type(Id, NewMap),
	  NewMap1 = enter_type(Id, FunType, Map),
	  NewMap2 =
	    case state__get_rec_var(Id, State) of
	      {ok, Var} -> enter_type(Var, FunType, NewMap1);
	      error -> NewMap1
	    end,
	  {ok, dict:store(Id, NewMap2, NewMapDict), NewMap2}
      end
  end;
solve_ref_or_list(#constraint_ref{id=Id, deps=Deps, type=clause}, 
		  Map, MapDict, State) ->
  {OldLocalMap, Check} = 
    case dict:find(Id, MapDict) of
      error -> {dict:new(), false};
      {ok, M} -> {M, true}
    end,
  ?debug("Checking ref to clause: ~w\n", [Id]),
  case Check andalso maps_are_equal(OldLocalMap, Map, Deps) of
    true -> 
      ?debug("Equal\n", []),
      {ok, MapDict, Map};
    false ->
      ?debug("Not equal. Solving\n", []),
      Cs = state__get_cs(Id, State),
      case solve_ref_or_list(Cs, Map, MapDict, State) of
	{error, NewMapDict} ->	  
	  ?debug("Failed solving ~w\n", [Id]),
	  {error, dict:erase(Id, NewMapDict)};
	{ok, NewMapDict, NewMap} ->
	  ?debug("Done solving ~w\n", [Id]),
	  {ok, dict:store(Id, NewMap, NewMapDict), NewMap}
      end
  end;
solve_ref_or_list(#constraint_list{type=Type, list=Cs, deps = Deps, id=Id}, 
		  Map, MapDict, State) ->
  {OldLocalMap, Check} = 
    case dict:find(Id, MapDict) of
      error -> {dict:new(), false};
      {ok, M} -> 
	case Type of
	  conj -> {dict:new(), false};
	  disj -> {M, true}
	end
    end,
  case Check andalso maps_are_equal(OldLocalMap, Map, Deps) of
    true -> {ok, MapDict, Map};
    false -> solve_clist(Cs, Type, Id, Deps, MapDict, Map, State)
  end.

solve_self_recursive(Cs, Map, MapDict, Id, RecType0, State) ->
  ?debug("Solving self recursive ~w\n", [debug_lookup_name(Id)]),
  {ok, RecVar} = state__get_rec_var(Id, State),
  ?debug("OldRecType ~s\n", [t_to_string(RecType0)]),
  RecType = t_limit(RecType0, ?TYPE_LIMIT),
  Map1 = enter_type(RecVar, RecType, dict:erase(t_var_name(Id), Map)),
  ?debug("\tMap in: ~p\n",[[{X, format_type(Y)}||{X, Y}<-dict:to_list(Map1)]]),
  case solve_ref_or_list(Cs, Map1, MapDict, State) of
    {error, _} = Error -> Error;
    {ok, NewMapDict, NewMap} ->
      ?debug("\tMap: ~p\n",[[{X, format_type(Y)}
			     ||{X, Y}<-dict:to_list(NewMap)]]),
      NewRecType = unsafe_lookup_type(Id, NewMap),
      case t_is_equal(NewRecType, RecType0) of
	true -> 	  
	  {ok, NewMapDict, enter_type(RecVar, NewRecType, NewMap)};
	false ->
	  solve_self_recursive(Cs, Map, MapDict, Id, NewRecType, State)
      end
  end.

solve_clist(Cs, conj, Id, Deps, MapDict, Map, State) ->
  ?debug("Solving conj list ~w\n", [Id]),
  case solve_cs(Cs, Map, MapDict, State) of 
    {error, _} = Error ->
      Error;
    {ok, NewCs, NewDict, Map1} -> 
      case maps_are_equal(Map, Map1, Deps) of
	true -> 
	  ?debug("Done solving conj list ~w\n", [Id]),
	  {ok, NewDict, Map1};
	false -> 
	  ?debug("Looping\n", []),
	  solve_clist(NewCs, conj, Id, Deps, NewDict, Map1, State)
      end
  end;
solve_clist(Cs, disj, Id, _Deps, MapDict, Map, State) ->
  ?debug("Solving disj list ~w\n", [Id]),
  Fun = fun(C, Dict) ->
	    case solve_ref_or_list(C, Map, Dict, State) of
	      {error, NewDict} -> {error, NewDict};
	      {ok, NewDict, NewMap} -> {{ok, NewMap}, NewDict}
	    end
	end,  
  {Maps, NewDict} = lists:mapfoldl(Fun, MapDict, Cs),
  case [X || {ok, X} <- Maps] of
    [] -> {error, NewDict};
    MapList -> 
      ?debug("Done solving disj list ~w\n", [Id]),
      NewMap = join_maps(MapList),      
      {ok, dict:store(Id, NewMap, NewDict), NewMap}
  end.

solve_cs(List, Map, MapDict, State) ->
  solve_cs(List, Map, MapDict, State, []).

solve_cs([C = #constraint_ref{}|Tail], Map, MapDict, State, Acc) ->
  case solve_ref_or_list(C, Map, MapDict, State) of
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, State, [C|Acc]);
    {error, NewMapDict} -> {error, NewMapDict}
  end;
solve_cs([C = #constraint_list{type=conj, list=List}|Tail], Map, 
	 MapDict, State, Acc) ->
  case solve_ref_or_list(List, Map, MapDict, State) of
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, State, [C|Acc]);
    {error, NewMapDict} -> {error, NewMapDict}
  end;
solve_cs([C = #constraint_list{type=disj}|Tail], Map, MapDict, State, Acc) ->
  case solve_ref_or_list(C, Map, MapDict, State) of
    {error, NewMapDict} -> {error, NewMapDict};
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, State, [C|Acc])
  end;
solve_cs([C = #constraint{lhs=Lhs, rhs=Rhs, op=Op}|Tail],
	 Map, MapDict, State, Acc) ->  
  case solve_one_c(C, Map) of
    error ->
      ?debug("+++++++++++\nFailed: ~s :: ~s ~w ~s :: ~s\n+++++++++++\n",
	     [format_type(Lhs), format_type(lookup_type(Lhs, Map)), Op,
	      format_type(Rhs), format_type(lookup_type(Rhs, Map))]),
      {error, MapDict};
    {ok, NewMap} -> 
 %%      State1 = case t_is_atom(Inf)  of 
%% 		   true -> state_add_atom(Inf,State);
%% 		   false -> State
%% 	       end,
      case is_literal(Rhs) of
	true -> 
	  solve_cs(Tail, NewMap, MapDict, State, Acc);
	false -> 
	  case is_literal(Lhs) andalso (Op =:= eq) of
	    true ->
	      solve_cs(Tail, NewMap, MapDict, State, Acc);
	    false ->
	      solve_cs(Tail, NewMap, MapDict, State, [C|Acc])
	  end
      end
  end;
solve_cs([], Map, MapDict, _State, Acc) ->
  {ok, Acc, MapDict, Map}.

solve_one_c(C, Map) ->
  Lhs = C#constraint.lhs,
  Rhs = C#constraint.rhs,
  Op  = C#constraint.op,
  LhsType = lookup_type(Lhs, Map),
  RhsType = lookup_type(Rhs, Map),
  Inf = t_inf(LhsType, RhsType),
  ?debug("Solving: ~s :: ~s ~w ~s :: ~s\n\tInf: ~s\n",
	 [format_type(Lhs), format_type(LhsType), Op,
	  format_type(Rhs), format_type(RhsType), format_type(Inf)]),
  case t_is_none(Inf) of 
    true -> error;
    false ->
      %%io:format("Constraint:\n~p\n", [C]),
      %%io:format("Inf:\n~p\n", [Inf]),
      case solve_subtype(Lhs, Inf, Map) of
	error -> error;
	{ok, Map1} ->	 
 	  case t_is_atom(Inf)  of  %% module_atom(Inf) or t_is_function_atom(Inf) of 
 	      true -> atom_collector!{self(), add,Inf};
 	      false -> ok
 	  end,
	  case Op of
	    sub -> {ok, Map1};
	    eq ->
	      case solve_subtype(Rhs, Inf, Map1) of
		error -> error;
		  {ok, Map2} -> {ok, Map2}
	      end
	  end
      end
  end.

solve_subtype(Type, Inf, Map) ->
  case is_literal(Type) of
    true -> 
      case t_is_subtype(t_from_term(concrete(Type)), Inf) of
	true -> {ok, Map};
	false -> error
      end;
    false ->
      try t_unify(Type, Inf) of
	{_, List} ->
	  {Vars, Types} = lists:unzip(List),
	  NewTypes = t_inf_lists(Types, lookup_type_list(Vars, Map)),
	  case any_none(NewTypes) of
	    true -> error;
	    false -> 
		  {ok, enter_type_lists(Vars, NewTypes, Map)}
	  end
      catch
	throw:{mismatch, _T1, _T2} -> 
	  ?debug("Mismatch between ~s and ~s\n", 
		 [t_to_string(_T1), t_to_string(_T2)]),
	  error
      end
  end.


%%% ============================================================================
%%%
%%%  Maps and types.
%%%
%%% ============================================================================

join_maps(Maps) ->
  ?debug("Joining maps:\n", []),
  [?debug("\tMap: ~p\n", [[{X, format_type(Y)}||{X, Y}<-dict:to_list(_Map)]])
   || _Map <- Maps],
  Fun = fun(T, Map1, Map2)->
	    t_sup(lookup_type(T, Map1), lookup_type(T, Map2))
	end,
  Keys = lists:foldl(fun(TmpMap, AccKeys) -> 
			 Keys1 = ordsets:from_list(dict:fetch_keys(TmpMap)),
			 ordsets:intersection(Keys1, AccKeys)
		     end, 
		     ordsets:from_list(dict:fetch_keys(hd(Maps))), tl(Maps)),
  Res = merge_maps(Maps, Fun, Keys),
  ?debug("Result:\n", []),
  ?debug("\tMap: ~p\n", [[{X, format_type(Y)}||{X, Y}<-dict:to_list(Res)]]),
  Res.

merge_maps([], _Fun, _Keys) ->
  dict:new();
merge_maps([Map], _Fun, _Keys) ->
  Map;
merge_maps([Map1, Map2|Tail], Fun, Keys) ->
  Map = merge_two_maps(Keys, Map1, Map2, Fun, dict:new()),
  merge_maps([Map|Tail], Fun, Keys).

merge_two_maps([Key|Tail], Map1, Map2, Fun, AccMap) ->
  NewAccMap = enter_type(Key, Fun(Key, Map1, Map2), AccMap),
  merge_two_maps(Tail, Map1, Map2, Fun, NewAccMap);
merge_two_maps([], _Map1, _Map2, _Fun, AccMap) ->
  AccMap.

maps_are_equal(Map1, Map2, [H|Tail]) ->
  T1 = lookup_type(H, Map1),
  T2 = lookup_type(H, Map2),
  case t_is_equal(T1, T2) of
    true -> maps_are_equal(Map1, Map2, Tail);
    false -> 
      ?debug("~w: ~s =/= ~s\n", [H, t_to_string(T1), t_to_string(T2)]),
      false      
  end;
maps_are_equal(_Map1, _Map2, []) ->
  true.


enter_type(Key, Val, Map) when is_integer(Key) ->
  ?debug("Entering ~s :: ~s\n", [format_type(t_var(Key)), format_type(Val)]),
  case t_is_any(Val) of
    true ->
      dict:erase(Key, Map);
    false ->
      dict:store(Key, t_limit(Val, ?INTERNAL_TYPE_LIMIT), Map)
  end;
enter_type(Key, Val, Map) ->
  ?debug("Entering ~s :: ~s\n", [format_type(Key), format_type(Val)]),
  case t_is_var(Key) of
    true -> 
      case t_is_any(Val) of
	true ->
	  dict:erase(t_var_name(Key), Map);
	false ->
	  dict:store(t_var_name(Key), t_limit(Val, ?INTERNAL_TYPE_LIMIT), Map)
      end;
    false ->
      Map
  end.

enter_type_lists([Key|KeyTail], [Val|ValTail], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_lists(KeyTail, ValTail, Map1);
enter_type_lists([], [], Map) ->
  Map.


lookup_type(Key, Map) ->
  lookup(Key, Map, t_any()).

lookup_type_list(List, Map) ->
  [lookup_type(X, Map)||X<-List].
      
unsafe_lookup_type(Key, Map) ->
  lookup(Key, Map, t_none()).

unsafe_lookup_type_list(List, Map) ->
  [unsafe_lookup_type(X, Map)||X<-List].


lookup(Key, Map, AnyNone) when is_integer(Key) ->
  case dict:find(Key, Map) of
    error -> AnyNone;
    {ok, Val} -> Val
  end;
lookup(#fun_var{'fun'=Fun}, Map, _AnyNone) ->
  Fun(Map);
lookup(Key, Map, AnyNone) ->
  case is_literal(Key) of
    true -> t_from_term(concrete(Key));
    false -> 
      case t_is_var(Key) of
	true ->
	  case dict:find(t_var_name(Key), Map) of
	    {ok, Val} -> t_subst(Val, Map);
	    error -> AnyNone
	  end;
	false ->
	  Res = t_subst(Key, Map),
	  t_sup(Res, Res)
      end
  end.

mk_var(Var) ->
  case is_literal(Var) of
    true -> Var;
    false -> 
      case is_c_values(Var) of
	true -> t_product(mk_var_no_lit_list(values_es(Var)));
	false -> t_var(get_label(Var))
      end
  end.

mk_var_list(List) ->
  [mk_var(X)||X<-List].

mk_var_no_lit(Var) ->
  case is_literal(Var) of
    true -> t_from_term(concrete(Var));
    false -> mk_var(Var)
  end.

mk_var_no_lit_list(List) ->
  [mk_var_no_lit(X)||X<-List].


get_label(T) ->
  case get_ann(T) of
    [{label, L} | _] -> L;
    _ -> throw({missing_label, T})
  end.

any_none([X | Xs]) ->
  case t_is_none(X) of
    true ->
      true;
    false ->
      any_none(Xs)
  end;
any_none([]) -> false.

merge_mapdicts(MapDicts, TopMap) ->
  FoldFun = fun merge_mapdicts_fold/2,
  try 
    NewMapDict1 = lists:foldl(FoldFun, hd(MapDicts), tl(MapDicts)),
    dict:store(top, TopMap, NewMapDict1)
  catch
    throw:{merge_failed, Dict1, Dict2, Key} ->
      io:format("Failed merge for key: ~p\n", [Key]),
      io:format("Dict1\n", []),
      [io:format("\t~p :: ~s\n", [X, t_to_string(T)])
       ||{X, T} <- lists:keysort(1, dict:to_list(dict:fetch(Key, Dict1)))],
      io:format("Dict2\n", []),
      [io:format("\t~p :: ~s\n", [X, t_to_string(T)])
       ||{X, T} <- lists:keysort(1, dict:to_list(dict:fetch(Key, Dict2)))],
      erlang:fault({merge_failed, Key})
  end.

merge_mapdicts_fold(Dict1, Dict2) ->
  dict:merge(fun(_Key, Val, Val) -> Val;
		(Key, _, _) -> throw({merge_failed, Dict1, Dict2, Key})
	     end,
	     Dict1, Dict2).

%%% ============================================================================
%%%
%%%  The State.
%%%
%%% ============================================================================

new_state(SCC0, NextLabel, CallGraph, Plt, PropArgs) ->
  NameMap = dict:from_list([{MFA, Var} || {MFA, {Var, _Fun}} <- SCC0]),
  SCC = [mk_var(Fun) || {_MFA, {_Var, Fun}} <- SCC0],
  #state{cs=[], callgraph=CallGraph, cmap=dict:new(), fun_arities=dict:new(),
	 fun_map=[], name_map=NameMap, next_label=NextLabel, 
	 prop_args=PropArgs, prototypes=dict:new(), plt=Plt, sccs=[SCC], atoms=[]}.

state__get_fun_prototype(Op, Arity, State = #state{prototypes=FunProt}) ->
  case t_is_fun(Op) of
    true -> {State, Op};
    false ->
      Label = t_var_name(Op),
      case dict:find(Label, FunProt) of
	error -> 
	  {State1, [Ret|Args]} = state__mk_vars(Arity+1, State),
	  Fun = t_fun(Args, Ret),
	  NewFunProt = dict:store(Label, Fun, FunProt),
	  {State1#state{prototypes=NewFunProt}, Fun};
	{ok, Fun} ->
	  {State, Fun}
      end
  end.
    
state__lookup_name(MFA, #state{name_map=NameMap}) ->
  ?debug("Looking for ~w\n", [MFA]),
  ?debug("Map is ~p\n", [dict:to_list(NameMap)]),
  dict:find(MFA, NameMap).

state__store_fun_arity(Tree, State=#state{fun_arities=Map}) ->
  Arity = length(fun_vars(Tree)),
  Id = mk_var(Tree),
  State#state{fun_arities=dict:store(Id, Arity, Map)}.

state__fun_arity(Id, #state{fun_arities=Map}) ->
  dict:fetch(Id, Map).

state__lookup_undef_var(Tree, #state{callgraph=CG, plt=Plt}) ->  
  Label = get_label(Tree),
  case dialyzer_callgraph:lookup_rec_var(Label, CG) of
    error -> error;
    {ok, MFA} -> 
      case dialyzer_plt:lookup(Plt, MFA) of
	none -> error;
	{value, {RetType, ArgTypes}} -> {ok, t_fun(ArgTypes, RetType)}
      end
  end.

   
state__sccs(#state{sccs=SCCs}) ->
  SCCs.

state__plt(#state{plt=Plt}) ->
  Plt.

state__new_constraint_context(State) ->
  State#state{cs=[]}.

state__add_prop_constrs(Tree, State = #state{prop_args=PropArgs}) ->
  Label = get_label(Tree),
  case dict:find(Label, PropArgs) of
    error -> State;
    {ok, ArgCs} ->
      case any_none(ArgCs) of
	true -> not_called;
	false ->
	  ?debug("Adding arg constrs: ~s for function ~w\n", 
		 [t_to_string(t_product(ArgCs)), 
		  debug_lookup_name(mk_var(Tree))]),
	  Args = mk_var_list(fun_vars(Tree)),
	  state__store_conj_lists(Args, sub, ArgCs, State)
      end
  end.

state__cs(#state{cs=Cs}) ->
  mk_conj_constraint_list(Cs).

state__has_constraints(#state{cs=[]}) -> false;
state__has_constraints(#state{}) -> true.

state__store_conj(C, State = #state{cs=Cs}) ->
  State#state{cs=[C|Cs]}.

state__store_conj_list([H|T], State) ->
  State1 = state__store_conj(H, State),
  state__store_conj_list(T, State1);
state__store_conj_list([], State) ->
  State.

state__store_conj(Lhs, Op, Rhs, State = #state{cs=Cs}) ->
  State#state{cs=[mk_constraint(Lhs, Op, Rhs)|Cs]}.

state__store_conj_lists(List1, Op, List2, State) ->
  {NewList1, NewList2} = strip_of_any_constrs(List1, List2),
  state__store_conj_lists_1(NewList1, Op, NewList2, State).

strip_of_any_constrs(List1, List2) ->
  strip_of_any_constrs(List1, List2, [], []).

strip_of_any_constrs([T1|Left1], [T2|Left2], Acc1, Acc2) ->
  case t_is_any(T1) orelse t_is_any(T2) of
    true -> strip_of_any_constrs(Left1, Left2, Acc1, Acc2);
    false -> strip_of_any_constrs(Left1, Left2, [T1|Acc1], [T2|Acc2])
  end;
strip_of_any_constrs([], [], Acc1, Acc2) ->
  {Acc1, Acc2}.

  
state__store_conj_lists_1([Arg1|Arg1Tail], Op, [Arg2|Arg2Tail], State) ->
  State1 = state__store_conj(Arg1, Op, Arg2, State),
  state__store_conj_lists_1(Arg1Tail, Op, Arg2Tail, State1);
state__store_conj_lists_1([], _Op, [], State) ->
  State.

state__mk_var(State = #state{next_label=NL}) ->
  {State#state{next_label=NL+1}, t_var(NL)}.
  
state__mk_vars(N, State = #state{next_label=NL}) ->
  NewLabel = NL + N,
  {State#state{next_label=NewLabel}, [t_var(X)||X<-lists:seq(NL, NewLabel-1)]}.

state__store_constrs(Id, Cs, State = #state{cmap=Dict}) ->  
  NewDict = dict:store(Id, Cs, Dict),
  State#state{cmap=NewDict}.

state__store_constrs_list([{Id, Cs}|Left], State) ->
  State1 = state__store_constrs(Id, Cs, State),
  state__store_constrs_list(Left, State1);
state__store_constrs_list([], State) ->
  State.

state__get_cs(Var, #state{cmap=Dict}) ->  
  dict:fetch(Var, Dict).

state__is_self_rec(Fun, #state{callgraph=CallGraph}) ->
  dialyzer_callgraph:is_self_rec(t_var_name(Fun), CallGraph).

state__store_funs(Vars0, Funs0, State = #state{fun_map=Map}) ->
  debug_make_name_map(Vars0, Funs0),
  Vars = mk_var_list(Vars0),
  Funs = mk_var_list(Funs0),
  NewMap = 
    lists:foldl(fun({Var, Fun}, TmpMap) -> orddict:store(Var, Fun, TmpMap)end,
		Map, lists:zip(Vars, Funs)),
  State#state{fun_map=NewMap}.

state__get_rec_var(Fun, #state{fun_map=Map}) ->
  case [X || {X, Y} <- Map, Y =:= Fun] of
    [Var] -> {ok, Var};
    [] -> error
  end.

state__finalize(State) ->
  State1 = enumerate_constraints(State),
  order_fun_constraints(State1).

%%% ============================================================================
%%%
%%%  Constraints
%%%
%%% ============================================================================


mk_constraint(Lhs, Op, Rhs) ->
  case t_is_any(Lhs) orelse t_is_any(Rhs) of
    false ->
      Deps = find_constraint_deps([Lhs, Rhs]),
      C0 = mk_constraint_1(Lhs, Op, Rhs),
      C = C0#constraint{deps=Deps},
      case Deps =:= [] of
	true ->
	  %% This constraint is constant. Solve it immediately.
	  case solve_one_c(C, dict:new()) of
	    error -> throw(error);
	    _ -> 
	      %% This is always true, keep it anyway for logistic reasons
	      C
	  end;
	false ->
	  C
      end;
    true ->
      C = mk_constraint_1(t_any(), Op, t_any()),
      C#constraint{deps=[]}
  end.

mk_fun_var(Fun, Deps0) ->
  Deps = [X || X <- Deps0, is_literal(X) =:= false],
  #fun_var{'fun'=Fun, deps=ordsets:from_list(filter_vars(Deps))}.

get_deps(#constraint{deps=D}) -> D;
get_deps(#constraint_list{deps=D}) -> D;
get_deps(#constraint_ref{deps=D}) -> D.

find_constraint_deps(List) ->
  ordsets:from_list(find_constraint_deps(List, [])).

find_constraint_deps([#fun_var{deps=Deps}|Tail], Acc) ->
  find_constraint_deps(Tail, [Deps|Acc]);
find_constraint_deps([Type|Tail], Acc) ->
  Vars = 
    case t_is_var(Type) of
      true -> [Type];
      false ->
	case t_is_fun(Type) of
	  true -> [t_fun_range(Type)|t_fun_args(Type)];
	  false ->
	    case t_is_tuple(Type) of
	      true -> 
		case t_tuple_subtypes(Type) of
		  any -> [];
		  TupleList ->
		    lists:flatten([t_tuple_args(T) || T <- TupleList])
		end;
	      false -> t_components(Type)
	    end
	end
    end,
  find_constraint_deps(Tail, [filter_vars(Vars)|Acc]);
find_constraint_deps([], Acc) ->
  lists:flatten(Acc).
		
filter_vars([Var|Tail]) ->		
  case t_is_var(Var) of
    true -> [Var|filter_vars(Tail)];
    false -> filter_vars(Tail)
  end;
filter_vars(any) ->
  [];
filter_vars(none) ->
  [];
filter_vars([]) ->
  [].

mk_constraint_1(Lhs, eq, Rhs) when Lhs < Rhs ->
  #constraint{lhs=Lhs, op=eq, rhs=Rhs};
mk_constraint_1(Lhs, eq, Rhs) ->
  #constraint{lhs=Rhs, op=eq, rhs=Lhs};
mk_constraint_1(Lhs, Op, Rhs) ->
  #constraint{lhs=Lhs, op=Op, rhs=Rhs}.  

mk_constraints([Lhs|LhsTail], Op, [Rhs|RhsTail]) ->
  [mk_constraint(Lhs, Op, Rhs)|mk_constraints(LhsTail, Op, RhsTail)];
mk_constraints([], _Op, []) ->
  [].

mk_constraint_ref(Id, Deps, Type) ->
  #constraint_ref{id=Id, deps=Deps, type=Type}.

mk_constraint_list(Type, List) ->
  List1 = ordsets:from_list(lift_lists(Type, List)),
  List2 = ordsets:filter(fun(X) -> get_deps(X) =/= [] end, List1),
  Deps = calculate_deps(List2),
  case Deps =:= [] of
    true -> mk_constraint(t_any(), eq, t_any());
    false -> #constraint_list{type = Type, list = List2, deps = Deps}
  end.

lift_lists(Type, List) ->
  lift_lists(Type, List, []).

lift_lists(Type, [#constraint_list{type=Type, list=List}|Tail], Acc) ->
  lift_lists(Type, Tail, List++Acc);
lift_lists(Type, [C|Tail], Acc) ->
  lift_lists(Type, Tail, [C|Acc]);
lift_lists(_Type, [], Acc) ->
  Acc.

update_constraint_list(CL = #constraint_list{}, List) ->
  CL#constraint_list{list=List}.

calculate_deps(List) ->
  calculate_deps(List, []).

calculate_deps([H|Tail], Acc) ->
  Deps = get_deps(H),
  calculate_deps(Tail, ordsets:union(Deps, Acc));
calculate_deps([], Acc) ->
  Acc.

mk_conj_constraint_list(List) ->
  mk_constraint_list(conj, List).

mk_disj_constraint_list([NotReallyAList]) ->
  NotReallyAList;
mk_disj_constraint_list(List) ->
  mk_constraint_list(disj, List).

enumerate_constraints(State) ->
  Cs = [mk_constraint_ref(Id, get_deps(state__get_cs(Id, State)), 'fun') 
	|| Id <- lists:flatten(state__sccs(State))],
  {_, _, NewState} = enumerate_constraints(Cs, 0, [], State),
  NewState.

enumerate_constraints([C = #constraint_ref{id=Id}|Tail], N, Acc, State) ->
  Cs = state__get_cs(Id, State),
  {[NewCs], NewN, NewState1} = enumerate_constraints([Cs], N, [], State),
  NewState2 = state__store_constrs(Id, NewCs, NewState1),  
  enumerate_constraints(Tail, NewN+1, [C|Acc], NewState2);
enumerate_constraints([C = #constraint_list{list=List}|Tail], N, Acc, State) ->
  {NewList, NewN, NewState} = enumerate_constraints(List, N, [], State),
  NewAcc = [C#constraint_list{list=NewList, id={list, NewN}}|Acc],
  enumerate_constraints(Tail, NewN+1, NewAcc, NewState);
enumerate_constraints([C = #constraint{}|Tail], N, Acc, State) ->
  enumerate_constraints(Tail, N, [C|Acc], State);
enumerate_constraints([], N, Acc, State) ->
  {lists:reverse(Acc), N, State}.

%% Put the fun ref constraints last in any conjunction since we need
%% to separate the environment from the interior of the function.
order_fun_constraints(State) ->
  Cs = [mk_constraint_ref(Id, get_deps(state__get_cs(Id, State)), 'fun') 
	|| Id <- lists:flatten(state__sccs(State))],
  order_fun_constraints(Cs, State).

order_fun_constraints([#constraint_ref{type='fun', id=Id}|Tail], State) ->
  Cs = state__get_cs(Id, State),
  {[NewCs], State1} = order_fun_constraints([Cs], [], [], State),
  NewState = state__store_constrs(Id, NewCs, State1),
  order_fun_constraints(Tail, NewState);
order_fun_constraints([], State) ->
  State.

order_fun_constraints([C=#constraint_ref{type='fun'}|Tail], Funs, Acc, State) ->
  order_fun_constraints(Tail, [C|Funs], Acc, State);
order_fun_constraints([C=#constraint_ref{id=Id}|Tail], Funs, Acc, State) ->
  Cs = state__get_cs(Id, State),
  {[NewCs], NewState1} = order_fun_constraints([Cs], [], [], State),
  NewState2 = state__store_constrs(Id, NewCs, NewState1),
  order_fun_constraints(Tail, Funs, [C|Acc], NewState2);
order_fun_constraints([C=#constraint_list{list=List}|Tail], Funs, Acc, State) ->
  {NewList, NewState} = 
    case C#constraint_list.type of
      conj -> order_fun_constraints(List, [], [], State);
      disj ->
	FoldFun = fun(X, AccState) -> 
		      {[NewX], NewAccState} = 
			order_fun_constraints([X], [], [], AccState),
		      {NewX, NewAccState}
		  end,
	lists:mapfoldl(FoldFun, State, List)
    end,
  NewAcc = [update_constraint_list(C, NewList)|Acc],
  order_fun_constraints(Tail, Funs, NewAcc, NewState);
order_fun_constraints([C = #constraint{}|Tail], Funs, Acc, State) ->
  order_fun_constraints(Tail, Funs, [C|Acc], State);
order_fun_constraints([], Funs, Acc, State) ->
  NewState = order_fun_constraints(Funs, State),
  {lists:reverse(Acc)++Funs, NewState}.


%%% ============================================================================
%%%
%%%  Utilities.
%%%
%%% ============================================================================

%% format_type(#fun_var{deps=Deps}) ->
%%   io_lib:format("Fun(~s)", [lists:flatten([format_type(X)||X<-Deps])]);
%% format_type(Type) ->
%%   case is_literal(Type) of
%%     true -> io_lib:format("~w", [concrete(Type)]);
%%     false -> t_to_string(Type)
%%   end.
      
duplicate(0, _Element) ->
  [];
duplicate(N, Element) ->
  [Element|duplicate(N-1, Element)].

get_def_plt() ->
  dialyzer_plt:from_file(dialyzer_typesig_plt, 
			 filename:join([code:lib_dir(dialyzer),
					"plt","dialyzer_init_plt"])).



%%% ============================================================================
%%%
%%%  Pretty printer and debug facilities.
%%%
%%% ============================================================================

%% pp_signatures([{{_, module_info, 0}, _}|Left]) -> 
%%   pp_signatures(Left);
%% pp_signatures([{{_, module_info, 1}, _}|Left]) -> 
%%   pp_signatures(Left);
%% pp_signatures([{{M, F, A}, Type}|Left]) ->
%%   io:format("~w:~w/~w :: ~s\n", [M, F, A, format_type(Type)]),
%%   pp_signatures(Left);
%% pp_signatures([]) ->
%%   ok.

-ifdef(DEBUG_NAME_MAP).
debug_make_name_map(Vars, Funs) ->
  Map = get(dialyzer_typesig_map),
  NewMap = 
    if Map =:= undefined -> debug_make_name_map(Vars, Funs, dict:new());
       true              -> debug_make_name_map(Vars, Funs, Map)
    end,
  put(dialyzer_typesig_map, NewMap).

debug_make_name_map([Var|VarLeft], [Fun|FunLeft], Map) ->
  Name = {fname_id(Var), fname_arity(Var)},
  FunLabel = get_label(Fun),
  debug_make_name_map(VarLeft, FunLeft, dict:store(FunLabel, Name, Map));
debug_make_name_map([], [], Map) ->
  Map.

debug_lookup_name(Var) ->
  case dict:find(t_var_name(Var), get(dialyzer_typesig_map)) of
    error -> Var;
    {ok, Name} -> Name
  end.

-else.
debug_make_name_map(_Vars, _Funs) ->
  ok.
-endif.

-ifdef(DEBUG_CONSTRAINTS).
pp_constrs_scc(Scc, State) ->
  [pp_constrs(Fun, state__get_cs(Fun, State), State)||Fun <- Scc].

pp_constrs(Fun, Cs, State) ->
  io:format("Constraints for fun: ~w\n", [debug_lookup_name(Fun)]),
  MaxDepth = pp_constraints(Cs, State),
  io:format("Depth: ~w\n", [MaxDepth]).


pp_constraints(Cs, State) ->
  Res = pp_constraints([Cs], none, 0, 0, State),
  io:nl(),
  Res.

pp_constraints([List|Tail], Separator, Level, MaxDepth, 
	       State) when is_list(List) ->
  pp_constraints(List++Tail, Separator, Level, MaxDepth, State);
pp_constraints([#constraint_ref{id=Id, type=Type}|Left], Separator, 
	       Level, MaxDepth, State) ->
  Cs = state__get_cs(Id, State),
  io:format("%Ref ~w ~w%", [Type, t_var_name(Id)]),
  pp_constraints([Cs|Left], Separator, Level, MaxDepth, State);
pp_constraints([#constraint{lhs=Lhs, op=Op, rhs=Rhs}], _Separator, 
	       Level, MaxDepth, _State) ->
  io:format("~s ~w ~s", [format_type(Lhs), Op, format_type(Rhs)]),
  lists:max([Level, MaxDepth]);
pp_constraints([#constraint{lhs=Lhs, op=Op, rhs=Rhs}|Tail], Separator,
	       Level, MaxDepth, State) ->
  io:format("~s ~w ~s ~s ", [format_type(Lhs), Op, format_type(Rhs),Separator]),
  pp_constraints(Tail, Separator, Level, MaxDepth, State);
pp_constraints([#constraint_list{type=Type, list=List, id=Id}], _Separator, 
	       Level, MaxDepth, State) ->
  io:format("%List ~w(", [Id]),
  NewSeparator = case Type of
		   conj -> "*";
		   disj -> "+"
		 end,
  NewMaxDepth = pp_constraints(List, NewSeparator, Level + 1, MaxDepth, State),
  io:format(")", []),
  NewMaxDepth;
pp_constraints([#constraint_list{type=Type, list=List, id=Id}|Tail],Separator,
	       Level, MaxDepth, State) ->
  io:format("List ~w(", [Id]),
  NewSeparator = case Type of
		   conj -> "*";
		   disj -> "+"
		 end,
  NewMaxDepth = pp_constraints(List, NewSeparator, Level+1, MaxDepth, State),
  io:format(") ~s\n~s ", [Separator, Separator]),
  pp_constraints(Tail, Separator, Level, NewMaxDepth, State).
-else.
pp_constrs_scc(_Scc, _State) ->
  ok.

-endif.

-ifdef(DEBUG_PP).
debug_pp(Tree, Map) -> 
  Tree1 = strip_annotations(Tree),
  io:put_chars(cerl_prettypr:format(Tree1)),
  io:nl(),
  [io:format("~w :: ~s\n", [Var, format_type(Type)])
   ||{Var, Type} <- dict:to_list(Map)],
  ok.
      
strip_annotations(Tree) ->
  cerl_trees:map(fun(T) ->
		     case is_literal(T) orelse is_c_values(T) of
		       true -> set_ann(T, []);
		       false ->
			 Label = get_label(T),
			 set_ann(T, [{'label', Label}])
		     end
		 end, Tree).
			    
-else.
debug_pp(_Tree, _Map) -> 
  ok.
-endif.

get_top_level_signatures(Code) ->
  Tree = cerl:from_records(Code),
  {LabeledTree, NextLabel} = cerl_trees:label(Tree),
  %%io:format("LabeledTree:\n~p\n", [LabeledTree]),
  Plt = get_def_plt(),
  dialyzer_plt:delete_module(Plt, atom_val(module_name(LabeledTree))),
  FunTypes = analyze_module(LabeledTree, NextLabel, Plt),
  dialyzer_plt:delete(Plt),
  Sigs = [{{fname_id(V), fname_arity(V)}, dict:fetch(get_label(F), FunTypes)} 
	  || {V, F} <- module_defs(LabeledTree)],  %% V: function name; F: function.
  ordsets:from_list(Sigs).

%% Added by HL
literal_ann(#literal{ann = A}) ->
    A.

call_ann(#call{ann=A}) ->
		A.
get_atom_info(Module) ->
    {ok, _, Code} =refac_compile:file(Module, [to_core, binary, strict_record_tests]),
    register(atom_collector, spawn_link(fun() ->
					   atom_collector([]) end)),
    _Sigs0 = get_top_level_signatures(Code),
    atom_collector! {self(), get},
    Res = receive
	      {atom_collector, Atoms} -> Atoms		    
	      end,
    atom_collector! stop,
    Res.
    
atom_collector(Atoms) ->
    receive
	{_From, add, Atom} -> 
	    NewAtoms = 
		case Atom#c.ann of 
		    [0|_T] ->
			Atoms;
		    _  -> case t_is_module_atom(Atom) or t_is_function_atom(Atom) 
			   or t_is_process_atom(Atom) of 
			   true -> [Atom]++ Atoms;
			   false -> Atoms
			  end
		end,
	    atom_collector(NewAtoms);
	{From, get} ->  From  ! {atom_collector,Atoms},
			atom_collector(Atoms);
        stop  -> ok
    end.
	
    
	    
    
