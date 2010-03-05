-module(refac_misc).


-export([group_by/2, filehash/1,collect_var_source_def_pos_info/1,
	get_start_end_loc/1,variable_replaceable/1,apply_style_funs/0,
	testserver_callback_funs/0,eqc_statem_callback_funs/0,
	eqc_fsm_callback_funs/0,commontest_callback_funs/0, try_eval/4,
	make_new_name/2,collect_var_names/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
group_by(N, TupleList) ->
    SortedTupleList = lists:keysort(N, lists:usort(TupleList)),
    group_by(N, SortedTupleList, []).

group_by(_N,[],Acc) -> Acc;
group_by(N,TupleList = [T| _Ts],Acc) ->
    E = element(N,T),
    {TupleList1,TupleList2} = 
	lists:partition(fun (T1) ->
				element(N,T1) == E
			end,
			TupleList),
    group_by(N,TupleList2,Acc ++ [TupleList1]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filehash(FileName) ->
    case file:open(FileName, [read, raw, binary]) of
      {ok, IoDevice} ->
	  Hash = filehash(IoDevice, 0),
	  file:close(IoDevice),
	  Hash;
      _ -> 0
    end.

filehash(IoDevice, Crc) ->
    case file:read(IoDevice, 1024) of
        {ok, Data} ->
            filehash(IoDevice, erlang:crc32(Crc, Data));
        eof ->
            Crc;
        {error, _Reason} ->
            0 %% TODO error handling
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_var_source_def_pos_info(Nodes) when is_list(Nodes) ->
    lists:flatmap(fun (N) -> collect_var_source_def_pos_info(N) end, Nodes);
collect_var_source_def_pos_info(Node) ->
    F= fun(T, S) ->
	       case refac_syntax:type(T) of 
		   variable ->
		       SourcePos = refac_syntax:get_pos(T),
		       case lists:keysearch(def, 1, refac_syntax:get_ann(T)) of 
			   {value, {def, DefinePos}} ->
			       VarName = refac_syntax:variable_name(T),
			       S ++ [{VarName, SourcePos, DefinePos}];
			   _ ->
			       S
		       end;
		   _  -> S
	       end
       end,
    refac_syntax_lib:fold(F, [], Node).

get_start_end_loc(Exprs) when is_list(Exprs) ->
    E1= hd(Exprs),
    En = lists:last(Exprs),
    {S, _E} = refac_util:get_range(E1),
    {_S, E} = refac_util:get_range(En),
    {S, E};
get_start_end_loc(Expr) ->
    refac_util:get_range(Expr).


%% expressions which should not be replaced by a variable.
%% how about expressions has side effects?
variable_replaceable(Exp) ->
    case lists:keysearch(category, 1, refac_syntax:get_ann(Exp)) of
	{value, {category, record_field}} -> false;
	{value, {category, record_type}} -> false;
	{value, {category, guard_expression}} -> false;
	{value, {category, macro_name}} -> false;
	{value, {category, pattern}} ->
	    case refac_syntax:is_literal(Exp) orelse
		refac_syntax:type(Exp) == variable
	    of
		true ->
		    true;
		_ -> false
	    end;
      _ -> 
	    T = refac_syntax:type(Exp),
	    not lists:member(T, [match_expr, operator]) andalso
		refac_util:get_var_exports(Exp) == []
    end.

apply_style_funs() ->
    [{{erlang, apply, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn, 4}, [node, modulename, functionname, arglist], term},
     {{erlang, spawn_link, 3}, [modulename, functionname, arglist], term},
     {{erlang, spawn_link, 4}, [term, modulename, functioname, arglist], term},
     {{erlang, spawn_monitor, 3}, [term, modulename, functionname, arglist], term},
     {{test_server, timecall, 3}, [modulename, functionname, arglist], term},
     {{test_server, do_times, 4}, [integer, modulename, functionname, arglist], term},
     {{test_server, call_crash, 3}, [modulename, functionname, arglist], term},
     {{test_server, call_crash, 4}, [term, modulename, functionname, arglist], term},
     {{test_server, call_crash, 5}, [term, term, modulename, functionname, arglist], term}].
 
testserver_callback_funs() ->
    [{all, 0}, {init_per_suite, 1}, {end_per_suite, 1}, {init_per_testcase, 2}, {fin_per_testcase, 2}].

eqc_statem_callback_funs() ->
    [{initial_state, 0}, {precondition, 2}, {command, 1}, {postcondition, 3}, {next_state, 3}].

eqc_fsm_callback_funs() ->
    [{initial_state, 0}, {initial_state_data, 0}, {next_state_data, 5},
     {precondition, 4}, {postcondition, 5}].

commontest_callback_funs() ->
    [{all, 0}, {groups, 0}, {suite, 0}, {init_per_suite, 1}, {end_per_suite, 1}, {init_per_group, 2},
     {end_per_group, 2}, {init_per_testcase, 2}, {end_per_testcase, 2}, {testcase, 0}, {testcase, 1}].

try_eval(none, Node, _, _) ->
    try
      erl_eval:exprs([refac_syntax:revert(Node)], [])
    of
      {value, Val, _} -> {value, Val}
    catch
      _E1:_E2 ->
	  {error, no_value}
    end;
try_eval(FileName, Node, SearchPaths, TabWidth) ->
    try
      erl_eval:exprs([refac_syntax:revert(Node)], [])
    of
      {value, Val, _} -> {value, Val}
    catch
      _:_ ->
	  case has_macros(Node) andalso refac_util:get_free_vars(Node) == [] of
	    true ->
		Dir = filename:dirname(FileName),
		DefaultIncl2 = [filename:join(Dir, X) || X <- refac_util:default_incls()],
		NewSearchPaths = SearchPaths ++ DefaultIncl2,
		{Ms, UMs} = case refac_epp:parse_file(FileName, NewSearchPaths, []) of
			      {ok, _, {Defs, Uses}} ->
				  {dict:from_list(Defs), dict:from_list(Uses)};
			      _ -> {[], []}
			    end,
		NodeToks = get_toks(FileName, Node, TabWidth),
		try
		  refac_epp:expand_macros(NodeToks, {Ms, UMs})
		of
		  NewToks when is_list(NewToks) ->
		      case refac_parse:parse_exprs(NewToks ++ [{dot, {999, 0}}]) of
			{ok, Exprs} ->
			    try
			      erl_eval:exprs(Exprs, [])
			    of
			      {value, Val, _} -> {value, Val}
			    catch
			      _:_ -> {error, no_value}
			    end;
			_ -> {error, no_value}
		      end
		catch
		  _:__ -> {error, no_value}
		end;
	    false ->
		{error, no_value}
	  end
    end.

get_toks(FileName, Node, TabWidth) ->
    Toks = refac_util:tokenize(FileName, false, TabWidth),
    {StartPos, EndPos} = refac_util:get_range(Node),
    Toks1 = lists:dropwhile(fun (T) ->
				    token_loc(T) < StartPos
			    end, Toks),
    lists:takewhile(fun (T) ->
			    token_loc(T) =< EndPos
		    end, Toks1).

token_loc(T) ->
    case T of
      {_, L, _V} -> L;
      {_, L1} -> L1
    end.

has_macros(Node) ->
    F = fun (N, _Others) ->
		case refac_syntax:type(N) of
		  macro -> {N, true};
		  _ -> {[], false}
		end
	end,
    {_, Res} = refac_util:once_tdTU(F, Node, []),
    Res.
    



make_new_name(VarName, UsedVarNames) ->
    NewVarName = list_to_atom(atom_to_list(VarName)++"_1"),
    case ordsets:is_element(NewVarName, UsedVarNames) of
	true ->
	    make_new_name(NewVarName, UsedVarNames);
	_ -> 
	    NewVarName
    end.

collect_var_names(Node) when is_list(Node) ->
    collect_var_names_1(refac_syntax:block_expr(Node));
collect_var_names(Node) ->
    collect_var_names_1(Node).
collect_var_names_1(Node) ->
    F = fun (N, S) ->
		case refac_syntax:type(N) of
		    variable ->
			case lists:keysearch(category, 1, refac_syntax:get_ann(N)) of
			    {value, {category, macro_name}} -> S;
			    _ ->
				VarName = refac_syntax:variable_name(N),
				ordsets:add_element(VarName, S)
			end;
		    _ -> S
		end
	end,
    ordsets:to_list(refac_syntax_lib:fold(F, ordsets:new(), Node)).

