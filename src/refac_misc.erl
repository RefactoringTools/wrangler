-module(refac_misc).


-export([group_by/2, filehash/1,collect_var_source_def_pos_info/1,
	get_start_end_loc/1,variable_replaceable/1,apply_style_funs/0,
	testserver_callback_funs/0,eqc_statem_callback_funs/0,
	eqc_fsm_callback_funs/0,commontest_callback_funs/0, try_eval/4,
	make_new_name/2,collect_var_names/1]).

-export([collect_used_macros/1]).

-export([collect_used_records/1]).

-export([ghead/2]).

-export([glast/2]).

-export([to_upper/1]).

-export([to_lower/1]).

-export([is_var_name/1]).

-export([remove_duplicates/1]).

-export([is_fun_name/1]).

-export([format_search_paths/1]).

-export([default_incls/0]).

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
	    refac_syntax:is_literal(Exp) orelse
		refac_syntax:type(Exp) == variable;	    
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
		DefaultIncl2 = [filename:join(Dir, X) || X <- default_incls()],
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
    {_, Res} = ast_traverse_api:once_tdTU(F, Node, []),
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


collect_used_macros(Node) ->
    F = fun(T, S) ->
		case refac_syntax:type(T) of
		    macro ->
			Name = refac_syntax:macro_name(T),
			case refac_syntax:type(Name) of 
			    variable -> [refac_syntax:variable_name(Name)|S];
			    atom -> [refac_syntax:atom_value(Name) |S]
			end;
		    _  -> S
		end
	end,
    lists:usort(refac_syntax_lib:fold(F, [], Node)).

collect_used_records(Node) ->
    Fun = fun(T, S) ->
		  case refac_syntax:type(T) of
		      record_access ->
			  Type = refac_syntax:record_access_type(T),
			  case refac_syntax:type(Type) of
			      atom -> 
				  ordsets:add_element(refac_syntax:atom_value(Type), S);
			      _ -> S
			  end;
		      record_expr ->
			  Type = refac_syntax:record_expr_type(T),
			  case refac_syntax:type(Type) of
			      atom -> 
				  ordsets:add_element(refac_syntax:atom_value(Type), S);
			      _ -> S
			  end;
		      record_index_expr->
			  Type = refac_syntax:record_index_expr_type(T),
			  case refac_syntax:type(Type) of
			      atom ->
				  ordsets:add_element(refac_syntax:atom_value(Type), S);
			      _ -> S
			  end;
		      _ -> S		  
		  end
	  end,
    ordsets:to_list(refac_syntax_lib:fold(Fun, ordsets:new(), Node)).

%% =====================================================================
%% @spec(ghead(Info::string(),List::[any()]) -> any()).
%% @doc Same as erlang:hd/1, except the first argument which is the
%%  error message when the list is empty.
%% @see glast/2

-spec(ghead(Info::string(),List::[any()]) -> any()).
ghead(Info, []) -> erlang:error(Info);
ghead(_Info, List) -> hd(List).

%% =====================================================================
%% @spec glast(Info::term(), List::[term()]) -> term()
%% @doc Same as lists:last(L), except the first argument which is the 
%%  error message when the list is empty.
%% @see ghead/2

-spec(glast(Info::string(), List::[any()]) -> any()).
glast(Info, []) -> erlang:error(Info);
glast(_Info, List) -> lists:last(List).

%% =====================================================================
%% @spec to_upper(Str::string()) -> string()
%% @doc Convert a string into upper case.
%% @see to_lower/1

-spec(to_upper(Str::string()) -> string()).
to_upper(Str) ->
    to_upper(Str, []).

to_upper([C | Cs], Acc) when C >= 97, C =< 122 ->
    to_upper(Cs, [C - (97 - 65) | Acc]);
to_upper([C | Cs], Acc) -> to_upper(Cs, [C | Acc]);
to_upper([], Acc) -> lists:reverse(Acc).


%% =====================================================================
%% @spec to_lower(Str::string()) -> string()
%% @doc Convert a string into lower case.
%% @see to_upper/1

-spec(to_lower(Str::string()) -> string()).
to_lower(Str) ->
    to_lower(Str, []).

to_lower([C | Cs], Acc) when C >= 65, C =< 90 ->
    to_lower(Cs, [C + (97 - 65) | Acc]);
to_lower([C | Cs], Acc) -> to_lower(Cs, [C | Acc]);
to_lower([], Acc) -> lists:reverse(Acc).

%% =====================================================================
%% @spec is_var_name(Name:: [any()])-> boolean()
%% @doc Return true if a string is lexically a  variable name.
-spec(is_var_name(Name:: [any()])-> boolean()).
is_var_name(Name) ->
    case Name of
      [] -> false;
      [H] -> is_upper(H) and (H =/= 95);
      [H| T] -> (is_upper(H) or (H == 95)) and is_var_name_tail(T)
    end.

is_var_name_tail(Name) ->
    case Name of
      [H| T] ->
	  (is_upper(H) or is_lower(H) or 
	   is_digit(H) or (H == 64) or (H == 95)) and
	    is_var_name_tail(T);
      [] -> true
    end.

is_upper(L) -> (L >= 65) and (90 >= L).

is_lower(L) -> (L >= 97) and (122 >= L).

is_digit(L) -> (L >= 48) and (57 >= L).
    

%% =====================================================================
%% @spec is_fun_name(Name:: [any()])-> boolean()
%% @doc Return true if a name is lexically a function name.
-spec(is_fun_name(Name:: [any()])-> boolean()).
is_fun_name(Name) ->
    case Name of
      [H| T] -> is_lower(H) and is_var_name_tail(T);
      [] -> false
    end.


remove_duplicates(L) ->
    remove_duplicates(L, []).
remove_duplicates([],Acc) ->
     lists:reverse(Acc);
remove_duplicates([H|T], Acc) ->
    case lists:member(H, Acc) of
	true ->
	    remove_duplicates(T, Acc);
	_ ->
	    remove_duplicates(T, [H|Acc])
    end.


 
format_search_paths(Paths) ->
    format_search_paths(Paths, "").
format_search_paths([], Str)->
    Str;
format_search_paths([P|T], Str)->
    case Str of
	[] ->format_search_paths(T, "\""++P++"\"");
	_ ->format_search_paths(T, Str++", \""++P++"\"")
    end.
    
default_incls() ->
  [".", "..", "../hrl", "../incl", "../inc", "../include",
   "../../hrl", "../../incl", "../../inc", "../../include",
   "../../../hrl", "../../../incl", "../../../inc", "../../../include"].
