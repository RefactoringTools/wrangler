%% =====================================================================
%% Some Interface Functions to Emacs
%%
%% Copyright (C) 2006-2008  Huiqing Li, Simon Thompson

%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.

%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =====================================================================

-module(wrangler_distel).

-compile(export_all).

-include("../hrl/wrangler.hrl").

-spec(rename_var/5::(filename(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {ok, string()}).
rename_var(Fname, Line, Col, NewName, SearchPaths) ->
    case initial_checking(SearchPaths) of
	ok ->
	    Res=wrangler:rename_var(Fname, Line, Col, NewName, SearchPaths),
	    check_wrangler_error_logger(),
	    Res;		
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(rename_fun/5::(string(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {ok, [filename()]}).
rename_fun(Fname, Line, Col, NewName,SearchPaths) ->
    case initial_checking(SearchPaths) of
	ok -> Res =wrangler:rename_fun(Fname, Line, Col, NewName,SearchPaths),
	      check_wrangler_error_logger(),
	      Res;		
	{error, Reason} ->
	   {error, Reason}
    end.

-spec(rename_mod/3::(filename(), string(), [dir()]) -> {error, string()} | {ok, [filename()]}). 
rename_mod(Fname, NewName,SearchPaths) ->
    case initial_checking(SearchPaths) of 
	ok ->  Res = wrangler:rename_mod(Fname, NewName,SearchPaths),
	        check_wrangler_error_logger(),
	       Res;		
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(rename_process/5::(filename(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {undecidables, string()}| {ok, [filename()]}).
rename_process(Fname, Line, Col, NewName, SearchPaths) ->
    case initial_checking(SearchPaths) of
	ok -> Res=wrangler:rename_process(Fname, Line, Col, NewName, SearchPaths),
	      check_wrangler_error_logger(),
	      Res;
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(generalise/7::(filename(),integer(), integer(),integer(), integer(),string(), dir()) -> {ok, string()} | {error, string()}).
generalise(Fname, StartLine, StartCol, EndLine, EndCol, ParName, SearchPaths)->
    case initial_checking(SearchPaths) of 
	ok -> Res =wrangler:generalise(Fname, {StartLine, StartCol}, {EndLine, EndCol}, ParName, SearchPaths),
	      check_wrangler_error_logger(),
	      Res;
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(move_fun/6::(filename(),integer(),integer(), string(), atom(),[dir()])
        -> {ok, [{filename(), filename()}]}
           | {error, string()}).
move_fun(FName, Line, Col, ModName, CreateNewFile, SearchPaths) ->
    case initial_checking(SearchPaths) of
	ok -> Res = wrangler:move_fun(FName, Line, Col, ModName, CreateNewFile, SearchPaths),
	      check_wrangler_error_logger(),
	      Res;
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(duplicated_code_in_buffer/3::(filename(), string(), string()) ->{ok, string()}).       
duplicated_code_in_buffer(FName, MinLines,  MinClones) ->
    wrangler:duplicated_code_in_buffer(FName, MinLines, MinClones).

-spec(duplicated_code_in_dirs/3::([dir()], string(), string()) ->{ok, string()}).
duplicated_code_in_dirs(FName, MinLines, MinClones) ->
    wrangler:duplicated_code_in_dirs(FName, MinLines, MinClones).

-spec(expression_search/5::(filename(), integer(), integer(), integer(), integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).
expression_search(FName, StartLine, StartCol, EndLine, EndCol) ->
    wrangler:expression_search(FName, {StartLine, StartCol}, {EndLine, EndCol}).

-spec(fun_extraction/6::(filename(), integer(), integer(), integer(), integer(), string()) ->
	      {error, string()} | {ok, string()}).
fun_extraction(FName, StartLine, StartCol, EndLine, EndCol, FunName) ->
    case check_undo_process() of 
	ok -> Res = wrangler:fun_extraction(FName, {StartLine, StartCol}, {EndLine, EndCol}, FunName),
	      check_wrangler_error_logger(),
	      Res;
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(fold_expr_by_loc/4::
      (filename(), integer(), integer(), [dir()]) -> {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
							 | {error, string()}).
fold_expr_by_loc(FName, Line, Col, SearchPaths) ->
    case initial_checking(SearchPaths) of
      ok -> Res = wrangler:fold_expr_by_loc(FName, Line, Col, SearchPaths),
	    check_wrangler_error_logger(),
	    Res;
      {error, Reason} -> {error, Reason}
    end.

-spec(fold_expr_by_name/6::(filename(), string(), string(), string(), string(), [dir()]) ->
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
		 | {error, string()}).
fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths) ->
   case initial_checking(SearchPaths) of
      ok -> Res = wrangler:fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths),
	    check_wrangler_error_logger(),
	    Res;
      {error, Reason} -> {error, Reason}
    end.

	       
-spec(instrument_prog/2::(filename(), [dir()]) ->{ok, [filename()]} | {error, string()}).
instrument_prog(FName, SearchPaths) ->
    case initial_checking(SearchPaths) of 
	ok ->
	    Res = wrangler:instrument_prog(FName, SearchPaths),
	    check_wrangler_error_logger(),
	    Res;
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(tuple_funpar/5::(filename(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {ok, [filename()]}).
tuple_funpar(Fname, Line, Col, Number, SearchPaths) ->
    case initial_checking(SearchPaths) of
	ok -> Res = wrangler:tuple_funpar(Fname, Line, Col, Number, SearchPaths),
	      check_wrangler_error_logger(),
	      Res;
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(tuple_to_record/8::(filename(), integer(), integer(), integer(), integer(), string(), [string()], [dir()]) ->
	     {error, string()} | {ok, [filename()]}).
tuple_to_record(File, FLine, FCol, LLine, LCol, 
                RecName, FieldString, SearchPaths)->
    case initial_checking(SearchPaths) of
	ok -> Res = wrangler:tuple_to_record(File, FLine, FCol, LLine, LCol, 
					     RecName, FieldString, SearchPaths),
	      check_wrangler_error_logger(),
	      Res;
	{error, Reason} ->
	    {error, Reason}
    end.
    
-spec(uninstrument_prog/2::(filename(), [dir()]) ->{ok, [filename()]} | {error, string()}).
uninstrument_prog(FName, SearchPaths) ->
    case initial_checking(SearchPaths) of 
	ok ->
	    Res = wrangler:uninstrument_prog(FName, SearchPaths),
	    check_wrangler_error_logger(),
	    Res;
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(add_a_tag/5::(filename(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {ok, [filename()]}).
add_a_tag(FileName, Line, Col, Tag, SearchPaths) ->
    case initial_checking(SearchPaths) of 
	ok ->
	    Res = wrangler:add_a_tag(FileName, Line, Col, Tag, SearchPaths),
	    check_wrangler_error_logger(),
	    Res;
	{error, Reason} ->
	    {error,Reason}
    end.


-spec(register_pid/7::(filename(), integer(), integer(), integer(),integer(), string(), [dir()]) ->
    {error, string()}|{ok, [filename()]}).
register_pid(FileName, StartLine, StartCol, EndLine, EndCol, RegName, SearchPaths) ->
    case initial_checking(SearchPaths) of 
	ok ->
	    Res = wrangler:register_pid(FileName, {StartLine, StartCol}, {EndLine, EndCol}, RegName, SearchPaths),
	    check_wrangler_error_logger(),
	    Res;
	{error, Reason} ->
	    {error,Reason}
    end.

-spec(fun_to_process/5::(filename(), integer(), integer(), string(), [dir()])->
	     {error, string()} | undecidables | {ok, [filename()]}).
fun_to_process(Fname, Line, Col, ProcessName,SearchPaths) ->
    case initial_checking(SearchPaths) of
	ok -> Res = wrangler:fun_to_process(Fname, Line, Col, ProcessName,SearchPaths),
	      check_wrangler_error_logger(),
	      Res;
	{error, Reason} ->
	   {error, Reason}
    end.

initial_checking(SearchPaths) ->
     case check_searchpaths(SearchPaths) of 
 	ok ->
 	    check_undo_process();
 	{error, Reason} -> {error, Reason}
     end.

check_searchpaths(SearchPaths) ->
    InValidSearchPaths = lists:filter(fun (X) -> not filelib:is_dir(X) end, SearchPaths),
    case InValidSearchPaths of
      [] -> ok;
      [_D | T] ->
	  case T of
	    [] ->
		{error,
		 "The search paths specified contain an "
		 "invalid directory, please check the "
		 "customisation!"};
	    _ ->
		{error,
		 "The search paths specified contain invalid "
		 "directories, please check the customisation!"}
	  end
    end.
			  
check_undo_process() ->
    case erlang:whereis(refactor_undo) of
	undefined ->
	    {error, "The UNDO process is not working, please restart the refactorer!"};
	_ ->
	    ok
    end.

check_wrangler_error_logger() ->
    Errors = wrangler_error_logger:get_logged_errors(),
    case Errors of 
	[] ->
	     ok;
	_ ->  io:format("\n===============================WARNING===============================\n"),
	      io:format("There are errors in the program, and functions/attribute containing errors are not affected by the refactoring process.\n"),
	      lists:foreach(fun({FileName, Errs}) ->
				    %%Errs1 = lists:map(fun({Pos, _Mod, Msg}) -> {Pos, Msg} end, Errs),
				    io:format("File:\n ~p\n", [FileName]),
				    io:format("Error(s):\n"),
				    lists:foreach(fun(E) ->
							  case E of 
							      {Pos, _Mod, Msg} ->io:format(" ** ~p:~p **\n", [Pos, Msg]);
							      M -> io:format("**~s**\n", [M])
							  end
						  end,
						  lists:reverse(Errs)) %% lists:flatten(Msg)
			    end, Errors)
    end.
    
find_var_instances(FName, Line, Col, SearchPaths) ->
    {ok, {AnnAST, _Info0}} = refac_util:parse_annotate_file(FName, true, SearchPaths),
    case refac_util:pos_to_var_name(AnnAST, {Line, Col}) of
	{ok, {_VarName, DefinePos, _C}} ->
	    if DefinePos == [{0,0}] ->
		    {error, "The selected variable is a free variable!"};
	       true -> 
		    F = fun(T, S) ->
				case refac_syntax:type(T) of 
				    variable -> 
					case lists:keysearch(def, 1, refac_syntax:get_ann(T)) of 
					    {value, {def, DefinePos}} -> 
						Range = refac_util:get_range(T),
						[Range | S];
					    _ -> S
					end;
				    _ -> S
				end
			end,
		    Locs = lists:usort(refac_syntax_lib:fold(F, [], AnnAST)),
		    {ok, Locs, DefinePos}
	    end;
	{error, Reason} -> {error, Reason}
    end.


nested_if_exprs(FName, NestLevel, SearchPaths) ->
    nested_exprs(FName, NestLevel,if_expr, SearchPaths).

nested_case_exprs(FName, NestLevel, SearchPaths) -> 
    nested_exprs(FName, NestLevel, case_expr, SearchPaths).

nested_receive_exprs(FName, NestLevel, SearchPaths) ->
    nested_exprs(FName, NestLevel, receive_expr, SearchPaths).


nested_exprs(FName, NestLevel,  ExprType, SearchPaths)->
    case list_to_integer(NestLevel)>0 of 
	true -> nested_case_exprs_1(FName, list_to_integer(NestLevel), ExprType, SearchPaths);
	false -> {error, "Invalid nest level!"}
    end.
nested_case_exprs_1(FName, NestLevel, ExprType, SearchPaths) ->
    {ok, {AnnAST, _Info0}} = refac_util:parse_annotate_file(FName, true, SearchPaths),
    Fun = fun(T,S) ->
		  case refac_syntax:type(T) of 
		      function ->
			  FunName = refac_syntax:atom_value(refac_syntax:function_name(T)),
			  Arity = refac_syntax:function_arity(T),
			  Fun1 = fun(Node, S1) ->
					 case refac_syntax:type(Node) of 
					     ExprType -> Range = refac_util:get_range(Node),
							  [{FunName, Arity, Range} |S1];
					     _  -> S1
					 end
				 end,
			  refac_syntax_lib:fold(Fun1, S, T);
		      _  -> S
		  end
	  end,
    Ranges = lists:usort(refac_syntax_lib:fold(Fun, [], AnnAST)),
    SortedRanges = sort_ranges(Ranges),
    ResRanges = lists:filter(fun(R) -> length(R) >= NestLevel end, SortedRanges),
    Funs = lists:usort(lists:map(fun(R) -> {F, A, _R} = hd(R), 
					   {F, A}
				 end, ResRanges)),	
    format_result(Funs, NestLevel, ExprType),
    {ok, Funs}.
	
	
format_result(Funs, NestLevel, ExprType) ->
    ExprType1 = case ExprType of 
		    case_expr -> "Case";
		    if_expr -> "If";
		    receive_expr -> "Receive"
		end,
    case Funs of 
	[] -> io:format("\nNo function in this module contains ~s expressions nested up to ~p levels.\n", [ExprType1, NestLevel]);
	_ -> io:format("\nThe following function(s) contains ~s expressions nested up to ~p levels: ", [ExprType1, NestLevel]),
	     format_result_1(Funs)
    end.

format_result_1([]) ->
    io:format(".");
format_result_1([{F, A}|Fs]) ->
    case Fs of 
	[] -> io:format("~p/~p", [F, A]);
	_ -> io:format("~p/~p,", [F, A])
    end,
    format_result_1(Fs).

	    
sort_ranges(Ranges) ->
    sort_ranges(Ranges, []).
sort_ranges([], Acc) ->
    Acc;
sort_ranges(Rs, Acc) ->
    [Hd|Tail] = Rs,
    Nested = get_enclosed([Hd], Tail),
    sort_ranges(Rs--Nested, [lists:reverse(Nested)|Acc]).

get_enclosed(Cur, Rs) ->
    Lst = hd(Cur),
    {_FName1, _Arity1, {Start1, End1}} = Lst,
    Enclosed = lists:usort(lists:filter(fun (R) ->
						{_FName2, _Arity2, {Start2, End2}} = R,
						Start1 =< Start2 andalso End2 =< End1
					end,
					Rs)),
    case Enclosed of
      [] -> Cur;
      [Hd | _] -> get_enclosed([Hd | Cur], Rs -- [Hd])
    end.
				   
			       
	
caller_called_modules(FName, SearchPaths) ->
    {ok, {AnnAST, _Info0}} = refac_util:parse_annotate_file(FName, true, SearchPaths),
    AbsFileName = filename:absname(filename:join(filename:split(FName))),
    ClientFiles = wrangler_modulegraph_server:get_client_files(AbsFileName, SearchPaths),
    io:format("ClientFiles:\n~p\n", [ClientFiles]),
    ClientMods = lists:map(fun({M, _Dir}) -> list_to_atom(M) end, 
			   refac_util:get_modules_by_file(ClientFiles)),
    case ClientFiles of 
	[] -> io:format("\nThis module does not have any caller modules.\n");
	_ -> io:format("\nThis module has the following caller modules:\n"),
	     io:format("~p\n", [refac_util:get_modules_by_file(ClientMods)])
    end,
    CalledMods = refac_module_graph:collect_called_modules(AnnAST),
    case CalledMods of 
	[] -> io:format("\nThis module does not have any called modules.\n");
	_  -> io:format("\n This module has the following called modules.\n"),
	      io:format("~p\n", [CalledMods])
    end.


long_functions(FName, Lines, SearchPaths) ->
    case  list_to_integer(Lines)>=0 of 
	true ->
	    long_functions_1(FName, list_to_integer(Lines), SearchPaths);
	false ->{error, "Invalid number of lines!"}
    end.

long_functions_1(FName, Lines, SearchPaths) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths),
    case lists:keysearch(module, 1, Info) of
      {value, {module, ModName}} -> ModName;
      _ -> ModName = list_to_atom(filename:basename(FName, ".erl")), ModName
    end,
    Fun = fun (Node, S) ->
		  case refac_syntax:type(Node) of
		      function ->
			  {{StartLine, _StartCol}, {EndLine, _EndCol}} = refac_util:get_range(Node),
			  Toks = refac_util:get_toks(Node),
			  GroupedToks = group_by_line(Toks),
			  WhiteLines = length(lists:filter(fun (Ts) ->
								   Line =element(1, element(2, hd(Ts))),
								   lists:all(fun (T) -> ((element(1, T) == whitespace) or
											 (element(1, T) == comment)) and
											(Line>=StartLine) and (Line =< EndLine)
									   end, Ts)
							 end,
							 GroupedToks)),
			LinesOfCode = EndLine - StartLine +1 - WhiteLines,
			case LinesOfCode > Lines of
			  true ->
			      FunName = refac_syntax:atom_value(refac_syntax:function_name(Node)),
			      Arity = refac_syntax:function_arity(Node),
			      [{ModName, FunName, Arity} | S];
			  _ -> S
			end;
		    _ -> S
		  end
	  end,
    LongFuns = lists:usort(refac_syntax_lib:fold(Fun, [], AnnAST)),
    case LongFuns of
	[] ->
	    io:format("\n No Function in this module has more than ~p lines.\n",
		    [Lines]);
	_ ->
	    io:format("\n The following functions have more than ~p lines of code:\n",
		      [Lines]),
	    io:format("~p\n", [LongFuns])
    end.
    
			
group_by_line(TupleList) ->
    group_by_1(lists:keysort(2, TupleList)).

group_by_1([]) -> [];
group_by_1(TupleList=[E|_Es]) ->
    Line = element(1, element(2, E)),
    {E1,E2} = lists:splitwith(fun(T) -> element(1,element(2,T)) == Line end, TupleList),
    [E1 | group_by_1(E2)].
    	  
				  



large_modules(Lines, SearchPaths) ->
    case  list_to_integer(Lines)>=0 of 
	true ->
	    large_modules_1(list_to_integer(Lines), SearchPaths);
	false ->{error, "Invalid number of lines!"}
    end.


large_modules_1(Lines, SearchPaths) ->
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    LargeModules = lists:filter(fun(File) ->
					is_large_module(File, Lines)
				end, Files),
    case LargeModules of 
	[] ->
	    io:format("\n No Module with more than ~p line of code has been found.\n", [Lines]);
	_ ->
	    io:format("The following modules have more than ~p lines of code:\n",
		      [Lines]),
	    io:format("~p\n", [LargeModules])
    end.
	    
is_large_module(FName, Lines) ->
    io:format("Current file being checked:~p\n", [FName]),
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, []),
    Fun = fun(Node, S) ->
		  case refac_syntax:type(Node) of 
		      function ->
			  {{StartLine, _StartCol}, {EndLine, _EndCol}} = refac_util:get_range(Node),
			  Toks = refac_util:get_toks(Node),
			  GroupedToks = group_by_line(Toks),
			  WhiteLines = length(lists:filter(fun (Ts) ->
								   Line =element(1, element(2, hd(Ts))),
								   lists:all(fun (T) -> ((element(1, T) == whitespace) or
											 (element(1, T) == comment)) and
										       (Line>=StartLine) and (Line =< EndLine)
									     end, Ts)
							   end,
							 GroupedToks)),
			  LinesOfCode = EndLine - StartLine +1 - WhiteLines,
			  LinesOfCode+S;
		      attribute ->
			  {{StartLine, _StartCol}, {EndLine, _EndCol}} = refac_util:get_range(Node),
			  LinesOfCode1 = EndLine - StartLine +1,
			  LinesOfCode2 = case LinesOfCode1 >0 of 
					     true -> LinesOfCode1;
					     _ -> 1
					 end,
			  S + LinesOfCode2;
		      _ -> S
		  end
	  end,
    LinesOfCode = refac_syntax_lib:fold(Fun, 0, AnnAST),
    io:format("LinesOfcode:\n~p\n", [LinesOfCode]),
    LinesOfCode > Lines.
    

    
    
	     
		  
			  
