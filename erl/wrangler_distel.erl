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

-include("../hrl/wrangler.hrl").

-compile(export_all).

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
    
