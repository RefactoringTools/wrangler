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

-export([ rename_fun/5, rename_var/5, rename_mod/3, rename_process/5, generalise/7, move_fun/6, tuple_to_record/8,
         duplicated_code_in_buffer/3, duplicated_code_in_dirs/3, expression_search/5, fun_extraction/6, fold_expr_by_loc/4, 
	 fold_expr_by_name/6, tuple_funpar/5,
	 instrument_prog/2, uninstrument_prog/2, add_a_tag/5,register_pid/7, fun_to_process/5]).
	 

-include("../hrl/wrangler.hrl").

-spec(rename_var/5::(filename(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {ok, string()}).
rename_var(Fname, Line, Col, NewName, SearchPaths) ->
    case check_undo_process() of
	ok ->
	    Res=wrangler:rename_var(Fname, Line, Col, NewName, SearchPaths),
	    case Res of 
		{ok, _} ->
		    check_wrangler_error_logger();
		{error, _} -> ok
	    end,
	    Res;		
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(rename_fun/5::(string(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {ok, [filename()]}).
rename_fun(Fname, Line, Col, NewName,SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:rename_fun(Fname, Line, Col, NewName,SearchPaths);
	{error, Reason} ->
	   {error, Reason}
    end.

-spec(rename_mod/3::(filename(), string(), [dir()]) -> {error, string()} | {ok, [filename()]}).  
rename_mod(Fname, NewName,SearchPaths) ->
    case check_undo_process() of 
	ok ->  wrangler:rename_mod(Fname, NewName,SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(rename_process/5::(filename(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {undecidables, string()}| {ok, [filename()]}).
rename_process(Fname, Line, Col, NewName, SearchPaths) ->
    case check_undo_process() of
	ok -> Res=wrangler:rename_process(Fname, Line, Col, NewName, SearchPaths),
	      case Res of 
		  {ok, _} ->
		      check_wrangler_error_logger();
		  _ -> ok
	      end,
	      Res;		
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(generalise/7::(filename(),integer(), integer(),integer(), integer(),string(), dir()) -> {ok, string()} | {error, string()}).
generalise(Fname, StartLine, StartCol, EndLine, EndCol, ParName, SearchPaths)->
    case check_undo_process() of 
	ok -> wrangler:generalise(Fname, {StartLine, StartCol}, {EndLine, EndCol}, ParName, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(move_fun/6::(filename(),integer(),integer(), string(), atom(),[dir()])
        -> {ok, [{filename(), filename()}]}
           | {error, string()}).
move_fun(FName, Line, Col, ModName, CreateNewFile, SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:move_fun(FName, Line, Col, ModName, CreateNewFile, SearchPaths);
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
	ok -> wrangler:fun_extraction(FName, {StartLine, StartCol}, {EndLine, EndCol}, FunName);
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(fold_expr_by_loc/4::
      (filename(), integer(), integer(), [dir()]) -> {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
							 | {error, string()}).
fold_expr_by_loc(FName, Line, Col, SearchPaths) ->
    case check_undo_process() of
      ok -> wrangler:fold_expr_by_loc(FName, Line, Col, SearchPaths);
      {error, Reason} -> {error, Reason}
    end.

-spec(fold_expr_by_name/6::(filename(), string(), string(), string(), string(), [dir()]) ->
	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
		 | {error, string()}).
fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths) ->
   case check_undo_process() of
      ok -> wrangler:fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths);
      {error, Reason} -> {error, Reason}
    end.

	       
-spec(instrument_prog/2::(filename(), [dir()]) ->{ok, [filename()]} | {error, string()}).
instrument_prog(FName, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:instrument_prog(FName, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(tuple_funpar/5::(filename(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {ok, [filename()]}).
tuple_funpar(Fname, Line, Col, Number, SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:tuple_funpar(Fname, Line, Col, Number, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(tuple_to_record/8::(filename(), integer(), integer(), integer(), integer(), string(), [string()], [dir()]) ->
	     {error, string()} | {ok, [filename()]}).
tuple_to_record(File, FLine, FCol, LLine, LCol, 
                RecName, FieldString, SearchPaths)->
    case check_undo_process() of
	ok -> wrangler:tuple_to_record(File, FLine, FCol, LLine, LCol, 
                                       RecName, FieldString, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.
    
-spec(uninstrument_prog/2::(filename(), [dir()]) ->{ok, [filename()]} | {error, string()}).
uninstrument_prog(FName, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:uninstrument_prog(FName, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

-spec(add_a_tag/5::(filename(), integer(), integer(), string(), [dir()]) ->
	     {error, string()} | {ok, [filename()]}).
add_a_tag(FileName, Line, Col, Tag, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:add_a_tag(FileName, Line, Col, Tag, SearchPaths);
	{error, Reason} ->
	    {error,Reason}
    end.

-spec(register_pid/7::(filename(), integer(), integer(), integer(),integer(), string(), [dir()]) ->
    {error, string()}|{ok, [filename()]}).
register_pid(FileName, StartLine, StartCol, EndLine, EndCol, RegName, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:register_pid(FileName, {StartLine, StartCol}, {EndLine, EndCol}, RegName, SearchPaths);
	{error, Reason} ->
	    {error,Reason}
    end.

-spec(fun_to_process/5::(filename(), integer(), integer(), string(), [dir()])->
	     {error, string()} | undecidables | {ok, [filename()]}).
fun_to_process(Fname, Line, Col, ProcessName,SearchPaths) ->
    case check_undo_process() of
	ok -> Res = wrangler:fun_to_process(Fname, Line, Col, ProcessName,SearchPaths),
	      case Res of 
		  {ok, _} ->
		      check_wrangler_error_logger();
		  _ -> ok
	      end,
	      Res;
	{error, Reason} ->
	   {error, Reason}
    end.


check_undo_process() ->
    case erlang:whereis(refactor_undo) of
	undefined ->
	    {error, "The UNDO process is not working, please restart the refactorer!"};
	_ ->
	    ok
    end.

check_wrangler_error_logger() -> wrangler_error_logger:check_logged_errors().
    
