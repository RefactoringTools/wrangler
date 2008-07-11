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

-export([ rename_fun/5, rename_var/5, rename_mod/3, generalise/7, move_fun/6, tuple_to_record/8,
         duplicated_code_in_buffer/3, duplicated_code_in_dirs/3, expression_search/5, fun_extraction/6, fold_expression/3, tuple_funpar/5,
	 instrument_prog/2, uninstrument_prog/2, add_a_tag/5,register_pid/7, fun_to_process/5,
	 start_processes/1, stop_processes/0, error_logger/1,
         undo/0, undo_init/0]).


rename_var(Fname, Line, Col, NewName, SearchPaths) ->
    case check_undo_process() of
	ok -> Res=wrangler:rename_var(Fname, Line, Col, NewName, SearchPaths),
	      case Res of 
		  {ok, _} ->
		      check_error_logger();
		  {error, _} -> ok
	      end,
	      Res;		
	{error, Reason} ->
	    {error, Reason}
    end.

rename_fun(Fname, Line, Col, NewName,SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:rename_fun(Fname, Line, Col, NewName,SearchPaths);
	{error, Reason} ->
	   {error, Reason}
    end.

rename_mod(Fname, NewName,SearchPaths) ->
    case check_undo_process() of 
	ok ->  wrangler:rename_mod(Fname, NewName,SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

generalise(Fname, StartLine, StartCol, EndLine, EndCol, ParName, SearchPaths)->
    case check_undo_process() of 
	ok -> wrangler:generalise(Fname, {StartLine, StartCol}, {EndLine, EndCol}, ParName, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

move_fun(FName, Line, Col, ModName, CreateNewFile, SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:move_fun(FName, Line, Col, ModName, CreateNewFile, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

duplicated_code_in_buffer(FName, MinLines,  MinClones) ->
    wrangler:duplicated_code_in_buffer(FName, MinLines, MinClones).

duplicated_code_in_dirs(FName, MinLines, MinClones) ->
    wrangler:duplicated_code_in_dirs(FName, MinLines, MinClones).

expression_search(FName, StartLine, StartCol, EndLine, EndCol) ->
    wrangler:expression_search(FName, {StartLine, StartCol}, {EndLine, EndCol}).

fun_extraction(FName, StartLine, StartCol, EndLine, EndCol, FunName) ->
    case check_undo_process() of 
	ok -> wrangler:fun_extraction(FName, {StartLine, StartCol}, {EndLine, EndCol}, FunName);
	{error, Reason} ->
	    {error, Reason}
    end.


fold_expression(FName, Line, Col) ->
    case check_undo_process() of 
	ok ->  wrangler:fold_expression(FName, Line, Col);
	{error, Reason} ->
	    {error, Reason}
    end.
	       
instrument_prog(FName, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:instrument_prog(FName, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

tuple_funpar(Fname, Line, Col, Number, SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:tuple_funpar(Fname, Line, Col, Number, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.

tuple_to_record(File, FLine, FCol, LLine, LCol, 
                RecName, FieldString, SearchPaths)->
    case check_undo_process() of
	ok -> wrangler:tuple_to_record(File, FLine, FCol, LLine, LCol, 
                                       RecName, FieldString, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.
    

uninstrument_prog(FName, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:uninstrument_prog(FName, SearchPaths);
	{error, Reason} ->
	    {error, Reason}
    end.


add_a_tag(FileName, Line, Col, Tag, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:add_a_tag(FileName, Line, Col, Tag, SearchPaths);
	{error, Reason} ->
	    {error,Reason}
    end.


register_pid(FileName, StartLine, StartCol, EndLine, EndCol, RegName, SearchPaths) ->
    case check_undo_process() of 
	ok ->
	    wrangler:register_pid(FileName, {StartLine, StartCol}, {EndLine, EndCol}, RegName, SearchPaths);
	{error, Reason} ->
	    {error,Reason}
    end.


fun_to_process(Fname, Line, Col, ProcessName,SearchPaths) ->
    case check_undo_process() of
	ok -> wrangler:fun_to_process(Fname, Line, Col, ProcessName,SearchPaths);
	{error, Reason} ->
	   {error, Reason}
    end.
%% tuple_to_record(Fname, StartLine, StartCol, EndLine, EndCol) ->
%%     refac_record:tuple_to_record(Fname, {StartLine, StartCol}, {EndLine, EndCol}).


check_undo_process() ->
    case erlang:whereis(refactor_undo) of
	undefined ->
	    {error, "The UNDO process is not working, please restart the refactorer!"};
	_ ->
	    ok
    end.

undo() ->
    refactor_undo ! {self(),undo},
    receive
	{refactor_undo, Reply} ->
	    Reply
    end.
    

start_processes(SearchPaths) ->
    start_undo_process(),
    start_error_logger(),
    refac_ast_server:start_ast_server(SearchPaths).
    
stop_processes() ->
    stop_undo_process(),
    unregister(refactor_undo),
    stop_error_logger(),
    unregister(error_logger),
    refac_ast_server:stop_ast_server(),
    unregister(ast_server).

    
start_undo_process() ->
    spawn_link(wrangler_distel, undo_init, []).



stop_undo_process()-> 
    refactor_undo ! stop.
    
undo_init()->
    case erlang:whereis(refactor_undo) of 
	undefined -> ok;
	_         -> erlang:unregister(refactor_undo)
    end,
    register(refactor_undo, self()),
    History=[],
    undo_loop(History).
    

undo_files(Files) -> 
    case Files of 
	[] ->
	    ok;
	[{{OldFileName,NewFileName}, Content}|T] -> 
	    case OldFileName == NewFileName of
		true ->  file:write_file(OldFileName, Content),
			 undo_files(T);
		false -> file:write_file(OldFileName, Content),
			 file:delete(NewFileName),
			 undo_files(T)
	    end
    end.

undo_loop(History) ->
    receive 
	{From, undo} -> case History of 
			    [] ->
				From ! {refactor_undo, {error, "No more history to undo!"}},
				undo_loop(History);
			    [H|T] -> 
				ok = undo_files(H),
                          	Modified = lists:map(fun({{OldFileName, NewFileName}, _Con})->
							   [OldFileName, NewFileName] end,H),
				From ! {refactor_undo, {ok, Modified}},
			        undo_loop(T)
			end;
       {add, Files} ->    
	               History1=[Files|History],
		       undo_loop(lists:sublist(History1,15)); %%KEEP 15 HISTORY VERSIONS. IS THIS ENOUGH?
	stop -> ok    
    end.
	
start_error_logger() ->	       
    Pid = spawn_link(wrangler_distel, error_logger, [[]]),
    register(error_logger, Pid).

stop_error_logger() ->
    error_logger ! stop.

init_error_logger() ->
    error_logger ! init.

check_error_logger() ->
    error_logger ! {get, self()},
    receive 
	{error_logger, State} ->
	    case State of 
		[] ->
		     ok;
		_ ->
		    io:format("\n===============================WARNING===============================\n"),
		    io:format("Due to the following syntactical errors in the program, attributes/functions affected "
			      "by these errors were not affected by this refactoring!\n"),
		    lists:map(fun({FileName, Errors}) ->
				      Errors1 = lists:map(fun({Pos, _Mod, Msg}) -> {Pos, Msg} end, Errors),
				      io:format("File:\n ~p\n", [FileName]),
				      io:format("Error(s):\n"),
				      lists:map(fun({Pos, Msg}) -> io:format(" ** ~p:~s **\n", [Pos, Msg]) end, lists:reverse(Errors1)) %% lists:flatten(Msg)
			      end, State)
	    end
    end,
    init_error_logger().
    

error_logger(State) ->
    receive
	{add, Error} ->
	    error_logger(lists:usort([Error|State]));
	{get, From} ->
	    From ! {error_logger, State},
	    error_logger(State);
	init -> 
	    error_logger([]);
	stop ->
	    ok
    end.

    
