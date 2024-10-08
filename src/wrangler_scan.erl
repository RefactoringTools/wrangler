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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%% Modified: 17 Jan 2007 by  Huiqing Li <hl@kent.ac.uk>
%%
%% Erlang token scanning functions of io library.


%% @private
%% @hidden
-module(wrangler_scan).

-export([format_error/1, reserved_word/1, string/1, string/2,
	 string/4, tokens/3]).

-compile(export_all).
-compile(nowarn_export_all).

-import(lists, [member/2, reverse/1]).

-define(DEFAULT_TABWIDTH, 8).
-define(DEFAULT_FILEFORMAT, unix).

format_error({string, Quote, Head}) ->
    ["unterminated " ++
       (string_thing(Quote) ++
	  (" starting with " ++
	     io_lib:write_string(Head, Quote)))];
format_error({illegal, Type}) ->
    io_lib:fwrite("illegal ~w", [Type]);
format_error(char) -> "unterminated character";
format_error(scan) -> "premature end";
format_error({base, Base}) ->
    io_lib:fwrite("illegal base '~w'", [Base]);
format_error(float) -> "bad float";
%%
format_error(Other) -> io_lib:write(Other).

string_thing($') -> "atom";
string_thing(_) -> "string".

%% string(CharList, StartPos)
%%  Takes a list of characters and tries to tokenise them.
%%
%%  Returns:
%%	{ok,[Tok]}
%%	{error,{ErrorPos,?MODULE,What},EndPos}

string(Cs) ->
    string(Cs, {1, 1}, ?DEFAULT_TABWIDTH, ?DEFAULT_FILEFORMAT).


string(Cs, {Line, Col}) -> string(Cs, {Line, Col}, ?DEFAULT_TABWIDTH, ?DEFAULT_FILEFORMAT).

string(Cs, {Line, Col}, TabWidth, FileFormat)
    when is_list(Cs), is_integer(Line), is_integer(Col), is_integer(TabWidth) ->
    %     %% Debug replacement line for chopping string into 1-char segments
    %     scan([], [], [], Pos, Cs, []).
    scan(Cs, [], [], {Line, Col}, [], [],TabWidth,FileFormat).


%% tokens(Continuation, CharList, StartPos) ->
%%	{done, {ok, [Tok], EndPos}, Rest} |
%%	{done, {error,{ErrorPos,?MODULE,What}, EndPos}, Rest} |
%%	{more, Continuation'}
%%  This is the main function into the re-entrant scanner.
%%
%%  The continuation has the form:
%%      {RestChars,ScanStateStack,ScannedTokens,
%%       CurrentPos,ContState,ErrorStack,ContFunArity5}

%% definitely should sperate {Line, Col} and TabWidth;; HL.
tokens([], Chars, {{Line, Col}, TabWidth, FileFormat}) ->
    tokens({[], [], [], {Line, Col}, io, [], TabWidth, FileFormat, fun scan/8}, Chars, {{Line, Col}, TabWidth, FileFormat});
tokens({Cs, _Stack, _Toks, {Line, Col}, eof, TabWidth, FileFormat, _Fun}, eof, {_, TabWidth, FileFormat}) ->
    {done, {eof, {Line, Col}}, Cs};
tokens({Cs, Stack, Toks, {Line, Col}, _State, Errors,TabWidth,FileFormat,Fun}, eof, {_, TabWidth, FileFormat}) ->
    Fun(Cs ++ eof, Stack, Toks, {Line, Col}, eof, Errors, TabWidth,FileFormat);
tokens({Cs, Stack, Toks, {Line, Col}, State, Errors,TabWidth,FileFormat,Fun},Chars, {_, TabWidth,FileFormat}) ->
    Fun(Cs ++ Chars, Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat).

%% Scan loop.
%%
%% The scan_*/7 and sub_scan_*/7 functions does tail recursive calls
%% between themselves to change state. State data is kept on the Stack.
%% Results are passed on the Stack and on the stream (Cs). The variable
%% State in this loop is not the scan loop state, but the state for
%% instream handling by more/8 and done/6. The variable Stack is not
%% always a stack, it is just stacked state data for the scan loop, and
%% the variable Errors is a reversed list of scan error {Error,Pos} tuples.
%%
%% All the scan_*/7 functions have the same arguments (in the same order),
%% to keep the tail recursive calls (jumps) fast.
%%
%% When more data is needed from the stream, the tail recursion loop is
%% broken by calling more/8 that either returns to the I/O-server to
%% get more data or fetches it from a string, or by calling done/6 when
%% scanning is done.
%%
%% The last argument to more/8 is a fun to jump back to with more data
%% to continue scanning where it was interrupted.
%%
%% more/8 and done/6 handles scanning from I/O-server (Stream) or from String.
%%

%% String
more(Cs, Stack, Toks, {Line, Col}, eos, Errors, _TabWidth, _FileFormat, Fun) ->
    erlang:error(badstate, [Cs, Stack, Toks, {Line, Col}, eos, Errors, Fun]);
% %% Debug clause for chopping string into 1-char segments
% more(Cs, Stack, Toks, Pos, [H|T], Errors, Fun) ->
%     Fun(Cs++[H], Stack, Toks, Pos, T, Errors);
more(Cs, Stack, Toks, {Line, Col}, [], Errors, TabWidth, FileFormat, Fun) ->
    Fun(Cs ++ eof, Stack, Toks, {Line, Col}, eos, Errors, TabWidth,FileFormat);
%% Stream
more(Cs, Stack, Toks, {Line, Col}, eof, Errors, TabWidth, FileFormat, Fun) ->
    erlang:error(badstate, [Cs, Stack, Toks, {Line, Col}, eof, Errors, TabWidth, FileFormat, Fun]);
more(Cs, Stack, Toks, {Line, Col}, io, Errors, TabWidth, FileFormat, Fun) ->
    {more, {Cs, Stack, Toks, {Line, Col}, io, Errors,TabWidth, FileFormat, Fun}}.

%% String
done(eof, [], Toks, {Line, Col}, eos, _TabWidth,_FileFormat) ->
    {ok, reverse(Toks), {Line, Col}};
done(eof, Errors, _Toks, {Line, Col}, eos, _TabWidth,_FileFormat) ->
    {Error, ErrorPos} = lists:last(Errors),
    {error, {ErrorPos, ?MODULE, Error}, {Line, Col}};
done(Cs, Errors, Toks, {Line, Col}, eos, TabWidth,FileFormat) ->
    scan(Cs, [], Toks, {Line, Col}, eos, Errors, TabWidth,FileFormat);
%% Debug clause for chopping string into 1-char segments
%% done(Cs, Errors, Toks, Pos, [H|T]) ->
%%    scan(Cs++[H], [], Toks, Pos, T, Errors);
done(Cs, Errors, Toks, {Line, Col}, [], TabWidth,FileFormat) ->
    scan(Cs ++ eof, [], Toks, {Line, Col}, eos, Errors, TabWidth,FileFormat);
%% Stream
done(Cs, [], [{dot, _} | _] = Toks, {Line, Col}, io, _TabWidth,_FileFormat) ->
    {done, {ok, reverse(Toks), {Line, Col}}, Cs};
done(Cs, [], [_ | _], {Line, Col}, io, _TabWidth,_FileFormat) ->
    {done,
     {error, {{Line, Col}, ?MODULE, scan}, {Line, Col}}, Cs};
done(Cs, [], [], {Line, Col}, eof, _TabWidth,_FileFormat) ->
    {done, {eof, {Line, Col}}, Cs};
done(Cs, [], [{dot, _} | _] = Toks, {Line, Col}, eof, _TabWidth,_FileFormat) ->
    {done, {ok, reverse(Toks), {Line, Col}}, Cs};
done(Cs, [], _Toks, {Line, Col}, eof, _TabWidth,_FileFormat) ->
    {done,
     {error, {{Line, Col}, ?MODULE, scan}, {Line, Col}}, Cs};
done(Cs, Errors, _Toks, {Line, Col}, io, _TabWidth,_FileFormat) ->
    {Error, ErrorPos} = lists:last(Errors),
    {done, {error, {ErrorPos, ?MODULE, Error}, {Line, Col}},
     Cs};
done(Cs, Errors, _Toks, {Line, Col}, eof, _TabWidth,_FileFormat) ->
    {Error, ErrorPos} = lists:last(Errors),
    {done, {error, {ErrorPos, ?MODULE, Error}, {Line, Col}},
     Cs}.

%% The actual scan loop
%% Stack is assumed to be [].

scan([$\r|Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->   %CR
    case FileFormat of 
	mac ->scan(Cs, Stack, Toks, {Line+1, 1}, State, Errors, TabWidth,FileFormat);
	_ ->
	    scan(Cs, Stack, Toks, {Line, Col+1}, State, Errors, TabWidth,FileFormat)
    end;
scan([$\n | Cs], Stack, Toks, {Line, _Col}, State, Errors, TabWidth,FileFormat) ->      % Newline - skip
    scan(Cs, Stack, Toks, {Line + 1, 1}, State, Errors, TabWidth,FileFormat);

%%Begin of Adding by Huiqing
scan([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C == $\t  ->                          
    scan(Cs, Stack, Toks, {Line, Col + TabWidth}, State, Errors, TabWidth,FileFormat);
%% End of adding by Huiqing

scan([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $\000, C =<$\s ->                          % Control chars - skip
    scan(Cs, Stack, Toks, {Line, Col + 1}, State, Errors, TabWidth,FileFormat);  %% This is problematic; not all chars occupy one space.

scan([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $\200, C =< $\240 ->                        % Control chars -skip
    scan(Cs, Stack, Toks, {Line, Col + 1}, State, Errors, TabWidth,FileFormat);
scan([C | Cs], _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $a, C =< $z ->                              % Atoms
    sub_scan_name(Cs, [C, fun scan_atom/8], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan([C | Cs], _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $\337, C =< $\377, C /= $\367 ->                     % Atoms
    sub_scan_name(Cs, [C, fun scan_atom/8], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan([C | Cs], _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $A,C =< $Z ->                              % Variables
    sub_scan_name(Cs, [C, fun scan_variable/8], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan([C | Cs], _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $\300, C =< $\336, C /= $\327 ->                     % Variables
    sub_scan_name(Cs, [C, fun scan_variable/8], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan([$_ | Cs], _Stack, Toks, {Line, Col}, State,   Errors, TabWidth,FileFormat) ->      % _Variables
    sub_scan_name(Cs, [$_, fun scan_variable/8], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan([C | Cs], _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $0, C =< $9 ->                            % Numbers
    scan_number(Cs, [C], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan([$$ | Cs], Stack, Toks, {Line, Col}, State,
     Errors, TabWidth,FileFormat) ->        % Character constant
    scan_char(Cs, Stack, Toks, {Line, Col+1}, State, Errors, TabWidth,FileFormat);
scan([$' | Cs], _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->      % Quoted atom
    scan_qatom(Cs, [$', {Line, Col}], Toks, {Line, Col+1}, State, Errors, TabWidth,FileFormat);
scan([$" | Cs], _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->      % String
    scan_string(Cs, [$", {Line, Col}], Toks, {Line, Col+1},State, Errors, TabWidth,FileFormat);
scan([$% | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->       % Comment
     scan_comment(Cs, Stack, Toks, {Line, Col+2}, State,Errors, TabWidth,FileFormat);
%% Punctuation characters and operators, first recognise multiples.
%% Clauses are rouped by first character (a short with the same head has
%% to come after a longer).
%%
%% << <- <=
scan("<<" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'<<', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("<-" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'<-', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("<=" ++ Cs, Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'<=', {Line, Col}} | Toks],{Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("<" = Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat, fun scan/8 );
%% >> >=
scan(">>" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'>>', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan(">=" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'>=', {Line, Col}} | Toks],{Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan(">" = Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat, fun scan/8);
%% -> --
scan("->" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'->', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("--" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'--', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("-" = Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun scan/8);
%% ++
scan("++" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'++', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("+" = Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat, fun scan/8);
%% =:= =/= =< ==
scan("=:=" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'=:=', {Line, Col}} | Toks],{Line, Col + 3}, State, Errors, TabWidth,FileFormat);
scan("=:" = Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun scan/8);
scan("=/=" ++ Cs, Stack, Toks, {Line, Col}, State,  Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'=/=', {Line, Col}} | Toks], {Line, Col + 3}, State, Errors, TabWidth,FileFormat);
scan("=/" = Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun scan/8);
scan("=<" ++ Cs, Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'=<', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("=>" ++ Cs, Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'=>', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan(":=" ++ Cs, Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{':=', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("==" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'==', {Line, Col}} | Toks],{Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("=" = Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun scan/8);
%% /=
scan("/=" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'/=', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("/" = Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun scan/8);
%% ||
scan("||" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'||', {Line, Col}} | Toks], {Line, Col + 2}, State, Errors, TabWidth,FileFormat);
scan("|" = Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat, fun scan/8);
%% :-
scan(":-" ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{':-', {Line, Col}} | Toks],{Line, Col + 2}, State, Errors, TabWidth,FileFormat);
%% :: for typed records
scan("::"++Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'::',{Line, Col}}|Toks], {Line, Col+2}, State, Errors, TabWidth,FileFormat);
scan(":" = Cs, Stack, Toks, {Line, Col}, State,  Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat, fun scan/8);

scan("..."++Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat) ->
    scan(Cs, Stack, [{'...', {Line, Col}}|Toks], {Line, Col+3}, State, Errors, TabWidth, FileFormat);
scan(".."=Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun scan/8);
scan(".."++Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat) ->
    scan(Cs, Stack, [{'..', {Line, Col}}|Toks], {Line, Col+2}, State, Errors, TabWidth, FileFormat);
scan("."=Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun scan/8);
%% Full stop and plain '.'
scan("." ++ Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan_dot(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
%% All single-char punctuation characters and operators (except '.')
scan([C | Cs], Stack, Toks, {Line, Col}, State,  Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{list_to_atom([C]), {Line, Col}} | Toks], {Line, Col + 1}, State, Errors, TabWidth,FileFormat);
%%
scan([], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat,fun scan/8);
scan(Eof, _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    done(Eof, Errors, Toks, {Line, Col}, State, TabWidth,FileFormat).

scan_atom(Cs, Name, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    case catch list_to_atom(Name) of
      Atom when is_atom(Atom) ->
	  case reserved_word(Atom) of
	    true ->
		  scan(Cs, [], [{Atom, {Line, Col}} | Toks],
		     {Line, Col + length(Name)}, State, Errors, TabWidth,FileFormat);
	    false ->
		  scan(Cs, [], [{atom, {Line, Col}, Atom} | Toks],
		     {Line, Col + length(Name)}, State, Errors, TabWidth,FileFormat)
	  end;
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, atom}, {Line, Col}} | Errors],TabWidth,FileFormat)
    end.

scan_variable(Cs, Name, Toks, {Line, Col}, State,
	      Errors, TabWidth,FileFormat) ->
    case catch list_to_atom(Name) of
      A when is_atom(A) ->
	  scan(Cs, [], [{var, {Line, Col}, A} | Toks],
	       {Line, Col + length(Name)}, State, Errors, TabWidth,FileFormat);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, var}, {Line, Col}} | Errors], TabWidth,FileFormat)
    end.

%% Scan for a name - unqouted atom or variable, after the first character.
%%
%% Stack argument: return fun.
%% Returns the scanned name on the stack, unreversed.
%%
sub_scan_name([C | Cs] = Css, Stack, Toks, {Line, Col},State, Errors,TabWidth,FileFormat) ->
    case name_char(C) of
      true ->
	  sub_scan_name(Cs, [C | Stack], Toks, {Line, Col}, State,Errors, TabWidth,FileFormat);
      false ->
	  [Fun | Name] = reverse(Stack),
	  Fun(Css, Name, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    end;
sub_scan_name([], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun sub_scan_name/8);
sub_scan_name(Eof, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    [Fun | Name] = reverse(Stack),
    Fun(Eof, Name, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat).

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $\337, C =< $\377, C /= $\367 -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $\300, C =< $\336, C /= $\327 -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char($_) -> true;
name_char($@) -> true;
name_char(_) -> false.

scan_char([$\\ | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    sub_scan_escape(Cs, [fun scan_char_escape/8, $\\|Stack], Toks, {Line, Col+1}, State, Errors, TabWidth,FileFormat);
scan_char([$\n | Cs], _Stack, Toks, {Line, Col}, State,  Errors, TabWidth,FileFormat) ->
    scan(Cs, [], [{char, {Line, Col}, $\n} | Toks], {Line + 1, Col}, State, Errors, TabWidth,FileFormat);
scan_char([$  | Cs], _Stack, Toks, {Line, Col}, State,  Errors, TabWidth,FileFormat) ->
    scan(Cs, [], [{char, {Line, Col}, 32} | Toks], {Line, Col+1}, State, Errors, TabWidth,FileFormat);
scan_char([], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun scan_char/8);
scan_char(Cs, Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    scan_char_escape(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat).
 
scan_char_escape([nl | Cs], _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, [], [{char, {Line, Col}, $\n} | Toks], {Line + 1, 1}, State, Errors, TabWidth,FileFormat);
scan_char_escape([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
     C1 = case Stack of  [$\\|_] ->
		  list_to_atom("$\\"++[C]);
	     _ ->
		 C 
	 end,
    scan(Cs, [], [{char, {Line, Col-1}, C1} | Toks],{Line, Col + 1}, State, Errors, TabWidth,FileFormat);
scan_char_escape(Eof, _Stack, _Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    done(Eof, [{char, {Line, Col}} | Errors], [], {Line, Col + 1}, State, TabWidth,FileFormat).

scan_string([$" | Cs], Stack, Toks, {Line, Col}, State,  Errors, TabWidth, FileFormat) ->
    [StartPos, $" | S] = reverse(Stack),
    scan(Cs, [], [{string, StartPos, S} | Toks],{Line, Col+1}, State, Errors, TabWidth,FileFormat);

scan_string([$\r | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    case FileFormat of 
	mac -> scan_string(Cs, [$\r | Stack], Toks, {Line + 1, 1},
			   State, Errors, TabWidth,FileFormat);
	_ -> scan_string(Cs, [$\r | Stack], Toks, {Line, Col+1},
			 State, Errors, TabWidth,FileFormat)
    end;
scan_string([$\n | Cs], Stack, Toks, {Line, _Col}, State,  Errors, TabWidth,FileFormat) ->
    scan_string(Cs, [$\n | Stack], Toks, {Line + 1, 1}, State, Errors, TabWidth,FileFormat);
scan_string([$\\ | Cs], Stack, Toks, {Line, Col}, State,  Errors, TabWidth,FileFormat) ->
     sub_scan_escape( Cs, [fun scan_string_escape/8, $\\ | Stack], Toks, {Line, Col+1}, State, Errors, TabWidth,FileFormat);
scan_string([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) 
   when C==$\t ->
    scan_string(Cs, [C | Stack], Toks, {Line, Col+TabWidth}, State,Errors, TabWidth,FileFormat);
scan_string([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan_string(Cs, [C | Stack], Toks, {Line, Col+1}, State,Errors, TabWidth,FileFormat);
scan_string([], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun scan_string/8);
scan_string(Eof, Stack, _Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
     [StartPos, $" | S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string, $", SS}, StartPos} | Errors], [],
	 {Line, Col}, State, TabWidth,FileFormat).

scan_string_escape([nl | Cs], Stack, Toks, {Line, _Col}, State, Errors, TabWidth,FileFormat) ->
    scan_string(Cs, [$\n | Stack], Toks, {Line + 1, 1}, State, Errors, TabWidth,FileFormat);
scan_string_escape([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan_string(Cs, [C| Stack], Toks, {Line, Col+1}, State, Errors, TabWidth,FileFormat);
scan_string_escape(Eof, Stack, _Toks, {Line, Col},State, Errors, TabWidth,FileFormat) ->
    [StartPos, $" | S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string, $", SS}, StartPos} | Errors], [],
	 {Line, Col + length(S) + 2}, State, TabWidth,FileFormat).

scan_qatom([$' | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    [StartPos, $' | S] = reverse(Stack),
    case catch list_to_atom(S) of
      A when is_atom(A) ->
	  scan(Cs, [], [{atom, StartPos, A} | Toks],{Line, Col + 1}, State, Errors, TabWidth,FileFormat);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,[{{illegal, atom}, StartPos} | Errors], TabWidth,FileFormat)
    end;
scan_qatom([$\r|Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    case FileFormat of 
	mac -> scan_qatom(Cs,[$\r|Stack], Toks, {Line+1, 1}, State, Errors, TabWidth,FileFormat);
	_ -> scan_qatom(Cs,[$\r|Stack], Toks, {Line, Col+1}, State, Errors, TabWidth,FileFormat)
    end;
scan_qatom([$\n | Cs], Stack, Toks, {Line, _Col}, State, Errors, TabWidth,FileFormat) ->
    scan_qatom(Cs, [$\n | Stack], Toks, {Line + 1, 1},State, Errors, TabWidth,FileFormat);
%% scan_qatom([$\\ | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
%%     sub_scan_escape(Cs, [fun scan_qatom_escape/8, $\\ | Stack], Toks, {Line, Col+1}, State, Errors, TabWidth,FileFormat);
scan_qatom([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
  when C==$\t ->
    scan_qatom(Cs, [C | Stack], Toks, {Line, Col+TabWidth}, State,  Errors, TabWidth,FileFormat);
scan_qatom([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan_qatom(Cs, [C | Stack], Toks, {Line, Col+1}, State,  Errors, TabWidth,FileFormat);
scan_qatom([], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat, fun scan_qatom/8);
scan_qatom(Eof, Stack, _Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    [StartPos, $' | S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string, $', SS}, StartPos} | Errors], [],{Line, Col}, State, TabWidth,FileFormat).

scan_qatom_escape([nl | Cs], Stack, Toks, {Line, _Col}, State, Errors, TabWidth,FileFormat) ->
    scan_qatom(Cs, [$\n | Stack], Toks, {Line + 1, 1}, State, Errors, TabWidth,FileFormat);
scan_qatom_escape([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan_qatom(Cs, [C | Stack], Toks, {Line, Col+1}, State, Errors, TabWidth,FileFormat);
scan_qatom_escape(Eof, Stack, _Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    [StartPos, $' | S] = reverse(Stack),
    SS = string:substr(S, 1, 16),
    done(Eof, [{{string, $', SS}, StartPos} | Errors], [],
	 {Line, Col}, State, TabWidth,FileFormat).

%% Scan for a character escape sequence, in character literal or string.
%% A string is a syntactical sugar list (e.g "abc")
%% or a quoted atom (e.g 'EXIT').
%%
%% Stack argument: return fun.
%% Returns the resulting escape character on the stream.
%% The return atom 'nl' means that the escape sequence Backslash Newline
%% was found, i.e an actual Newline in the input.
%%
%% \<1-3> octal digits
sub_scan_escape([O1, O2, O3 | Cs], [Fun|Stack], Toks,
		{Line, Col}, State, Errors, TabWidth,FileFormat)
    when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0,
	 O3 =< $7 ->
    case reverse(Stack) of 
        [_,$"|_]->
           Fun([O1, O2, O3 | Cs], Stack, Toks, {Line, Col+2}, State,
                Errors, TabWidth,FileFormat);
        _  ->
           C1=list_to_atom("$\\"++[O1, O2, O3]),
           scan(Cs, [], [{char, {Line, Col-2}, C1} | Toks],{Line, Col + 3}, State, Errors, TabWidth,FileFormat)
     end;    
sub_scan_escape([O1, O2] = Cs, Stack, Toks, {Line, Col},
		State, Errors, TabWidth,FileFormat)
    when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat,
	 fun sub_scan_escape/8);
sub_scan_escape([O1, O2 | Cs], [_Fun | _Stack], Toks,
		{Line, Col}, State, Errors, TabWidth,FileFormat)
    when O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    C1=list_to_atom("$\\"++[O1, O2]),
    scan(Cs, [], [{char, {Line, Col-2}, C1} | Toks],{Line, Col + 2}, State, Errors, TabWidth,FileFormat);
    %%Val = O1 * 8 + O2 - 9 * $0,
    %% Fun([O1,O2 | Cs], Stack, Toks, {Line, Col+1}, State,
    %%     Errors, TabWidth,FileFormat);
sub_scan_escape([O1] = Cs, Stack, Toks, {Line, Col},
		State, Errors, TabWidth,FileFormat)
    when O1 >= $0, O1 =< $7 ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat,
	 fun sub_scan_escape/8);
sub_scan_escape([O1 | Cs], [Fun | Stack], Toks,
		{Line, Col}, State, Errors, TabWidth,FileFormat)
    when O1 >= $0, O1 =< $7 ->
   %% Val = O1 - $0,
    Fun([O1 | Cs], Stack, Toks, {Line, Col}, State,
	Errors, TabWidth,FileFormat);
%% \^X -> CTL-X
sub_scan_escape([$^, C | Cs], [Fun | Stack], Toks,
		{Line, Col}, State, Errors, TabWidth,FileFormat) ->
   %% Val = C band 31,
    Fun([C | Cs], Stack, Toks, {Line, Col}, State,
	Errors, TabWidth,FileFormat);
sub_scan_escape([$^] = Cs, Stack, Toks, {Line, Col},
		State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat,
	 fun sub_scan_escape/8);
sub_scan_escape([$^ | Eof], [Fun | Stack], Toks,
		{Line, Col}, State, Errors, TabWidth,FileFormat) ->
    Fun(Eof, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
%% \NL (backslash newline)
sub_scan_escape([$\n | Cs], [Fun | Stack], Toks,
		{Line, Col}, State, Errors, TabWidth,FileFormat) ->
    Fun([nl | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
%% \X - familiar escape sequences
sub_scan_escape([C | Cs], [Fun | Stack], Toks,
		{Line, Col}, State, Errors, TabWidth,FileFormat) ->
    %%Val = escape_char(C),
    Fun([C | Cs], Stack, Toks, {Line, Col}, State,
	Errors, TabWidth,FileFormat);
%%
sub_scan_escape([], Stack, Toks, {Line, Col}, State,
		Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat,
	 fun sub_scan_escape/8);
sub_scan_escape(Eof, [Fun | Stack], Toks, {Line, Col},
		State, Errors, TabWidth,FileFormat) ->
    Fun(Eof, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat).

escape_char($n) -> $\n;                         %\n = LF
escape_char($r) -> $\r;                         %\r = CR
escape_char($t) ->
    $\t;                         %\t = TAB
escape_char($v) -> $\v;                         %\v = VT
escape_char($b) -> $\b;                         %\b = BS
escape_char($f) -> $\f;                         %\f = FF
escape_char($e) ->
    $\e;                         %\e = ESC
escape_char($s) ->
    $\s;                         %\s = SPC
escape_char($d) ->
    $\d;                         %\d = DEL
escape_char(C) -> C.

scan_number([$., C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $0, C =< $9 ->
    scan_fraction(Cs, [C, $. | Stack], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan_number([$.] = Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more(Cs, Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat,fun scan_number/8);
scan_number([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $0, C =< $9 ->
    scan_number(Cs, [C | Stack], Toks, {Line, Col}, State,Errors, TabWidth,FileFormat);
scan_number([$# | Cs], Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    case catch list_to_integer(reverse(Stack)) of
      B when is_integer(B), B >= 2, B =< 1 + $Z - $A + 10 ->
	  scan_based_int(Cs, [B], Toks, {Line, Col}, State,Errors, TabWidth,FileFormat);
      B ->
	  scan(Cs, [], Toks, {Line, Col}, State,[{{base, B}, {Line, Col}} | Errors], TabWidth,FileFormat)
    end;
scan_number([], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat, fun scan_number/8);
scan_number(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    case catch list_to_integer(reverse(Stack)) of
      N when is_integer(N) ->
	    scan(Cs, [], [{integer, {Line, Col}, reverse(Stack)} | Toks],
	       {Line, Col + length(Stack)}, State, Errors, TabWidth,FileFormat);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, integer}, {Line, Col}} | Errors], TabWidth,FileFormat)
    end.

scan_based_int([C | Cs], [B | Stack], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $0, C =< $9, C < $0 + B ->
    scan_based_int(Cs, [B, C | Stack], Toks, {Line, Col},State, Errors, TabWidth,FileFormat);
scan_based_int([C | Cs], [B | Stack], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $A, B > 10, C < $A + B - 10 ->
    scan_based_int(Cs, [B, C | Stack], Toks, {Line, Col},State, Errors, TabWidth,FileFormat);
scan_based_int([C | Cs], [B | Stack], Toks, {Line, Col},State, Errors,TabWidth,FileFormat)
    when C >= $a, B > 10, C < $a + B - 10 ->
    scan_based_int(Cs, [B, C | Stack], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan_based_int([], Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat,fun scan_based_int/8);
scan_based_int(Cs, [B | Stack], Toks, {Line, Col},State, Errors, TabWidth,FileFormat) ->
     case catch erlang:list_to_integer(reverse(Stack), B) of
      N when is_integer(N) ->
	  scan(Cs, [], [{integer, {Line, Col}, integer_to_list(B)++[$#| reverse(Stack)]} | Toks],   %% "replaced 'N' with 'reverse(Stack)'";
	       {Line, Col + length(integer_to_list(B))+1+length(Stack)}, State, Errors, TabWidth,FileFormat);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, integer}, {Line, Col}} | Errors], TabWidth,FileFormat)
    end.

scan_fraction([C | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat)
    when C >= $0, C =< $9 ->
    scan_fraction(Cs, [C | Stack], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan_fraction([$e | Cs], Stack, Toks, {Line, Col},State, Errors, TabWidth,FileFormat) ->
    scan_exponent_sign(Cs, [$E | Stack], Toks, {Line, Col},State, Errors, TabWidth,FileFormat);
scan_fraction([$E | Cs], Stack, Toks, {Line, Col},State, Errors, TabWidth,FileFormat) ->
    scan_exponent_sign(Cs, [$E | Stack], Toks, {Line, Col},State, Errors, TabWidth,FileFormat);
scan_fraction([], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat, fun scan_fraction/8);
scan_fraction(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    case catch list_to_float(reverse(Stack)) of
      F when is_float(F) ->
	  scan(Cs, [], [{float, {Line, Col}, F} | Toks],
	       {Line, Col + length(Stack)}, State, Errors, TabWidth,FileFormat);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, float}, {Line, Col}} | Errors], TabWidth,FileFormat)
    end.

scan_exponent_sign([$+ | Cs], Stack, Toks, {Line, Col},State, Errors, TabWidth,FileFormat) ->
    scan_exponent(Cs, [$+ | Stack], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan_exponent_sign([$- | Cs], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan_exponent(Cs, [$- | Stack], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan_exponent_sign([], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors, TabWidth, FileFormat, fun scan_exponent_sign/8);
scan_exponent_sign(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan_exponent(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat).

scan_exponent([C | Cs], Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat)
    when C >= $0, C =< $9 ->
    scan_exponent(Cs, [C | Stack], Toks, {Line, Col}, State, Errors, TabWidth,FileFormat);
scan_exponent([], Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat, fun scan_exponent/8);
scan_exponent(Cs, Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    case catch list_to_float(reverse(Stack)) of
      F when is_float(F) ->
	  scan(Cs, [], [{float, {Line, Col}, F} | Toks],
	       {Line, Col + length(Stack)}, State, Errors, TabWidth,FileFormat);
      _ ->
	  scan(Cs, [], Toks, {Line, Col}, State,
	       [{{illegal, float}, {Line, Col}} | Errors], TabWidth,FileFormat)
    end.

scan_comment([$\r| Cs], Stack, Toks, {Line, Col},State, Errors, TabWidth,FileFormat) ->
    case FileFormat of 
	mac ->
	    scan(Cs, Stack, Toks, {Line + 1, 1}, State, Errors, TabWidth,FileFormat);
	_ ->
	   scan(Cs, Stack, Toks, {Line, Col+1}, State, Errors, TabWidth,FileFormat)
	end;
scan_comment([$\n| Cs], Stack, Toks, {Line, _Col},State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, Toks, {Line + 1, 1}, State, Errors, TabWidth,FileFormat);	
scan_comment([_ | Cs], Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat) ->
    scan_comment(Cs, Stack, Toks, {Line, Col + 1}, State,Errors, TabWidth,FileFormat);
scan_comment([], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat,fun scan_comment/8);
scan_comment(Eof, _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    done(Eof, Errors, Toks, {Line, Col}, State, TabWidth,FileFormat).

scan_dot([$% | _] = Cs, _Stack, Toks, {Line, Col},State, Errors, TabWidth,FileFormat) ->
    done(Cs, Errors, [{dot, {Line, Col}} | Toks], {Line, Col + 1}, State, TabWidth,FileFormat);
scan_dot([$\r | Cs], _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    case FileFormat of 
	mac ->
	    done(Cs, Errors, [{dot, {Line, Col}} | Toks],{Line + 1, 1}, State, TabWidth,FileFormat);
	_ -> 
	    done(Cs, Errors, [{dot, {Line, Col}} | Toks],{Line, Col+1}, State, TabWidth,FileFormat)
    end;	
scan_dot([$\n | Cs], _Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    done(Cs, Errors, [{dot, {Line, Col}} | Toks],{Line + 1, 1}, State, TabWidth,FileFormat);
scan_dot([C | Cs], _Stack, Toks, {Line, Col}, State,Errors, TabWidth,FileFormat)
  when C >= $\000, C =< $\s ->   %% This is problematic; some characters occupy 2 spaces.
      done(Cs, Errors, [{dot, {Line, Col}} | Toks], {Line, Col + 2}, State, TabWidth,FileFormat);
scan_dot([C | Cs], _Stack, Toks, {Line, Col}, State,Errors,TabWidth,FileFormat)
    when C >= $\200, C =< $\240 -> 
    done(Cs, Errors, [{dot, {Line, Col}} | Toks],{Line, Col + 2}, State, TabWidth,FileFormat);
scan_dot([], Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    more([], Stack, Toks, {Line, Col}, State, Errors,TabWidth, FileFormat, fun scan_dot/8);
scan_dot(eof, _Stack, Toks, {Line, Col}, State,	 Errors, TabWidth,FileFormat) ->
    done(eof, Errors, [{dot, {Line, Col}} | Toks],{Line, Col}, State, TabWidth,FileFormat);
scan_dot(Cs, Stack, Toks, {Line, Col}, State, Errors, TabWidth,FileFormat) ->
    scan(Cs, Stack, [{'.', {Line, Col}} | Toks], {Line, Col + 1}, State, Errors, TabWidth,FileFormat).

%% reserved_word(Atom) -> Bool
%%   return 'true' if Atom is an Erlang reserved word, else 'false'.

reserved_word('after') -> true;
reserved_word('begin') -> true;
reserved_word('case') -> true;
reserved_word('try') ->
    Opts = get_compiler_options(),
    not member(disable_try, Opts);
reserved_word('cond') ->
    Opts = get_compiler_options(),
    not member(disable_cond, Opts);
reserved_word('catch') -> true;
reserved_word('andalso') -> true;
reserved_word('orelse') -> true;
reserved_word('end') -> true;
reserved_word('fun') -> true;
reserved_word('if') -> true;
reserved_word('let') -> true;
reserved_word('of') -> true;
reserved_word('query') -> true;
reserved_word('receive') -> true;
reserved_word('when') -> true;
reserved_word('bnot') -> true;
reserved_word('not') -> true;
reserved_word('div') -> true;
reserved_word('rem') -> true;
reserved_word('band') -> true;
reserved_word('and') -> true;
reserved_word('bor') -> true;
reserved_word('bxor') -> true;
reserved_word('bsl') -> true;
reserved_word('bsr') -> true;
reserved_word('or') -> true;
reserved_word('xor') -> true;
reserved_word(_) -> false.


get_compiler_options() ->
    %% Who said that Erlang has no global variables?
    case get(compiler_options) of
      undefined ->
	  Opts = case catch ets:lookup(compiler__tab,
				       compiler_options)
		     of
		   [{compiler_options, O}] -> O;
		   _ -> []
		 end,
	  put(compiler_options, Opts),
	  Opts;
      Opts -> Opts
    end.
