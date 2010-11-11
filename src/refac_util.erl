%% Copyright (c) 2010, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%% =====================================================================
%% Some utility functions used by the refactorer.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================

-module(refac_util).

-export([tokenize/3,file_format/1]).

-export([expand_files/2]).

-export([get_modules_by_file/1,test_framework_used/1,concat_toks/1]).

-include("../include/wrangler.hrl").

-include_lib("kernel/include/file.hrl").

%% =====================================================================
%%-spec(tokenize(File::filename(), WithLayout::boolean(), TabWidth::integer()) -> [token()]).
tokenize(File, WithLayout, TabWidth) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    S = erlang:binary_to_list(Bin),
	    case WithLayout of 
		true -> 
		    {ok, Ts, _} = refac_scan_with_layout:string(S, {1,1}, TabWidth, file_format(File)),
		    Ts;
		_ -> {ok, Ts, _} = refac_scan:string(S, {1,1}, TabWidth,file_format(File)),
		     Ts
	    end;
	{error, Reason} ->
	    Msg = lists:flatten(io_lib:format("Wrangler could not read file ~s: ~w \n", 
				[filename:dirname(File), Reason])),
	    throw({error, Msg})
    end.

%% =====================================================================
%% @doc Recursively collect all the files with the given file extension 
%%  in the specified directoris/files.

%%-spec(expand_files(FileDirs::[filename()|dir()], Ext::string()) -> [filename()]).
expand_files(FileDirs, Ext) ->
    expand_files(FileDirs, Ext, []).

expand_files([FileOrDir | Left], Ext, Acc) ->
    case filelib:is_dir(FileOrDir) of
      true ->
	    case file:list_dir(FileOrDir) of 
		{ok, List} ->
		    NewFiles = [filename:join(FileOrDir, X)
				|| X <- List, filelib:is_file(filename:join(FileOrDir, X)), filename:extension(X) == Ext],
		    NewDirs = [filename:join(FileOrDir, X) || X <- List, filelib:is_dir(filename:join(FileOrDir, X))],
		    expand_files(NewDirs ++ Left, Ext, NewFiles ++ Acc);
		{error, Reason} ->
		     Msg = io_lib:format("Wrangler could not read directory ~s: ~w \n", 
				[filename:dirname(FileOrDir), Reason]),
		    throw({error, lists:flatten(Msg)})
	    end;
	false ->
	    case filelib:is_regular(FileOrDir) of
		true ->
		    case filename:extension(FileOrDir) == Ext of
			true -> expand_files(Left, Ext, [FileOrDir | Acc]);
			false -> expand_files(Left, Ext, Acc)
		    end;
		_ -> expand_files(Left, Ext, Acc)
	    end
    end;
expand_files([], _Ext, Acc) -> ordsets:from_list(Acc).


%% =====================================================================
%% @doc The a list of files to a list of two-element tuples, with the first 
%% element of the tuple being the module name, and the second element 
%% binding the directory name of the file to which the module belongs.

%%-spec(get_modules_by_file(Files::[filename()]) -> [{atom(), dir()}]).
get_modules_by_file(Files) ->
    get_modules_by_file(Files, []).

get_modules_by_file([File | Left], Acc) ->
    BaseName = filename:basename(File, ".erl"),
    Dir = filename:dirname(File),
    get_modules_by_file(Left, [{list_to_atom(BaseName), Dir} | Acc]);
get_modules_by_file([], Acc) -> lists:reverse(Acc).


%%-spec file_format(filename()) ->dos|mac|unix. 		 
file_format(File) -> 
    case file:read_file(File) of 
	{ok, Bin} ->
	    S = erlang:binary_to_list(Bin),
	    LEs = scan_line_endings(S),
	    case LEs of 
		[] -> unix;    %% default fileformat;
		_ ->  case lists:all(fun(E) -> E=="\r\n" end, LEs) of 
			  true -> dos;
			  _ -> case lists:all(fun(E) -> E=="\r" end, LEs)  of
				   true ->
				       mac;
				   _ -> case lists:all(fun(E)-> E=="\n" end, LEs) of
					    true -> unix;
					    _ -> throw({error, File ++ " uses a mixture of line endings,"
							" please normalise it to one of the standard file "
							"formats (i.e. unix/dos/mac) before performing any refactorings."})
					end
			       end
		      end
	    end;
	{error, Reason} ->
	    Msg = io_lib:format("Wrangler could not read file ~s: ~w \n", 
				[filename:dirname(File), Reason]),
	    throw({error, lists:flatten(Msg)})
    end.

scan_line_endings(Cs)->
    scan_lines(Cs, [], []).

scan_lines([$\r|Cs], [], Acc) ->
    scan_line_endings(Cs, [$\r], Acc);
scan_lines([$\n|Cs], [], Acc) ->
    scan_lines(Cs, [], [[$\n]|Acc]);
scan_lines([_C|Cs], [], Acc) ->
    scan_lines(Cs, [], Acc);
scan_lines([],[],Acc) ->
    Acc.

scan_line_endings([$\r|Cs], Cs1,Acc) ->
    scan_line_endings(Cs,[$\r|Cs1], Acc);
scan_line_endings([$\n|Cs], Cs1, Acc) ->
    scan_lines(Cs, [],[lists:reverse([$\n|Cs1])| Acc]);
scan_line_endings([_C|Cs], Cs1, Acc)->
    scan_lines(Cs, [], [lists:usort(lists:reverse(Cs1))|Acc]);
scan_line_endings([], Cs1, Acc)->
    lists:reverse([lists:usort(lists:reverse(Cs1))|Acc]).

%%-spec test_framework_used(filename()) ->[atom()]. 			 
test_framework_used(FileName) ->
    case refac_epp_dodger:parse_file(FileName, []) of
      {ok, Forms} ->
	  Strs = lists:flatmap(fun (F) ->
				       case refac_syntax:type(F) of
					   attribute ->
					       Name = refac_syntax:attribute_name(F),
					       Args = refac_syntax:attribute_arguments(F),
					       case refac_syntax:type(Name) of
					       atom ->
						       AName = refac_syntax:atom_value(Name),
						   case AName == include orelse AName == include_lib of
						       true ->
							   lists:flatmap(fun (A) -> case A of
											{string, _, Str} -> [Str];
											_ -> []
										    end
									 end, Args);
						       _ -> []
						   end;
						   _ -> []
					       end;
					   _ -> []
				       end
			       end, Forms),
	  Eunit = lists:any(fun (S) -> lists:suffix("eunit.hrl", S) end, Strs),
	  EQC = lists:any(fun (S) -> lists:suffix("eqc.hrl", S) end, Strs),
	  EQC_STATEM = lists:any(fun (S) -> lists:suffix("eqc_statem.hrl", S) end, Strs),
	  EQC_FSM = lists:any(fun (S) -> lists:suffix("eqc_fsm.hrl", S) end, Strs),
	  TestSever = lists:suffix(FileName, "_SUITE.erl") and
			lists:any(fun (S) -> lists:suffix("test_server.hrl", S) end, Strs),
	  CommonTest = lists:suffix(FileName, "_SUITE.erl") and
			 lists:any(fun (S) -> lists:suffix("ct.hrl", S) end, Strs),
	  lists:flatmap(fun ({F, V}) -> case V of
					  true -> [F];
					  _ -> []
					end
			end, [{eunit, Eunit}, {eqc, EQC}, {eqc_statem, EQC_STATEM},
			      {eqc_fsm, EQC_FSM},
			      {testserver, TestSever}, {commontest, CommonTest}]);
      _ -> []
    end.
   

%%-spec(concat_toks(Toks::[token()]) ->string()).
concat_toks(Toks) ->
    concat_toks(Toks, "").

concat_toks([], Acc) ->
     lists:concat(lists:reverse(Acc));
concat_toks([T|Ts], Acc) ->
     case T of 
	 {atom, _,  V} -> S = io_lib:write_atom(V), 
			  concat_toks(Ts, [S|Acc]);
	 {qatom, _, V} -> S=atom_to_list(V),
			  concat_toks(Ts, [S|Acc]);
	 {string, _, V} -> concat_toks(Ts,["\"", V, "\""|Acc]);
	 {char, _, V} when is_atom(V)->concat_toks(Ts,[atom_to_list(V)|Acc]);
	 {char, _, V} when is_integer(V) and (V =< 127)-> concat_toks(Ts,[io_lib:write_char(V)|Acc]);
	 {char, _, V} when is_integer(V) ->
	     {ok, [Num], _} = io_lib:fread("~u", integer_to_list(V)),
	     [Str] = io_lib:fwrite("~.8B", [Num]),
	     S = "$\\"++Str,
	      concat_toks(Ts, [S|Acc]); 
	 {float, _, V} -> concat_toks(Ts,[io_lib:write(V)|Acc]);
     	 {_, _, V} -> concat_toks(Ts, [V|Acc]);
	 {dot, _} ->concat_toks(Ts, ['.'|Acc]);
      	 {V, _} -> 
	     concat_toks(Ts, [V|Acc])
     end.

