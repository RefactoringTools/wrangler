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
%% Module-level call graph construction.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%======================================================================

-module(refac_module_graph). 

-export([module_graph/1, module_graph_to_dot/2, module_subgraph_to_dot/3]). 

-export([collect_called_modules/2]).

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.

-include("../include/wrangler.hrl").
 
-spec(module_graph/1::([dir()]) -> [{filename(), [filename()]}]).
module_graph(SearchPaths) ->
    ModCallerCallee = create_caller_callee_graph(SearchPaths),
    reverse_module_graph(ModCallerCallee).

create_caller_callee_graph(SearchPaths) ->
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    ?debug("Files:\n~p\n", [Files]),
    ModMap = refac_util:get_modules_by_file(Files),
    analyze_all_files(ModMap, SearchPaths).
   
    
analyze_all_files([], _SearchPaths)->
    ets:foldr(fun({{Mod, Dir}, CalledMods, _CheckSum}, S) -> 
			    FileName = filename:join(Dir, atom_to_list(Mod)++".erl"),
			    case filelib:is_file(FileName) of 
				true ->
				    [{{Mod, Dir}, CalledMods}|S];
				_ -> S
			    end
		    end, [], ?ModuleGraphTab);
    
analyze_all_files([{Mod, Dir}|Left], SearchPaths) ->  
    FileName = filename:join(Dir,atom_to_list(Mod)++".erl"),
    NewCheckSum = wrangler_ast_server:filehash(FileName),
    case ets:lookup(?ModuleGraphTab, {Mod, Dir}) of
    	[] -> 
	    {called_modules, Called} = analyze_mod({Mod, Dir}, SearchPaths),
	    ets:insert(?ModuleGraphTab, {{Mod, Dir}, Called,NewCheckSum}),
	    analyze_all_files(Left, SearchPaths);
	[{{Mod, Dir}, _CalledMods, OldCheckSum}] ->
	    case NewCheckSum =:= OldCheckSum of 
		true ->
		    analyze_all_files(Left, SearchPaths);
		false -> 
		    ets:delete(?ModuleGraphTab, {Mod, Dir}),
		    {called_modules, CalledMods1} = analyze_mod({Mod, Dir}, SearchPaths),
		    ets:insert(?ModuleGraphTab, {{Mod, Dir}, CalledMods1, NewCheckSum}),
		    analyze_all_files(Left, SearchPaths)		
	    end
    end.
   
analyze_mod({Mod, Dir}, SearchPaths) ->
    File = filename:join(Dir, atom_to_list(Mod) ++ ".erl"),
    Files = refac_util:expand_files(SearchPaths, ".erl"),
    ModNames = [M || {M, _} <- refac_util:get_modules_by_file(Files)],
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(File, true, SearchPaths),
    ImportedMods0 = case lists:keysearch(imports, 1, Info) of
		      {value, {imports, Imps}} ->
			  lists:map(fun ({M, _Funs}) -> M end, Imps);
		      false -> []
		    end,
    ImportedMods1 = case lists:keysearch(module_imports, 1, Info) of
		      {value, {module_imports, Mods}} -> Mods;
		      _ -> []
		    end,
    %% I took a conservative approach here.
    {CalledMods, PossibleCalledMods} = collect_called_modules(AnnAST, ModNames),
    CalledMods1 = [M || M <- CalledMods, lists:member(M, ModNames)],
    {called_modules, lists:usort(ImportedMods0 ++ ImportedMods1 ++ 
				 CalledMods1 ++ PossibleCalledMods)}.

-spec(collect_called_modules(AnnAST::syntaxTree(), ModNames::[atom()])
      ->{[modulename()], [modulename()]}).
collect_called_modules(AnnAST, ModNames) ->
    Fun1 = fun(T, Acc) ->
		  case refac_syntax:type(T) of 
		      atom ->
			  As = refac_syntax:get_ann(T),
			  case lists:keysearch(type, 1, As) of
			      {value, {type, m_atom}} ->
				  ModName = refac_syntax:atom_value(T),
				  ordsets:add_element(ModName, Acc);
			      _ -> Acc
			  end;
		      _-> Acc
		  end
	  end,
    CalledMods = refac_syntax_lib:fold(Fun1, ordsets:new(), AnnAST),
    Fun2 = fun(T, Acc) ->
		   case refac_syntax:type(T) of
		       function ->
			   Acc++refac_rename_fun:collect_atoms(T, ModNames);
		       _ -> Acc
		   end
	   end,
    UnSures = refac_syntax_lib:fold(Fun2, [], AnnAST),
    UnSures1 = [Name||{atom, _Pos, Name} <- UnSures, 
	        not lists:member(Name, CalledMods)],
    {CalledMods, ordsets:from_list(UnSures1)}.

reverse_module_graph(List) ->
    reverse_module_graph_1(List,List, []).
reverse_module_graph_1([], _List,Acc) ->
    Acc;
reverse_module_graph_1([{{Mod, Dir},_Called_Mods}|T], List, Acc) ->
    Client_Modules = get_client_modules({Mod,Dir}, List),
    reverse_module_graph_1(T,List, [Client_Modules|Acc]).

get_client_modules({Mod, Dir}, List) ->
    F = fun ({{M, Dir1}, Called_Modules}) ->
		case lists:member(Mod, Called_Modules) of
		  true ->
		      case filelib:is_file(filename:join(Dir1, atom_to_list(Mod) ++ ".erl"))
			     andalso Dir =/= Dir1
			  of
			true ->
			    [];
			_ -> [filename:join([Dir1, atom_to_list(M) ++ ".erl"])]
		      end;
		  false -> []
		end
	end,
    {filename:join([Dir, atom_to_list(Mod) ++ ".erl"]), lists:flatmap(F, List)}.

-spec (module_subgraph_to_dot/3::(filename(), [modulename()], [filename()|dir()]) ->true).
module_subgraph_to_dot(OutFile, ModNames, SearchPaths) ->
    DotFile = filename:dirname(OutFile)++filename:rootname(OutFile)++".dot",
    ModCallerCallees = create_caller_callee_graph(SearchPaths),
    MG = digraph:new(),
    add_edges(ModCallerCallees, MG),
    SG=digraph_utils:subgraph(MG, ModNames, []),
    to_dot(SG,DotFile),
    digraph:delete(SG),
    digraph:delete(MG).

-spec (module_graph_to_dot/2::(filename(), [filename()|dir()]) ->true).				  
module_graph_to_dot(OutFile, SearchPaths) -> 
    DotFile = filename:dirname(OutFile)++filename:rootname(OutFile)++".dot",
    ModCallerCallees = create_caller_callee_graph(SearchPaths),
    MG = digraph:new(),
    add_edges(ModCallerCallees, MG),
    to_dot(MG,DotFile),
    digraph:delete(MG).

add_edges([], MG) ->
    MG;
add_edges([{{CallerMod,_}, CalleeMods}|Left], MG) ->
    Edges =[{CallerMod, CalleeMod} || CalleeMod <- CalleeMods],
    add_edges(Left, digraph_add_edges(Edges, MG)).

digraph_add_edges([], MG)-> 
    MG;
digraph_add_edges([{From, To}|Left], MG) ->
    digraph_add_edges(Left, digraph_add_edge(From, To, MG)).    
    
    
digraph_add_edge(From, To, MG) ->
    case digraph:vertex(MG, From) of 
	false ->
	    digraph:add_vertex(MG, From);
	{From, _} ->
	    ok
    end,
    case digraph:vertex(MG, To) of 
	false ->
	    digraph:add_vertex(MG, To);
	{To,_} -> ok
    end,
    digraph:add_edge(MG, {From, To}, From, To, []),
    MG.

to_dot(MG, File) ->
    Edges = digraph:edges(MG),
    hipe_dot:translate_list(Edges, File, "ModuleGraph", []).
    
