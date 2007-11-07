%% =====================================================================
%% Module-level call graph construction.
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
%%======================================================================

-module(refac_module_graph).
-export([module_callgraph/1]). 

-define(OPTS, 
	[type_only, no_type_warnings,
	 {use_callgraph, fixpoint}, get_called_modules,no_inline_fp, {pmatch, no_duplicates}, {target, x86}]).

-define(SRC_COMPILE_OPTS, 
	[to_core, binary,report_errors, no_inline]).

-include("wrangler.hrl").

module_callgraph(Options) when is_list(Options) ->
    WranglerOptions = wrangler_options:build(Options),
    InitAnalysis = build_analysis_record(WranglerOptions),
    case run_analysis(InitAnalysis) of
       {module_callgraph, _List1, List2} ->
 	  List2;
       _  -> []
   end.

build_analysis_record(WranglerOptions) ->
  IncludeDirs = WranglerOptions#options.include_dirs,
  Defines = WranglerOptions#options.defines,
  Files = ordsets:from_list(WranglerOptions#options.files),
  CoreTransform = WranglerOptions#options.core_transform,
  ModuleGraph = WranglerOptions#options.module_graph,
  #analysis{fixpoint=first, core_transform=CoreTransform,
	    defines=Defines, granularity=module,
	    include_dirs=IncludeDirs,  
	    files=Files, module_graph=ModuleGraph}.


run_analysis(Analysis) ->
  Files = Analysis#analysis.files,
  Ext = ".erl",
  NewAnalysis = case refac_util:expand_files(Files, Ext) of
		    [] ->
		%% 	io:format("WARNING: Wrangler could not find any .erl files in the specified searchpaths to caluate the client modules, please ensure the searchpaths have been set correctly."),
                        Analysis#analysis{files=[]};
		    NewFiles ->
			Analysis#analysis{files=NewFiles}
		end,
  analyze_all_files(NewAnalysis).


analyze_mod({Mod, Dir},Analysis) ->
   Res =analyze_core_file(Dir, Mod, Analysis),
   case Res of
    {error, _What} ->
      error; 
    AnalysisRes ->
      case AnalysisRes of
	{{ok, _}, {called_modules, Called}} ->
 	  {called_modules, Called};
	{ok, _} ->
	  ok;
	{'EXIT', _Why} ->
	  error 
      end
  end.

analyze_core_file(Dir, Mod, Analysis) ->  
  DefaultIncl1 = ["..", "../incl", "../inc", "../include"],
  DefaultIncl2 = [{i, filename:join(Dir,X)} || X <- DefaultIncl1],
  UserIncludes = [{i, X} || X <- Analysis#analysis.include_dirs],
  Includes = UserIncludes++DefaultIncl2,
  Defines = [{d, M, V} || {M,V} <- Analysis#analysis.defines],
  CompOpts = [{i, Dir} | ?SRC_COMPILE_OPTS] ++ Defines ++ Includes,
  ModPath = filename:join(Dir, Mod),
  case compile:file(ModPath, CompOpts) of
    {ok, _, Core} ->
      catch hipe:compile_core(list_to_atom(Mod), Core, [], ?OPTS);
    {error, Errors, _} ->
      FormatedErrors = format_errors(Errors),
      {error, lgts:flatten(FormatedErrors)}
  end.


format_errors([{Mod, Errors}|Left])->
  FormatedError = 
    [io_lib:format("~s:~w: ~s\n", [Mod, Line,apply(M,format_error, [Desc])])
     || {Line, M, Desc} <- Errors],
  [lists:flatten(FormatedError) | format_errors(Left)];
format_errors([]) ->
  [].

analyze_all_files(Analysis) ->
  Files = Analysis#analysis.files,
  ModMap = refac_util:get_modules_by_file(Files),
  Callgraph = Analysis#analysis.module_graph,
  case file:read_file(Callgraph) of 
      {ok, Res} -> analyze_all_files(ModMap, Analysis, [], {filelib:last_modified(Callgraph),binary_to_term(Res)});
      _ -> analyze_all_files(ModMap, Analysis, [],{{0,0,0},[]})
  end.

analyze_all_files([{Mod, Dir}|Left], Analysis, Acc, {CallgraphModifiedTime, Callgraph}) ->
  FileName = Dir++"/"++Mod++".erl",
  FileModifiedTime = filelib:last_modified(FileName),
  R = lists:keysearch({Mod, Dir},1, Callgraph),
  if (FileModifiedTime < CallgraphModifiedTime) and (R /= false) ->
	  {value, R1} = R,
	  analyze_all_files(Left, Analysis, [R1|Acc], {CallgraphModifiedTime, Callgraph});
     true -> 
	  case analyze_mod({Mod, Dir}, Analysis) of
		 error ->
		     analyze_all_files(Left, Analysis, Acc, {CallgraphModifiedTime, Callgraph});
		 ok ->
		     analyze_all_files(Left, Analysis, Acc, {CallgraphModifiedTime, Callgraph});
		 {called_modules, Called} ->
		     ImportedMods = imported_modules(FileName),
		     Called1 = lists:usort(Called++ImportedMods),
		     analyze_all_files(Left, Analysis, [{{Mod,Dir}, Called1}|lists:keydelete({Mod, Dir}, 1, Acc)],
							{CallgraphModifiedTime, Callgraph})
	  end
  end; 	   

  
analyze_all_files([], Analysis, Acc, _Callgraph) ->
  Callgraph = Analysis#analysis.module_graph,    
  case Acc of
    [] ->
      ok;
    List when is_list(List) ->
      case file:open(Callgraph,[write,binary]) of 
	  {ok, File} -> file:write_file(Callgraph, term_to_binary(List)),
			file:close(File);
	  {error, Reason} ->  io:format("Could not open module callgraph output file, Reason ~p\n",
					[Reason])
      end,
      {module_callgraph, List, reverse_module_callgraph(List)}
  end.

reverse_module_callgraph(List) ->
    reverse_module_callgraph_1(List,List, []).
reverse_module_callgraph_1([], _List,Acc) ->
    Acc;
reverse_module_callgraph_1([{{Mod, Dir},_Called_Mods}|T], List, Acc) ->
    Client_Modules = get_client_modules({Mod,Dir}, List),
    reverse_module_callgraph_1(T,List, [Client_Modules|Acc]).

get_client_modules({Mod,Dir}, List) ->
    F = fun({{M,Dir1},Called_Modules}) ->
		case lists:member(list_to_atom(Mod), Called_Modules) of 
		    true -> [filename:join([Dir1, M++".erl"])];
		    false -> []
		end
	end,
    {filename:join([Dir, Mod++".erl"]), lists:flatmap(F, List)}.

imported_modules(File) ->
      case  epp_dodger:parse_file(File) of 
	  {ok, Forms} ->
	      Info = refac_syntax_lib:analyze_forms(Forms),
	      case lists:keysearch(imports,1, Info) of 
		  {value, {imports, Imps}} -> lists:map(fun({M, _Funs}) -> M end, Imps);
		  _  -> []
	      end;
	  _ -> []           
      end.					  


    
