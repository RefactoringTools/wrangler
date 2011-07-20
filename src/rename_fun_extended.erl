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
%% The refactoring command level API that can be run in an Erlang shell.
%%
%% Author contact: H.Li@kent.ac.uk, Simon.J.Thompson@kent.ac.uk
%%
%% =====================================================================

%% @private
-module(rename_fun_extended).

-export([rename_fun/5]).

-include("../include/wrangler_internal.hrl").

-spec rename_fun(ModOrFile::{filter, file,  fun((File::filename()) -> boolean())}|
                            {filter, module, fun((Mod::atom()) -> boolean())} |
                            atom()|filename(),
                 OldFunName::{filter, fun((FunName::atom())-> boolean())}|atom(), 
                 Arity::{filter, fun((Arity::integer())->boolean())}|integer(),
                 NewFunName::{generator, fun(({M::atom(),F::atom(),A::integer()})->atom())}|
                             {user_input, Prompt::fun(({M::atom(), F::atom(),A::integer()})->
                                                             string())}|atom(),
                 SearchPaths::[filename()|dir()]) ->
                        [{wranlger_api, rename_fun, [any()]}]. 


rename_fun(ModOrFile, OldFunName, Arity, NewFunName, SearchPaths)->
    Files= get_files(ModOrFile, SearchPaths),
    CmdLists=[rename_fun_1(File, OldFunName, Arity, NewFunName, SearchPaths)
              ||File<-Files],
    lists:append(CmdLists).
    
      
get_files(ModOrFile, SearchPaths) ->
    Files = refac_misc:expand_files(SearchPaths, ".erl"),
    case ModOrFile of 
        {filter, file, FileFilter} ->
            [F||F<-Files, FileFilter(F)];
        {filter, module, ModFilter} ->
            Files = refac_misc:expand_files(SearchPaths, ".erl"),
            [F||F<-Files, ModFilter(filename:basename(F, ".erl"))];
        _ when is_atom(ModOrFile) ->
            [F||F<-Files, filename:basename(F,".erl")==atom_to_list(ModOrFile)];
        _ ->
            case filelib:is_regular(ModOrFile) of 
                true ->
                    [ModOrFile];
                false ->
                    throw({error, "Invalid argument"})
            end
    end.

rename_fun_1(File, OldFunName, Arity, NewFunName, SearchPaths) ->
    FAs= get_old_fa(File, OldFunName, Arity),
    [{refactoring, {wrangler_api, rename_fun, [File, F, A, new_name_gen(File, F, A, NewFunName), SearchPaths]}}
     ||{F, A}<-FAs].

get_old_fa(File, OldFunName, Arity) ->
    {ok, ModuleInfo} = api_refac:get_module_info(File),
    case lists:keyfind(functions, 1, ModuleInfo) of
        {functions, Funs} ->
            case OldFunName of
                {filter, OldFunNameFilter} ->
                    FAs=[{F,A}||{F,A}<-Funs, OldFunNameFilter(F)],
                    filter_with_arity(FAs, Arity);
                _ when is_atom(OldFunName) ->
                    FAs=[{F,A}||{F,A}<-Funs, F==OldFunName],
                    filter_with_arity(FAs, Arity);
                _ -> 
                    throw({error, "Invalid function name."})
            end;
        false ->
            []
    end.

filter_with_arity(FAs, Arity) ->
    case Arity of 
        {filter, ArityFilter} ->
            [{F,A}||{F,A}<-FAs,
                    ArityFilter(A)];
        _ when is_integer(Arity) ->
            [{F,A}||{F,A}<-FAs, A==Arity];
        _ ->
            throw({error, "Invalid arity."})
    end.


new_name_gen(File, F, A, NewFunName) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    case NewFunName of
        {generator, GenFun} ->
            GenFun({ModName, F, A});
        {user_input, GenPrompt} ->
            {prompt, GenPrompt({ModName, F, A})};
        _ when is_atom(NewFunName) ->
            NewFunName;
        _ ->
            throw({error, "Invalid new funname."})
    end.
        
            
%% refac_io:format("\n~p\n", [rename_fun_extended:rename_fun({filter, file, fun(File) -> true end}, {filter, fun(F)-> true end}, {filter, fun(A)-> true end}, {user_input, fun({M,F,A})->lists:flatten(io_lib:format("New function name for ~p:~p/~p", [M,F,A])) end}, ["c:/cygwin/home/hl/test"])]).
