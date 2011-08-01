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

-module(wrangler_extended).

-export([rename_fun/3, 
         rename_var/4, 
         swap_args/4,
         fold_expr/4,
         gen_fun/5]).

-include("../include/wrangler.hrl").

            

 %% wrangler_extended:gen_fun({test,fun(_Fun) -> true end,
 %%                           fun(_Arity) -> true end},
 %%                          fun(Expr) ->
 %%                                  refac_api:splice(Expr)=="8"
 %%                          end, 
 %%                          {user_input, fun(_) ->"New parameter name: " end},
 %%                          ["c:/cygwin/home/hl/test/test.erl"], callgraph_bottom_up).

gen_fun({ModOrFile, FunName, Arity}, Expr, NewParName, SearchPaths, GenOrder) ->
    refac_io:format("ModOrFile:\n~p\n", [ModOrFile]),
    Files= get_files(ModOrFile, SearchPaths),
    refac_io:format("Files:\n~p\n", [Files]),
    CmdLists=[gen_fun_1(File, FunName, Arity, Expr, NewParName, SearchPaths, GenOrder)
              ||File<-Files],
    lists:append(CmdLists).

gen_fun_1(ModOrFile, FunName, Arity, Expr, NewParName, SearchPaths, GenOrder) ->
    Files= get_files(ModOrFile, SearchPaths),
    Refacs=lists:append([gen_fun_2(File, FunName, Arity,Expr, NewParName, SearchPaths, GenOrder)
                         ||File<-Files]),
    case Refacs of 
        [] -> [];
        [R|_Rs] ->
            {R,{generator, fun()->
                                   gen_fun_1(ModOrFile, FunName, Arity, Expr, NewParName, SearchPaths, GenOrder)
                           end}}     
        end.
                                
gen_fun_2(File, FunName, Arity,ExprFilter, NewParName, SearchPaths, GenOrder)->
    FAs= get_fa(File, FunName, Arity, GenOrder),
    [{refactoring, {refac_gen, generalise, 
                    [File, F, A, Exprs, new_name_gen(File, F, A, Exprs, NewParName),
                     SearchPaths, composite_emacs]}, gen_question(gen_fun,{File,F,A,Exprs})}
     ||{F, A}<-FAs, Exprs<-[get_exprs(File, F, A, ExprFilter)]].


get_exprs(File, FunName, Arity, ExprFilter) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    FunDef=refac_api:mfa_to_fun_def({ModName, FunName, Arity}, File),
    Ranges=?FULL_TD_TU([?COLLECT(?T("E@"),
                                 refac_api:start_end_loc(E@),
                                 refac_api:is_expr(E@) andalso
                                 ExprFilter(E@))], FunDef),
    {range, {File, lists:sort(Ranges)}}.
    


fold_expr(CurModOrFile, {ModOrFile, FunName, Arity}, ClauseIndex, SearchPaths) ->
    Files= get_files(CurModOrFile, SearchPaths),
    refac_io:format("Files:\n~p\n", [Files]),
    refac_io:format("current files:~p\n", [get_files(ModOrFile, SearchPaths)]),
    [TargetFile] = get_files(ModOrFile, SearchPaths),
    ModName=list_to_atom(filename:basename(TargetFile, ".erl")),
    [{F,A}] = get_fa(TargetFile, FunName, Arity),
    CmdLists=[fold_expr_1(File,  ModName, F, A, ClauseIndex, SearchPaths)
              ||File<-Files],
    lists:append(CmdLists).


fold_expr_1(File, ModName, FunName, Arity, ClauseIndex, SearchPaths) ->
    [{refactoring, {refac_fold_expression, fold_expr_by_name, 
                    [File, ModName, FunName, Arity, ClauseIndex, SearchPaths, composite_emacs]},
      lists:flatten(io_lib:format("Do you want to fold against function ~p:~p/~p?", 
                                  [ModName,FunName,Arity]))}].

rename_fun({ModOrFile, OldFunName, Arity}, NewFunName, SearchPaths)->
    Files= get_files(ModOrFile, SearchPaths),
    CmdLists=[rename_fun_1(File, OldFunName, Arity, NewFunName, SearchPaths)
              ||File<-Files],
    lists:append(CmdLists).
    
      
rename_fun_1(File, OldFunName, Arity, NewFunName, SearchPaths) ->
    FAs= get_fa(File, OldFunName, Arity),
    [{refactoring, {refac_rename_fun, rename_fun_by_name, 
                    [File, F, A, new_name_gen(File, F, A, NewFunName), 
                     SearchPaths, composite_emacs]}, gen_question(rename_fun,{File, F,A})}
     ||{F, A}<-FAs].

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
        
new_name_gen(File, F, A, {range, {_File, _Loc}, V}, NewVarName) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    case NewVarName of
        {generator, GenFun} ->
            GenFun({ModName, F, A, V});
        {user_input, GenPrompt} ->
            {prompt, GenPrompt({ModName, F, A,V})};
        _ when is_atom(NewVarName) ->
            NewVarName;
        _ ->
            throw({error, "Invalid new variable name."})
    end;
new_name_gen(File, F, A, OldVarName, NewVarName) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    case NewVarName of
        {generator, GenFun} ->
            GenFun({ModName, F, A, OldVarName});
        {user_input, GenPrompt} ->
            {prompt, GenPrompt({ModName, F, A,OldVarName})};
        _ when is_atom(NewVarName) ->
            NewVarName;
        _ ->
            throw({error, "Invalid new variable name."})
    end.

%% -spec rename_var(ModOrFile::{file,  fun((File::filename()) -> boolean())}|
%%                             {module, fun((Mod::atom()) -> boolean())} |
%%                             atom()|filename(),
%%                  FunName::fun((FunName::atom())-> boolean())|atom(), 
%%                  Arity::fun((Arity::integer())->boolean())|integer(),
%%                  OldVarName::fun((VarName::atom())-> boolean())|atom(),
%%                  NewVarName::{generator, fun(({M::atom(),F::atom(),A::integer(),V::atom()})->atom())}|
%%                              {user_input, Prompt::fun(({M::atom(), F::atom(),A::integer(), V::atom()})->
%%                                                              string())}|atom(),
%%                  SearchPaths::[filename()|dir()]) ->
%%                         [{refactoring, {refac_rename_var, rename_var, [any()]}, string()}]. 

                           

rename_var({ModOrFile, FunName, Arity}, OldVarName, NewVarName, SearchPaths) ->
    rename_var({ModOrFile, FunName, Arity}, OldVarName, NewVarName, SearchPaths,0).

rename_var({ModOrFile, FunName, Arity}, OldVarName, NewVarName, SearchPaths,N) ->
    Files= get_files(ModOrFile, SearchPaths),
    CmdLists=[rename_var_1(File, FunName, Arity, OldVarName, NewVarName, SearchPaths)
              ||File<-Files],
    Refacs=lists:append(CmdLists),
    case Refacs of 
        [] -> [];
        [R] -> R;
        [R|Rs] ->
            case N of 
                0 ->
                    {R,{generator, fun()->
                                           rename_var({ModOrFile, FunName, Arity}, OldVarName, NewVarName, SearchPaths, length(Rs))
                                   end}};
                _ ->
                    Nth = length(Refacs)-N+1,
                    {lists:nth(Nth, Refacs), 
                     {generator, fun()->
                                         rename_var({ModOrFile, FunName, Arity}, OldVarName, NewVarName, SearchPaths, N-1)
                                 end}}
            end
    end.
                                   


rename_var_1(File, FunName, Arity, VarFilter, NewVarName, SearchPaths) ->
    FAs= get_fa(File, FunName, Arity),
    refac_io:format("FAs:\n~p\n", [FAs]),
    [{refactoring, {refac_rename_var, rename_var, 
                    [File, F, A, V, new_name_gen(File, F, A, V, NewVarName),
                     SearchPaths, composite_emacs]}, gen_question(rename_var,{File,F,A,V})}
     ||{F, A}<-FAs, V<-get_vars(File, F, A, VarFilter)].

get_files(ModOrFile, SearchPaths) ->
    Files = refac_misc:expand_files(SearchPaths, ".erl"),    
    case ModOrFile of 
        {file, FileFilter} ->
            [F||F<-Files, FileFilter(F)];
        {module, ModFilter} ->
            Files = refac_misc:expand_files(SearchPaths, ".erl"),
            [F||F<-Files, ModFilter(filename:basename(F, ".erl"))];
        _ when is_atom(ModOrFile) ->
            [F||F<-Files, filename:basename(F,".erl")==atom_to_list(ModOrFile)];
        _ when is_list(ModOrFile)->
            case filelib:is_regular(ModOrFile) of 
                true ->
                    [ModOrFile];
                false ->
                    [F||F<-Files, filename:basename(F,".erl")==ModOrFile]
            end;
        _ -> 
            throw({error, "invalid argument."})
    end.



get_fa(File, FunName, Arity) ->
    get_fa(File, FunName, Arity, default).

get_fa(File, FunName, Arity, Order) ->
    Funs = case Order == callgraph_topdown orelse 
              Order == callgraph_bottom_up of 
              true ->
                  SortedFuns=wrangler_callgraph_server:get_sorted_funs(File),
                   {MFAs, _} = lists:unzip(SortedFuns),
                   refac_io:format("MFAs:\n~p\n", [MFAs]),
                  case Order of 
                      callgraph_topdown ->
                          lists:reverse([{F,A}||{_M,F,A}<-MFAs]);
                      callgraph_bottom_up ->
                          [{F,A}||{_M,F,A}<-MFAs]
                  end;
              false ->
                  {ok, ModuleInfo} = refac_api:get_module_info(File),
                  case lists:keyfind(functions, 1, ModuleInfo) of
                      {functions, Fs} ->
                          Fs;
                      false -> 
                          []
                  end
          end,
    if is_function(FunName) ->
            FAs=[{F,A}||{F,A}<-Funs, FunName(F)],
            filter_with_arity(FAs, Arity);
       is_atom(FunName) ->
            FAs=[{F,A}||{F,A} <- Funs, F == FunName],
            filter_with_arity(FAs, Arity);
       is_list(FunName) ->
            FAs=[{F,A}||{F,A} <- Funs, F == list_to_atom(FunName)],
            filter_with_arity(FAs, Arity);
       true ->
            throw({error, "Invalid function name."})
    end.

filter_with_arity(FAs, ArityFilter) when is_function(ArityFilter)->
    [{F,A}||{F,A}<-FAs, ArityFilter(A)];
filter_with_arity(FAs, Arity) when is_integer(Arity) ->   
    [{F,A}||{F,A}<-FAs, A==Arity];
filter_with_arity(FAs, Arity) when is_list(Arity) ->
    [{F,A}||{F,A}<-FAs, A==list_to_integer(Arity)];
filter_with_arity(_FAs, _Arity)->
    throw({error, "Invalid arity."}).


get_vars(File, FunName, Arity, VarFilter) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    FunDef=refac_api:mfa_to_fun_def({ModName, FunName, Arity}, File),
    Vars=?FULL_TD_TU([?COLLECT(?T("V@"), 
                               {range, {File, [refac_api:start_end_loc(V@)]},?SPLICE(V@)},
                               refac_api:type(V@)==variable andalso
                               refac_api:bound_vars(V@)/=[] andalso 
                               case VarFilter of 
                                   atom ->
                                       atom_to_list(VarFilter)==?SPLICE(V@);
                                   _ when is_function(VarFilter) ->
                                       VarFilter(list_to_atom(?SPLICE(V@)));
                                   _ -> false
                               end)], FunDef),
    Vars.

gen_question(rename_fun,{File,F,A}) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to rename function ~p:~p/~p?", [M,F,A]));
gen_question(rename_var,{File,F,A,{range, {_File, _Loc}, V}}) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to rename variable ~s in function ~p:~p/~p?", [V,M,F,A]));
gen_question(swap_args, {File,F,A}) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to swap the parameters of function ~p:~p/~p?", 
                                [M,F,A]));
gen_question(gen_fun, {File, F, A, _Expr}) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to generalise function ~p:~p/~p over the expression(s) highlighted?",
                                [M,F,A])).

swap_args({ModOrFile, FunName, Arity}, Index1, Index2, SearchPaths) ->
    Files= get_files(ModOrFile, SearchPaths),
    CmdLists=[swap_args_1(File, FunName, Arity, Index1, Index2, SearchPaths)
              ||File<-Files],
    lists:append(CmdLists).
    
swap_args_1(File, FunName, Arity, Index1, Index2, SearchPaths) ->
    FAs= get_fa(File, FunName, Arity),
    [{refactoring, {refac_swap_args, swap_args, [{File, F, A}, index_gen(Index1), index_gen(Index2),   %% paremeters to index_gen?
                                                 SearchPaths, composite_emacs]}, gen_question(swap_args,{File, F,A})}
     ||{F, A}<-FAs].


%% This will be changed!
index_gen(Index) ->
    case Index of
        {generator, GenFun} ->
            GenFun();
        {user_input, GenPrompt} ->
            {prompt, GenPrompt()};
        _ when is_atom(Index) ->
            Index;
        _ ->
            throw({error, "Invalid new funname."})
    end.
        
            
%% refac_io:format("\n~p\n", [rename_fun_extended:rename_fun({filter, file, fun(File) -> true end}, {filter, fun(F)-> true end}, {filter, fun(A)-> true end}, {user_input, fun({M,F,A})->lists:flatten(io_lib:format("New function name for ~p:~p/~p", [M,F,A])) end}, ["c:/cygwin/home/hl/test"])]).



%% -spec rename_fun(ModOrFile::{filter, file,  fun((File::filename()) -> boolean())}|
%%                             {filter, module, fun((Mod::atom()) -> boolean())} |
%%                             atom()|filename(),
%%                  OldFunName::{filter, fun((FunName::atom())-> boolean())}|atom(), 
%%                  Arity::{filter, fun((Arity::integer())->boolean())}|integer(),
%%                  NewFunName::{generator, fun(({M::atom(),F::atom(),A::integer()})->atom())}|
%%                              {user_input, Prompt::fun(({M::atom(), F::atom(),A::integer()})->
%%                                                              string())}|atom(),
%%                  SearchPaths::[filename()|dir()]) ->
%%                         [{wranlger_api, rename_fun, [any()]}]. 
