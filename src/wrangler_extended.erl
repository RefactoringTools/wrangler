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
%%@hidden
%%@private
-module(wrangler_extended).

-export([rename_fun/4, 
         rename_var/5, 
         swap_args/5,
         tuple_args/5,
         fold_expr/5,
         gen_fun/5,
         gen_fun/6,
         move_fun/4,
         unfold_fun_app/3]).

-export([gen_question/2]).

-include("../include/wrangler.hrl").
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                              %%
%% Generalised interface for moving a function btw modules.     %%
%%                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type (file_filter()::{file,fun((File::filename()) -> boolean())}).

-type (module_filter():: {module, fun((Mod::atom()) -> boolean())}).

-type (mod_or_file()::file_filter() | module_filter() | atom() |filename()).

-type (fa()::fun(({FunName::atom(), Arity::integer()})->boolean())
           |{atom(), integer()}).

-type (search_paths()::[filename()|dir()]).

-type (elementary_refac()::{refactoring, {atom(), atom()}, [term()]}).

-type (lazy_refac()::{elementary_refac(), {generator, function()}}).

-spec move_fun(mod_or_file(),fa(),
               file_filter() |module_filter()|atom()|filename()
               |{user_input, Prompt::fun(({M::atom(),FA::{atom(),integer()}})->
                                                string())},
               search_paths()) ->[elementary_refac()].
move_fun(SrcModOrFile, FA, TgtModOrFile, SearchPaths) ->
    CmdLists=[{refactoring, {refac_move_fun, move_fun_by_name},
               [File, {F, A}, TargetFile, SearchPaths]}
              ||File<-gen_file_names(SrcModOrFile, SearchPaths),
                {F,A}<-get_fun_arity(File, FA),
                TargetFile<-gen_target_file_name(
                              {File, FA}, TgtModOrFile,SearchPaths)],
    lists:append(CmdLists).

gen_target_file_name(PreArgs, TgtModOrFile, SearchPaths) ->
    case TgtModOrFile of
        {user_input, GenPrompt} ->
            {prompt, GenPrompt(PreArgs)};
        _ -> 
            case gen_file_names(TgtModOrFile, SearchPaths) of 
                [File] ->
                    [File];
                [] ->
                    throw({error, "Invlaid specification of target file."})
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                              %%
%% Generalised interface for renaming a function name.          %%
%%                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec rename_fun(ModOrFile::mod_or_file(),
                 Fa:: fa(),
                 NewFunName::{generator, fun(({M::atom(),FA::{atom(),integer()}})->atom())}
                           | {user_input, Prompt::fun(({M::atom(),FA::{atom(),integer()}})->
                                                             string())}
                           |atom(),
                 SearchPaths::search_paths()) ->[elementary_refac()].
rename_fun(ModOrFile,FA, NewFunName, SearchPaths)->
    Files= gen_file_names(ModOrFile, SearchPaths),
    CmdLists=[rename_fun_1(File, FA, NewFunName, SearchPaths)
              ||File<-Files],
    lists:append(CmdLists).

rename_fun_1(File, FA, NewFunName, SearchPaths) ->
    FAs= get_fun_arity(File, FA),
    [{refactoring, {refac_rename_fun, rename_fun_by_name}, 
      [File, {F, A}, new_name_gen(File, {F, A}, NewFunName),
       SearchPaths]}
     ||{F, A}<-FAs].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                              %%
%% Generalised interface for renaming a variable name.          %%
%%                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec rename_var(ModOrFile::mod_or_file(), FA::fa(),
                 OldVarName::fun((VarName::atom())-> boolean())
                           |atom(),
                 NewVarName::{generator, fun(({M::atom(),FA::{atom(),integer()},V::atom()})->atom())}
                           |{user_input, Prompt::fun(({M::atom(), FA::{atom(),integer()}, V::atom()})->
                                                            string())}
                           |atom(),
                 SearchPaths::search_paths()) ->
                        [elementary_refac()] | lazy_refac().
                         
rename_var(ModOrFile, FA, OldVarName, NewVarName, SearchPaths) ->
    rename_var(ModOrFile, FA, OldVarName, NewVarName, SearchPaths, -1).

rename_var(_ModOrFile, _FA, _OldVarName, _NewVarName, _SearchPaths, N) 
  when N==0-> [];
rename_var(ModOrFile, FA, OldVarName, NewVarName, SearchPaths,N) ->
    Files= gen_file_names(ModOrFile, SearchPaths),
    CmdLists=[rename_var_1(File, FA, OldVarName, NewVarName, SearchPaths)
              ||File<-Files],
    Refacs=lists:append(CmdLists),
    case Refacs of 
        [R|Rs] when N==-1 ->
            {R,{generator, fun()->
                                   rename_var(ModOrFile, FA, OldVarName,
                                              NewVarName, SearchPaths, length(Rs))
                           end}};
        [_R|_Rs] ->
            Nth = length(Refacs)-N+1,
            {lists:nth(Nth, Refacs), 
             {generator, fun()->
                                 rename_var(ModOrFile, FA, 
                                            OldVarName, NewVarName, SearchPaths, N-1)
                         end}};
        _ -> Refacs
    end.

rename_var_1(File, FA, VarFilter, NewVarName, SearchPaths) ->
    FAs= get_fun_arity(File, FA),
    [{refactoring, {refac_rename_var, rename_var}, 
      [File, {F, A}, V, new_name_gen(File, {F, A}, V, NewVarName), SearchPaths]}
     ||{F, A}<-FAs, V<-get_vars(File, F, A, VarFilter)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                              %%
%% Generalised interface for function generalisation.           %%
%%                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec gen_fun(ModOrFile::mod_or_file(),FA::fa(),ExprStr::string(),
              NewParName::{user_input, Prompt::fun(({M::atom(), FA::{atom(),integer()}, 
                                                     ExprStr::string()})->
                                                          string())}
                        |atom()
                        |string(),
              SearchPaths::search_paths()) ->[elementary_refac()].

gen_fun(ModOrFile, FAFilter, ExprStr, NewParName, SearchPaths) ->
    gen_fun(ModOrFile, FAFilter, ExprStr, NewParName, SearchPaths, textual).
gen_fun(ModOrFile, FAFilter, ExprStr, NewParName, SearchPaths, GenOrder) ->
    Files= gen_file_names(ModOrFile, SearchPaths),
    FFAs = [{File,FA}||File<-Files, FA<-get_fun_arity(File, FAFilter, GenOrder)],
    gen_fun_1(FFAs,  ExprStr, NewParName, SearchPaths).
    
                                
gen_fun_1([], _ExprStr, _NewParName, _SearchPaths) ->
    [];
gen_fun_1([{File,FA}|FFAs], ExprStr, NewParName, SearchPaths)->
    case get_exprs(File, FA, ExprStr) of 
        [] ->
            gen_fun_1(FFAs, ExprStr, NewParName, SearchPaths);
        Ranges ->
            Exprs={range, {File, Ranges}},
            {{refactoring, {refac_gen, generalise_composite},
              [File, FA, Exprs, hd(Ranges), 
               new_name_gen(File, FA, Exprs, NewParName),
               SearchPaths]},
             {generator, fun()->
                                 gen_fun_1(FFAs, ExprStr, NewParName, SearchPaths)
                         end}}
    end.

get_exprs(File, {FunName, Arity}, ExprStr) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    FunDef=api_refac:mfa_to_fun_def({ModName, FunName, Arity}, File),
    case FunDef of
        none -> none;
        _ ->
            ?FULL_TD_TU(
               [?COLLECT(?T("E@"),  
                         api_refac:start_end_loc(E@),
                         api_refac:is_expr(E@) andalso
                         ?SPLICE(E@)==ExprStr)], FunDef)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                              %%
%% Generalised interface for folding against expressions.       %%
%%                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fold_expr(mod_or_file(), mod_or_file(), fa(), integer(), search_paths()) ->
                       [elementary_refac()].
fold_expr(CurModOrFile, ModOrFile, FA, ClauseIndex, SearchPaths) ->
    Files= gen_file_names(CurModOrFile, SearchPaths),
    case locate_one_file(ModOrFile, SearchPaths) of
        {error, Reason} ->
            wrangler_io:format("Warning: ~p\n", [Reason]),
            [];
        TgtFile ->
            ModName=list_to_atom(filename:basename(TgtFile, ".erl")),
            case get_fun_arity(TgtFile, FA) of
                [{F,A}] ->
                    CmdLists=[fold_expr_1(File,  ModName, F, A, ClauseIndex, SearchPaths)
                              ||File<-Files],
                    lists:append(CmdLists);
                _ ->
                    []
            end
    end.
        
fold_expr_1(File, ModName, FunName, Arity, ClauseIndex, SearchPaths) ->
    [{refactoring, {refac_fold_expression, fold_expr_by_name}, 
                    [File, atom_to_list(ModName), atom_to_list(FunName), 
                     integer_to_list(Arity), integer_to_list(ClauseIndex), 
                     SearchPaths]}].
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                              %%
%% Generalised interface for unfolding a function application.  %%
%%                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec unfold_fun_app(mod_or_file(), pos(), search_paths())->
                            [elementary_refac()].
unfold_fun_app(ModOrFile, Pos, SearchPaths) ->
    Files= gen_file_names(ModOrFile, SearchPaths),
    Refacs=[unfold_fun_app_1(File, Pos, SearchPaths)
            ||File<-Files],
    lists:append(Refacs).

unfold_fun_app_1(File, PosGen, SearchPaths) ->
    Pos = pos_gen(PosGen),
    [{refactoring, {refac_unfold_fun_app, unfold_fun_app},
      [File, Pos, SearchPaths]}].
                   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                              %%
%% Generalised interface for swapping function arguments.       %%
%%                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec swap_args(ModOrFile::mod_or_file(),FA::fa(),
                Index1:: integer()| {user_input, Prompt::fun(({M::atom(), FA::{atom(),integer()}})->
                                                               string())},
                Index2:: integer()|{user_input,Prompt::fun(({M::atom(), FA::{atom(),integer()}})->
                                                                  string())},
                SearchPaths::search_paths())->[elementary_refac()].
swap_args(ModOrFile, FA, Index1, Index2, SearchPaths) ->
    Files= gen_file_names(ModOrFile, SearchPaths),
    CmdLists=[swap_args_1(File, FA, index_gen(Index1, {File, FA}), 
                          index_gen(Index2, {File, FA}), SearchPaths)
              ||File<-Files],
    lists:append(CmdLists).

swap_args_1(File, FA, Index1, Index2, SearchPaths) ->
    FAs= get_fun_arity(File, FA),
    [{refactoring, {refac_swap_args, swap_args}, 
      [File, {F, A}, Index1, Index2,SearchPaths]} 
     ||{F, A}<-FAs].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                              %%
%% Generalised interface for tupling function arguments.        %%
%%                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec tuple_args(ModOrFile::mod_or_file(),FA::fa(),
                 Index1:: integer()| {user_input, Prompt::fun(({M::atom(), FA::{atom(),integer()}})->
                                                                     string())},
                 Index2:: integer()|{user_input,Prompt::fun(({M::atom(), FA::{atom(),integer()}})->
                                                                   string())},
                 SearchPaths::search_paths())->[elementary_refac()].
tuple_args(ModOrFile, FA, Index1,Index2,SearchPaths)->
    Files= gen_file_names(ModOrFile, SearchPaths),
    CmdLists=[tuple_args_1(File, FA, Index1, Index2, SearchPaths)
              ||File<-Files],
    lists:append(CmdLists).

tuple_args_1(File, FA, Index1, Index2, SearchPaths) ->
    FAs= get_fun_arity(File, FA),
    [{refactoring, {refac_tuple, tuple_args}, 
      [File, {F, A},  index_gen(Index1, {File, FA}), 
       index_gen(Index2, {File, FA}), SearchPaths]}
     ||{F, A}<-FAs].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                              %%
%%                Utility functions.                            %%
%%                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_file_names(ModOrFile, SearchPaths) ->
    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),    
    case ModOrFile of 
        {file, FileFilter} ->
            [F||F<-Files, FileFilter(F)];
        {module, ModFilter} ->
            Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
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


locate_one_file(ModOrFile, SearchPaths) ->
    Res= gen_file_names(ModOrFile, SearchPaths),
    case Res of 
        [] ->
            {error, lists:flatten(
                      io_lib:format("File/Module does not exist in the "
                                    "searchpaths specified: ~p.\n", 
                                    [ModOrFile]))};
        [File] ->
            File;
        [File|_] ->
            {error, lists:flatten(
                      io_lib:format("File/module specified is not unique "
                                    "in the searchpaths specified.:~p\n", 
                                    [File]))}
    end.


get_funs(File, Order) ->
    case Order == td orelse  Order == bu of
        true ->
            SortedFuns=wrangler_callgraph_server:get_sorted_funs(File),
            {MFAs, _} = lists:unzip(SortedFuns),
            case Order of
                td ->
                    lists:reverse([{F,A}||{_M,F,A} <- MFAs]);
                bu ->
                    [{F,A}||{_M,F,A} <- MFAs]
            end;
        false ->
            {ok, ModuleInfo} = api_refac:get_module_info(File),
            case lists:keyfind(functions, 1, ModuleInfo) of
                {functions, Fs} ->
                    Fs;
                false ->
                    []
            end
    end.

 
get_fun_arity(File, FA) ->
    get_fun_arity(File, FA, default).

get_fun_arity(File, FA, Order) ->
    Funs = get_funs(File, Order),
    if is_function(FA) ->
            [{F,A}||{F,A}<-Funs, FA({F,A})];
       is_tuple(FA) ->
            {F, A} = FA,
            F1= case is_list(F) of 
                    true -> list_to_atom(F);
                    _ -> F
                end,
            A1 =case is_list(A) of 
                    true -> list_to_integer(A);
                    _ -> A
                end,
            [{F1,A1}];       
       true ->
            throw({error, "Invalid function-arity."})
    end.

get_vars(File, FunName, Arity, VarFilter) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    FunDef=api_refac:mfa_to_fun_def({ModName, FunName, Arity}, File),
    case FunDef of 
        none -> 
            [];
        _ ->   
            ?FULL_TD_TU(
               [?COLLECT(?T("V@"), 
                         {range, {File, [api_refac:start_end_loc(V@)]},?SPLICE(V@)},
                         api_refac:type(V@)==variable andalso
                         api_refac:bound_vars(V@)/=[] andalso 
                         case VarFilter of 
                             atom ->
                                 atom_to_list(VarFilter)==?SPLICE(V@);
                             _ when is_function(VarFilter) ->
                                 VarFilter(list_to_atom(?SPLICE(V@)));
                             _ -> false
                         end)], FunDef)
    end.
 
gen_question(rename_fun_by_name,[File,{F,A}, _NewName, _SearchPaths]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to rename function ~p:~p/~p?", [M,F,A]));
gen_question(rename_var,[File,{F,A},{range, {_File, _Loc}, V}, _, _]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to rename variable ~s in function ~p:~p/~p?", [V,M,F,A]));
gen_question(swap_args, [File,{F,A}, _, _, _]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to swap the parameters of function ~p:~p/~p?", 
                                [M,F,A]));
gen_question(gen_fun, [File, {F, A}, _Expr, _, _]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to generalise function ~p:~p/~p over the expression(s) highlighted?",
                                [M,F,A]));
gen_question(move_fun_by_name, [SrcFile, {F, A}, TgtFile, _SearchPaths]) ->
    M=list_to_atom(filename:basename(SrcFile, ".erl")),
    lists:flatten(io_lib:format("Do you want to move function ~p:~p/~p to file ~p?",
                                [M,F,A, TgtFile]));
gen_question(unfold_fun_app, [File, Loc, _]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to unfold the function application at "
                                "location ~p in module ~p?",
                                [Loc, M]));
gen_question(fold_expr_by_name, [File, M, {F, A}, _, _]) ->
    CurMod=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to fold expressions in module ~p,against function ~p:~p/~p?",
                                [CurMod, M, F, A]));
gen_question(tuple_args, [File, {F,A}, Index1, Index2, _SearchPaths]) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    lists:flatten(io_lib:format("Do you want to turn the ~p arguments, starting from index ~p, "
                                "of function ~p:~p/~p into a tuple?",
                                [Index2-Index1+1, Index1, M, F, A])).


index_gen(Index, PreArgs) ->
    case Index of
        {generator, GenFun} ->
            GenFun(PreArgs);
        {user_input, GenPrompt} ->
            {prompt, GenPrompt(PreArgs)};
        _ when is_integer(Index) ->
            Index;
        _ ->
            throw({error, "Invalid Index."})
    end.


pos_gen(PosGen) ->
    case PosGen of 
        {Line, Col} when is_integer(Line) andalso
                         is_integer(Col)->
            {Line, Col};
        _ when is_function(PosGen)->
            PosGen()
    end.


new_name_gen(File, FA, NewName) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    case NewName of
        {generator, GenFun} ->
            GenFun({ModName, FA});
        {user_input, GenPrompt} ->
            {prompt, GenPrompt({ModName, FA})};
        _ when is_atom(NewName) ->
            NewName;
        _ ->
            throw({error, "Invalid new funname."})
    end.
        
new_name_gen(File, FA, {range, {_File, _Loc}, V}, NewName) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    case NewName of
        {generator, GenFun} ->
            GenFun({ModName, FA, V});
        {user_input, GenPrompt} ->
            {prompt, GenPrompt({ModName, FA,V})};
        _ when is_atom(NewName) ->
            NewName;
        _ ->
            throw({error, "Invalid new variable name."})
    end;
new_name_gen(File, FA, OldName, NewName) ->
    ModName=list_to_atom(filename:basename(File, ".erl")),
    case NewName of
        {generator, GenFun} ->
            GenFun({ModName, FA,  OldName});
        {user_input, GenPrompt} ->
            {prompt, GenPrompt({ModName, FA, OldName})};
        _ when is_atom(NewName) ->
            NewName;
        _ ->
            throw({error, "Invalid new variable name."})
    end.
