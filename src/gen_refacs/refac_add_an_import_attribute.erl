%% Copyright (c) 2013, Huiqing Li, Simon Thompson
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

%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%@doc This module shows how to write refactorings
%% use the Wrangler API.

%% The refactoring implemented in this module adds an
%% import attribute which explicitly imports the
%% functions which are defined in a user specified
%% module, and used in the current module by remote
%% function calls.
%% @hidden
%% @private
-module(refac_add_an_import_attribute).

-behaviour(gen_refac).

%% export of callback function.
-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("wrangler.hrl").

%% Ask the user which module to import.
-spec input_par_prompts() -> [string()].
input_par_prompts() ->
    ["Module name:"].

%% no focus selection is needed.
-spec select_focus(#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args) ->
    {ok, none}.

%% Pre-condition checking.
-spec check_pre_cond(#args{}) -> ok | {error, term()}.
check_pre_cond(Args=#args{user_inputs=[ModuleName]}) ->
    case collect_uses(Args) of
        [] ->
            Msg =io_lib:format(
                   "There is no remote calls to "
                   "functions from module '~s'.",
                   [ModuleName]),
            {error, lists:flatten(Msg)};
        _-> ok
    end.

selective() ->
    false.

%%Do the actual program transformation here.
-spec transform(#args{}) -> {ok, [{{filename(), filename()}, syntaxTree()}]}.
transform(Args=#args{current_file_name=File,
                     user_inputs=[ModuleName]}) ->
    %% collect the functions that are defined
    %% in ModuleNaem, and are remotely called
    %% in the current module.
    FAs=lists:usort(collect_uses(Args)),
    FAs1 = api_refac:imported_funs(File, ModuleName),
    %% Functions that need to be imported.
    FunsToImport=FAs--FAs1,
    {ok,AST} = api_refac:get_ast(File),
    case FunsToImport of
        [] ->
            {ok, [{_, NewAST}]}=?FULL_TD_TP([rule(Args)], [{File, AST}]),
            {ok, [{{File, File}, NewAST}]};
        _ ->
            Import=make_import_attr(ModuleName, FunsToImport),
            AST1=api_refac:insert_an_attr(AST,Import),
            {ok, [{_,NewAST}]}=?FULL_TD_TP([rule(Args)], [{File, AST1}]),
            {ok, [{{File, File}, NewAST}]}
    end.

collect_uses(_Args=#args{current_file_name=File,
                         user_inputs=[ModuleName]}) ->
    ?FULL_TD_TU([?COLLECT(?T("M@:F@(Args@@)"),
                          {list_to_atom(?PP(F@)), length(Args@@)},
                          ?PP(M@)==ModuleName)],
                [File]).

rule(_Args=#args{user_inputs=[ModuleName]}) ->
    ?RULE(?T("M@:F@(Args@@)"),?TO_AST("F@(Args@@)"),
          ?PP(M@)==ModuleName).

make_import_attr(ModuleName, FAs) ->
    ?TO_AST("-import("++ModuleName++","++
               format_fa_list(FAs)++").").

%% format_fa_list([]) ->
%%     "[]";
format_fa_list(FAs) ->
    "["++lists:flatten(format_fas(FAs))++"]".

format_fas([]) ->
    "";
format_fas([{F,A}]) ->
    io_lib:format("~p/~p", [F,A]);
format_fas([{F,A}|T]) ->
    io_lib:format("~p/~p,", [F,A]) ++
        format_fas(T).
