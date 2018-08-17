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
%%
%%@doc This module shows how to write refactorings
%% use the Wrangler API.

%% The refactoring implemented in this module adds an
%% import attribute which explicitly imports the
%% functions which are defined in a user specified
%% module, and used in the current module by remote
%% function calls.
%% @hidden
%% @private
-module(refac_add_to_export).

-behaviour(gen_refac).

%% export of callback function.
-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-export([add_to_export/5]).

-include("wrangler.hrl").

%% Ask the user which module to import.
-spec input_par_prompts() -> [string()].
input_par_prompts() ->
    [].

%% no focus selection is needed.
-spec select_focus(#args{}) -> {ok, syntaxTree()} | {ok, none}.
select_focus(_Args=#args{current_file_name=File,
                         cursor_pos=Pos}) ->
    api_interface:pos_to_fun_def(File, Pos).

%% Pre-condition checking.
-spec check_pre_cond(#args{}) -> ok.
check_pre_cond(_Args) ->
    ok.

selective() ->
    false.

%%Do the actual program transformation here.
-spec transform(#args{}) -> {ok, [{{filename(), filename()}, syntaxTree()}]}.
transform(_Args=#args{current_file_name=File,
                     focus_sel=FunDef}) ->
    {_M, F, A} = api_refac:fun_define_info(FunDef),
    case api_refac:is_exported({F, A}, File) of
        true ->
            {ok, []};
        false ->
            case collect_exports(File) of
                [] ->
                    {ok,AST} = api_refac:get_ast(File),
                    Export=make_export_attr({F,A}),
                    NewAST=api_refac:insert_an_attr(AST,Export),
                    {ok, [{{File, File}, NewAST}]};
                Exports ->
                    Export = lists:last(Exports),
                    ?STOP_TD_TP([rule(Export, {F, A})], [File])
            end
    end.


rule(Export, FA) ->
    ?RULE(?T("Form@"),
          api_refac:add_to_export(Form@, FA),
          Form@==Export).


collect_exports(File) ->
    ?STOP_TD_TU([?COLLECT(?T("Form@"),
                          _This@,
                          api_refac:is_attribute(Form@, export))],
                [File]).

make_export_attr(FA) ->
    ?TO_AST("-export(["++format_fa(FA)++"]).").

format_fa({F,A}) ->
    lists:flatten(io_lib:format("~p/~p", [F,A])).


add_to_export(FileName, {FunName, Arity}, SearchPaths, Editor, TabWidth)->
    {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    ModName=list_to_atom(filename:basename(FileName, ".erl")),
    case wrangler_misc:funname_to_defpos(AnnAST, {ModName, FunName, Arity}) of
	{ok, DefPos} ->
            {ok, FunDef} = api_interface:pos_to_fun_def(FileName, DefPos),
            Args=#args{current_file_name=FileName,
                       focus_sel=FunDef,
                       search_paths=SearchPaths,
                       tabwidth=TabWidth},
            {ok, Res}=transform(Args),
            wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"");
        {error, Reason} ->
            {error, Reason}
    end.
