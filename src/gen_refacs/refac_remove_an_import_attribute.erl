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

%%@doc This module shows how to write refactorings 
%% use the Wrangler API. 

%% The refactoring implemented in this module removes
%% the import attributes importing a user specified
%% module, and qualify the calls to those functions 
%% imported from that module.

%% @hidden
%% @private
-module(refac_remove_an_import_attribute).

-behaviour(gen_refac).

%% export of callback function.
-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../../include/wrangler.hrl").

%% The user needs to input the module name. 
-spec (input_par_prompts/0::() -> [string()]).                           
input_par_prompts() -> ["Module name:"].

%% No focus selection is needed.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) ->{ok, none}.

%% Pre-condition checking.
-spec (check_pre_cond/1::(#args{}) -> ok|{error, term()}).  
check_pre_cond(_Args=#args{current_file_name=File,
                           user_inputs=[ModuleName]}) ->
    case is_imported(File, ModuleName) of 
        true -> 
            ok;
        false ->
            {error, "The module specified is not imported"}
    end.

selective()->
    false.

%%Do the actual program transformation here.
-spec (transform/1::(#args{}) -> {ok, [{{filename(), filename()}, syntaxTree()}]}|{error, term()}).
transform(Args=#args{current_file_name=File})->
    ?FULL_TD_TP([rule1(Args),
                 rule2(Args)], [File]).

%% qualify function calls.
rule1(_Args=#args{user_inputs=[ModuleName]}) ->
    ?RULE(?T("F@(Args@@)"),
          ?TO_AST(ModuleName++":F@(Args@@)"),
          api_refac:type(F@) /= module_qualifier andalso
          list_to_atom(ModuleName) == element(1,api_refac:fun_define_info(F@))).


%% remove import attributes related.
rule2(_Args=#args{user_inputs=[ModuleName]}) ->
    ?RULE(?T("A@"), ?TO_AST(""),
          api_refac:is_import(A@, list_to_atom(ModuleName))).

%%utility functions.
is_imported(File, ModuleName) ->
    api_refac:imported_funs(File, ModuleName) /= [].

