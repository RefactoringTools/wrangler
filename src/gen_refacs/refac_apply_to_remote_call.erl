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
%%@hidden
%%@private
-module(refac_apply_to_remote_call).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../../include/wrangler.hrl").

%% The Emacs mini-buffer prompts for the user input parameters. 
-spec (input_par_prompts/0::() -> [string()]).                           
input_par_prompts() -> [].

%% Select the focus of interest. If no selection is neeeded, 
%% then return {ok, none}.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) ->{ok, none}.

%% Pre-condition checking to ensure that the refactoring preserves the 
%% behaviour of the program.
-spec (check_pre_cond/1::(#args{}) -> ok).  
check_pre_cond(_Args) ->
    ok.

selective() ->
    true.

%%Do the actual program transformation here.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}).
transform(_Args=#args{search_paths=SearchPaths})->
    ?FULL_TD_TP([rule(),
                 rule1(),
                 rule2()
                ], SearchPaths).

rule() ->
    ?RULE(?T("Op@(N@@, M@, F@, [Args@@])"),
          ?TO_AST("M@:F@(Args@@)"),
          {erlang,apply,3} == api_refac:fun_define_info(Op@)).
          
rule1() ->
    ?RULE(?T("Op@(N@@, M@, F@, [])"),
          ?TO_AST("M@:F@()"),
          {erlang,apply,3} == api_refac:fun_define_info(Op@)). 
         
rule2() ->
    ?RULE(?T("Op@(Fun@, [Args@@])"),
          begin
              {M,F,_A} = api_refac:fun_define_info(Fun@),
              ?TO_AST(atom_to_list(M)++":"++atom_to_list(F)++"(Args@@)")
          end,
          case api_refac:fun_define_info(Fun@) of
              {_,_,_}->
                  true;
              _ -> 
                  false
          end).
          
