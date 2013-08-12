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
%%@doc An example script for evolving a WS eqc_statem test module.
-module(refac_evolve_api).

-export([composite_refac/1, input_par_prompts/0, select_focus/1]).

-include_lib("../../include/wrangler.hrl").

-behaviour(gen_composite_refac).

input_par_prompts() ->[].

select_focus(_Args) -> {ok, none}.

composite_refac(_Args=#args{current_file_name=File})->
    ?atomic(
       [{refactoring, add_op, [File, 2,"next_state(S, _R, {call, ?MODULE, find_all_rooms, []}) -> S;\n",
                               2,"precondition(S, {call, ?MODULE, find_all_rooms, []})-> true;\n",
                               2,"postcondition(S, {call, ?MODULE, find_all_rooms, []}, Result)-> true;\n",
                               2,"{call, ?MODULE, find_all_rooms, []},\n", [File], 'emacs']},
        {refactoring, add_op_arg, [File,"find_devices","SortBy","3", "gen_sort_by",[File], 'emacs']},
        {refactoring, add_op_arg, [File,"find_devices","Order","4", "gen_order",[File], 'emacs']},
        {refactoring, add_op_arg, [File,"find_devices","Query","5", "gen_query",[File], 'emacs']}
       ]).

