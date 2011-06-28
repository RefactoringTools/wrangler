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
%%@version 0.1 
%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%
%%

-module(batch_rename_fun).

-export([composite_refac/0, is_atomic/0, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").


is_atomic()->
    false.


input_par_prompts() ->
    [].

composite_refac() ->
    [rename_fun_extended:rename_fun({filter, file, fun(_M) -> true end}, 
                                    bar, 
                                    {filter, fun(_A)-> true end}, 
                                    {user_input, fun({M,F,A})->
                                                         lists:flatten(io_lib:format("New function name for ~p:~p/~p: ", 
                                                                                     [M,F,A])) 
                                                 end},
                                    ["c:/cygwin/home/hl/test"])].


     %% {maybe, "Would you like to rename other functions?", 
     %%  rename_fun_extended:rename_fun(test, test1, 2,
     %%                                 {user_input,  fun(_)->"new funcion name: " end}, 
     %%                                 ["c:/cygwin/home/hl/test"])}].
