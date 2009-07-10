%% Copyright (c) 2009, Huiqing Li, Simon Thompson
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
%% ===========================================================================================
%% Refactoring: Search an user-selected expression/expression sequence from the current buffer.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
-module(refac_sim_code).

-export([sim_code_detection/3]).

-include("../include/wrangler.hrl").

-define(DefaultSimiScore, 0.8).

sim_code_detection(FName, SearchPaths, TabWidth) ->
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    {AnnAST1, _} = generalise(AnnAST),
    refac_util:write_refactored_files_for_preview([{{FName, FName}, AnnAST1}]),
    {ok, [FName]}.
    

generalise(AnnAST) ->
    refac_util:stop_tdTP(fun generalise/2, AnnAST, []).

generalise(Node, []) ->
    case variable_replaceable(Node) of
	true ->
	    {refac_syntax:variable('Var'), true};
	_ -> {Node, false}
    end.


variable_replaceable(Exp) ->
    Res1 = case lists:keysearch(category, 1, refac_syntax:get_ann(Exp)) of
	       {value, {category, C}} ->
		   not(lists:member(C, [record_field, record_type, guard_expression]));
	       _ -> false
	   end,
    Exp_Export_Vars =refac_util:get_var_exports(Exp),
    refac_util:is_expr(Exp) andalso (not (refac_syntax:type(Exp) == variable)) andalso
    (not (refac_syntax:type(Exp)==macro)) andalso
			      (not (refac_syntax:type(Exp)==application))
	andalso Res1 andalso (Exp_Export_Vars ==[]). 

