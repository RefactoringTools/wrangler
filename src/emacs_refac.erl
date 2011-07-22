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
%% Some Interface Functions to Emacs
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =====================================================================

%% @private
%% @hidden
-module(emacs_refac).    

-include("../include/wrangler_internal.hrl").
  
-compile(export_all).

%%-spec(rename_var/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
%%	     {error, string()} | {ok, string()}).
rename_var(Fname, Line, Col, NewName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, rename_var, [Fname, Line, Col, NewName, SearchPaths, TabWidth], SearchPaths).
 
%%-spec(rename_fun/6::(string(), integer(), integer(), string(), [dir()], integer()) ->
%%	     {error, string()} |{warning, string()}| {ok, [filename()]}).

rename_fun(Fname, Line, Col, NewName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, rename_fun, [Fname, Line, Col, NewName, SearchPaths, TabWidth], SearchPaths).

%%-spec(rename_mod/4::(filename(), string(), [dir()], integer()) -> 
%%	     {error, string()} | {question, string()} | {warning, string()} |{ok, [filename()]}).
rename_mod(Fname, NewName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, rename_mod, [Fname, NewName, SearchPaths, TabWidth], SearchPaths).


%%-spec(rename_process/6::(filename(), integer(), integer(), string(), [dir()], integer()) ->
%%	     {error, string()} | {undecidables, string()}| {ok, [filename()]}).

rename_process(Fname, Line, Col, NewName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, rename_process, [Fname, Line, Col, NewName, SearchPaths, TabWidth], SearchPaths).


%%-spec(generalise/8::(filename(),integer(), integer(),integer(), integer(),string(), dir(), integer()) ->
%%	     {ok, [filename()]}
%%		 |{error, string()}
%%                 |{multiple_instances, {atom(), atom(), integer(), pos(), syntaxTree(), boolean(),[{pos(), pos()}], string()}}
%%		 |{unknown_side_effect, {atom(), atom(),integer(), pos(), syntaxTree(), integer(),
%%					 [{pos(), pos()}], [{pos(),pos()}], string()}}
%%		 |{more_than_one_clause, {atom(), atom(), integer(), pos(), syntaxTree(), boolean(),
%%					  [{pos(), pos()}], [{pos(),pos()}], string()}}). 
generalise(Fname, StartLine, StartCol, EndLine, EndCol, ParName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, generalise, [Fname, {StartLine, StartCol}, {EndLine, EndCol}, ParName, SearchPaths, TabWidth], SearchPaths).
	

%%-spec(move_fun/6::(filename(),integer(),integer(), string(),[dir()], integer())
%%        -> {ok, [{filename(), filename()}]} | {question, string()} | {error, string()}).

move_fun(FName, Line, Col, ModName,SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, move_fun, [FName, Line, Col, ModName, SearchPaths, TabWidth], SearchPaths).


%%-spec(duplicated_code_in_buffer/5::(filename(), string(), string(), string(), integer()) ->{ok, string()}).      
duplicated_code_in_buffer(FName, MinLines,  MinClones, MaxPars, TabWidth) ->
    wrangler_refacs:duplicated_code_in_buffer(FName, MinLines, MinClones, MaxPars, TabWidth).

%%-spec(duplicated_code_in_dirs/5::([dir()], string(), string(), string(), integer()) ->{ok, string()}).
duplicated_code_in_dirs(SearchPaths, MinLines, MinClones, MaxPars, TabWidth) ->
    case check_searchpaths(SearchPaths) of 
 	ok ->
	    wrangler_refacs:duplicated_code_in_dirs(SearchPaths, MinLines, MinClones, MaxPars, TabWidth);
 	{error, Reason} -> {error, Reason}
     end.
  
%%-spec(expression_search_in_buffer/7::(filename(), integer(), integer(), integer(), integer(),[dir()],integer()) -> 
%%				 {ok, [{filename(),{{integer(), integer()}, {integer(), integer()}}}]}| {error, string()}).
expression_search_in_buffer(FName, StartLine, StartCol, EndLine, EndCol, SearchPaths, TabWidth) ->
   wrangler_refacs:identical_expression_search_in_buffer(FName, {StartLine, StartCol}, {EndLine, EndCol}, SearchPaths, TabWidth).

%%-spec(expression_search_in_dirs/7::(filename(), integer(), integer(), integer(), integer(),[dir()],integer()) -> 
%%					 {ok, [{filename(),{{integer(), integer()}, {integer(), integer()}}}]} |{error, string()}).
expression_search_in_dirs(FName, StartLine, StartCol, EndLine, EndCol, SearchPaths, TabWidth) ->
    wrangler_refacs:identical_expression_search_in_dirs(FName, {StartLine, StartCol}, {EndLine, EndCol}, SearchPaths, TabWidth).

%%-spec(similar_expression_search_in_buffer/8::(filename(), integer(), integer(), integer(), integer(),string(),[dir()], integer()) ->
%%	     {ok, [{integer(), integer(), integer(), integer()}]}).
similar_expression_search_in_buffer(FName, StartLine, StartCol, EndLine, EndCol, SimiScore, SearchPaths, TabWidth) ->
    wrangler_refacs:similar_expression_search_in_buffer(FName, {StartLine, StartCol}, {EndLine, EndCol}, SimiScore, SearchPaths, TabWidth).


%%-spec(similar_expression_search_in_dirs/8::(filename(), integer(), integer(), integer(), integer(),string(),[dir()], integer()) ->
%%	     {ok, [{integer(), integer(), integer(), integer()}]}).
similar_expression_search_in_dirs(FName, StartLine, StartCol, EndLine, EndCol, SimiScore, SearchPaths, TabWidth) ->
    wrangler_refacs:similar_expression_search_in_dirs(FName, {StartLine, StartCol}, {EndLine, EndCol}, SimiScore, SearchPaths, TabWidth).

%%-spec(fun_extraction/7::(filename(), integer(), integer(), integer(), integer(), string(), integer()) ->
%%	      {error, string()} | {ok, string()}).

fun_extraction(FName, StartLine, StartCol, EndLine, EndCol, FunName, TabWidth) ->
    apply_refactoring(wrangler_refacs, fun_extraction, [FName, {StartLine, StartCol}, {EndLine, EndCol}, FunName, TabWidth], []).
	 

%%-spec(unfold_fun_app/5::(FileName::filename(), StartLine::integer(), StartCol::integer(), SearchPaths::[dir()], TabWidth::integer)
%%      ->{error, string()} | {'ok', [string()]}).
unfold_fun_app(FileName, StartLine, StartCol, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, unfold_fun_app, [FileName, {StartLine, StartCol}, SearchPaths, TabWidth], SearchPaths).
   
%%-spec(new_macro/8::(filename(), integer(), integer(), integer(), integer(), string(), [dir()], integer()) ->
%%	      {error, string()} | {ok, string()}).

new_macro(FName, StartLine, StartCol, EndLine, EndCol, MacroName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, new_macro, [FName, {StartLine, StartCol}, {EndLine, EndCol}, MacroName, SearchPaths, TabWidth], SearchPaths).

%%-spec(intro_new_var/7::(filename(), integer(), integer(), integer(), integer(), string(),integer()) ->
%%	      {error, string()} | {ok, string()}).

intro_new_var(FName, StartLine, StartCol, EndLine, EndCol, NewVarName, TabWidth) ->
    apply_refactoring(wrangler_refacs, intro_new_var, [FName, {StartLine, StartCol}, {EndLine, EndCol}, NewVarName, [], TabWidth], []).


%%-spec(inline_var/5::(filename(), integer(), integer(), [dir()],integer()) ->
%%			   {error, string()} | {ok, string()}).
inline_var(FName, Line, Col, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, inline_var, [FName, Line, Col, SearchPaths, TabWidth], []).

%%-spec(fold_against_macro/5::(filename(), integer(), integer(), [dir()], integer()) ->
%%	      {error, string()} | {ok, [{{{integer(), integer()}, {integer(), integer()}}, syntaxTree()}]}).
fold_against_macro(FileName, Line, Col,  SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, fold_against_macro, [FileName, Line, Col, SearchPaths, TabWidth], SearchPaths).

%%-spec(fold_expr_by_loc/5::
%%      (filename(), integer(), integer(), [dir()], integer()) -> {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
%%							 | {error, string()}).
fold_expr_by_loc(FName, Line, Col, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, fold_expr_by_loc, [FName, Line, Col, SearchPaths, TabWidth], SearchPaths).


%%-spec(fold_expr_by_name/7::(filename(), string(), string(), string(), string(), [dir()], integer()) ->
%%	     {ok, [{integer(), integer(), integer(), integer(), syntaxTree(), {syntaxTree(), integer()}}]}
%%		 | {error, string()}).

fold_expr_by_name(FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, fold_expr_by_name, [FileName, ModName, FunName, Arity, ClauseIndex, SearchPaths, TabWidth], SearchPaths).


%%-spec(tuple_funpar/7::(filename(), integer(), integer(), integer(), integer(), [dir()], integer()) ->
%%	     {error, string()} | {ok, [filename()]}).
tuple_funpar(Fname, StartLine, StartCol, EndLine, EndCol, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, tuple_funpar, [Fname, {StartLine, StartCol}, {EndLine, EndCol}, SearchPaths, TabWidth], SearchPaths).

    
%%-spec(add_a_tag/6::(filename(), integer(), integer(), string(), [dir()], filename()) ->
%%	     {error, string()} | {ok, [filename()]}).
add_a_tag(FileName, Line, Col, Tag, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, add_a_tag, [FileName, Line, Col, Tag, SearchPaths, TabWidth], SearchPaths).

%%-spec(normalise_record_expr/6::(filename(), integer(), integer(), boolean(),[dir()], integer()) -> {error, string()} | {ok, [filename()]}).
normalise_record_expr(FileName, Line, Col, ShowDefault, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, normalise_record_expr, [FileName, Line, Col, ShowDefault, SearchPaths, TabWidth], SearchPaths).

%%-spec(register_pid/8::(filename(), integer(), integer(), integer(),integer(), string(), [dir()], integer()) ->
%%    {error, string()}|{ok, [filename()]}).
register_pid(FileName, StartLine, StartCol, EndLine, EndCol, RegName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, register_pid, [FileName, {StartLine, StartCol}, {EndLine, EndCol}, RegName, SearchPaths, TabWidth], SearchPaths).


%%-spec(fun_to_process/6::(filename(), integer(), integer(), string(), [dir()], integer())->
%%	     {error, string()} | {undecidables, string()} | {ok, [filename()]}).
fun_to_process(Fname, Line, Col, ProcessName, SearchPaths, TabWidth ) ->
    apply_refactoring(wrangler_refacs, fun_to_process, [Fname, Line, Col, ProcessName, SearchPaths, TabWidth], SearchPaths).


%%-spec(new_let/8::(filename(), integer(), integer(), integer(),integer(), string(), [dir()], integer()) ->
%%	      {error, string()} | {ok, string()}).
new_let(FileName, StartLine, StartCol, EndLine, EndCol, PatName, SearchPaths, TabWidth) -> 
    apply_refactoring(wrangler_refacs, new_let, [FileName, {StartLine, StartCol}, {EndLine, EndCol}, PatName, SearchPaths, TabWidth], SearchPaths).


%%-spec(merge_let/3::(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) ->
%%	     {not_found, string()} |{ok, [{integer(), integer(), integer(), integer(), string()}], string()}).
merge_let(FileName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, merge_let, [FileName, SearchPaths, TabWidth], SearchPaths).


%%-spec(merge_forall/3::(FileName::filename(), SearchPaths::[dir()], TabWidth::integer()) ->
%%	     {not_found, string()} |{ok, [{integer(), integer(), integer(), integer(), string()}], string()}).
merge_forall(FileName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, merge_forall, [FileName, SearchPaths, TabWidth], SearchPaths).

eqc_statem_to_record(FileName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, eqc_statem_to_record, [FileName, SearchPaths, TabWidth], SearchPaths).

eqc_fsm_to_record(FileName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, eqc_fsm_to_record, [FileName, SearchPaths, TabWidth], SearchPaths).

gen_fsm_to_record(FileName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, gen_fsm_to_record, [FileName, SearchPaths, TabWidth], SearchPaths).

eqc_statem_to_fsm(FileName, StateName, SearchPaths, TabWidth) ->
    apply_refactoring(wrangler_refacs, eqc_statem_to_fsm, [FileName, StateName, SearchPaths, TabWidth], SearchPaths).

%%-spec(partition_exports/4::(File::filename(), DistTreshold::string(), 
%%			    SearchPaths::[filename()|dir()],TabWidth::integer()) ->
%%				 {error, string()}|{ok, [filename()]}).
partition_exports(File, DistThreshold, SearchPaths, TabWidth)->
    apply_refactoring(wrangler_refacs, partition_exports, [File, DistThreshold, SearchPaths, TabWidth], SearchPaths).


apply_refactoring(Mod, Fun, Args, SearchPaths) ->
    case initial_checking(SearchPaths) of
      ok ->
	  Res = apply(Mod, Fun, Args),
	  output_logged_msg(),
	  Res;
      {error, Reason} -> {error, Reason}
    end.
  
initial_checking(SearchPaths1) ->
    case check_searchpaths(SearchPaths1) of
      ok ->
	  check_undo_process();
      {error, Reason} -> {error, Reason}
    end.

check_searchpaths(SearchPaths) ->
    InValidSearchPaths = lists:filter(fun (X) -> not filelib:is_dir(X) end, SearchPaths),
    case InValidSearchPaths of
	[] -> ok;
	_ -> ?wrangler_io("\n===============================WARNING===============================\n",[]), 
	     ?wrangler_io("The following directories specified in the search paths do not exist:\n~s", [InValidSearchPaths]),
	     {error, "Some directories specified in the search paths do not exist!"}
    end.

			  
check_undo_process() ->
    case erlang:whereis(refactor_undo) of
	undefined ->
	    {error, "The Wrangler application is not started, or is not working properly, please restart the refactorer!"};
	_ ->
	    ok
    end.

output_logged_msg() ->
    _Msg = wrangler_refacs:get_log_msg(),
    ?wrangler_io(_Msg, []).
