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
%%       names of its contributors may be used to endoorse or promote products
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
%% ============================================================================================
%% Refactoring: From eqc_statem to eqc_fsm
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================

%% =============================================================================================

%%%NOTE: THIS MODULE IS NOT FINISHED YET.
-module(refac_statem_to_fsm).

-export([eqc_statem_to_fsm/4, eqc_statem_to_fsm_eclipse/4]).

-import(refac_state_to_record, [is_statemachine_used/2]).

-include("../include/wrangler.hrl").

-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.


%% =============================================================================================
-spec(eqc_statem_to_fsm/4::(filename(), string(), [dir()], integer()) -> {ok, [filename()]}).
eqc_statem_to_fsm(FileName, StateName,  SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:eqc_statem_to_fsm(~p,~p, ~p, ~p).\n",
		 [?MODULE, FileName,StateName, SearchPaths, TabWidth]),
    eqc_statem_to_fsm(FileName, StateName, SearchPaths, TabWidth, emacs).

eqc_statem_to_fsm_eclipse(FileName, StateName, SearchPaths, TabWidth) ->
    eqc_statem_to_fsm(FileName, StateName, SearchPaths, TabWidth, eclipse).

eqc_statem_to_fsm(FileName, StateName, SearchPaths, TabWidth, Editor) ->
    Cmd = "CMD: "++ atom_to_list(?MODULE) ++ ":eqc_state_to_fsm(" ++"\"" ++
	FileName ++ "\"," ++ StateName  ++ "\"," ++
	"[" ++ refac_util:format_search_paths(SearchPaths) ++ "],"++integer_to_list(TabWidth) ++ ").",
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    pre_cond_check(Info, StateName),
    AnnAST1 = do_eqc_state_to_fsm(AnnAST, list_to_atom(StateName)),
    refac_util:write_refactored_files(FileName, AnnAST1, Editor, Cmd).
   

pre_cond_check(Info, StateName) ->
    case refac_util:is_fun_name(StateName) of
	true ->
	    ok;
	_ ->
	    throw({error, "Invalide state name"})
    end,
    is_statemachine_used(Info, eqc_statem).
	

do_eqc_state_to_fsm(AnnAST, StateName) ->
    Forms = refac_syntax:form_list_elements(AnnAST),
    NewForms = lists:append([transform(F, StateName) || F<-Forms]),
    refac_syntax:form_list(NewForms).

transform(F, StateName) ->
    case refac_syntax:type(F) of
	attribute ->
	    case is_include_statem(F) of
		true ->
		    include_statem_to_fsm(F);
		false ->
		    [F]
	    end;
	function->
	    transform_function(F, StateName);
	_ -> [F]
    end.

		    
is_include_statem(_F) ->
    false.

include_statem_to_fsm(F) ->	    
    [F].

transform_function(F, StateName) ->
    FunName = refac_syntax:function_name(F),
    Arity = refac_syntax:function_arity(F),
    case refac_syntax:type(FunName) of 
	atom ->
	    FunName1 = refac_syntax:atom_value(FunName),
	    case {FunName1, Arity} of 
		{initial_state, 0} -> transform_initial_state(F, StateName);
		{next_state,3} -> transform_next_state(F);
		{command, 1} -> transform_command(F);
		{precondition,2} -> transform_precondition(F);
		{postcondition,3} ->transform_postcondition(F);
		_ -> [F]
	    end;
	_ -> [F]
    end.
  
transform_initial_state(F, StateName) ->
    FunName = refac_syntax:function_name(F),
    Cs = refac_syntax:function_clauses(F),
    NewFunName = refac_util:rewrite(FunName, refac_syntax:atom(initial_state_data)),
    F1 = refac_util:rewrite(F, refac_syntax:function(NewFunName, Cs)),
    InitStateFName = refac_syntax:atom(initial_state),
    InitStateCs = [refac_syntax:clause([], none, [refac_syntax:atom(StateName)])],
    InitStateFun = refac_syntax:function(InitStateFName, InitStateCs),
    [InitStateFun, F1].
 
transform_next_state(F) ->
    FunName = refac_syntax:function_name(F),
    Cs = refac_syntax:function_clauses(F),
    NewFunName = refac_util:rewrite(FunName, refac_syntax:atom(next_state_data)),
    Cs1 =[transform_next_state_clause(C)||C<-Cs],
    NewF =refac_syntax:function(NewFunName, Cs1),
    [NewF].

transform_next_state_clause(C) ->
    Ps = refac_syntax:clause_patterns(C),
    G = refac_syntax:clause_guard(C),
    B = refac_syntax:clause_body(C),
    P1 = hd(Ps),
    Ps1 = [refac_util:rewrite(P1,refac_syntax:variable('_From')),
	   refac_util:rewrite(P1,refac_syntax:variable('_To'))|Ps],
    refac_util:rewrite(C, refac_syntax:clause(Ps1, G, B)).
	   
	   
transform_command(F) ->
    [F].
transform_precondition(F) ->
    [F].
transform_postcondition(F) ->
    FunName = refac_syntax:function_name(F),
    Cs = refac_syntax:function_clauses(F),
    Cs1 =[transform_next_state_clause(C)||C<-Cs],
    NewF =refac_syntax:function(FunName, Cs1),
    [NewF].

