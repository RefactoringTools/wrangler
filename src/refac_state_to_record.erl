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
%% Refactoring: State data to record.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================

%% =============================================================================================
%% @private
-module(refac_state_to_record).

-export([eqc_statem_to_record/4, eqc_statem_to_record_1/8,
	 eqc_statem_to_record_eclipse/3, eqc_statem_to_record_1_eclipse/7,
	 eqc_fsm_to_record/4,  eqc_fsm_to_record_1/8,
	 eqc_fsm_to_record_eclipse/3, eqc_fsm_to_record_1_eclipse/7,
	 gen_fsm_to_record/4,  gen_fsm_to_record_1/8,
	 gen_fsm_to_record_eclipse/3, gen_fsm_to_record_1_eclipse/7]).

-export([is_statemachine_used/2]).

-export([format_state_funs/1, format_field_names/1]).
-include("../include/wrangler_internal.hrl").


-define(Msg, "Wrangler failed to infer the current data type of the state.").

%% =============================================================================================
-spec(eqc_statem_to_record/4::(filename(),[dir()], atom(), integer()) -> {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
								 {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).

eqc_statem_to_record(FileName, SearchPaths, Context, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:eqc_statem_to_record(~p,~p, ~p, ~p).\n",
		 [?MODULE, FileName,SearchPaths, Context, TabWidth]),
    eqc_statem_to_record_1(FileName, SearchPaths, Context, TabWidth).

-spec(eqc_statem_to_record_eclipse/3::(filename(),[dir()], integer()) -> {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
								 {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).
eqc_statem_to_record_eclipse(FileName, SearchPaths, TabWidth) ->
    eqc_statem_to_record_1(FileName, SearchPaths, eclipse, TabWidth).


eqc_statem_to_record_1(FileName, SearchPaths, _Editor, TabWidth) ->
    state_to_record(FileName, SearchPaths, TabWidth, eqc_statem).


eqc_statem_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, Editor, TabWidth) ->
    format_args(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth,	"eqc_statem_to_record_1"),
    eqc_statem_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, Editor, "").

-spec(eqc_statem_to_record_1_eclipse/7::(FileName::filename(), RecordName::string(), RecordFields::[string()],
					StateFuns::[{atom(), atom(), integer()}], IsTuple::boolean(),
					SearchPaths::[dir()], TabWidth::integer()) ->
					      {ok, [{filename(), filename(), string()}]}).
eqc_statem_to_record_1_eclipse(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    eqc_statem_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, eclipse, "").

eqc_statem_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, Editor, Cmd) ->
    state_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, 
		      eqc_statem, SearchPaths, TabWidth, Editor, Cmd).


%% =============================================================================================
-spec(eqc_fsm_to_record/4::(filename(),[dir()], atom(), integer()) -> {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
		                                              {'ok', {tuple, integer()}, [{atom(), atom(),integer()}]}).
eqc_fsm_to_record(FileName, SearchPaths, Context, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:eqc_fsm_to_record(~p,~p, ~p, ~p).\n", [?MODULE, FileName,SearchPaths, Context, TabWidth]),
    eqc_fsm_to_record_1(FileName, SearchPaths, Context, TabWidth).

-spec(eqc_fsm_to_record_eclipse/3::(filename(),[dir()], integer()) 
                                   -> {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
                                      {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).
eqc_fsm_to_record_eclipse(FileName, SearchPaths, TabWidth) ->
    eqc_fsm_to_record_1(FileName, SearchPaths, eclipse, TabWidth).


eqc_fsm_to_record_1(FileName, SearchPaths, _Editor, TabWidth) ->
    state_to_record(FileName, SearchPaths, TabWidth, eqc_fsm).

eqc_fsm_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, Context, TabWidth) ->
    format_args(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth,	"eqc_fsm_to_record_1"),
    eqc_fsm_to_record(FileName, RecordName, RecordFields, StateFuns,
		      IsTuple, SearchPaths, TabWidth, Context, "").


-spec(eqc_fsm_to_record_1_eclipse/7::(FileName::filename(), RecordName::string(), RecordFields::[string()],
					StateFuns::[{atom(), atom(), integer()}], IsTuple::boolean(),
					SearchPaths::[dir()], TabWidth::integer()) ->
					      {ok, [{filename(), filename(), string()}]}).
eqc_fsm_to_record_1_eclipse(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    eqc_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, eclipse, "").

eqc_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, Editor, Cmd) ->
    state_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, 
		      eqc_fsm, SearchPaths, TabWidth, Editor, Cmd).



%% =============================================================================================
%%-spec(gen_fsm_to_record/3::(filename(),[dir()], atom(), integer()) -> {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
%%							      {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).

gen_fsm_to_record(FileName, SearchPaths, Editor, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:gen_fsm_to_record(~p,~p, ~p).\n", [?MODULE, FileName,SearchPaths, TabWidth]),
    gen_fsm_to_record_1(FileName, SearchPaths, Editor, TabWidth).


-spec(gen_fsm_to_record_eclipse/3::(filename(),[dir()], integer()) -> {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
								 {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).
gen_fsm_to_record_eclipse(FileName, SearchPaths, TabWidth) ->
    gen_fsm_to_record_1(FileName, SearchPaths, eclipse, TabWidth).


gen_fsm_to_record_1(FileName, SearchPaths, _Editor, TabWidth) ->
    state_to_record(FileName, SearchPaths, TabWidth, gen_fsm).


gen_fsm_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, Editor, TabWidth) ->
    format_args(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, "gen_fsm_to_record_1"),
    gen_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, Editor, "").


-spec(gen_fsm_to_record_1_eclipse/7::(FileName::filename(), RecordName::string(), RecordFields::[string()],
					StateFuns::[{atom(), atom(), integer()}], IsTuple::boolean(),
					SearchPaths::[dir()], TabWidth::integer()) ->
					      {ok, [{filename(), filename(), string()}]}).
gen_fsm_to_record_1_eclipse(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    gen_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, eclipse, "").

gen_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, Editor, Cmd) ->
    state_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, 
		      gen_fsm, SearchPaths, TabWidth, Editor, Cmd).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  format refactoring command                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format_args(_FileName, _RecordName, _RecordFields, _StateFuns, _IsTuple, _SearchPaths, _TabWidth, _FunName) ->
    ?wrangler_io("\nCMD: ~p:" ++ _FunName ++ "(~p,~p,", [?MODULE, _FileName, _RecordName]),
    ?wrangler_io(format_field_names(_RecordFields) ++ ",", []),
    ?wrangler_io(format_state_funs(_StateFuns), []),
    ?wrangler_io(",~p,~p, ~p).\n", [_IsTuple, _SearchPaths, _TabWidth]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pre-condition check                                                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pre_cond_check(RecordName, FieldNames, ModInfo) ->
    check_record_and_field_names(RecordName, FieldNames),
    FieldNames1 = [list_to_atom(F) || F <-FieldNames],
    check_existing_records(list_to_atom(RecordName),  FieldNames1, ModInfo).

check_record_and_field_names(RecordName, FieldNames) ->
    case api_refac:is_fun_name(RecordName) of
	true ->
	    ok;
	_ -> throw({error, "Invalid record name."})
    end,
    lists:foreach(
      fun (F) ->
	      case api_refac:is_fun_name(F) of
		  true ->
		      ok;
		  _ -> throw({error, "Invalid field name: " ++ F ++ "."})
	      end
      end, FieldNames),
    case length(lists:usort(FieldNames)) =/= length(FieldNames) of
	true ->
	    throw({error, "Some field names are the same."});
	false ->
	    ok
    end.

check_existing_records(RecordName, FieldNames, Info) ->
    case lists:keysearch(records, 1, Info) of 
	{value, {records, RecordList}} ->
	    case lists:keysearch(RecordName, 1, RecordList) of 
		{value, {RecordName, Fields}} ->
		    case length(Fields) =/= length(FieldNames) of
			true ->
			    throw({error, "Record with the same name, "
				   "but different number of fields, "
				   "is already in scope."});
			false ->
			    ok
		    end,
		    FieldNames1 = [F || {F, _V} <- Fields],
		    case lists:usort(FieldNames)== lists:usort(FieldNames1) of 
			true ->
			    true;
			false ->
			    throw({error, "Record with the same name, "
				   "but different field names, "
				   "is already in scope."})
		    end
	    end;
	false ->
	    false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%%                  Transformation                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


state_to_record(FileName, SearchPaths, TabWidth, SM) ->
    {ok, {_, ModInfo}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, ModInfo),
    case is_statemachine_used(ModInfo, SM) of
	true ->
	    check_current_state_type(FileName, ModName, ModInfo, SM);
	{error, Msg} -> throw({error, Msg})
    end.

state_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SM, SearchPaths, TabWidth, Editor, Cmd) ->
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    RecordExists = pre_cond_check(RecordName, RecordFields, Info),
    RecordFields1 = [list_to_atom(F) || F <- RecordFields],
    AnnAST1 = do_state_to_record(ModName, Info, AnnAST, list_to_atom(RecordName), RecordFields1,
				 StateFuns, RecordExists, IsTuple, SM),
    wrangler_write_file:write_refactored_files([{{FileName,FileName}, AnnAST1}], Editor, TabWidth, Cmd).
  


do_state_to_record(ModName, ModInfo,  AnnAST, RecordName, RecordFields, StateFuns, RecordExists, IsTuple, SM) ->
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    Funs = get_possible_conversion_funs(Forms, RecordName),
    TupleToRecordFunName = tuple_to_record_fun_name(ModInfo, Funs, RecordName, RecordFields),
    RecordToTupleFunName = record_to_tuple_fun_name(ModInfo, Funs, RecordName, RecordFields),
    Fun = fun (F) ->
		  case wrangler_syntax:type(F) of
		    function ->
			do_state_to_record_1(ModName, F, RecordName, RecordFields, StateFuns, IsTuple, SM, 
					     TupleToRecordFunName, RecordToTupleFunName);
		    _ -> F
		  end
	  end,
    Forms0 = [Fun(F) || F <- Forms],
    Forms1 = unfold_conversion_apps(Forms0, RecordToTupleFunName, TupleToRecordFunName, RecordName, RecordFields, IsTuple),    
    Forms2 = add_util_funs(Forms1, ModInfo, RecordName, RecordFields,TupleToRecordFunName, RecordToTupleFunName),
    case RecordExists of
      true ->
	  wrangler_syntax:form_list(Forms2);
      false ->
	  RecordDef = mk_record_attribute(RecordName, RecordFields),
	  NewForms =  insert_record_attribute(Forms2, RecordDef),
	  wrangler_syntax:form_list(NewForms)
    end.

do_state_to_record_1(ModName, Fun, RecordName, RecordFields, StateFuns, IsTuple, 
		     SM, TupleToRecordFunName, RecordToTupleFunName) ->
    As = wrangler_syntax:get_ann(Fun),
    {value, {fun_def, {M, F, A, _, _}}} = lists:keysearch(fun_def, 1, As),
    CallBacks =  callbacks(ModName, StateFuns, SM),
    NewFun = case lists:keysearch({M, F, A}, 1, CallBacks) of
		 {value, {{M, F, A}, {PatIndex, ReturnState}}} ->
		     ?debug("MFA1:\n~p\n", [{M,F,A}]),
		     do_state_to_record_in_callback_fun(
		       PatIndex, Fun, RecordName, RecordFields, IsTuple, ReturnState, SM, 
		       TupleToRecordFunName, RecordToTupleFunName);
		 false -> Fun
	     end,
    wrap_fun_interface(NewFun, ModName, RecordName, RecordFields, IsTuple, StateFuns, SM,
		      TupleToRecordFunName, RecordToTupleFunName).
	  
		 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


do_state_to_record_in_callback_fun(PatIndex, Fun, RecordName, RecordFields, IsTuple, ReturnState, SM,
				   TupleToRecordFunName, RecordToTupleFunName) ->
    FunName = wrangler_syntax:function_name(Fun),
    Cs = wrangler_syntax:function_clauses(Fun),
    Cs1 = [do_state_to_record_in_callback_clause(PatIndex, C, RecordName, RecordFields,
						 IsTuple, ReturnState, SM, TupleToRecordFunName,
						 RecordToTupleFunName)
	   || C <- Cs],
    wrangler_misc:rewrite(Fun, wrangler_syntax:function(FunName, Cs1)).

do_state_to_record_in_callback_clause(PatIndex, C, RecordName, RecordFields, IsTuple, ReturnState, SM,
				      TupleToRecordFunName, RecordToTupleFunName) ->
    Ps = wrangler_syntax:clause_patterns(C),
    G = wrangler_syntax:clause_guard(C),
    B = wrangler_syntax:clause_body(C),
    {Ps1, DefPs} = do_state_to_record_in_pats(Ps, PatIndex, RecordName, RecordFields, IsTuple),
    B1 = do_state_to_record_in_callback_fun_clause_body(
	   B, RecordName, RecordFields, DefPs, IsTuple, ReturnState, SM, TupleToRecordFunName, RecordToTupleFunName),
    B2 = remove_record_tuple_conversions(wrangler_syntax:block_expr(B1), TupleToRecordFunName, RecordToTupleFunName),
    B3 = wrangler_syntax:block_expr_body(B2),
    wrangler_misc:rewrite(C, wrangler_syntax:clause(Ps1, G, B3)).


do_state_to_record_in_pats(Ps, PatIndex, RecordName, RecordFields, IsTuple) ->
    Res =[do_state_to_record_in_pats_1({P, Index}, PatIndex, RecordName, RecordFields, IsTuple) || 
	     {P, Index}<- lists:zip(Ps, lists:seq(1, length(Ps)))],
    {NewPs, DefPs} =lists:unzip(Res),
    {NewPs, lists:append(DefPs)}.

do_state_to_record_in_pats_1({P, Index}, PatIndex, RecordName, RecordFields, IsTuple) ->
    case lists:member(Index, PatIndex) of 
	true ->
	    do_state_to_record_in_pats_2(P, RecordName, RecordFields, IsTuple);
	false ->
	    {P,[]}
    end.

do_state_to_record_in_pats_2(P, RecordName, RecordFields, IsTuple) ->
    case wrangler_syntax:type(P) of
	variable ->
	    {P, [wrangler_syntax:get_pos(P)]};
	tuple when IsTuple ->
	    RecordExpr = tuple_to_record_expr(P, RecordName, RecordFields),
	    {wrangler_misc:rewrite(P, RecordExpr), []};
	match_expr ->
	    tuple_to_record_in_match_expr_pattern(
	      P, RecordName, RecordFields, IsTuple);
	underscore ->
	    {P, []};
	_ when IsTuple ->
	    Pos = wrangler_syntax:get_pos(P),
	    throw({error, "Wrangler did not know how to transform the pattern at location: "
			     ++ io_lib:format("~p", [Pos])});
	_ when length(RecordFields) == 1 ->
	    Fields = [mk_record_field(hd(RecordFields), P)],
	    P1 = wrangler_misc:rewrite(P, mk_record_expr(RecordName, Fields)),
	    {P1, []};
	_ ->
	    Pos = wrangler_syntax:get_pos(P),
	    throw({error, "Wrangler did not know how to transform the pattern at location: "
			     ++ io_lib:format("~p", [Pos])})
    end.

tuple_to_record_in_match_expr_pattern(State, RecordName, RecordFields, IsTuple) ->
    P = wrangler_syntax:match_expr_pattern(State),
    B = wrangler_syntax:match_expr_body(State),
    {P1, Pos1} =
	do_state_to_record_in_pats_2(P, RecordName, RecordFields, IsTuple),
    {B1, Pos2} =
	do_state_to_record_in_pats_2(B, RecordName, RecordFields, IsTuple),
    {wrangler_misc:rewrite(State, wrangler_syntax:match_expr(P1, B1)), Pos1 ++ Pos2}.


do_state_to_record_in_callback_fun_clause_body(Body, RecordName, RecordFields,
					       DefPs, IsTuple, ReturnState, SM,
					       TupleToRecordFunName, RecordToTupleFunName) ->
    ?debug("RecordFields:\n~p\n", [RecordFields]),
    Fun = fun (Node, _Others) ->
		  case wrangler_syntax:type(Node) of
                      application when IsTuple ->
                          As = wrangler_syntax:get_ann(wrangler_syntax:application_operator(Node)),
                          case lists:keysearch(fun_def, 1, As) of
                              {value, {fun_def, {erlang, element, 2, _, _}}} ->
                                  element_to_record_access(RecordName, RecordFields, DefPs, Node, RecordToTupleFunName);
                              {value, {fun_def, {erlang, setelement, 3, _, _}}} ->
                                  setelement_to_record_expr(Node, RecordName, RecordFields, DefPs, IsTuple,
                                                            TupleToRecordFunName, RecordToTupleFunName);
                              _ -> Node
                          end;
                      variable ->
                          As = wrangler_syntax:get_ann(Node),
                          case lists:keysearch(def, 1, As) of
                              {value, {def, DefinePos}} ->
                                  case DefinePos -- DefPs =/= DefinePos of
                                      true ->
                                          make_record_to_tuple_app(Node, RecordName, RecordFields, IsTuple,
                                                                   TupleToRecordFunName, RecordToTupleFunName);
                                      false ->
                                          Node
                                  end;
                              false -> Node
                          end;
                      _ -> Node
		  end
	  end,
    Body1 = api_ast_traverse:full_buTP(Fun, wrangler_syntax:block_expr(Body), {}),
    Body2 = is_tuple_to_is_record(Body1, RecordName, RecordToTupleFunName),
    Body3 = wrangler_syntax:block_expr_body(Body2),
    case ReturnState of
        true ->
            do_state_to_record_in_init_fun_clause_body(
              Body3, RecordName, RecordFields, DefPs, IsTuple, SM,
              TupleToRecordFunName, RecordToTupleFunName);
        false ->
            Body3
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM,
				      TupleToRecordFunName, RecordToTupleFunName) ->
    P = wrangler_syntax:clause_patterns(C),
    G = wrangler_syntax:clause_guard(C),
    B = wrangler_syntax:clause_body(C),
    B1 = do_state_to_record_in_init_fun_clause_body(B, RecordName, RecordFields, [], IsTuple, SM,
						    TupleToRecordFunName, RecordToTupleFunName),
    wrangler_misc:rewrite(C, wrangler_syntax:clause(P, G, B1)).

do_state_to_record_in_init_fun_clause_body(Body, RecordName, RecordFields, DefPs, IsTuple, SM,
					   TupleToRecordFunName, RecordToTupleFunName) ->
    Msg = "Wrangler did not know how to transform the expression at location: ",
    [LastExpr| Exprs] = lists:reverse(Body),
    Pos = wrangler_syntax:get_pos(LastExpr),
    case wrangler_syntax:type(LastExpr) of
	tuple when SM == gen_fsm ->
	    LastExpr1 = tuple_to_record_in_gen_fsm(
			  LastExpr, RecordName, RecordFields, IsTuple, Msg, Pos,
			  TupleToRecordFunName, RecordToTupleFunName),
	    lists:reverse([LastExpr1| Exprs]);
	tuple when IsTuple ->
	    LastExpr1 = tuple_to_record_expr(LastExpr, RecordName, RecordFields),
	    lists:reverse([LastExpr1| Exprs]);
	variable ->
	    As = wrangler_syntax:get_ann(LastExpr),
	    case lists:keysearch(def, 1, As) of
		{value, {def, DefinePos}} ->
		    case is_used_only_once(wrangler_syntax:block_expr(Body), DefinePos) of
			true ->
			    {Body1, Modified} =
				do_state_to_record_in_match_expr(
				  wrangler_syntax:block_expr(Body), LastExpr, DefinePos, RecordName, RecordFields, IsTuple,
				  SM, TupleToRecordFunName, RecordToTupleFunName),
			    case Modified of
				true ->
				    wrangler_syntax:block_expr_body(Body1);
				false ->
				    fail_or_tuple_to_record_app(Body, RecordName, RecordFields, IsTuple, SM,
								TupleToRecordFunName, RecordToTupleFunName)
			    end;
			false ->
			    fail_or_tuple_to_record_app(Body, RecordName, RecordFields, IsTuple, SM,
							TupleToRecordFunName, RecordToTupleFunName)
		    end;
		_ ->
		    fail_or_tuple_to_record_app(Body, RecordName, RecordFields, IsTuple, SM,
						TupleToRecordFunName, RecordToTupleFunName)
	    end;
	case_expr ->
	    Args = wrangler_syntax:case_expr_argument(LastExpr),
	    Cs = wrangler_syntax:case_expr_clauses(LastExpr),
	    Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM,
							 TupleToRecordFunName, RecordToTupleFunName) || C <- Cs],
	    LastExpr1 = wrangler_misc:rewrite(LastExpr, wrangler_syntax:case_expr(Args, Cs1)),
	    lists:reverse([LastExpr1| Exprs]);
	if_expr ->
	    Cs = wrangler_syntax:if_expr_clauses(LastExpr),
	    Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM,
							 TupleToRecordFunName, RecordToTupleFunName) || C <- Cs],
	    LastExpr1 = wrangler_misc:rewrite(LastExpr, wrangler_syntax:if_expr(Cs1)),
	    lists:reverse([LastExpr1| Exprs]);
	cond_expr ->
	    Cs = wrangler_syntax:cond_expr_clauses(LastExpr),
	    Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM,
							 TupleToRecordFunName, RecordToTupleFunName) || C <- Cs],
	    LastExpr1 = wrangler_misc:rewrite(LastExpr, wrangler_syntax:cond_expr(Cs1)),
	    lists:reverse([LastExpr1| Exprs]);
	block_expr ->
	    B = wrangler_syntax:block_expr_body(LastExpr),
	    B1 = do_state_to_record_in_init_fun_clause_body(B, RecordName, RecordFields, DefPs, IsTuple, SM,
							    TupleToRecordFunName, RecordToTupleFunName),
	    LastExpr1 = wrangler_misc:rewrite(LastExpr, wrangler_syntax:block_expr(B1)),
	    lists:reverse([LastExpr1| Exprs]);
	receive_expr ->
	    Cs = wrangler_syntax:receive_expr_clauses(LastExpr),
	    T = wrangler_syntax:receive_expr_timeout(LastExpr),
	    A = wrangler_syntax:receive_expr_action(LastExpr),
	    Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM,
							 TupleToRecordFunName, RecordToTupleFunName) || C <- Cs],
	    LastExpr1 = wrangler_misc:rewrite(LastExpr, wrangler_syntax:receive_expr(Cs1, T, A)),
	    lists:reverse([LastExpr1| Exprs]);
	atom when SM == gen_fsm ->
	    Body;
	_ when IsTuple ->
	    fail_or_tuple_to_record_app(Body, RecordName, RecordFields, IsTuple, SM,
					TupleToRecordFunName, RecordToTupleFunName);
	_ when SM == gen_fsm ->
	    throw({error, Msg});  %%  ++ io_lib:format("~p", [Pos])});
	_ ->
	    Fields = [mk_record_field(hd(RecordFields), LastExpr)],
	    LastExpr1 = wrangler_misc:rewrite(LastExpr, mk_record_expr(RecordName, Fields)),
	    lists:reverse([LastExpr1| Exprs])
    end.

fail_or_tuple_to_record_app(Body, RecordName, RecordFields, IsTuple, SM, 
			    TupleToRecordFunName, RecordToTupleFunName) ->
    Msg = "Wrangler did not know how to transform the expression at location: ",
    [LastExpr| Exprs] = lists:reverse(Body),
    Pos = wrangler_syntax:get_pos(LastExpr),
    case SM of 
	gen_fsm ->
	    throw({error, Msg ++ io_lib:format("~p", [Pos])});
	_ ->
	    LastExpr1 = make_tuple_to_record_app(LastExpr, RecordName, RecordFields, IsTuple,
						TupleToRecordFunName, RecordToTupleFunName),
	    lists:reverse([LastExpr1| Exprs])
    end.

do_state_to_record_in_match_expr(Body, _LastExpr, DefinePos, RecordName, RecordFields, _IsTuple, SM,
				 _TupleToRecordFunName, RecordToTupleFunName)
    when SM == eqc_statem orelse SM == eqc_fsm ->
    Fun = fun (Node, _Others) ->
		  case wrangler_syntax:type(Node) of
		      match_expr ->
			  P = wrangler_syntax:match_expr_pattern(Node),
			  B = wrangler_syntax:match_expr_body(Node),
			  case wrangler_syntax:type(P) of
			      variable ->
				  Pos = wrangler_syntax:get_pos(P),
				  case lists:member(Pos, DefinePos) of
				      true ->
					  case wrangler_syntax:type(B) of
					      tuple ->
						  B1 = tuple_to_record_expr(B, RecordName, RecordFields),
						  Node1 = wrangler_syntax:match_expr(P, wrangler_misc:rewrite(B, B1)),
						  {wrangler_misc:rewrite(Node, Node1), true};
					      application ->
						  case is_app(B, {RecordToTupleFunName, 1}) of
						      true ->
							  [T] = wrangler_syntax:application_arguments(B),
							  {T, true};
						      false ->
							  {Node, false}
						  end;
					      _ ->
						  {Node, false}
					  end;
				      false ->
					  {Node, false}
				  end;
			      _ -> {Node, false}
			  end;
		      _ -> {Node, false}
		  end
	  end,
    api_ast_traverse:stop_tdTP(Fun, Body, {});

do_state_to_record_in_match_expr(Body, LastExpr, DefinePos, RecordName, RecordFields, IsTuple, SM,
				 TupleToRecordFunName, RecordToTupleFunName)
    when SM == gen_fsm ->
    Msg = "Wrangler did not know how to transform the expression at location: ",
    Pos = wrangler_syntax:get_pos(LastExpr),
    Fun1 = fun (B) ->
		   case wrangler_syntax:type(B) of
		       tuple -> {tuple_to_record_in_gen_fsm(B, RecordName, RecordFields,
							    IsTuple, Msg, Pos, TupleToRecordFunName,
							    RecordToTupleFunName), true};
		       variable ->
			   {B, false};
		       _ ->
			   throw({error, Msg ++ io_lib:format("~p", [Pos])})
		   end
	   end,
    Fun = fun (Node, _Others) ->
		  case wrangler_syntax:type(Node) of
		      match_expr ->
			  P = wrangler_syntax:match_expr_pattern(Node),
			  B = wrangler_syntax:match_expr_body(Node),
			  case wrangler_syntax:type(P) of
			      variable ->
				  Pos1 = wrangler_syntax:get_pos(P),
				  case lists:member(Pos1, DefinePos) of
				      true ->
					  {B1, Modified} = Fun1(B),
					  case Modified of
					      true ->
						  Node1 = wrangler_syntax:match_expr(P, B1),
						  {wrangler_misc:rewrite(Node, Node1), true};
					      false ->
						  {Node, false}
					  end;
				      false ->
					  {Node, false}
				  end;
			      _ -> {Node, false}
			  end;
		      _ -> {Node, false}
		  end
	  end,
    api_ast_traverse:stop_tdTP(Fun, Body, {}).

tuple_to_record_in_gen_fsm(Tuple, RecordName, RecordFields, IsTuple, Msg, Pos,
			   TupleToRecordFunName, RecordToTupleFunName) ->
    Es = wrangler_syntax:tuple_elements(Tuple),
    Size = length(Es),
    case Size =< 2 of
	true -> Tuple;
	false ->
	    Tag = hd(Es),
	    case wrangler_syntax:type(Tag) of
		atom ->
		    case wrangler_syntax:atom_value(Tag) of
			ok ->
			    gen_fsm_state_to_record(RecordName, RecordFields, Tuple, 3, IsTuple, 
						   TupleToRecordFunName, RecordToTupleFunName);
			next_state ->
			    gen_fsm_state_to_record(RecordName, RecordFields, Tuple, 3, IsTuple,
						   TupleToRecordFunName, RecordToTupleFunName);
			reply ->
			    gen_fsm_state_to_record(RecordName, RecordFields, Tuple, 4, IsTuple,
						   TupleToRecordFunName, RecordToTupleFunName);
			stop ->
			    gen_fsm_state_to_record(RecordName, RecordFields, Tuple, Size, IsTuple,
						   TupleToRecordFunName, RecordToTupleFunName);
			_ ->
			    throw({error, Msg ++ io_lib:format("~p", [Pos])})
		    end;
		_ -> throw({error, Msg ++ io_lib:format("~p", [Pos])})
	    end
    end.

gen_fsm_state_to_record(RecordName, RecordFields, B, Nth, IsTuple,
			TupleToRecordFunName, RecordToTupleFunName) ->
    Es = list_to_tuple(wrangler_syntax:tuple_elements(B)),
    State = element(Nth, Es),
    case wrangler_syntax:type(State) of
	tuple when IsTuple ->
	    State1 = tuple_to_record_expr(State, RecordName, RecordFields),
	    Es1 = tuple_to_list(setelement(Nth, Es, State1)),
	    wrangler_misc:rewrite(B, wrangler_syntax:tuple(Es1));
	_ ->
	    State1 = make_tuple_to_record_app(State, RecordName, RecordFields, IsTuple,
					      TupleToRecordFunName, RecordToTupleFunName),
	    Es1 = tuple_to_list(setelement(Nth, Es, State1)),
	    wrangler_misc:rewrite(B, wrangler_syntax:tuple(Es1))
    end.



wrap_fun_interface(Form, ModName, RecordName, RecordFields, IsTuple, StateFuns,SM,
		  TupleToRecordFunName, RecordToTupleFunName) ->
    Form1 = wrap_fun_interface_in_arg(Form, ModName, RecordName, RecordFields, IsTuple, StateFuns,SM,
				     TupleToRecordFunName, RecordToTupleFunName),
    wrap_fun_interface_in_return(Form1, ModName, RecordName, RecordFields, IsTuple, StateFuns, SM,
				TupleToRecordFunName, RecordToTupleFunName).

wrap_fun_interface_in_return(Form, ModName, RecordName, RecordFields, IsTuple, StateFuns, SM,
			     TupleToRecordFunName, RecordToTupleFunName) ->
    Fun = fun (Node, _Others) ->
		  case is_callback_fun_app(Node, ModName, StateFuns, SM) of
		      {true, _PatIndex, true} ->
			  case SM of
			      gen_fsm ->
				  throw({error, "Callback functions are called as normal functions."});
			      _ ->
				  make_record_to_tuple_app(Node, RecordName, RecordFields, IsTuple,
							   TupleToRecordFunName, RecordToTupleFunName)
			  end;
		      false ->
			  Node
		  end
	  end,
    Form1 = api_ast_traverse:full_buTP(Fun, Form, {}),
    case SM of
	gen_fsm ->
	    Form1;
	_ ->
	    Res = check_use_of_run_commands(Form1, SM),
	    case Res of
		[] -> Form1;
		_ ->
		    {Form2, _} = api_ast_traverse:stop_tdTP(fun wrap_run_commands_result/2, Form1,
							    {RecordName, RecordFields, Res, IsTuple,
							     wrangler_misc:collect_var_names(Form1), SM}),
		    Form2
	    end
    end.

check_use_of_run_commands(Form, SM) ->
    Fun = fun (Node, {Acc1, Acc2}) ->
		  case wrangler_syntax:type(Node) of
		      match_expr ->
			  P = wrangler_syntax:match_expr_pattern(Node),
			  B = wrangler_syntax:match_expr_body(Node),
			  case is_app(B, {SM, run_commands, 2}) orelse 
				 is_app(B, {SM, run_commands, 3})
			      of
			      true ->
				  case wrangler_syntax:type(P) of
				      tuple ->
					  Es = wrangler_syntax:tuple_elements(P),
					  case Es of
					      [_H, S, _Res] ->
						  case wrangler_syntax:type(S) of
						      variable ->
							  case api_refac:free_vars(S) of
							      [] ->
								  {[wrangler_syntax:get_pos(S)| Acc1],
								   [wrangler_syntax:get_pos(B)| Acc2]};
							      _ ->
								  {[wrangler_syntax:get_pos(B)| Acc1], Acc2}
							  end;
						      _ -> {[wrangler_syntax:get_pos(B)| Acc1], Acc2}
						  end;
					      _ -> {[wrangler_syntax:get_pos(B)| Acc1], Acc2}
					  end;
				      _ -> {[wrangler_syntax:get_pos(B)| Acc1], Acc2}
				  end;
			      _ -> {Acc1, Acc2}
			  end;
		      application -> case is_app(Node, {SM, run_commands, 2}) orelse 
					    is_app(Node, {SM, run_commands, 3})
					 of
					 true ->
					     Pos = wrangler_syntax:get_pos(Node),
					     case lists:member(Pos, Acc2) of
						 true -> {Acc1, Acc2};
						 false ->
						     {[Pos| Acc1], Acc2}
					     end;
					 false ->
					     {Acc1, Acc2}
				     end;
		      _ -> {Acc1, Acc2}
		  end
	  end,
    {Acc1, Acc2} = api_ast_traverse:fold(Fun, {[], []}, Form),
    lists:usort(Acc1) -- lists:usort(Acc2).

wrap_run_commands_result(Node, {RecordName, RecordFields, DefPs, IsTuple, UsedVars, SM}) ->
    case wrangler_syntax:type(Node) of
	application ->
	    Pos = wrangler_syntax:get_pos(Node),
	    case lists:member(Pos, DefPs) of
		true ->
		    Node2 = transform_run_command(Node, UsedVars, RecordName, RecordFields, IsTuple, SM),
		    {Node2, true};
		false -> {Node, false}
	    end;
	variable ->
	    As = wrangler_syntax:get_ann(Node),
	    case lists:keysearch(def, 1, As) of
		{value, {def, DefinePos}} ->
		    case
			DefinePos -- DefPs =/= DefinePos andalso not lists:member(wrangler_syntax:get_pos(Node), DefPs)
			of
			true ->
			    case SM of
				eqc_statem ->
				    Node1 = wrangler_syntax:remove_comments(wrangler_misc:reset_attrs(Node)),
				    Es2 = transform_run_command_statem(Node1, RecordName, RecordFields, IsTuple),
				    {wrangler_syntax:copy_comments(Node, set_pos(wrangler_syntax:get_pos(Node), Es2)), true};
				eqc_fsm ->
				    Node1 = wrangler_syntax:remove_comments(wrangler_misc:reset_attrs(Node)),
				    Node2 = transform_run_command_fsm(Node1, RecordName, RecordFields, IsTuple),
				    {wrangler_syntax:copy_comments(Node, set_pos(wrangler_syntax:get_pos(Node), Node2)), true}
			    end;
			false ->
			    {Node, false}
		    end;
		false ->
		    {Node, false}
	    end;
	_ -> {Node, false}
    end.

transform_run_command(Node, UsedVars, RecordName, RecordFields, IsTuple, SM) ->
    Node1 = wrangler_misc:reset_attrs(Node),
    H = wrangler_syntax:variable(api_refac:make_new_name('H', UsedVars)),
    S = wrangler_syntax:variable(api_refac:make_new_name('S', UsedVars)),
    Res = wrangler_syntax:variable(api_refac:make_new_name('Res', UsedVars)),
    Es = [H, S, Res],
    Tuple = wrangler_syntax:tuple(Es),
    Expr1 = wrangler_syntax:match_expr(Tuple, Node1),
    S1 = case SM of
	     eqc_statem ->
		 transform_run_command_statem(S, RecordName, RecordFields, IsTuple);
	     eqc_fsm -> transform_run_command_fsm(S, RecordName, RecordFields, IsTuple)
	 end,
    Expr2 = wrangler_syntax:tuple([H, S1, Res]),
    set_pos(wrangler_syntax:get_pos(Node), wrangler_syntax:block_expr([Expr1, Expr2])).

transform_run_command_statem(State, RecordName, RecordFields, IsTuple) ->
    Es1 = [wrangler_syntax:record_access(State, wrangler_syntax:atom(RecordName), wrangler_syntax:atom(Field))
	   || Field <- RecordFields],
    Res = case IsTuple of
	      true -> wrangler_syntax:tuple(Es1);
	      false -> hd(Es1)
	  end,
    wrangler_misc:rewrite(State, Res).

transform_run_command_fsm(State, RecordName, RecordFields, IsTuple) ->
    StateName = wrangler_syntax:application(wrangler_syntax:atom(element), [wrangler_syntax:integer(1), State]),
    StateVal = wrangler_syntax:application(wrangler_syntax:atom(element), [wrangler_syntax:integer(2), State]),
    Es2 = transform_run_command_statem(StateVal, RecordName, RecordFields, IsTuple),
    wrangler_misc:rewrite(State, wrangler_syntax:tuple([StateName, Es2])).
  

wrap_fun_interface_in_arg(Form, ModName, RecordName, RecordFields, IsTuple, StateFuns, SM,
			  TupleToRecordFunName, RecordToTupleFunName) ->
    Fun = fun (Node, _Others) ->
		  case is_callback_fun_app(Node, ModName, StateFuns, SM) of
		    {true, PatIndex, _} ->
			do_transform_actual_pars(Node, PatIndex, RecordName, RecordFields, IsTuple,
						 TupleToRecordFunName, RecordToTupleFunName);
		    false ->
			Node
		  end
	  end,
    api_ast_traverse:full_buTP(Fun, Form, {}).

do_transform_actual_pars(Node, PatIndexes, RecordName, RecordFields, IsTuple,
			 TupleToRecordFunName, RecordToTupleFunName) ->
    Op = wrangler_syntax:application_operator(Node),
    Args = wrangler_syntax:application_arguments(Node),
    NewArgs = [do_transform_actual_pars_1({A, Index}, PatIndexes, RecordName, RecordFields, IsTuple,
					  TupleToRecordFunName, RecordToTupleFunName)
	       || {A, Index} <- lists:zip(Args, lists:seq(1, length(Args)))],
    Node1 = wrangler_syntax:application(Op, NewArgs),
    wrangler_misc:rewrite(Node, Node1).

do_transform_actual_pars_1({Arg, Index}, PatIndexes, RecordName, RecordFields, IsTuple,
			  TupleToRecordFunName, RecordToTupleFunName) ->
    case lists:member(Index, PatIndexes) of
	true ->
	    case wrangler_syntax:type(Arg) of
		tuple when IsTuple->
		    tuple_to_record_expr(Arg, RecordName, RecordFields);
		_ ->
		    make_tuple_to_record_app(Arg, RecordName, RecordFields, IsTuple, 
					     TupleToRecordFunName, RecordToTupleFunName)
	    end;
	false ->
	    Arg		    
    end.
	
	    

is_callback_fun_app(Node, ModName, StateFuns, SM) ->
    case wrangler_syntax:type(Node) of
	application ->
	    Op = wrangler_syntax:application_operator(Node),
	    case wrangler_syntax:type(Op) of
		atom ->
		    FunName = wrangler_syntax:atom_value(Op),
		    Args = wrangler_syntax:application_arguments(Node),
		    Arity = length(Args),
		    CallBacks = callbacks(ModName,StateFuns, SM),
		    case lists:keysearch({ModName, FunName, Arity},1, CallBacks) of
			{value, {{ModName, FunName, Arity}, {PatIndex, ReturnState}}} ->
			    {true, PatIndex, ReturnState};
			false ->
			    false
		    end;
		_ -> false
	    end;
	_ -> false
    end.

record_to_tuple_fun_name(ModInfo, Funs, RecordName, RecordFields) ->
    FunName = list_to_atom(atom_to_list(RecordName) ++ "_to_tuple"),
    {value, {module, ModName}} = lists:keysearch(module, 1, ModInfo),
    InscopeFuns = api_refac:inscope_funs(ModInfo),
    gen_fun_name(ModName, Funs, RecordName, RecordFields,
		 FunName, InscopeFuns, 0, record_to_tuple).

tuple_to_record_fun_name(ModInfo, Funs, RecordName, RecordFields) ->
    FunName = list_to_atom("tuple_to_" ++ atom_to_list(RecordName)),
    {value, {module, ModName}} = lists:keysearch(module, 1, ModInfo),
    InscopeFuns = api_refac:inscope_funs(ModInfo),
    gen_fun_name(ModName, Funs, RecordName, RecordFields,
		 FunName, InscopeFuns, 0, tuple_to_record).


gen_fun_name(ModName, Funs, RecordName, RecordFields, 
	     FunName, InscopeFuns, I, C) ->
    NewFunName = case I of 
		     0 -> FunName;
		     _ -> 
			 list_to_atom(atom_to_list(FunName)++"_"++integer_to_list(I))			  
		 end,
    case lists:member({ModName, NewFunName, 1}, InscopeFuns) of 
	true ->
	    case lists:keysearch({NewFunName,1},1, Funs) of 
		{value, {{NewFunName, 1}, FunDef}} ->
		    FunDef1 = case C of 
				  tuple_to_record ->
				      mk_tuple_to_record_fun(RecordName, RecordFields, NewFunName);
				  record_to_tuple ->
				      mk_record_to_tuple_fun(RecordName, RecordFields, NewFunName)
			      end,
		    case erl_prettypr:format(FunDef)== erl_prettypr:format(FunDef1) of
			true -> 
			    NewFunName;
			false ->
			    gen_fun_name(ModName, Funs, RecordName, RecordFields, 
					 FunName, InscopeFuns, I+1, C)
		    end;
		false ->
		    gen_fun_name(ModName, Funs, RecordName, RecordFields, 
				 FunName, InscopeFuns, I+1, C)
	    end;
	false ->
	    NewFunName
     end.
    

get_possible_conversion_funs(Forms, RecordName) ->
    RecordToTuple = atom_to_list(RecordName) ++ "_to_tuple",
    TupleToRecord = "tuple_to_" ++ atom_to_list(RecordName),
    Fun = fun (F) ->
		  FName = wrangler_syntax:function_name(F),
		  Arity = wrangler_syntax:function_arity(F),
		  case wrangler_syntax:type(FName) of
		    atom ->
			FName1 = atom_to_list(wrangler_syntax:atom_value(FName)),
			(lists:prefix(RecordToTuple, FName1) orelse
			   lists:prefix(TupleToRecord, FName1)) andalso Arity == 1;
		    _ ->
			false
		  end
	  end,
    [{{wrangler_syntax:atom_value(wrangler_syntax:function_name(F)),
       wrangler_syntax:function_arity(F)}, F} || F <- Forms, wrangler_syntax:type(F) == function, Fun(F)].


unfold_conversion_apps(Forms, RecordToTupleFunName, TupleToRecordFunName, RecordName, RecordFields, IsTuple) ->
    Fun = fun (Node, _Others) ->
		  case wrangler_syntax:type(Node) of
		    application ->
			case is_app(Node, {RecordToTupleFunName, 1}) of
			  true ->
			      Arg = hd(wrangler_syntax:application_arguments(Node)),
			      case wrangler_syntax:type(Arg) of
				variable ->
				    transform_run_command_statem(Arg, RecordName, RecordFields, IsTuple);
				_ -> Node
			      end;
			  false ->
			      case is_app(Node, {TupleToRecordFunName, 1}) of
				true ->
				    Arg = hd(wrangler_syntax:application_arguments(Node)),
				    case wrangler_syntax:type(Arg) of
				      tuple ->
					  Fields = mk_record_fields(RecordFields, wrangler_syntax:tuple_elements(Arg)),
					  mk_record_expr(RecordName, Fields);
				      _ -> Node
				    end;
				false ->
				    Node
			      end
			end;
		    _ -> Node
		  end
	  end,
    [api_ast_traverse:full_buTP(Fun, F, {}) || F <- Forms].

add_util_funs(Forms, ModInfo, RecordName, RecordFields, TupleToRecordFunName, RecordToTupleFunName) ->
    Fun = fun (Node, Acc) ->
		  case wrangler_syntax:type(Node) of
		      application ->
			  case is_app(Node, {RecordToTupleFunName, 1}) of
			      true ->
				  [{RecordToTupleFunName, 1}| Acc];
			      false ->
				  case is_app(Node, {TupleToRecordFunName, 1}) of
				      true ->
					  [{TupleToRecordFunName, 1}| Acc];
				      false ->
					  Acc
				  end
			  end;
		      _ -> Acc
		  end
	  end,
    Acc = lists:append([api_ast_traverse:fold(Fun, [], F) || F <- Forms]),
    ExistingFuns = case lists:keysearch(functions, 1, ModInfo) of
		       {value, {functions, Funs}} ->
			   Funs;
		       _ ->
			   []
		   end,
    case lists:member({RecordToTupleFunName, 1}, Acc) andalso 
	    not  lists:member({RecordToTupleFunName, 1}, ExistingFuns)
	of
	true ->
	    F1 = mk_record_to_tuple_fun(RecordName, RecordFields, RecordToTupleFunName),
	    case lists:member({TupleToRecordFunName, 1}, Acc) andalso 
		    not  lists:member({TupleToRecordFunName, 1}, ExistingFuns)
		of
		true ->
		    F2 = mk_tuple_to_record_fun(RecordName, RecordFields, TupleToRecordFunName),
		    Forms ++ [F1, F2];
		false ->
		    Forms ++ [F1]
	    end;
	false ->
	    case lists:member({TupleToRecordFunName, 1}, Acc) andalso 
		    not  lists:member({TupleToRecordFunName, 1}, ExistingFuns)
		of
		true ->
		    F2 = mk_tuple_to_record_fun(RecordName, RecordFields, TupleToRecordFunName),
		    Forms ++ [F2];
		false ->
		    Forms
	    end
    end.
%% ======================================================================

is_statemachine_used(ModuleInfo, gen_fsm) ->
    CallBacks = [{init, 1}],
    case lists:keysearch(functions, 1, ModuleInfo) of
	{value, {functions, Funs}} ->
	    case CallBacks -- Funs == [] of 
		true -> true;
		false ->
		    {error, "gen_fsm callback function init/1 is not defined."}
	    end;			
	false ->
	    {error, "gen_fsm callback function init/1 is not defined."}
    end;
is_statemachine_used(ModuleInfo, eqc_statem) ->
    CallBacks = [{initial_state, 0}, {precondition, 2}, {command, 1},
		 {postcondition, 3}, {next_state, 3}],
    is_statemachine_used_1(ModuleInfo, CallBacks, eqc_statem);
   

is_statemachine_used(ModuleInfo, eqc_fsm) ->
    CallBacks = [{initial_state_data, 0}, {next_state_data, 5}, 
		 {precondition, 4}, {postcondition, 5}],
    is_statemachine_used_1(ModuleInfo, CallBacks, eqc_fsm).


is_statemachine_used_1(ModuleInfo, CallBacks, SM) ->
    Msg = atom_to_list(SM)++".hrl is not imported by this module.",
    case lists:keysearch(imports, 1, ModuleInfo) of
	{value, {imports, Imps}} ->
	    Ms = [M || {M, _} <- Imps],
	    case lists:member(SM, Ms) of 
		true ->
		    case lists:keysearch(functions, 1, ModuleInfo) of
			{value, {functions, Funs}} ->
			    case CallBacks -- Funs of
				[] -> 
				    true;
				Fs -> 
				    case length(Fs) of
					1 ->
					    {error, atom_to_list(SM)++" callback function, " 
					     ++  format_funs(Fs) ++ ", is not defined."};
					_ ->
					    {error, atom_to_list(SM)++" callback functions: "
					     ++ format_funs(Fs)++ ", are not defined."}
				    end
			    end;
			false ->
			    {error, "none of the "++atom_to_list(SM)++" callback functions is defined."}
		    end;
		false ->
		    {error, Msg}
	    end;
	false ->
	    {error, Msg}
    end.

format_funs([]) ->
     "";
format_funs([{F,A}|T]) ->
    atom_to_list(F)++"/"++integer_to_list(A) ++
   	case T of 
	    [] -> format_funs(T);
	    [_T1] -> " and "++ format_funs(T);
	    _ -> ","++format_funs(T)
	end.
		   

%%=====================================================================

check_current_state_type(File, ModName, ModInfo, SM) ->
    TypeInfo = try wrangler_type_info:get_type_info_using_typer(File) of
                       V -> V
               catch
                   _E1:E2 ->
                       throw({error, E2})
               end,
    ?debug("TypeInfo:\n~p\n", [TypeInfo]),
    TypeInfo1 =[{F, Type, RecDict} || {F, Type, RecDict} <-TypeInfo, F == File],
    {Type, RecDict} = case TypeInfo1 of
			  [] ->
			      throw({error, ?Msg});
			  [{File, T, Rec}] ->
			      {T, Rec}
		      end,
    {Ts, StateFuns} =get_current_state_type_1(Type, ModName, ModInfo, SM),
    ?debug("RecDict:\n~p\n", [RecDict]),
    ?debug("Current Type:\n~p\n", [Ts]),
    Ts1 = lists:usort([case Tag of 
			   tuple ->
			       case Qual of
				   {Arity, {c, atom, [AtomTag], _}} ->
				       case lookup_record(AtomTag, Arity - 1, RecDict) of
					   error -> {Tag, length(Es)};
					   {ok, _Fields} -> record
				       end;
				   _ -> {Tag, length(Es)}
			       end;
			   tuple_set ->{tuple, length(Es)};
			   _ -> Tag
		       end 
		       ||{c,Tag, Es, Qual} <-lists:usort(Ts) --[any],
			is_list(Es)]),
    case Ts1 of
	[record] ->
	    throw({error, "The state of the state machine is already a record."});
	[{tuple, A}|Tail] ->
	    case Tail of 
		[] ->
		    {ok,{tuple, A}, StateFuns};
		_ ->
		    throw({error, ?Msg})
	    end;
	_ ->
	    {ok, non_tuple, StateFuns}
    end.
    
 
get_current_state_type_1(TypeInfo, ModName, _ModInfo, eqc_statem) ->
    Pars = callbacks(ModName, undefined, eqc_statem),
    get_current_state_type_2(TypeInfo, [], Pars);

get_current_state_type_1(TypeInfo, ModName, _ModInfo, eqc_fsm) ->
    StateFuns = get_eqc_fsm_state_functions(ModName, TypeInfo),
    ?debug("StateFuns:\n~p\n", [StateFuns]),
    Pars = callbacks(ModName, StateFuns, eqc_fsm),
    get_current_state_type_2(TypeInfo, StateFuns, Pars);

get_current_state_type_1(TypeInfo, ModName, ModInfo, gen_fsm) ->
    StateFuns = get_gen_fsm_state_functions(ModName, ModInfo, TypeInfo),
    Pars = callbacks(ModName, StateFuns, gen_fsm),
    Fun = fun ({{M, F, A}, RetType, ArgTypes}) ->
		  case lists:keysearch({M, F, A}, 1, Pars) of
		    {value, {{M, F, A}, {Is, ReturnState}}} ->
			T = case ReturnState of
			      true ->
				  get_gen_fsm_return_types(RetType);
			      false -> []
			    end,
			T ++ [lists:nth(I, ArgTypes) || I <- Is];
		    false ->
			[]
		  end
	  end,
    {lists:append([Fun(T) || T <- TypeInfo]), StateFuns}.



get_current_state_type_2(TypeInfo, StateFuns, Pars) ->
    Fun = fun ({{M, F, A}, RetType, ArgTypes}) ->
		  case lists:keysearch({M, F, A}, 1, Pars) of
		      {value, {{M, F, A}, {Is, ReturnState}}} ->
			  T =case ReturnState of
				 true -> [RetType];
				 _ -> []
			     end,
			  T ++ [lists:nth(I, ArgTypes)|| I<-Is];			      
		      false ->
			  []
		  end
	  end,
    {lists:append([Fun(T) || T <- TypeInfo]), StateFuns}.


get_gen_fsm_return_types(RetType) ->
    case RetType of 
	{c, tuple, Es, _} ->
	    case length(Es)>2 of 
		true ->
		    Tag = hd(Es),
		    case Tag of
			{c, atom, [ok], _} ->
			    [lists:nth(3, Es)];
			{c, atom, [next_state], _} ->
			    [lists:nth(3, Es)];
			{c, atom, [reply], _} ->
			    [lists:nth(4, Es)];
			{c, atom, [stop], _} ->
			    [lists:last(Es)];
			_ -> 
			    []
		    end;			
		false ->
		    []
	    end;
	_ -> []
    end.

get_eqc_fsm_state_functions(ModName, TypeInfo) ->
    case lists:keysearch({ModName, initial_state, 0}, 1, TypeInfo) of
      {value, {{ModName, initial_state, 0}, RetType, _ArgsType}} ->
	  case RetType of
	    {c, atom, [StateName], _} ->
		get_eqc_fsm_state_functions_1(ModName, TypeInfo, [StateName]);
	    _ -> throw({error, "Wrangler failed to infer the initial state of the fsm."})
	  end;
      false ->
	  throw({error, "eqc_fsm callback function initial_state/0 is not defined."})
    end.

get_eqc_fsm_state_functions_1(ModName, TypeInfo, StateNames) ->
    StateNames1 = get_eqc_fsm_state_functions_2(TypeInfo, StateNames),
    case StateNames == StateNames1 of
	true ->
	    [{ModName, S, 1} || S <- StateNames, S /= history];
	_ ->
	    get_eqc_fsm_state_functions_1(ModName, TypeInfo,
					  lists:usort(StateNames ++ StateNames1))
    end.

get_eqc_fsm_state_functions_2(TypeInfo, StateNames) ->
    Fun = fun ({{_M, F, A}, RetType, _ArgTypes}) ->
		  case A == 1 andalso lists:member(F, StateNames) of
		      true ->
			  case RetType of
			      {c, list, Es, _} ->
				  case Es of
				      [{c, tuple_set, [{_, Tuples}], _}| _] ->
				    	  SNames = [[S || {c, atom, S, _} <- [T]] || {c, tuple, [T| _], _} <- Tuples],
				    	  lists:flatten(SNames);
				      [{c, tuple, [S, _Tuples], _}|_] ->
					  SNames = [SName || {c, atom, SName, _} <- [S]],
					  lists:flatten(SNames);
				      [] ->
					  [];
				      _ ->
					  throw({error, "Wrangler failed to infer the target state of '"
						 ++ atom_to_list(F) ++ "'."})
				  end;
			      _ ->
				  throw({error, "Wrangler failed to infer the target state of '"
					 ++ atom_to_list(F) ++ "'."})
			  end;
		      false ->
			  []
		  end
	  end,
    Res = lists:append([Fun(T) || T <- TypeInfo]),
    lists:usort(Res).

get_gen_fsm_state_functions(ModName, ModInfo, TypeInfo) ->
    case lists:keysearch({ModName, init, 1}, 1, TypeInfo) of 
	{value, {{ModName, init, 1}, RetType, _ArgsType}} ->
	    ?debug("RetType:\n~p\n", [RetType]),
	    case RetType of 
		{c, tuple ,[_, {c, atom, [StateName], _}|_],_} ->
		    get_gen_fsm_state_functions_1(ModName, ModInfo, TypeInfo,[StateName]);
		_ -> throw({error, "Wrangler failed to infer the initial state name of the fsm."})
	    end;
	false ->
	    throw({error, "gen_fsm callback function init/1 is not defined."})
   end.
    
get_gen_fsm_state_functions_1(ModName, ModInfo, TypeInfo, StateNames) ->
    StateNames1 = get_gen_fsm_state_functions_2(TypeInfo, StateNames),
    case StateNames == StateNames1 of 
	true ->
	    StateFuns =lists:append([[{ModName, S, 2}, {ModName, S, 3}] || S <- StateNames]),
	    case lists:keysearch(functions,1, ModInfo) of
		{value, {functions, Funs}} ->
		    [{M, F, A}||{M, F, A}<-StateFuns, lists:member({F, A}, Funs)];
		_ ->
		    []
	    end;
	false ->
	    get_gen_fsm_state_functions_1(ModName, ModInfo, TypeInfo, 
					  lists:usort(StateNames++StateNames1))
    end.

get_gen_fsm_state_functions_2(TypeInfo, StateNames) ->
    Fun = fun ({{_M, F, A}, RetType, _ArgTypes}) ->
		  case lists:member(A, [2, 3]) andalso lists:member(F, StateNames) of
		    true ->
			  ?debug("RetType0:\n~p\n", [RetType]),
			case RetType of
			  {c, tuple, Es, _} ->
				?debug("RetType1:\n~p\n", [RetType]),
			      case Es of
				[{c, atom, [next_state], _},
				 {c, atom, S, _}| _T] ->
				    S;
				[{c, atom, [reply], _},
				 _,
				 {c, atom, S, _}| _T] ->
				    S;
				_ ->
				      throw({error, "Wrangler failed to infer the next state of '"
						    ++ atom_to_list(F) ++ "'."})
			      end;
			  _ ->
				throw({error, "Wrangler failed to infer the next state of '"
					      ++ atom_to_list(F) ++ "'."})
			end;
		    false ->
			[]
		  end
	  end,
    Res = lists:append([Fun(T) || T <- TypeInfo]),
    lists:usort(Res).


is_tuple_to_is_record(Tree, RecordName, RecordToTupleFunName) ->
    ?debug("\nis_tuple_to_is_record\n", []),
    Fun = fun (Node, _Others) ->
		  case is_app(Node, {erlang, is_tuple, 1}) of
		    true ->
			[Arg] = wrangler_syntax:application_arguments(Node),
			case is_app(Arg, {RecordToTupleFunName, 1}) of
			  true ->
			      [T] = wrangler_syntax:application_arguments(Arg),
			      wrangler_syntax:application(wrangler_syntax:atom(is_record),
						          [T, wrangler_syntax:atom(RecordName)]);
			  _ -> Node
			end;
		    _ -> Node
		  end
	  end,
    api_ast_traverse:full_buTP(Fun, Tree, {}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%%                 Transformation: element to record access                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

element_to_record_access(RecordName, RecordFields, DefPs, Node,RecordToTupleFunName) ->
    [N, T] = wrangler_syntax:application_arguments(Node),
    case {wrangler_syntax:type(N), wrangler_syntax:type(T)} of
	{integer, variable} ->
	    As1 = wrangler_syntax:get_ann(T),
	    case lists:keysearch(def, 1, As1) of
		{value, {def, DefinePos}} ->
		    case DefinePos -- DefPs =/= DefinePos of
			true -> 
			    element_to_record_access_1(T, N, RecordName, RecordFields);
			false ->
			    Node
		    end;
		false ->
		    Node
	    end;
	{integer, application} ->
	    case is_app(T, {RecordToTupleFunName, 1}) of
		true ->
		    [Arg] = wrangler_syntax:application_arguments(T),
		    element_to_record_access_1(Arg, N, RecordName, RecordFields);
		false ->
		    Node
	    end;
	_ -> Node
    end.

element_to_record_access_1(Tuple, Nth, RecordName, RecordFields) ->
    FieldName = lists:nth(wrangler_syntax:integer_value(Nth), RecordFields),
    RecordName1 = wrangler_syntax:atom(RecordName),
    FieldName1 = wrangler_syntax:atom(FieldName),
    wrangler_misc:rewrite(Tuple, wrangler_syntax:record_access(Tuple, RecordName1, FieldName1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%%                 Transformation: setelement to record expression             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setelement_to_record_expr(Node, RecordName, RecordFields, DefPs, IsTuple,
			 TupleToRecordFunName, RecordToTupleFunName) ->
    [Index, T, V] = wrangler_syntax:application_arguments(Node),
    case {wrangler_syntax:type(Index), wrangler_syntax:type(T)} of
      {integer, variable} ->
	  As1 = wrangler_syntax:get_ann(T),
	  case lists:keysearch(def, 1, As1) of
	    {value, {def, DefinePos}} ->
		case DefinePos -- DefPs =/= DefinePos of
		  true ->
		      setelement_to_record_expr_1(T, Index, V, RecordName, RecordFields, IsTuple,
						 TupleToRecordFunName, RecordToTupleFunName);
		  false ->
		      Node
		end;
	    false -> Node
	  end;
      {integer, application} ->
	  case is_app(T, {RecordToTupleFunName, 1}) of
	    true ->
		[Arg] = wrangler_syntax:application_arguments(T),
		setelement_to_record_expr_1(Arg, Index, V, RecordName, RecordFields, IsTuple,
					   TupleToRecordFunName, RecordToTupleFunName);
	    false ->
		Node
	  end;
      _ -> Node
    end.

setelement_to_record_expr_1(Expr, Index, Val, RecordName, RecordFields, IsTuple, 
			   TupleToRecordFunName, RecordToTupleFunName) ->
    FieldName = lists:nth(wrangler_syntax:integer_value(Index), RecordFields),
    Field = mk_record_field(FieldName, Val),
    RecordExpr = mk_record_expr(Expr, RecordName, [Field]),
    make_record_to_tuple_app(RecordExpr, RecordName, RecordFields, IsTuple, 
			     TupleToRecordFunName, RecordToTupleFunName).

remove_record_tuple_conversions(Tree, TupleToRecordFunName, RecordToTupleFunName) ->
    Fun = fun (Node, Acc) ->
		  case wrangler_syntax:type(Node) of
		      match_expr ->
			  P = wrangler_syntax:match_expr_pattern(Node),
			  B = wrangler_syntax:match_expr_body(Node),
			  case {wrangler_syntax:type(P), wrangler_syntax:type(B)} of
			      {variable, application} ->
				  Pos = wrangler_syntax:get_pos(P),
				  case is_app(B, {RecordToTupleFunName, 1}) of
				      true ->
					  case check_sole_use_of_var(Tree, Pos, TupleToRecordFunName) of
					      true ->
						  [{Pos, TupleToRecordFunName}| Acc];
					      false ->
						  Acc
					  end;
				      false ->
					  case is_app(B, {TupleToRecordFunName, 1}) of
					      true ->
						  case check_sole_use_of_var(Tree, Pos, RecordToTupleFunName) of
						      true ->
							  [{Pos, RecordToTupleFunName}| Acc];
						      false ->
							  Acc
						  end;
					      false ->
						  Acc
					  end
				  end;
			      _ -> Acc
			  end;
		      _ -> Acc
		  end
	  end,
    Fun1 = fun (Node, {Pos, FunName}) ->
		   case wrangler_syntax:type(Node) of
		       application ->
			   case is_app(Node, {FunName, 1}) of
			       true ->
				   [T] = wrangler_syntax:application_arguments(Node),
				   case wrangler_syntax:type(T) of
				       variable ->
					   As1 = wrangler_syntax:get_ann(T),
					   case lists:keysearch(def, 1, As1) of
					       {value, {def, [Pos]}} ->
						   T;
					       false ->
						   Node
					   end;
				       _ -> Node
				   end;
			       false -> Node
			   end;
		       match_expr ->
			   P = wrangler_syntax:match_expr_pattern(Node),
			   B = wrangler_syntax:match_expr_body(Node),
			   case {wrangler_syntax:type(P), wrangler_syntax:type(B)} of
			       {variable, application} ->
				   case wrangler_syntax:get_pos(P) of
				       Pos ->
					   [T] = wrangler_syntax:application_arguments(B),
					   wrangler_misc:rewrite(Node, wrangler_syntax:match_expr(P, T));
				       _ -> Node
				   end;
			       _ -> Node
			   end;
		       _ -> Node
		   end
	   end,
    Vs = api_ast_traverse:fold(Fun, [], Tree),
    Fun2 = fun (V, Node) -> api_ast_traverse:full_buTP(Fun1, Node, V) end,
    lists:foldl(Fun2, Tree, Vs).

check_sole_use_of_var(Tree, Pos, FunName) ->
    Fun = fun (Node, {Acc1, Acc2}) ->
		  case wrangler_syntax:type(Node) of
		      variable ->
			  case wrangler_syntax:get_pos(Node) of
			      Pos -> {Acc1, Acc2};
			      _ ->
				  As = wrangler_syntax:get_ann(Node),
				  case lists:keysearch(def, 1, As) of
				      {value, {def, [Pos]}} ->
					  {[wrangler_syntax:get_pos(Node)| Acc1], Acc2};
				      _ ->
					  {Acc1, Acc2}
				  end
			  end;
		      application ->
			  case is_app(Node, {FunName, 1}) of
			      true ->
				  [T] = wrangler_syntax:application_arguments(Node),
				  case wrangler_syntax:type(T) of
				      variable ->
					  As1 = wrangler_syntax:get_ann(T),
					  case lists:keysearch(def, 1, As1) of
					      {value, {def, [Pos]}} ->
						  {Acc1, [wrangler_syntax:get_pos(T)| Acc2]};
					      _ ->
						  {Acc1, Acc2}
					  end;
				      _ ->
					  {Acc1, Acc2}
				  end;
			      false ->
				  {Acc1, Acc2}
			  end;
		      _ ->
			  {Acc1, Acc2}
		  end
	  end,
    {Acc1, Acc2} = api_ast_traverse:fold(Fun, {[],[]}, Tree),
    ?debug("Acc1Acc2:\n~p\n", [{Acc1, Acc2}]),
    Acc1 == Acc2.

%%====================================================================================
%%
%% Some utility functions 
%%
%%====================================================================================



callbacks(ModName, _StateFuns, eqc_statem) ->
    [{{ModName, initial_state, 0}, {[], true}},
     {{ModName, precondition, 2}, {[1], false}},
     {{ModName, command, 1}, {[1], false}},
     {{ModName, postcondition, 3}, {[1], false}},
     {{ModName, next_state, 3}, {[1], true}}];


callbacks(ModName, StateFuns, eqc_fsm) ->
    CallBacks0 = [{{ModName, initial_state_data, 0}, {[], true}},
		  {{ModName, next_state_data, 5}, {[3], true}},
		  {{ModName, precondition, 4}, {[3], false}},
		  {{ModName, postcondition, 5}, {[3], false}}],	
    StateFunCallBacks = [{{M, F, A}, {[A], false}} 
			 || {M, F, A} <- StateFuns],
    CallBacks0 ++ StateFunCallBacks;

callbacks(ModName, StateFuns, gen_fsm) ->
    CallBacks0 = [{{ModName, init, 1}, {[], true}},
		  {{ModName, handle_event, 3}, {[3], true}},
		  {{ModName, handle_sync_event, 4}, {[4], true}},
		  {{ModName, handle_info, 3}, {[3], true}},
		  {{ModName, terminate, 3}, {[3], false}},
		  {{ModName, code_change,4}, {[3], true}},
		  {{ModName, enter_loop, 4}, {[4], false}},
		  {{ModName, enter_loop, 5}, {[4], false}},
		  {{ModName, enter_loop, 6}, {[4], false}}],		  
    StateFunCallBacks = [{{M, F, A}, {[A], true}}
			 || {M, F, A} <- StateFuns],
    CallBacks0 ++ StateFunCallBacks.
	

mk_tuple_to_record_fun(RecordName, RecordFields, FunName) ->
    Pars = [wrangler_syntax:variable("E" ++ integer_to_list(I))
	    || I <- lists:seq(1, length(RecordFields))],
    Fields = mk_record_fields(RecordFields, Pars),
    RecordExpr = mk_record_expr(RecordName, Fields),
    C = wrangler_syntax:clause([wrangler_syntax:tuple(Pars)], none, [RecordExpr]),
    wrangler_syntax:function(wrangler_syntax:atom(FunName), [C]).

mk_record_to_tuple_fun(RecordName, RecordFields, FunName) ->
    Pars = [wrangler_syntax:variable("E" ++ integer_to_list(I))
	    || I <- lists:seq(1, length(RecordFields))],
    Fields = mk_record_fields(RecordFields, Pars),
    RecordExpr = mk_record_expr(RecordName, Fields),
    C = wrangler_syntax:clause([RecordExpr], none, [wrangler_syntax:tuple(Pars)]),
    wrangler_syntax:function(wrangler_syntax:atom(FunName), [C]).

make_tuple_to_record_app(Expr, RecordName, RecordFields, IsTuple, TupleToRecordFunName, RecordToTupleFunName) ->
    case IsTuple of
	false ->
	    Field = mk_record_field(hd(RecordFields), Expr),
	    mk_record_expr(RecordName, [Field]);
	true ->
	    NewExpr = wrangler_misc:rewrite(
			   Expr, wrangler_syntax:application(
				      wrangler_syntax:atom(TupleToRecordFunName), [Expr])),
	    case is_app(Expr, {RecordToTupleFunName, 1}) of
		true ->
		    hd(wrangler_syntax:application_arguments(Expr));
		false ->
		    NewExpr
	    end
    end.

make_record_to_tuple_app(Expr, RecordName, RecordFields, IsTuple, TupleToRecordFunName, RecordToTupleFunName) ->
    case IsTuple of
	false ->
	    wrangler_misc:rewrite(Expr, wrangler_syntax:record_access(Expr, wrangler_syntax:atom(RecordName),
								      wrangler_syntax:atom(hd(RecordFields))));
	true ->
	    NewExpr = wrangler_misc:rewrite(Expr, wrangler_syntax:application(
						       wrangler_syntax:atom(RecordToTupleFunName), [Expr])),
	    case is_app(Expr, {TupleToRecordFunName, 1}) of
		true ->
		    hd(wrangler_syntax:application_arguments(Expr));
		false ->
		    NewExpr
	    end
    end.

mk_record_field(Name, Val) ->
    wrangler_misc:rewrite_with_wrapper(Val, wrangler_misc:reset_ann_and_pos(
                                                wrangler_syntax:record_field(
                                                     wrangler_syntax:atom(Name),
                                                     wrangler_syntax:remove_comments(Val)))).

mk_record_fields(RecordFields, Es) ->
    [mk_record_field(Name, Val)|| {Name, Val} <- lists:zip(RecordFields, Es),
				  wrangler_syntax:type(Val) =/= underscore].

mk_record_expr(RecordName, Fields) ->
    simplify_record_expr(wrangler_syntax:record_expr(wrangler_syntax:atom(RecordName), Fields)).

mk_record_expr(T, RecordName,Fields) ->
    simplify_record_expr(wrangler_syntax:record_expr(T, wrangler_syntax:atom(RecordName), Fields)).

simplify_record_expr(RecordExpr) ->
    RecordArg = wrangler_syntax:record_expr_argument(RecordExpr),
    _RecordName = wrangler_syntax:record_expr_type(RecordExpr),
    RecordFields = wrangler_syntax:record_expr_fields(RecordExpr),
    case RecordArg == none of
	false -> RecordExpr;
	true -> 
	    F = fun(Field) ->
			FName = wrangler_syntax:record_field_name(Field),
			FVal = wrangler_syntax:record_field_value(Field),
			case wrangler_syntax:type(FVal) of
			    record_access ->
				RecAccessArg = wrangler_syntax:record_access_argument(FVal),
				_RecAccessType =wrangler_syntax:record_access_type(FVal),
				RecAccessField = wrangler_syntax:record_access_field(FVal),
				case wrangler_syntax:data(FName) == wrangler_syntax:data(RecAccessField) of
				    true ->
					RecAccessArg;
				    false ->
					throw({error, "simplification failed"})
				end;
			    _ -> 
				throw({error, "simplification failed"})
			end
		end,
	    case catch {ok, [F(R)||R<-RecordFields]} of 
		{ok, Args} ->
		    case length(lists:usort([[erl_prettypr:format(A) || A<-Args]])) of
			1 ->
			    hd(Args);
			_ -> RecordExpr
		    end;
		_ -> 
		    RecordExpr
	    end
    end.


mk_record_attribute(RecordName, RecordFields) ->
    RecordName1 = wrangler_syntax:atom(RecordName),
    RecordFields1 = [wrangler_syntax:record_field(wrangler_syntax:atom(FieldName))
		     || FieldName <- RecordFields],
    wrangler_syntax:attribute(wrangler_syntax:atom(record),
	     [RecordName1, wrangler_syntax:tuple(RecordFields1)]).

insert_record_attribute(Forms, RecordDef) ->
    {Fs1, Fs2} = lists:splitwith(
		   fun (F) ->
			   wrangler_syntax:type(F) == comment
			      orelse is_not_type_attrubute(F)
		   end,
		   Forms),
    {Fs11, Fs12} = lists:splitwith(
		     fun (F) -> 
			     wrangler_syntax:type(F) == comment
		     end,
		     lists:reverse(Fs1)),
    lists:reverse(Fs12) ++ [RecordDef] ++ lists:reverse(Fs11) ++ Fs2.

tuple_to_record_expr(Tuple, RecordName, RecordFields) ->
    Es = wrangler_syntax:tuple_elements(Tuple),
    Fields = mk_record_fields(RecordFields, Es),
    wrangler_misc:rewrite(Tuple, mk_record_expr(RecordName, Fields)).
    
is_app(Expr, {F, A}) ->
    case wrangler_syntax:type(Expr) of
	application ->
	    Op = wrangler_syntax:application_operator(Expr),
	    case wrangler_syntax:type(Op) of
		atom->
		    Args = wrangler_syntax:application_arguments(Expr),
		    {F, A} == {wrangler_syntax:atom_value(Op),length(Args)};
		_ -> false
	    end;
	_ -> false
    end;
is_app(Expr, {M,F,A}) ->
    case wrangler_syntax:type(Expr) of
	application ->
	    As = wrangler_syntax:get_ann(wrangler_syntax:application_operator(Expr)),
	    case lists:keysearch(fun_def,1,As) of
		{value, {fun_def, {M, F, A, _ ,_}}}->
		    true;
		_ ->
		    false
	    end;
	_ -> false
    end.
    
is_not_type_attrubute(F) ->
    case wrangler_syntax:type(F) of
      attribute ->
	  Name = wrangler_syntax:attribute_name(F),
	  case wrangler_syntax:type(Name) of
	    atom ->
		not lists:member(wrangler_syntax:atom_value(Name),
				 ['type', 'spec']);
	    _ ->
		false
	  end;
      _ -> false
    end.

is_used_only_once(Body, DefinePos) ->
    Fun = fun (Node, Acc) ->
		  case wrangler_syntax:type(Node) of
		      variable ->
			  As = wrangler_syntax:get_ann(Node),
			  case lists:keysearch(def, 1, As) of
			      {value, {def, DefinePos}} ->
				  Pos = wrangler_syntax:get_pos(Node),
				  case lists:member(Pos, DefinePos) of
				      true ->
					  Acc;
				      false ->
					  [Node| Acc]
				  end;
			      _ ->
				  Acc
			  end;
		      _ -> Acc
		  end
	  end,
    length(api_ast_traverse:fold(Fun, [], Body)) == 1.

format_state_funs([]) -> "[]";
format_state_funs(MFAs) ->
     "[" ++ format_state_funs_1(MFAs).

format_state_funs_1([]) ->
    "";
format_state_funs_1([{M,F,A}|T]) ->
    case T of 
	[] ->
	   io_lib:format("~p]", [{M, F, A}])++
		format_state_funs_1(T);
	_ ->
	    io_lib:format("~p,", [{M, F, A}])++
		format_state_funs_1(T)
    end.
    
format_field_names([]) -> "[]";
format_field_names(MFAs) ->
     "[" ++ format_field_names_1(MFAs).

format_field_names_1([]) ->
    "";
format_field_names_1([F|T]) ->
    case T of 
	[] ->
	   io_lib:format("~p]", [F])++
		format_field_names_1(T);
	_ ->
	    io_lib:format("~p,", [F])++
		format_field_names_1(T)
    end.
    

lookup_record(Tag, Arity, RecDict) when is_atom(Tag) ->
    case dict:find({record, Tag}, RecDict) of
        {ok, [{Arity, Fields}]} -> {ok, Fields};
        {ok, OrdDict} -> orddict:find(Arity, OrdDict);
        error -> error
    end.


set_pos(Pos, Node) ->
    api_ast_traverse:full_buTP(fun (T, _Others) -> wrangler_syntax:set_pos(T, Pos)
			       end, Node, {}).
