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
%% Refactoring: Introduce a ?LET.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
%% =============================================================================================

%% =============================================================================================
-module(refac_state_to_record).

-export([eqc_statem_to_record/3, eqc_statem_to_record_1/7,
	 eqc_statem_to_record_eclipse/3, eqc_statem_to_record_1_eclipse/7,
	 eqc_fsm_to_record/3,  eqc_fsm_to_record_1/7,
	 eqc_fsm_to_record_eclipse/3, eqc_fsm_to_record_1_eclipse/7,
	 gen_fsm_to_record/3,  gen_fsm_to_record_1/7,
	 gen_fsm_to_record_eclipse/3, gen_fsm_to_record_1_eclipse/7]).

-include("../include/wrangler.hrl").

-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.


-define(Msg, "Wrangler failed to infer the current data type of the state.").

%% =============================================================================================
-spec(eqc_statem_to_record/3::(filename(),[dir()], integer()) -> {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
								 {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).

eqc_statem_to_record(FileName, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:eqc_statem_to_record(~p,~p, ~p).\n",
		 [?MODULE, FileName,SearchPaths, TabWidth]),
    eqc_statem_to_record(FileName, SearchPaths, TabWidth, emacs).

eqc_statem_to_record_eclipse(FileName, SearchPaths, TabWidth) ->
    eqc_statem_to_record(FileName,SearchPaths, TabWidth, eclipse).


eqc_statem_to_record(FileName, SearchPaths, TabWidth, _Editor) ->
    state_to_record(FileName, SearchPaths, TabWidth, eqc_statem).


eqc_statem_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:eqc_statem_to_record_1(~p,~p,~p,",
		 [?MODULE, FileName, RecordName, RecordFields]),
    ?wrangler_io(format_state_funs(StateFuns),[]),
    ?wrangler_io(",~p,~p, ~p).\n", [IsTuple, SearchPaths, TabWidth]),
    eqc_statem_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, emacs, "").

eqc_statem_to_record_1_eclipse(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    eqc_statem_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, eclipse, "").

eqc_statem_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, Editor, Cmd) ->
    state_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, 
		      eqc_statem, SearchPaths, TabWidth, Editor, Cmd).


%% =============================================================================================
-spec(eqc_fsm_to_record/3::(filename(),[dir()], integer()) -> {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
		                                              {'ok', {tuple, integer()}, [{atom(), atom(),integer()}]}).
eqc_fsm_to_record(FileName, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:eqc_fsm_to_record(~p,~p, ~p).\n",
		 [?MODULE, FileName,SearchPaths, TabWidth]),
    eqc_fsm_to_record(FileName, SearchPaths, TabWidth, emacs).

eqc_fsm_to_record_eclipse(FileName, SearchPaths, TabWidth) ->
    eqc_fsm_to_record(FileName,SearchPaths, TabWidth, eclipse).


eqc_fsm_to_record(FileName, SearchPaths, TabWidth, _Editor) ->
    state_to_record(FileName, SearchPaths, TabWidth, eqc_fsm).

eqc_fsm_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:eqc_fsm_to_record_1(~p,~p,~p,",
		 [?MODULE, FileName, RecordName, RecordFields]),
    ?wrangler_io(format_state_funs(StateFuns),[]),
    ?wrangler_io(",~p,~p, ~p).\n", [IsTuple, SearchPaths, TabWidth]),
    eqc_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, 
		      IsTuple, SearchPaths, TabWidth, emacs, "").

eqc_fsm_to_record_1_eclipse(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    eqc_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, eclipse, "").

eqc_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, Editor, Cmd) ->
    state_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, 
		      eqc_fsm, SearchPaths, TabWidth, Editor, Cmd).



%% =============================================================================================
-spec(gen_fsm_to_record/3::(filename(),[dir()], integer()) -> {'ok', non_tuple, [{atom(), atom(), integer()}]} | 
							      {'ok', {tuple, integer()}, [{atom(), atom(), integer()}]}).

gen_fsm_to_record(FileName, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:gen_fsm_to_record(~p,~p, ~p).\n",
		 [?MODULE, FileName,SearchPaths, TabWidth]),
    gen_fsm_to_record(FileName, SearchPaths, TabWidth, emacs).

gen_fsm_to_record_eclipse(FileName, SearchPaths, TabWidth) ->
    gen_fsm_to_record(FileName,SearchPaths, TabWidth, eclipse).


gen_fsm_to_record(FileName, SearchPaths, TabWidth, _Editor) ->
    state_to_record(FileName, SearchPaths, TabWidth, gen_fsm).


gen_fsm_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
     ?wrangler_io("\nCMD: ~p:gen_fsm_to_record_1(~p,~p,~p,",
		 [?MODULE, FileName, RecordName, RecordFields]),
    ?wrangler_io(format_state_funs(StateFuns),[]),
    ?wrangler_io(",~p,~p, ~p).\n", [IsTuple, SearchPaths, TabWidth]),
    gen_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, emacs, "").

gen_fsm_to_record_1_eclipse(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth) ->
    gen_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, eclipse, "").

gen_fsm_to_record(FileName, RecordName, RecordFields, StateFuns, IsTuple, SearchPaths, TabWidth, Editor, Cmd) ->
    state_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, 
		      gen_fsm, SearchPaths, TabWidth, Editor, Cmd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

state_to_record(FileName, SearchPaths, TabWidth, SM) ->
    {ok, {_, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    case is_statemachine_used(Info, SM) of
	true ->
	    check_current_state_type(FileName, ModName, SM);
	false -> throw({error, atom_to_list(SM)++ " is not used by this module."})
    end.


state_to_record_1(FileName, RecordName, RecordFields, StateFuns, IsTuple, SM, SearchPaths, TabWidth, Editor, Cmd) ->
    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file(FileName, true, SearchPaths, TabWidth),
    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
    RecordExists = pre_cond_check(RecordName, RecordFields, Info),
    RecordFields1 = [list_to_atom(F) || F <- RecordFields],
    RecordName1 = list_to_atom(RecordName),
    AnnAST1 = do_state_to_record(ModName, AnnAST, RecordName1, RecordFields1, 
				 StateFuns,RecordExists, IsTuple, SM),
    case Editor of
	emacs ->
	    refac_util:write_refactored_files_for_preview([{{FileName, FileName}, AnnAST1}], Cmd),
	    {ok, [FileName]};
	eclipse ->
	    Content = refac_prettypr:print_ast(refac_util:file_format(FileName), AnnAST1),
	    {ok, [{FileName, FileName, Content}]}
    end.


do_state_to_record(ModName, AnnAST, RecordName, RecordFields, StateFuns, RecordExists, IsTuple, SM) ->
    Forms = refac_syntax:form_list_elements(AnnAST),
    Fun = fun(F) ->
		  case refac_syntax:type(F) of
		      function ->
			  do_state_to_record_1(ModName, F, RecordName, RecordFields,
					       StateFuns, IsTuple, SM);
		      _ -> F
		  end
	  end,
    Forms1 = [Fun(F)|| F <- Forms],
    Forms2 = add_util_funs(Forms1, RecordName, RecordFields),
    case RecordExists of
	true ->
	    refac_syntax:form_list(Forms2);
	false ->
	    RecordName1 = refac_syntax:atom(RecordName),
	    RecordFields1 = [refac_syntax:record_field(refac_syntax:atom(FieldName))
			     || FieldName <- RecordFields],
	    RecordDef = refac_syntax:attribute(
			  refac_syntax:atom(record),
			  [RecordName1, refac_syntax:tuple(RecordFields1)]),
	    {Fs1, Fs2} = lists:splitwith(
			   fun (F) -> refac_syntax:type(F) == comment orelse is_not_type_attrubute(F)
			   end, Forms2),
	    {Fs11, Fs12} = lists:splitwith(
			     fun (F) -> refac_syntax:type(F) == comment end, lists:reverse(Fs1)),
	    refac_syntax:form_list(lists:reverse(Fs12) ++ [RecordDef] ++ lists:reverse(Fs11) ++ Fs2)
    end.

do_state_to_record_1(ModName, Fun, RecordName, RecordFields, StateFuns, IsTuple, SM) ->
    As = refac_syntax:get_ann(Fun),
    {value, {fun_def, {M, F, A, _, _}}} = lists:keysearch(fun_def, 1, As),
    CallBacks =  callbacks(ModName, StateFuns, SM),
    NewFun = case lists:keysearch({M, F, A}, 1, CallBacks) of
		 {value, {{M, F, A}, {PatIndex, ReturnState}}} ->
		     do_state_to_record_in_callback_fun(
		       PatIndex, Fun, RecordName, RecordFields, IsTuple, ReturnState, SM);
		 false -> Fun
	     end,
    wrap_fun_interface(NewFun, ModName, RecordName, RecordFields, IsTuple, StateFuns, SM).
	  
		 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 

do_state_to_record_in_callback_fun(0, Fun, RecordName, RecordFields, IsTuple, ReturnState, SM)->
    case ReturnState of 
	true ->
	    do_state_to_record_in_init_fun(Fun, RecordName, RecordFields, IsTuple, SM);
	false ->
	    Fun
    end;
do_state_to_record_in_callback_fun(PatIndex, Fun, RecordName, RecordFields, IsTuple, ReturnState, SM) ->
    FunName = refac_syntax:function_name(Fun),
    Cs = refac_syntax:function_clauses(Fun),
    Cs1 = [do_state_to_record_in_callback_clause(PatIndex, C, RecordName, RecordFields, 
						 IsTuple, ReturnState, SM) 
	   || C <- Cs],
    refac_util:rewrite(Fun, refac_syntax:function(FunName, Cs1)).


do_state_to_record_in_callback_clause(PatIndex, C, RecordName, RecordFields, IsTuple, ReturnState, SM) ->
    P = list_to_tuple(refac_syntax:clause_patterns(C)),
    G = refac_syntax:clause_guard(C),
    B = refac_syntax:clause_body(C),
    State = element(PatIndex, P),
    case refac_syntax:type(State) of
      match_expr ->
	  {State1, DefPs} = tuple_to_record_in_match_expr_pattern(
			      State, RecordName, RecordFields, IsTuple),
	  P1 = tuple_to_list(setelement(PatIndex, P, State1)),
	  B1 = do_state_to_record_in_callback_fun_clause_body(
		 PatIndex, B, RecordName, RecordFields, DefPs, IsTuple, ReturnState, SM),
	  refac_util:rewrite(C, refac_syntax:clause(P1, G, B1));
      tuple when IsTuple ->
	  RecordExpr = tuple_to_record_expr(State, RecordName, RecordFields),
	  State1 = refac_util:rewrite(State, RecordExpr),
	  P1 = tuple_to_list(setelement(PatIndex, P, State1)),
	  B1 = do_state_to_record_in_callback_fun_clause_body(
		 PatIndex, B, RecordName, RecordFields, [], IsTuple, ReturnState, SM),
	  refac_util:rewrite(C, refac_syntax:clause(P1, G, B1));
      variable ->
	  Pos = refac_syntax:get_pos(State),
	  B1 = do_state_to_record_in_callback_fun_clause_body(
		 PatIndex, B, RecordName, RecordFields, [Pos], IsTuple, ReturnState, SM),
	  refac_util:rewrite(C, refac_syntax:clause(tuple_to_list(P), G, B1));
      underscore ->
	  C;
      _ when IsTuple ->
	  Pos = refac_syntax:get_pos(State),
	  throw({error, "Wrangler did not know how to transform the pattern at location: " 
		 ++ io_lib:format("~p", [Pos])});
      _ when length(RecordFields) == 1 ->
	  Fields = [mk_record_field(hd(RecordFields), State)],
	  State1 = refac_util:rewrite(State, mk_record_expr(RecordName, Fields)),
	  P1 = tuple_to_list(setelement(PatIndex, P, State1)),
	  B1 = do_state_to_record_in_callback_fun_clause_body(
		 PatIndex, B, RecordName, RecordFields, [], IsTuple, ReturnState, SM),
	  refac_util:rewrite(C, refac_syntax:clause(P1, G, B1));
      _ ->
	  Pos = refac_syntax:get_pos(State),
	  throw({error, "Wrangler did not know how to transform the pattern at location: " 
		 ++ io_lib:format("~p", [Pos])})
    end.

tuple_to_record_in_match_expr_pattern(State, RecordName, RecordFields, IsTuple)->	   
    P=refac_syntax:match_expr_pattern(State),
    B = refac_syntax:match_expr_body(State),
    {P1, Pos1} =
	tuple_to_record_in_match_expr_pattern_1(P, RecordName, RecordFields, IsTuple),
    {B1, Pos2} = 
	tuple_to_record_in_match_expr_pattern_1(B, RecordName, RecordFields, IsTuple),
    {refac_util:rewrite(State, refac_syntax:match_expr(P1,B1)), Pos1++Pos2}.

tuple_to_record_in_match_expr_pattern_1(P, RecordName, RecordFields, IsTuple) ->
    case refac_syntax:type(P) of
      variable ->
	  {P, [refac_syntax:get_pos(P)]};
      tuple when IsTuple ->
	  RecordExpr = tuple_to_record_expr(P, RecordName, RecordFields),
	  RecP = refac_util:rewrite(P, RecordExpr),
	  {RecP, []};
      match_expr ->
	  tuple_to_record_in_match_expr_pattern(P, RecordName, RecordFields, IsTuple);
      _ ->
	  case IsTuple of
	    true ->
		Pos = refac_syntax:get_pos(P),
		throw({error, "Wrangler did not know how to transform the pattern at location: " ++
				io_lib:format("~p", [Pos])});
	    false ->
		Fields = [mk_record_field(hd(RecordFields), P)],
		RecP = refac_util:rewrite(P, mk_record_expr(RecordName, Fields)),
		{RecP, []}
	  end
    end.
do_state_to_record_in_callback_fun_clause_body(_PatIndex, Body, RecordName, RecordFields, 
					       DefPs, IsTuple, ReturnState, SM) ->
    refac_io:format("RecordFields:\n~p\n", [RecordFields]),
    Fun = fun (Node, _Others) ->
		  case refac_syntax:type(Node) of
		      application when IsTuple ->
			  As = refac_syntax:get_ann(refac_syntax:application_operator(Node)),
			  case lists:keysearch(fun_def, 1, As) of
			      {value, {fun_def, {erlang, element, 2, _, _}}} ->
				  element_to_record_access(RecordName, RecordFields, DefPs, Node);
			      {value, {fun_def, {erlang, setelement, 3, _, _}}} ->
				  setelement_to_record_expr(Node, RecordName, RecordFields, DefPs, IsTuple);			  
			      _ -> Node
			  end;
		      variable ->
			  As = refac_syntax:get_ann(Node),
			  case lists:keysearch(def, 1, As) of
			      {value, {def, DefinePos}} ->
				  case DefinePos -- DefPs =/= DefinePos of
				      true ->
					  make_record_to_tuple_app(Node, RecordName, RecordFields, IsTuple);
				      false ->
					  Node
				  end;
			      false -> Node
			  end;
		      _ -> Node
		  end
	  end,
    Body1 = refac_util:full_buTP(Fun, refac_syntax:block_expr(Body), {}),
    Body2 = is_tuple_to_is_record(Body1, RecordName),
    Body3 = refac_syntax:block_expr_body(Body2),
    case ReturnState of 
	true ->
 	    do_state_to_record_in_init_fun_clause_body(
	      Body3, RecordName, RecordFields, DefPs, IsTuple, SM);
	false ->
	    Body3
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
do_state_to_record_in_init_fun(Fun, RecordName, RecordFields, IsTuple, SM) ->
    FunName = refac_syntax:function_name(Fun),
    [C] = refac_syntax:function_clauses(Fun),
    C1=do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM),
    refac_util:rewrite(Fun, refac_syntax:function(FunName, [C1])).

do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM) ->
    P = refac_syntax:clause_patterns(C),
    G = refac_syntax:clause_guard(C),
    B = refac_syntax:clause_body(C),
    B1=do_state_to_record_in_init_fun_clause_body(B, RecordName, RecordFields,[], IsTuple, SM),
    refac_util:rewrite(C, refac_syntax:clause(P, G, B1)).

do_state_to_record_in_init_fun_clause_body(Body, RecordName, RecordFields, DefPs, IsTuple, SM)
    when SM == gen_fsm ->
    Msg = "Wrangler did not know how to transform the expression at location: ",
    [LastExpr| Exprs] = lists:reverse(Body),
    Pos = refac_syntax:get_pos(LastExpr),
    case refac_syntax:type(LastExpr) of
      atom -> Body;
      tuple ->
	    Es = refac_syntax:tuple_elements(LastExpr), 
	    Size = length(Es),
	    case Size =< 2  of
		true ->
		    Body;
		false ->
		    Tag = hd(refac_syntax:tuple_elements(LastExpr)),
		    case refac_syntax:type(Tag) of
			atom ->
			    case refac_syntax:atom_value(Tag) of
				ok -> 
				    gen_fsm_state_to_record(RecordName, RecordFields, LastExpr, 3, IsTuple);
				next_state ->
				    gen_fsm_state_to_record(RecordName, RecordFields, LastExpr, 3, IsTuple);
				reply -> 
					gen_fsm_state_to_record(RecordName, RecordFields, LastExpr, 4, IsTuple);
				stop ->
				    gen_fsm_state_to_record(RecordName, RecordFields, LastExpr, Size, IsTuple);
				_ ->
				    throw({error, Msg ++ io_lib:format("~p", [Pos])})
			    end;
			_ ->
			    throw({error, Msg ++ io_lib:format("~p", [Pos])})
		    end
	    end;
      variable ->
	  As = refac_syntax:get_ann(LastExpr),
	  case lists:keysearch(def, 1, As) of
	    {value, {def, DefinePos}} ->
		case is_used_only_once(refac_syntax:block_expr(Body), DefinePos) of
		  true ->
		      {Body1, Modified} =
			  do_state_to_record_in_match_expr(
			    refac_syntax:block_expr(Body), LastExpr, DefinePos, RecordName, RecordFields, IsTuple, SM),
		      case Modified of
			true ->
			    refac_syntax:block_expr_body(Body1);
			false ->
			    throw({error, Msg ++ io_lib:format("~p", [Pos])})
		      end;
		  false ->
		      throw({error, Msg ++ io_lib:format("~p", [Pos])})
		end;
	    _ ->
		throw({error, Msg ++ io_lib:format("~p", [Pos])})
	  end;
      case_expr ->
	  Args = refac_syntax:case_expr_argument(LastExpr),
	  Cs = refac_syntax:case_expr_clauses(LastExpr),
	  Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM) || C <- Cs],
	  LastExpr1 = refac_util:rewrite(LastExpr, refac_syntax:case_expr(Args, Cs1)),
	  lists:reverse([LastExpr1| Exprs]);
      if_expr ->
	  Cs = refac_syntax:if_expr_clauses(LastExpr),
	  Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM) || C <- Cs],
	  LastExpr1 = refac_util:rewrite(LastExpr, refac_syntax:if_expr(Cs1)),
	  lists:reverse([LastExpr1| Exprs]);
      cond_expr ->
	  Cs = refac_syntax:cond_expr_clauses(LastExpr),
	  Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM) || C <- Cs],
	  LastExpr1 = refac_util:rewrite(LastExpr, refac_syntax:cond_expr(Cs1)),
	  lists:reverse([LastExpr1| Exprs]);
      block_expr ->
	  B = refac_syntax:block_expr_body(LastExpr),
	  B1 = do_state_to_record_in_init_fun_clause_body(B, RecordName, RecordFields, DefPs, IsTuple, SM),
	  LastExpr1 = refac_util:rewrite(LastExpr, refac_syntax:block_expr(B1)),
	  lists:reverse([LastExpr1| Exprs]);
      receive_expr ->
	  Cs = refac_syntax:receive_expr_clauses(LastExpr),
	  T = refac_syntax:receive_expr_timeout(LastExpr),
	  A = refac_syntax:receive_expr_action(LastExpr),
	  Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM) || C <- Cs],
	  LastExpr1 = refac_util:rewrite(LastExpr, refac_syntax:receive_expr(Cs1, T, A)),
	  lists:reverse([LastExpr1| Exprs]);
      _ ->
	  throw({error, Msg ++ io_lib:format("~p", [Pos])})
    end;

do_state_to_record_in_init_fun_clause_body(Body, RecordName, RecordFields, DefPs, IsTuple, SM)
    when SM == eqc_statem orelse SM == eqc_fsm ->
    [LastExpr| Exprs] = lists:reverse(Body),
    case refac_syntax:type(LastExpr) of
      tuple when IsTuple ->
	  RecordExpr = tuple_to_record_expr(LastExpr, RecordName, RecordFields),
	  LastExpr1 = refac_util:rewrite(LastExpr, RecordExpr),
	  lists:reverse([LastExpr1| Exprs]);
      variable when IsTuple ->
	  As = refac_syntax:get_ann(LastExpr),
	  case lists:keysearch(def, 1, As) of
	    {value, {def, DefinePos}} ->
		case is_used_only_once(refac_syntax:block_expr(Body), DefinePos) of
		  true ->
		      {Body1, Modified} =
			  do_state_to_record_in_match_expr(
			    refac_syntax:block_expr(Body), LastExpr, DefinePos, RecordName, RecordFields, IsTuple, SM),
		      case Modified of
			true ->
			    refac_syntax:block_expr_body(Body1);
			false ->
			    LastExpr1 = make_tuple_to_record_app(LastExpr, RecordName, RecordFields, IsTuple),
			    lists:reverse([LastExpr1| Exprs])
		      end;
		  false ->
		      LastExpr1 = make_tuple_to_record_app(LastExpr, RecordName, RecordFields, IsTuple),
		      lists:reverse([LastExpr1| Exprs])
		end;
	    _ ->
		LastExpr1 = make_tuple_to_record_app(LastExpr, RecordName, RecordFields, IsTuple),
		lists:reverse([LastExpr1| Exprs])
	  end;
      case_expr ->
	  Args = refac_syntax:case_expr_argument(LastExpr),
	  Cs = refac_syntax:case_expr_clauses(LastExpr),
	  Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM) || C <- Cs],
	  LastExpr1 = refac_util:rewrite(LastExpr, refac_syntax:case_expr(Args, Cs1)),
	  lists:reverse([LastExpr1| Exprs]);
      if_expr ->
	  Cs = refac_syntax:if_expr_clauses(LastExpr),
	  Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM) || C <- Cs],
	  LastExpr1 = refac_util:rewrite(LastExpr, refac_syntax:if_expr(Cs1)),
	  lists:reverse([LastExpr1| Exprs]);
      cond_expr ->
	  Cs = refac_syntax:cond_expr_clauses(LastExpr),
	  Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM) || C <- Cs],
	  LastExpr1 = refac_util:rewrite(LastExpr, refac_syntax:cond_expr(Cs1)),
	  lists:reverse([LastExpr1| Exprs]);
      block_expr ->
	  B = refac_syntax:block_expr_body(LastExpr),
	  B1 = do_state_to_record_in_init_fun_clause_body(B, RecordName, RecordFields, DefPs, IsTuple, SM),
	  LastExpr1 = refac_util:rewrite(LastExpr, refac_syntax:block_expr(B1)),
	  lists:reverse([LastExpr1| Exprs]);
      receive_expr ->
	  Cs = refac_syntax:receive_expr_clauses(LastExpr),
	  T = refac_syntax:receive_expr_timeout(LastExpr),
	  A = refac_syntax:receive_expr_action(LastExpr),
	  Cs1 = [do_state_to_record_in_init_fun_clause(C, RecordName, RecordFields, IsTuple, SM) || C <- Cs],
	  LastExpr1 = refac_util:rewrite(LastExpr, refac_syntax:receive_expr(Cs1, T, A)),
	  lists:reverse([LastExpr1| Exprs]);
      _ when IsTuple ->
	  LastExpr1 = make_tuple_to_record_app(LastExpr, RecordName, RecordFields, IsTuple),
	  lists:reverse([LastExpr1| Exprs]);
      _ ->
	  Fields = [mk_record_field(hd(RecordFields), refac_util:reset_attrs(LastExpr))],
	  LastExpr1 = refac_util:rewrite(LastExpr, mk_record_expr(RecordName, Fields)),
	  lists:reverse([LastExpr1| Exprs])
    end.

do_state_to_record_in_match_expr(Body, _LastExpr, DefinePos, RecordName, RecordFields, _IsTuple, SM)
    when SM == eqc_statem orelse SM == eqc_fsm ->
    Fun = fun (Node, _Others) ->
		  case refac_syntax:type(Node) of
		    match_expr ->
			P = refac_syntax:match_expr_pattern(Node),
			B = refac_syntax:match_expr_body(Node),
			case refac_syntax:type(P) of
			  variable ->
			      Pos = refac_syntax:get_pos(P),
			      case lists:member(Pos, DefinePos) of
				true ->
				    case refac_syntax:type(B) of
				      tuple ->
					  B1 = tuple_to_record_expr(B, RecordName, RecordFields),
					  Node1 = refac_syntax:match_expr(P, refac_util:rewrite(B, B1)),
					  {refac_util:rewrite(Node, Node1), true};
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
    refac_util:stop_tdTP(Fun, Body, {});
do_state_to_record_in_match_expr(Body, LastExpr, DefinePos, RecordName, RecordFields, IsTuple,SM)
    when SM == gen_fsm ->
    Msg = "Wrangler did not know how to transform the expression at location: ",
    Pos = refac_syntax:get_pos(LastExpr),
    Fun1 = fun (B) ->
		   case refac_syntax:type(B) of
		       tuple ->
			   Es = refac_syntax:tuple_elements(B),
			   Size = length(Es),
			   case Size =< 2  of 
			       true ->{B, true};
			       false ->
				   Tag = hd(Es),
				   case refac_syntax:type(Tag) of
				       atom ->
					   case refac_syntax:atom_value(Tag) of
					       ok -> 
						   gen_fsm_state_to_record(RecordName, RecordFields, B, 3, IsTuple);
					       next_state -> 
						   gen_fsm_state_to_record(RecordName, RecordFields, B, 3, IsTuple);
					       reply ->
						   gen_fsm_state_to_record(RecordName, RecordFields, B, 4, IsTuple);
					       stop ->
						   gen_fsm_state_to_record(RecordName, RecordFields, B, Size, IsTuple);
					       _ ->
						   throw({error, Msg ++ io_lib:format("~p", [Pos])})
					   end;
				       _ -> throw({error, Msg ++ io_lib:format("~p", [Pos])})
				   end
			   end;
		     atom ->
			 {B, true};
		     _ ->
			 throw({error, Msg ++ io_lib:format("~p", [Pos])})
		   end
	   end,
    Fun = fun (Node, _Others) ->
		  case refac_syntax:type(Node) of
		    match_expr ->
			P = refac_syntax:match_expr_pattern(Node),
			B = refac_syntax:match_expr_body(Node),
			case refac_syntax:type(P) of
			  variable ->
			      Pos = refac_syntax:get_pos(P),
			      case lists:member(Pos, DefinePos) of
				true ->
				      {B1, Modified} = Fun1(B),
				      case Modified of
					true ->
					    Node1 = refac_syntax:match_expr(P, B1),
					    {refac_util:rewrite(Node, Node1), true};
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
    refac_util:stop_tdTP(Fun, Body, {}).

gen_fsm_state_to_record(RecordName, RecordFields, B, Nth, IsTuple) ->
    State = element(Nth, B),
    case refac_syntax:type(State) of
	tuple when IsTuple ->
	    State1 =tuple_to_record_expr(State, RecordName, RecordFields),
	    setelement(Nth, B, State1);
	_ -> 
	    State1 = make_tuple_to_record_app(State, RecordName, RecordFields, IsTuple),
	    setelement(Nth, B, State1)
    end.


pre_cond_check(RecordName, FieldNames, ModInfo) ->
    check_record_and_field_names(RecordName, FieldNames),
    FieldNames1 = [list_to_atom(F) || F <-FieldNames],
    check_existing_records(list_to_atom(RecordName),  FieldNames1, ModInfo).


check_record_and_field_names(RecordName, FieldNames) ->
    case refac_util:is_fun_name(RecordName) of
      true ->
	  ok;
      _ -> throw({error, "Invalid record name."})
    end,
    lists:foreach(
      fun (F) ->
	      case refac_util:is_fun_name(F) of
		true ->
		    ok;
		_ -> throw({error, "Invalid field name: " ++ F ++ "."})
	      end
      end, FieldNames),
    case length(lists:usort(FieldNames))=/= length(FieldNames) of
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
			    throw({error, "Record with the same name, but different number of fields, "
				   "is already in scope."});
			false ->
			    FieldNames1 = [F || {F, _V} <- Fields],
			    case lists:usort(FieldNames)== lists:usort(FieldNames1) of 
				true ->
				    true;
				false ->
				    throw({error, "Record with the same name, but different field names, "
					   "is already in scope."})
			    end
		    end;
		false ->
		    false
	    end;
	false ->
	    false
    end.
    
wrap_fun_interface(Form, ModName, RecordName, RecordFields, IsTuple, StateFuns,SM) ->
    Form1 = wrap_fun_interface_in_arg(Form, ModName, RecordName, RecordFields, IsTuple, StateFuns,SM),
    wrap_fun_interface_in_return(Form1, ModName, RecordName, RecordFields, IsTuple, StateFuns, SM).

wrap_fun_interface_in_return(Form, ModName, RecordName, RecordFields,IsTuple, StateFuns, SM) ->
    Fun= fun(Node, _Others) ->
		 case is_callback_fun_app(Node, ModName, StateFuns, SM) of
		     {true, _PatIndex, true} ->
			 case SM of 
			     gen_fsm ->
				 throw({error, "Callback functions are called as normal functions."});
			     _ ->
				 make_record_to_tuple_app(Node, RecordName, RecordFields, IsTuple)
			 end;
		     false ->
			 Node
		 end
	 end,
    refac_util:full_buTP(Fun, Form, {}).

wrap_fun_interface_in_arg(Form, ModName, RecordName, RecordFields, IsTuple, StateFuns, SM) ->
    Fun= fun(Node, _Others) ->
		 case is_callback_fun_app(Node, ModName, StateFuns,SM) of
		     {true, PatIndex, _} ->
			 do_transform_actual_pars(Node, PatIndex, RecordName, RecordFields, IsTuple);
		     false ->
			 Node
		 end							
	 end,
    refac_util:full_buTP(Fun, Form, {}).

do_transform_actual_pars(Node, PatIndex, RecordName, RecordFields, IsTuple) ->
    Op = refac_syntax:application_operator(Node),
    Args = refac_syntax:application_arguments(Node),
    Args1 = list_to_tuple(Args),
    Arg = element(PatIndex, Args1),
    case refac_syntax:type(Arg) of
	tuple when IsTuple->
	    R=tuple_to_record_expr(Arg, RecordName, RecordFields),
	    NewArgs = tuple_to_list(setelement(PatIndex, Args1, R)),
	    Node1 = refac_syntax:application(Op, NewArgs),
	    refac_util:rewrite(Node, Node1);
	_ ->
	    R1 = make_tuple_to_record_app(Arg, RecordName, RecordFields, IsTuple),
	    NewArgs = tuple_to_list(setelement(PatIndex, Args1, R1)),
	    Node1 = refac_syntax:application(Op, NewArgs),
	    refac_util:rewrite(Node, Node1)
    end.
	
	    

is_callback_fun_app(Node, ModName, StateFuns, SM) ->
    case refac_syntax:type(Node) of
	application ->
	    Op = refac_syntax:application_operator(Node),
	    case refac_syntax:type(Op) of 
		atom ->
		    FunName = refac_syntax:atom_value(Op),
		    Args = refac_syntax:application_arguments(Node),
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

add_util_funs(Forms, RecordName, RecordFields) ->
    TupleToRecordFunName = list_to_atom("tuple_to_" ++ atom_to_list(RecordName)),
    RecordToTupleFunName = list_to_atom(atom_to_list(RecordName) ++ "_to_tuple"),
    Fun = fun (Node, Acc) ->
		  case refac_syntax:type(Node) of
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
    Acc = lists:append([refac_syntax_lib:fold(Fun, [], F) || F <- Forms]),
    case lists:member({RecordToTupleFunName, 1}, Acc) of
      true ->
	  F1 = mk_record_to_tuple_fun(RecordName, RecordFields),
	  case lists:member({TupleToRecordFunName, 1}, Acc) of
	    true ->
		F2 = mk_tuple_to_record_fun(RecordName, RecordFields),
		Forms ++ [F1, F2];
	    false ->
		Forms ++ [F1]
	  end;
      false ->
	  case lists:member({TupleToRecordFunName, 1}, Acc) of
	    true ->
		F2 = mk_tuple_to_record_fun(RecordName, RecordFields),
		Forms ++ [F2];
	    false ->
		Forms
	  end
    end.
%% ======================================================================

is_statemachine_used(ModuleInfo, gen_fsm) ->
    case lists:keysearch(attributes, 1, ModuleInfo) of 
	{value, {attributes, Attrs}} ->
	    lists:member({behaviour, gen_fsm}, Attrs);
	false ->
	    false
    end;
is_statemachine_used(ModuleInfo, SM) 
  when SM==eqc_statem orelse SM == eqc_fsm ->
    case lists:keysearch(imports, 1, ModuleInfo) of
      {value, {imports, Imps}} ->
	  Ms = [M || {M, _} <- Imps],
	  lists:member(SM, Ms);
      false ->
	  false
    end;
is_statemachine_used(_ModuleInfo, _SM) ->
    false.

%%=====================================================================


check_current_state_type(File, ModName, SM) ->
    TypeInfo = try refac_type_info:get_type_info_using_typer(File) of
		   V -> V
	       catch
		   _E1:_E2 ->
		       throw({error, ?Msg})
	       end,
    TypeInfo1 =[{F, Type} || {F, Type} <-TypeInfo, F == File],
    Type = case TypeInfo1 of
	       [] ->
		   throw({error, ?Msg});
	       [{File, T}] ->
		   T
	   end,
    {Ts, StateFuns} =get_current_state_type_1(Type, ModName, SM),
    refac_io:format("Current Type:\n~p\n", [Ts]),
    Ts1 = lists:usort([case Tag of 
			   tuple -> {Tag, length(Es)};   %% Question: what is the different between tuple and tuple_set?
			   tuple_set ->{tuple, length(Es)};
			   _ -> Tag
		       end 
		       ||{c,Tag, Es, _} <-lists:usort(Ts) --[any],
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
    
 
get_current_state_type_1(TypeInfo, ModName, eqc_statem) ->    %% 0 in the [0] means the return type.
    Pars = [{{ModName, initial_state, 0}, [0]}, {{ModName, precondition, 2}, [1]},
	    {{ModName, command, 1}, [1]}, {{ModName, postcondition, 3}, [1]},
	    {{ModName, next_state, 3}, [0, 1]}],
    Fun = fun ({{M, F, A}, RetType, ArgTypes}) ->
		  case lists:keysearch({M, F, A}, 1, Pars) of
		    {value, {{M, F, A}, Is}} ->
			[case I of
			   0 -> RetType;
			   _ -> lists:nth(I, ArgTypes)
			 end || I <- Is];
		    false ->
			[]
		  end
	  end,
    {lists:append([Fun(T) || T <- TypeInfo]), []};

get_current_state_type_1(TypeInfo, ModName, eqc_fsm) ->
    Pars0 = [{{ModName, initial_state_data, 0}, [0]}, {{ModName, next_state_data, 5}, [0, 3]},
	     {{ModName, postcondition, 5}, [3]}, {{ModName, precondition, 4}, [3]}],
    StateFuns = get_eqc_fsm_state_functions(ModName, TypeInfo),
    Pars1 = [{{M, F, A}, [1]} || {M, F, A} <- StateFuns],
    Pars = Pars0 ++ Pars1,
    refac_io:format("Pars:\n~p\n", [Pars]),
    Fun = fun ({{M, F, A}, RetType, ArgTypes}) ->
		  case lists:keysearch({M, F, A}, 1, Pars) of
		    {value, {{M, F, A}, Is}} ->
			[case I of
			   0 -> RetType;
			   _ -> lists:nth(I, ArgTypes)
			 end || I <- Is];
		    false ->
			[]
		  end
	  end,
    {lists:append([Fun(T) || T <- TypeInfo]), StateFuns};
   
get_current_state_type_1(TypeInfo, ModName, gen_fsm) ->
    StateFuns = get_gen_fsm_state_functions(ModName, TypeInfo),
    case lists:keysearch({ModName, init, 1}, 1, TypeInfo) of
      {value, {{ModName, init, 1}, RetType, _ArgsType}} ->
	  case RetType of
	    {c, tuple, [{c, atom, [ok], _},
			{c, atom, [_StateName], _},
			StateData| _], _} ->
		{[StateData], StateFuns};
	    _ ->
		  throw({error, ?Msg})
	  end;
	false ->
	    throw({error, "gen_fsm callback function init/1 is not defined."})
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




get_gen_fsm_state_functions(ModName, TypeInfo) ->
    case lists:keysearch({ModName, init, 1}, 1, TypeInfo) of 
	{value, {{ModName, init, 1}, RetType, _ArgsType}} ->
	    refac_io:format("RetType:\n~p\n", [RetType]),
	    case RetType of 
		{c, tuple ,[_, {c, atom, [StateName], _}|_],_} ->
		    get_gen_fsm_state_functions_1(ModName, TypeInfo,[StateName]);
		_ -> throw({error, "Wrangler failed to infer the initial state name of the fsm."})
	    end;
	false ->
	    throw({error, "gen_fsm callback function init/1 is not defined."})
   end.
    
get_gen_fsm_state_functions_1(ModName, TypeInfo, StateNames) ->
    StateNames1 = get_gen_fsm_state_functions_2(TypeInfo, StateNames),
    case StateNames == StateNames1 of 
	true ->
	    [{ModName, S, 2} || S <- StateNames];
	false ->
	    get_gen_fsm_state_functions_1(ModName, TypeInfo, 
				    lists:usort(StateNames++StateNames1))
    end.

get_gen_fsm_state_functions_2(TypeInfo, StateNames) ->
    Fun = fun ({{_M, F, A}, RetType, _ArgTypes}) ->
		  case lists:member(A, [2, 3]) andalso lists:member(F, StateNames) of
		    true ->
			case RetType of
			  {c, tuple, Es, _} ->
			      case Es of
				[{c, atom, [next_state], _},
				 {c, atom, [S], _}| _T] ->
				    [S];
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






%%====================================================================================
%%
%% Some utility functions 
%%
%%====================================================================================

element_to_record_access(RecordName, RecordFields, DefPs, Node) ->
    RecordToTupleFunName = list_to_atom(atom_to_list(RecordName) ++ "_to_tuple"),
    [N, T] = refac_syntax:application_arguments(Node),
    case {refac_syntax:type(N), refac_syntax:type(T)} of
      {integer, variable} ->
	  As1 = refac_syntax:get_ann(T),
	  case lists:keysearch(def, 1, As1) of
	    {value, {def, DefinePos}} ->
		case DefinePos -- DefPs =/= DefinePos of
		  true ->
			FieldName = lists:nth(refac_syntax:integer_value(N), RecordFields),
			refac_syntax:record_access(T, refac_syntax:atom(RecordName),
						   refac_syntax:atom(FieldName));
		    false ->
			Node
		end;
	      false ->
		  Node
	  end;
	{integer, application} ->
	  case is_app(T, {RecordToTupleFunName, 1}) of
	      true ->
		  [Arg] = refac_syntax:application_arguments(T),
		  FieldName = lists:nth(refac_syntax:integer_value(N), RecordFields),
		  refac_syntax:record_access(Arg, refac_syntax:atom(RecordName),
					     refac_syntax:atom(FieldName));
	      false ->
		  Node
	  end;
	_ -> Node
    end.

setelement_to_record_expr(Node, RecordName, RecordFields, DefPs, IsTuple) ->
    RecordToTupleFunName = list_to_atom(atom_to_list(RecordName) ++ "_to_tuple"),
    [Index, T, V] = refac_syntax:application_arguments(Node),
    case {refac_syntax:type(Index), refac_syntax:type(T)} of
      {integer, variable} ->
	  As1 = refac_syntax:get_ann(T),
	  case lists:keysearch(def, 1, As1) of
	    {value, {def, DefinePos}} ->
		case DefinePos -- DefPs =/= DefinePos of
		  true ->
		      FieldName = lists:nth(refac_syntax:integer_value(Index), RecordFields),
		      Field = mk_record_field(FieldName, V),
		      RecordExpr = mk_record_expr(T, RecordName, Field),
		      make_record_to_tuple_app(RecordExpr, RecordName, RecordFields, IsTuple);
		  false ->
		      Node
		end;
	    false -> Node
	  end;
      {integer, application} ->
	  case is_app(T, {RecordToTupleFunName, 1}) of
	    true ->
		[Arg] = refac_syntax:application_arguments(T),
		FieldName = lists:nth(refac_syntax:integer_value(Index), RecordFields),
		Field = mk_record_field(FieldName, V),
		RecordExpr = mk_record_expr(Arg, RecordName, [Field]),
		refac_io:format("RecordExprs:\n~p\n", [RecordExpr]),
		make_record_to_tuple_app(RecordExpr, RecordName, RecordFields, IsTuple);
	    false ->
		Node
	  end;
      {integer, record_expr} ->
	  FieldName = lists:nth(refac_syntax:integer_value(Index), RecordFields),
	  Field = mk_record_field(FieldName, V),
	  mk_record_expr(T, RecordName, [Field]);
      _ -> Node
    end.


is_tuple_to_is_record(Tree, RecordName) ->
    RecordToTupleFunName = list_to_atom(atom_to_list(RecordName) ++ "_to_tuple"),
    Fun = fun(Node, _Others) ->
		  case is_app(Node, {erlang, is_tuple, 1}) of 
		      true ->
			  [Arg] = refac_syntax:application_arguments(Node),
			  case is_app(Arg, {RecordToTupleFunName, 1}) of 
			      true ->
				  [T] = refac_syntax:application_arguments(Arg),
				  refac_syntax:application(refac_syntax:atom(is_record),
							   [T, refac_syntax:atom(RecordName)]);
			      _ -> Node
			  end;
		      _ -> Node
		  end
	  end,
    refac_util:full_buTP(Fun, Tree, {}).


callbacks(ModName, _StateFuns, eqc_statem) ->
    [{{ModName, initial_state, 0}, {0, true}},
     {{ModName, precondition, 2}, {1, false}},
     {{ModName, command, 1}, {1, false}},
     {{ModName, postcondition, 3}, {1, false}},
     {{ModName, next_state, 3}, {1, false}}];

callbacks(ModName, StateFuns, eqc_fsm) ->
    CallBacks0 = [{{ModName, initial_state_data, 0}, {0, true}},
		  {{ModName, next_state_data, 3}, {3, true}},
		  {{ModName, precondition, 5}, {3, false}},
		  {{ModName, postcondition, 5}, {3, false}}],
    StateFunCallBacks = [{{ModName, FunName, Arity}, {Arity, false}} 
			 || {FunName, Arity} <- StateFuns],
    CallBacks0 ++ StateFunCallBacks;

callbacks(ModName, StateFuns, gen_fsm) ->
    CallBacks0 = [{{ModName, init, 1}, {0, true}},
		  {{ModName, handle_event, 3}, {3, true}},
		  {{ModName, handle_sync_event, 4}, {4, true}},
		  {{ModName, handle_info, 3}, {3, true}},
		  {{ModName, terminate, 3}, {3, false}},
		  {{ModName, code_change,4}, {3, true}},
		  {{ModName, enter_loop, 4}, {4, false}},
		  {{ModName, enter_loop, 5}, {4, false}},
		  {{ModName, enter_loop, 6}, {4, false}}],		  
    StateFunCallBacks = [{{ModName, FunName, Arity}, {Arity, true}}
			 || {FunName, Arity} <- StateFuns],
    CallBacks0 ++ StateFunCallBacks.
	

mk_tuple_to_record_fun(RecordName, RecordFields) ->
    FunName = list_to_atom("tuple_to_" ++ atom_to_list(RecordName)),
    Pars = [refac_syntax:variable("E" ++ integer_to_list(I))
	    || I <- lists:seq(1, length(RecordFields))],
    Fields = mk_record_fields(RecordFields, Pars),
    RecordExpr = mk_record_expr(RecordName, Fields),
    C = refac_syntax:clause([refac_syntax:tuple(Pars)], none, [RecordExpr]),
    refac_syntax:function(refac_syntax:atom(FunName), [C]).

mk_record_to_tuple_fun(RecordName, RecordFields) ->
    FunName = list_to_atom(atom_to_list(RecordName) ++ "_to_tuple"),
    Pars = [refac_syntax:variable("E" ++ integer_to_list(I))
	    || I <- lists:seq(1, length(RecordFields))],
    Fields = mk_record_fields(RecordFields, Pars),
    RecordExpr = mk_record_expr(RecordName, Fields),
    C = refac_syntax:clause([RecordExpr], none, [refac_syntax:tuple(Pars)]),
    refac_syntax:function(refac_syntax:atom(FunName), [C]).

make_tuple_to_record_app(Expr, RecordName, RecordFields, IsTuple) ->
    case IsTuple of
      false ->
	  Field = mk_record_field(hd(RecordFields), Expr),
	  mk_record_expr(RecordName, [Field]);
      true ->
	  TupleToRecordFunName = list_to_atom("tuple_to_" ++ atom_to_list(RecordName)),
	  RecordToTupleFunName = list_to_atom(atom_to_list(RecordName) ++ "_to_tuple"),
	  NewExpr = refac_util:rewrite(
		      Expr, refac_syntax:application(
			      refac_syntax:atom(TupleToRecordFunName), [Expr])),
	  case is_app(Expr, {RecordToTupleFunName, 1}) of
	    true ->
		hd(refac_syntax:application_arguments(Expr));
	    false ->
		NewExpr
	  end
    end.
 
   	    
make_record_to_tuple_app(Expr, RecordName, RecordFields, IsTuple) ->
    case IsTuple of 
	false ->
	    refac_syntax:record_access(Expr, refac_syntax:atom(RecordName),
				       refac_syntax:atom(hd(RecordFields)));
	true ->
	    TupleToRecordFunName = list_to_atom("tuple_to_"++ atom_to_list(RecordName)),
	    RecordToTupleFunName = list_to_atom(atom_to_list(RecordName) ++ "_to_tuple"),
	    NewExpr = refac_util:rewrite(Expr,
					 refac_syntax:application(
					   refac_syntax:atom(RecordToTupleFunName), [Expr])),
	    case is_app(Expr, {TupleToRecordFunName, 1}) of
		true ->
	    hd(refac_syntax:application_arguments(Expr));
		false ->
		    NewExpr
	    
	    end
    end.


mk_record_field(Name, Val) ->
    refac_syntax:record_field(refac_syntax:atom(Name),refac_syntax:remove_comments(Val)).

mk_record_fields(RecordFields, Es) ->
    [mk_record_field(Name, refac_syntax:remove_comments(Val))
     || {Name, Val} <- lists:zip(RecordFields, Es),
	refac_syntax:type(Val) =/= underscore].

mk_record_expr(RecordName, Fields) ->
    refac_util:reset_attrs(refac_syntax:record_expr(
			     refac_syntax:atom(RecordName), Fields)).

mk_record_expr(T, RecordName,Fields) ->
    refac_util:reset_attrs(refac_syntax:record_expr(
			     T, refac_syntax:atom(RecordName),Fields)).


tuple_to_record_expr(Tuple, RecordName, RecordFields) ->
    Es = refac_syntax:tuple_elements(Tuple),
    Fields = mk_record_fields(RecordFields, Es),
    mk_record_expr(RecordName, Fields).  
    
is_app(Expr, {F, A}) ->
    case refac_syntax:type(Expr) of
	application ->
	    Op = refac_syntax:application_operator(Expr),
	    case refac_syntax:type(Op) of
		atom->
		    Args = refac_syntax:application_arguments(Expr),
		    {F, A} =={refac_syntax:atom_value(Op),length(Args)};
		_ -> false
	    end;
	_ -> false
    end;
is_app(Expr, {M,F,A}) ->
    case refac_syntax:type(Expr) of
	application ->
	    As = refac_syntax:get_ann(refac_syntax:application_operator(Expr)),
	    case lists:keysearch(fun_def,1,As) of
		{value, {fun_def, {M, F, A, _ ,_}}}->
		    true;
		_ ->
		    false
	    end;
	_ -> false
    end.
    
is_not_type_attrubute(F) ->
    case refac_syntax:type(F) of
      attribute ->
	  Name = refac_syntax:attribute_name(F),
	  case refac_syntax:type(Name) of
	    atom ->
		not lists:member(refac_syntax:atom_value(Name),
				 ['type', 'spec']);
	    _ ->
		false
	  end;
      _ -> false
    end.
		
is_used_only_once(Body, DefinePos) ->
    Fun= fun(Node, Acc) ->
	     case refac_syntax:type(Node) of 
		 variable ->
		     As = refac_syntax:get_ann(Node),
		     case lists:keysearch(def, 1, As) of 
			 {value, {def, DefinePos}} ->
			     Pos = refac_syntax:get_pos(Node),
			     case lists:member(Pos, DefinePos) of
				 true ->
				    Acc; 
				 false ->
				     [Node|Acc]
			     end;
			 _->
			     Acc
		     end;
		 _ -> Acc
	     end
	 end,
    length(refac_syntax_lib:fold(Fun, [], Body))==1.

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
    
