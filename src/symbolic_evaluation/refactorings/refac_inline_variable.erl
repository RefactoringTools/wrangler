%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Roberto S. M. de Barros Filho, Simon  Thompson
%% @doc 
%% Inline Variable - Inline a variable definition. This refactoring uses Wrangler API and can be found in <em>Wrangler -> Refactor -> gen_refac Refacs -> Inline Variable</em>.
%%
%% To unfold instances of a variable, point the cursor to the variable assignment and the select
%% <em>Inline Variable</em> from the item <em>gen_refacs</em> in the Wrangler <em>Refactor </em> menu.
%% After that, Wrangler will search for the occurrences of the selected variable and let you choose which instance to unfold. 
%% Only variables defined via a match expression of the format: <em>VarName = Expr</em> can be inlined. 
%%
%% If the user chooses to inline all instances of a variable, the match expression is also removed.
%% @end
-module(refac_inline_variable).

-behaviour(gen_refac).

%% Include files
-include("wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1]).

-export([collect/2]).

%%%===================================================================
%%% gen_refac callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prompts for parameter inputs
%%
%% @spec input_par_prompts() -> [string()]
%% @end
%%--------------------------------------------------------------------
input_par_prompts() ->
   [].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Select the focus of the refactoring.
%%
%% @spec select_focus(Args::#args{}) ->
%%                {ok, syntaxTree()} |
%%                {ok, none}
%% @end
%%--------------------------------------------------------------------
select_focus(_Args=#args{current_file_name=File, 
                         cursor_pos=Pos}) -> 
	    Result = api_interface:pos_to_node(File, Pos, fun(Node) -> ?MATCH(?T("Var@ = Exp@"), Node) end),
	    case Result of
		{ok, Node} -> 
		    ?MATCH(?T("Var@ = Expr@"), Node),
                    {ok, {Var@, Expr@}};
		_ -> {error,"the expression selected needs to be an assignment!"}
	    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec(check_pre_cond(Args::args{}) -> ok | {error, Reason})
%% @end
%%--------------------------------------------------------------------
check_pre_cond(_Args=#args{current_file_name=File, 
                         cursor_pos=Pos}) ->
    Result = api_interface:pos_to_node(File, Pos, fun(Node) -> ?MATCH(?T("Var@ = Exp@"), Node) end),
	    case Result of
		{ok, Node} -> 
		    ?MATCH(?T("Var@ = Expr@"), Node),
		    Valid = api_refac:type(Var@) == variable andalso api_refac:variable_define_pos(Var@) == [Pos],
		    if
			Valid  -> 
			    Info = collect(Var@, File),
			    if
				Info /= [] -> ok;
				true -> {error, "there is no variable occurrence to inline!"}
			     end;
			true -> {error, "the expression selected needs to be an assignment on an unbound variable!"}
		    end;
		_ -> {error,"the expression selected needs to be an assignment!"}
	    end.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Selective transformation or not.
%%
%% @spec selective() -> boolean()
%% @end
%%--------------------------------------------------------------------
selective() ->
    true.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
collect(Var,Scope) ->
    ?FULL_TD_TU(    
       [collect_variables_occurrences(Var)],
       [Scope]
      ).

%%--------------------------------------------------------------------
%% @doc
%% Collects the occurrences of a specific variable.
%% @end
%%--------------------------------------------------------------------
collect_variables_occurrences(Var)->
    ?COLLECT(
       ?T("Var@"),
       true,
       api_refac:type(Var@) == variable  andalso
       Var@ /= Var andalso
       api_refac:variable_define_pos(Var) == api_refac:variable_define_pos(Var@)
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function does the actual transformation.
%%
%% @spec transform(Args::#args{}) -> 
%%            {ok, [{filename(), filename(), syntaxTree()}]} |
%%            {error, Reason}
%% @end
%%--------------------------------------------------------------------
transform(_Args=#args{current_file_name=File,
                      focus_sel={Var, Expr}})->
	    OccurrencesResult = ?FULL_TD_TP([variable_occurrences_rule(Var, Expr)], [File]),
	    tryToRemoveAssignment(OccurrencesResult, Var).    

%%--------------------------------------------------------------------
%% @doc
%% Rule to remove an assignment.
%% @end
%%-------------------------------------------------------------------- 
variable_assignment_rule(Var) ->
    ?RULE(
       ?T("Var@ = Expr@"),
       wrangler_syntax:empty_node(),
       api_refac:type(Var@) == variable  andalso
       Var == Var@
   ).

%%--------------------------------------------------------------------
%% @doc
%% Rule to substitute the variable by its value.
%% @end
%%--------------------------------------------------------------------
variable_occurrences_rule(Var, Expr)->
    ?RULE(
       ?T("Var@"), 
       Expr,
       api_refac:type(Var@) == variable  andalso
       Var@ /= Var andalso
       api_refac:variable_define_pos(Var) == api_refac:variable_define_pos(Var@)
      ).
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Removes an assignment if all the occurrences were removed.
%% @end
%%--------------------------------------------------------------------
tryToRemoveAssignment({ok, [{{File, File}, AST}]}, Var) ->
    CollectResult = collect(Var, AST),
    if
	CollectResult == []->
	    AssignmentResult = ?STOP_TD_TP([variable_assignment_rule(Var)], AST),
	    case AssignmentResult of
		{ok, NewAST} -> {ok, [{{File, File}, NewAST}]};
		Other -> Other
	    end;
	true -> {ok, [{{File, File}, AST}]}
    end;
tryToRemoveAssignment(OtherResult,_) -> OtherResult.

