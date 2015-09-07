%%%------------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Implements the functionality that redirects local calls and ?MODULE
%%% macros to their original modules. It also adds the Module variable
%%% where appropriate to the existing function headers. Only comm cluster.
%%% @end
%%% Created : 22 Jul 2015 by Pablo Lamela
%%%------------------------------------------------------------------------
-module(local_fix).

-export([fix_localities/4, add_fun_to_col/3]).

-include("cluster_records.hrl").

%%-----------------------------------------------------------------------
%% @doc
%% Fixes the SyntaxTree to behave as it did in the original module (by
%% redirecting references and the ?MODULE macro).
%% IsLeafOrExc must be true if the tree is known to have no references
%% or if it is in an exclusive cluster. HasFuncDec must be true
%% the root of the SyntaxTree is a legacy function declaration.
%% @end
%%-----------------------------------------------------------------------
-spec fix_localities(IsLeafOrExc :: boolean(), SyntaxTree :: wrangler_syntax:syntaxTree(),
		     HasFuncDec :: boolean(), ColData :: cluster_mapping:col_data()) ->
			    {wrangler_syntax:syntaxTree(), cluster_mapping:col_data()}.
fix_localities(true, SyntaxTree, _HasFuncDec, ColData) -> {SyntaxTree, ColData};
fix_localities(false, SyntaxTree, true, ColData) ->
    mapfold_clauses(fun add_module_param/3, ColData, SyntaxTree);
fix_localities(false, SyntaxTree, _, ColData) ->
    {FixedSyntaxTree, {_, NewColData}} = fix_localities_forms(SyntaxTree, {false, ColData}),
    {FixedSyntaxTree, NewColData}.

%%-----------------------------------------------------------------------
%% @doc
%% Fixes the local references and ?MODULE usages in BodyList,
%% adds the Module var when appropriate.
%% @end
%%-----------------------------------------------------------------------
-spec add_module_param(ParamList :: [string()],
		       BodyList :: [wrangler_syntax:syntaxTree()],
		       ColData :: cluster_mapping:col_data()) ->
			      {{FixedParamList :: [string()],
				FixedBodyList :: [wrangler_syntax:syntaxTree()]},
			       cluster_mapping:col_data()}.
add_module_param(ParamList, BodyList, #col_data{var_for_module = ModuleVar} = ColData) ->
    {FixedList, {HasLocalities, NewColData}} = fix_localities_forms(
						 BodyList, {false, ColData}),
    {{[wrangler_syntax:variable(
	 case HasLocalities of
	     true -> ModuleVar;
	     false -> [$_|ModuleVar]
	 end)|ParamList], FixedList}, NewColData}.

%%-----------------------------------------------------------------------
%% @doc
%% Map folds Fun through each clause of Fun.
%% @end
%%-----------------------------------------------------------------------
-spec mapfold_clauses(Fun :: fun(([string()], [wrangler_syntax:syntaxTree()], Acc) ->
					{{[string()], [wrangler_syntax:syntaxTree()]}, Acc}),
		      Acc, SyntaxTree :: wrangler_syntax:syntaxTree()) ->
			     {wrangler_syntax:syntaxTree(), Acc} when
      Acc :: term().
mapfold_clauses(Fun, ColData, SyntaxTree) ->
    Name = wrangler_syntax:function_name(SyntaxTree),
    Clauses = wrangler_syntax:function_clauses(SyntaxTree),
    {NewClauses, NewColData} = lists:mapfoldl(
				 fun (El, Acc) ->
					 map_clause(Fun, El, Acc)
				 end, ColData, Clauses),
    {wrangler_syntax:copy_attrs(
       SyntaxTree, wrangler_syntax:function(Name, NewClauses)), NewColData}.

%%-----------------------------------------------------------------------
%% @doc
%% Map folds Fun through a single Clause.
-spec map_clause(Fun :: fun(([string()], [wrangler_syntax:syntaxTree()], Acc) ->
					{{[string()], [wrangler_syntax:syntaxTree()]}, Acc}),
		 Clause :: wrangler_syntax:syntaxTree(), Acc) ->
			{wrangler_syntax:syntaxTree(), Acc} when
      Acc :: term().
map_clause(Fun, Clause, ColData) ->
    {Patterns, Guard, Body} = get_clause_info(Clause),
    {{NewPatterns, NewBody}, NewColData} = Fun(Patterns, Body, ColData),
    NewClause = wrangler_syntax:clause(NewPatterns, Guard, NewBody),
    {wrangler_syntax:copy_attrs(Clause, NewClause), NewColData}.

%%-----------------------------------------------------------------------
%% @doc
%% Extract all fields from a clause element Clause.
-spec get_clause_info(Clause :: wrangler_syntax:syntaxTree()) ->
			     {ClausePatterns :: [wrangler_syntax:syntaxTree()],
			      ClauseGuard :: wrangler_syntax:syntaxTree(),
			      ClauseBody :: [wrangler_syntax:syntaxTree()]}.
get_clause_info(Clause) ->
    Patterns = wrangler_syntax:clause_patterns(Clause),
    Guard = wrangler_syntax:clause_guard(Clause),
    Body = wrangler_syntax:clause_body(Clause),
    {Patterns, Guard, Body}.

%%-----------------------------------------------------------------------
%% @doc
%% Fixes the local references and ?MODULE macro usages to behave
%% as in the original module, and accumulates info about those
%% usages in Acc.
%% @end
%%-----------------------------------------------------------------------
-spec fix_localities_forms(SyntaxTree :: wrangler_syntax:syntaxTree()
				      | [wrangler_syntax:syntaxTree()],
			   Acc) -> Acc when
      Acc :: term().
fix_localities_forms(SyntaxTreeList, Acc) when is_list(SyntaxTreeList) ->
    lists:mapfoldl(fun fix_localities_forms/2, Acc, SyntaxTreeList);
fix_localities_forms(SyntaxTree, Acc) ->
    {NewSyntaxTree, NewAcc} =
	case wrangler_syntax:type(SyntaxTree) of
	    application -> fix_local_application(SyntaxTree, Acc);
	    implicit_fun -> fix_local_implicit_fun(SyntaxTree, Acc);
	    macro -> fix_module_macro(SyntaxTree, Acc);
	    variable -> log_variable_usage(SyntaxTree, Acc);
	    _ -> {SyntaxTree, Acc}
	end,
    api_ast_traverse:mapfold_subtrees(fun fix_localities_forms/2,
				      NewAcc, NewSyntaxTree).

%%-----------------------------------------------------------------------
%% @doc
%% Sets the first element of Acc to true if the variable SyntaxTree is
%% the module variable.
%% @end
%%-----------------------------------------------------------------------
-spec log_variable_usage(SyntaxTree :: wrangler_syntax:syntaxTree(),
			 Acc :: {boolean(), cluster_mapping:col_data()}) ->
				{wrangler_syntax:syntaxTree(),
				 {boolean(), cluster_mapping:col_data()}}.
log_variable_usage(SyntaxTree, {_, #col_data{var_for_module = ModuleVar} = ColData} = Acc) ->
    case wrangler_syntax:variable_literal(SyntaxTree) of
	String when String =:= ModuleVar -> {SyntaxTree, {true, ColData}};
	_ -> {SyntaxTree, Acc}
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Qualifies a local function application (SyntaxTree), with the Module
%% variable if it is local, (not a bif or imported external function).
%% It also sets the first element of Acc to true and updates the
%% col_data if it is a local reference.
%% @end
%%-----------------------------------------------------------------------
-spec fix_local_application(SyntaxTree :: wrangler_syntax:syntaxTree(),
			    Acc :: {boolean(), cluster_mapping:col_data()}) ->
				   {wrangler_syntax:syntaxTree(),
				 {boolean(), cluster_mapping:col_data()}}.
fix_local_application(SyntaxTree, Acc) ->
    Op = wrangler_syntax:application_operator(SyntaxTree),
    case wrangler_syntax:type(Op) of
	atom -> Args = wrangler_syntax:application_arguments(SyntaxTree),
                Arity = length(Args),
		fix_local_fun_reference(fun  add_module_to_application/4, SyntaxTree,
					Op, Args, Arity, Acc);
	_ -> {SyntaxTree, Acc}
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Qualifies an implicit function (SyntaxTree), with the Module
%% variable if it is a local reference, (not a bif or imported external
%% function). It also sets the first element of Acc to true and updates
%% the col_data if it is a local reference.
%% @end
%%-----------------------------------------------------------------------
-spec fix_local_implicit_fun(SyntaxTree :: wrangler_syntax:syntaxTree(),
			     Acc :: {boolean(), cluster_mapping:col_data()}) ->
				    {wrangler_syntax:syntaxTree(),
				     {boolean(), cluster_mapping:col_data()}}.
fix_local_implicit_fun(SyntaxTree, Acc) ->
    AQuali = wrangler_syntax:implicit_fun_name(SyntaxTree),
    case wrangler_syntax:type(AQuali) of
	arity_qualifier -> 
	    NameST = wrangler_syntax:arity_qualifier_body(AQuali),
	    ArityST = wrangler_syntax:arity_qualifier_argument(AQuali),
	    fix_local_arity_qualifier(SyntaxTree, NameST, ArityST, Acc);
	_ -> {SyntaxTree, Acc}
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Qualifies the implicit function SyntaxTree if NameST and ArityST,
%% from its arity qualifier, represent a local call, (not a bif
%% or an imported external function).
%% @end
%%-----------------------------------------------------------------------
-spec fix_local_arity_qualifier(SyntaxTree :: wrangler_syntax:syntaxTree(),
				NameST :: wrangler_syntax:syntaxTree(),
				ArityST :: wrangler_syntax:syntaxTree(),
				Acc :: {boolean(), cluster_mapping:col_data()}) ->
				       {wrangler_syntax:syntaxTree(),
					{boolean(), cluster_mapping:col_data()}}.
fix_local_arity_qualifier(SyntaxTree, NameST, ArityST, Acc) ->
    case {wrangler_syntax:type(NameST), wrangler_syntax:type(ArityST)} of
	{atom, integer} ->
	    Arity = wrangler_syntax:integer_value(ArityST),
	    fix_local_fun_reference(fun add_module_to_implicit_fun/4, SyntaxTree,
				    NameST, ArityST, Arity, Acc);
	_ -> {SyntaxTree, Acc}
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Uses the function Fun to fix a call to a local function, if
%% it is not a bif or an imported external function.
%% @end
%%-----------------------------------------------------------------------
-spec fix_local_fun_reference(fun((wrangler_syntax:syntaxTree(),Op,Args,string()) ->
					 wrangler_syntax:syntaxTree()),
			      SyntaxTree :: wrangler_syntax:syntaxTree(),
			      Op, Args, Arity :: integer(),
			      Acc :: {boolean(), cluster_mapping:col_data()}) ->
				     {wrangler_syntax:syntaxTree(),
					{boolean(), cluster_mapping:col_data()}}.
fix_local_fun_reference(Fun, SyntaxTree, Op, Args, Arity,
			{_, #col_data{inscope_funs1 = Inscope1,
				      inscope_funs2 = Inscope2,
				      var_for_module = ModuleVar} = ColData} = Acc) ->
    Name = wrangler_syntax:atom_value(Op),
    case erl_internal:bif(Name, Arity) orelse
	cluster_folding:is_in_scope(Name, Arity, Inscope1, Inscope2) of
	false -> {Fun(SyntaxTree,Op,Args,ModuleVar),
		  {true, add_fun_to_col(ColData, atom_to_list(Name), Arity)}};
        true -> {SyntaxTree, Acc}
    end.

%%-----------------------------------------------------------------------
%% @doc
%% Adds the Name and Arity to the field beh_funcs of ColData.
%% @end
%%-----------------------------------------------------------------------
-spec add_fun_to_col(ColData :: cluster_mapping:col_data(),
		     Name :: string(), Arity :: integer()) ->
			    cluster_mapping:col_data().
add_fun_to_col(ColData, Name, Arity) ->
    ColData#col_data{beh_funcs = sets:add_element(
				   {Name, Arity},
                                   ColData#col_data.beh_funcs)}.

%%-----------------------------------------------------------------------
%% @doc
%% Adds a variable with name ModuleVar as qualifier for the
%% local function application Application.
%% @end
%%-----------------------------------------------------------------------
-spec add_module_to_application(Application :: wrangler_syntax:syntaxTree(),
				Name :: wrangler_syntax:syntaxTree(),
				Args :: [wrangler_syntax:syntaxTree()],
				ModuleVar :: string()) -> wrangler_syntax:syntaxTree().
add_module_to_application(OldApp,Name,Args,ModuleVar) ->
    NewApp = wrangler_syntax:application(wrangler_syntax:variable(ModuleVar),
					 Name, Args),
    wrangler_syntax:copy_attrs(OldApp, NewApp).

%%-----------------------------------------------------------------------
%% @doc
%% Creates an implicit fun of the type fun ModuleVar:Name/Arity with
%% the attributes of ImplicitFun.
%% @end
%%-----------------------------------------------------------------------
-spec add_module_to_implicit_fun(ImplicitFun :: wrangler_syntax:syntaxTree(),
				 Name :: wrangler_syntax:syntaxTree(),
				 Args :: [wrangler_syntax:syntaxTree()],
				 Module :: string()) -> wrangler_syntax:syntaxTree().
add_module_to_implicit_fun(OldImpFun, Name, Arity, ModuleVar) ->
    NewImpFun = wrangler_syntax:implicit_fun(
		  wrangler_syntax:module_qualifier(
		    wrangler_syntax:variable(ModuleVar), Name),
		  Arity),
    wrangler_syntax:copy_attrs(OldImpFun, NewImpFun).

%%-----------------------------------------------------------------------
%% @doc
%% Replaces the first element in the Acc with true if SyntaxTree
%% is the macro ?MODULE. Assumes SyntaxTree is a macro.
%% @end
%%-----------------------------------------------------------------------
-spec fix_module_macro(SyntaxTree :: wrangler_syntax:syntaxTree(),
		       Acc :: {boolean(), cluster_mapping:col_data()}) ->
			      {wrangler_syntax:syntaxTree(),
			       {boolean(), cluster_mapping:col_data()}}.
fix_module_macro(SyntaxTree, {_, #col_data{var_for_module = ModuleVar} = ColData} = Acc) ->
    Name = wrangler_syntax:macro_name(SyntaxTree),
    case wrangler_syntax:type(Name) of
	variable -> TName = wrangler_syntax:variable_literal(Name),
		    TArity = wrangler_syntax:macro_arguments(SyntaxTree),
		    case {TName, TArity} of
			{"MODULE", none} ->
			    {wrangler_syntax:variable(ModuleVar), {true, ColData}};
			_ -> {SyntaxTree, Acc}
		    end;
	_ -> {SyntaxTree, Acc}
    end.

