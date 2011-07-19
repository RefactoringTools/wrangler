%%@hidden
%%@private
-module(expand_foreach).

-export([parse_transform/2]).

-define(ERROR(R, T, F, I),
	begin
	    rpt_error(R, T, F, I),
	    throw({error,erl_syntax:get_pos(
			   proplists:get_value(form,I)),{unknown,R}})
	end).

-import(erl_syntax, [clause/3,
		     clause_patterns/1,
		     clause_body/1,
		     clause_guard/1,
		     match_expr/2,
		     function_clauses/1,
		     get_pos/1,
		     add_ann/2,
		     get_ann/1]).


parse_transform(Forms, Options) ->
    parse_transform_1({gen_refac, search_and_transform, 2},
                      fun(Form, _Context) ->
                              case erl_syntax:application_arguments(Form) of
                                  [TemplatesAndConds, FileOrDirs] ->
                                      Es = erl_syntax:list_elements(TemplatesAndConds),
                                      TemplatesAndConds1 = erl_syntax:list([expand_transform_rule(E)||E <- Es]),
                                      Op = erl_syntax:application_operator(Form),
                                      erl_syntax:application(Op, [TemplatesAndConds1, FileOrDirs]);
                                  _Args ->
                                      Msg ="Illegal application of function code_inspection_api:collect/3",
                                      erlang:error(Msg)
                              end
                      end, Forms, Options).


expand_transform_rule(T) ->
    [TempBefore, TempAfter, Cond] = erl_syntax:tuple_elements(T),
    NewTempAfter = expand_temp_after(TempBefore, TempAfter),
    RenameCond = expand_cond(TempBefore, Cond),
    erl_syntax:tuple([TempBefore, NewTempAfter, RenameCond]).


expand_temp_after(TempBefore, TempAfter) ->
    case refac_syntax:type(TempAfter) of 
        block_expr ->
            expand_temp_after_1(TempBefore, TempAfter);
        _ ->
            expand_temp_after_1(TempBefore, 
                                erl_syntax:block_expr([TempAfter]))
    end.

expand_temp_after_1(_TempBefore, TempAfter) ->
    TempAfter1 = refac_syntax_lib:annotate_bindings(TempAfter, []),
    Body = erl_syntax:block_expr_body(TempAfter1),
    FreeVars = refac_util:get_free_vars(Body),
    MatchExprs = lists:reverse(tl(lists:reverse(Body))),
    MatchExprsToAdd = [make_match_expr(V)||{V, _}<-FreeVars, is_meta_variable_name(V)],
    %%refac_io:format("MatchExprsToAdd:\n~s\n",[erl_prettypr:format(erl_syntax:block_expr(MatchExprsToAdd))]),
    Binds = make_new_binds(MatchExprs),
    %%refac_io:format("Binds:\n~p\n", [Binds]),
    BindMatchExpr= erl_syntax:match_expr(erl_syntax:variable('Bind1'), erl_syntax:list(Binds)),
    %%refac_io:format("BindMatchExpr:\n~s\n", [erl_prettypr:format(erl_syntax:block_expr([BindMatchExpr]))]),
    NewBinds = erl_syntax:infix_expr(erl_syntax:variable('Bind1'), 
                                       erl_syntax:operator('++'),
                                       erl_syntax:variable('_Bind')),
    NewBindsMatchExpr =erl_syntax:match_expr(erl_syntax:variable('NewBinds'),
                                               NewBinds),
    %%refac_io:format("NewBindsMatchExpe:\n~s\n", [erl_prettypr:format(erl_syntax:block_expr([NewBindsMatchExpr]))]),     
    Op= erl_syntax:module_qualifier(erl_syntax:atom(gen_refac), erl_syntax:atom(parse_annotate_expr)),
    %%refac_io:format("Op:\n~p\n", [Op]),
    App =erl_syntax:application(Op, [lists:last(Body)]),
    %%refac_io:format("App:\n~p\n", [App]),
    TempMatchExpr = erl_syntax:match_expr(erl_syntax:variable('TempAfterAST'),App),
    %%refac_io:format("TempMatchExpr:\n~p\n", [TempMatchExpr]),
    %%refac_io:format("TempMatchExprStr:\n~s\n", [erl_prettypr:format(erl_syntax:block_expr([TempMatchExpr]))]),      
    Op1 = erl_syntax:module_qualifier(erl_syntax:atom(gen_refac), erl_syntax:atom(subst)),
    ASTAfter=erl_syntax:application(Op1,[erl_syntax:variable('TempAfterAST'),
                                          erl_syntax:variable('NewBinds')]),
    %%refac_io:format("ASTAfter:\n~s\n", [erl_prettypr:format(erl_syntax:block_expr([ASTAfter]))]),  
    NewBody = MatchExprsToAdd++MatchExprs++[BindMatchExpr,NewBindsMatchExpr,TempMatchExpr, ASTAfter],
    refac_io:format("\nddd:~s", [erl_prettypr:format(erl_syntax:block_expr(NewBody))]),
    make_fun_expr(NewBody).


is_meta_variable_name(VarName) ->
    lists:prefix("@", lists:reverse(atom_to_list(VarName))).
   
make_new_binds(MatchExprs) ->
    make_new_binds_1(MatchExprs, []).

make_new_binds_1([], Binds) ->
    Binds;
make_new_binds_1([M|T], Binds) ->
    Pattern = erl_syntax:match_expr_pattern(M),
    VarName = erl_syntax:variable_name(Pattern),
    Bind=erl_syntax:tuple([erl_syntax:atom(VarName), 
                             erl_syntax:variable(VarName)]),
    make_new_binds_1(T, [Bind|Binds]).

                    
expand_cond(TempBefore, Cond) ->
    {ok, _FreeVars} = check_template_str(TempBefore), %%TODO: CHECK WETHER THIS IS NEED!
    Cond1 = convert_meta_atom_to_meta_var(Cond),
    Cond2 = refac_syntax_lib:annotate_bindings(Cond1, []),
    FreeVars1 = refac_util:get_free_vars(Cond2),
    MatchExprs = [make_match_expr(V)||{V,_} <- FreeVars1, is_meta_variable_name(V)],
    make_fun_expr(MatchExprs ++ [Cond1]).


convert_meta_atom_to_meta_var(Node) ->
    {Node1, _} = ast_traverse_api:stop_tdTP(fun do_convert/2, Node, {}),
    Node1.

do_convert(Node, _Others) ->
    case refac_syntax:type(Node) of 
        atom ->
            case is_meta_atom(Node) of 
                true ->
                    AtomValue = refac_syntax:atom_value(Node),
                    AtomValueList=atom_to_list(AtomValue),
                    UpperAtomValue=list_to_atom(string:to_upper(AtomValueList)),
                    {refac_syntax:variable(UpperAtomValue), true};
                false ->
                    {Node, false}
            end;
        _ ->
            {Node, false}
    end.
                
is_meta_atom(Node) ->
    case refac_syntax:type(Node) of 
        atom ->
            AtomValue = refac_syntax:atom_value(Node),
            AtomValueList=atom_to_list(AtomValue),
            refac_util:is_fun_name(AtomValueList) andalso
                lists:prefix("@", lists:reverse(AtomValueList));
        _ ->
            false
    end.


make_fun_expr(Body) ->
    Pats =[erl_syntax:variable('_Bind')],
    C= erl_syntax:clause(Pats, none, Body),
    FunExpr=erl_syntax:fun_expr([C]),
    FunExprStr = erl_prettypr:format(FunExpr),
    refac_io:format("Cond:\n~s\n", [FunExprStr]),
    parse_str(FunExprStr).

parse_str(Str) ->
    {ok, Toks, _} = erl_scan:string(Str),
    case erl_parse:parse_exprs(Toks++[{dot, {999,0}}]) of
        {ok, Exprs} ->
            Exprs1=erl_syntax:form_list_elements(
                     refac_syntax_lib:annotate_bindings(erl_recomment:recomment_forms(Exprs,[]))),
            hd(Exprs1);
        _Error ->
            erlang:error("cannot parse")
    end.

make_match_expr(VarName) ->
    VarNameStr = io_lib:write_atom(VarName),
    ValueVar = atom_to_list(VarName)++"_V",
    Str="case lists:keysearch(" ++ VarNameStr ++
        ", 1, _Bind) of {value, {"++
        VarNameStr++","++ValueVar++"}} -> " ++ValueVar++"; false -> no_bind end",
    {ok, Toks, _} = erl_scan:string(Str),
    case erl_parse:parse_exprs(Toks++[{dot, {999,0}}]) of
        {ok, Exprs} ->
            Exprs1 =erl_recomment:recomment_forms(Exprs,[]),
            Exprs2=erl_syntax:form_list_elements(refac_syntax_lib:annotate_bindings(Exprs1)),
            case Exprs2 of
                [E] ->  erl_syntax:match_expr(erl_syntax:variable(VarName), E);
                _ -> erlang:error("cannot parse")
            end;
        _ -> erlang:error("cannot parse")
    end.

check_template_str(TemplateStr) ->
    case refac_syntax:type(TemplateStr) of 
        string ->
            case erl_scan:string(erl_syntax:string_value(TemplateStr)) of
                {ok, Toks, _Loc} ->
                    Toks1 = case lists:last(Toks) of
                                {dot, _} -> Toks;
                                _ -> Toks++[{dot, 999}]
                            end,
                    case erl_parse:parse_exprs(Toks1) of 
                        {ok, ExprList} ->
                            Exprs1 =refac_recomment:recomment_forms(ExprList,[]),
                            Exprs2 =refac_syntax_lib:annotate_bindings(Exprs1),
                            FreeVars = refac_util:get_free_vars(Exprs2),
                            {ok, FreeVars};
                        {error, _ErrorInfo} ->
                            case erl_parse:parse_form(Toks1) of 
                                {ok, AbsForm} ->
                                    AbsForm1=refac_syntax_lib:annotate_bindings(AbsForm),
                                    FreeVars = refac_util:get_free_vars(AbsForm1),
                                    {ok, FreeVars};
                                _ ->
                                    Msg = "String does not parse: "++io_lib:format("~s", [TemplateStr]),
                                    erlang:error(Msg)
                            end
                    end;
                _Error ->
                    Msg = "String does not parse: "++io_lib:format("~s", [TemplateStr]),
                    erlang:error(Msg)
            end;
        false ->
            Msg = "The first parameter of code_inspection_api:collect/3 must be a string.",
            erlang:error(Msg)
    end.



%%% API: function({Module, Function, Arity}, Fun, Forms, Options) ->
%%%         NewForms
%%%
%%% Forms and Options are the arguments passed to the parse_transform/2
%%% function.
%%% {Module, Function, Arity} is the function call to transform
%%% Fun(Form, Context) -> NewForm is the fun provided by the caller.
%%%
%%% Context is a property list, containing the following properties:
%%% - {file, Filename}
%%% - {module, ModuleName}
%%% - {function, FunctionName}       % name of the enclosing function
%%% - {arity, Arity :: integer()}    % arity of same
%%% - {var_names, Vars :: [atom()]}  % generated variables binding the
%%%                                  % function arguments.
%%%                                  % length(Vars) == Arity
%%%
parse_transform_1({_Module, _Function, _Arity} = MFA, F,
	          Forms, Options) when is_function(F) ->
    parse_transform(MFA, F, Forms, Options).

parse_transform(MFA, Fun, Forms, _Options) ->
    [File|_] = [F || {attribute,_,file,{F,_}} <- Forms],
    try begin
            NewTree = xform(MFA, Fun, Forms, [{file, File}]),
            revert_tree(NewTree)
	end
    catch
	throw:{error,Ln,What} ->
	    {error, [{File, [{Ln,?MODULE,What}]}], []}
    end.

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].


xform({M,F,A}, Fun, Forms, Context0) ->
    Bef = fun(function, Form, Ctxt) ->
                  {Form, Ctxt};
	     (_, Form, Context) ->
		  {Form, Context}
	  end,
    Aft = fun(application, Form, Context) ->
		  case erl_syntax_lib:analyze_application(Form) of
		      {M, {F, A}} ->
                          Fun(Form, Context);
		      _ ->
			  Form
		  end;
             (_, Form, _Context) ->
		  Form
	  end,
    [Module] = [Mx || {attribute, _, module, Mx} <- Forms],
    transform(Forms, Bef, Aft, [{module, Module}|Context0]).


transform(Forms, Before, After, Context) ->
    F1 =
	fun(Form) ->
                Form2 =
		    case erl_syntax:subtrees(Form) of
			[] ->
			    Form;
			List ->
			    NewList =
				transform(
				  List, Before, After, Context),
			    erl_syntax:update_tree(Form, NewList)
		    end,
                Type2 = erl_syntax:type(Form2),
		try After(Type2, Form2, Context)
		catch
		    error:Reason2 ->
			?ERROR(Reason2, 'after', After, 
			       [{type, Type2},
				{context, Context},
				{form, Form2}])
		end
	end,
    F2 = fun(List) when is_list(List) ->
		 map(F1, List);
	    (Form) ->
		 F1(Form)
	 end,
    map(F2, Forms).

%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
map(F, [Hd|Tail]) ->
    {Before, Res, After} =
	case F(Hd) of
	    {Be, _, Af} = Result when is_list(Be), is_list(Af) ->
		Result;
	    R1 ->
		{[], R1, []}
	end,
    Rs = map(F, Tail),
    Before ++ [Res| After ++ Rs];
map(F, []) when is_function(F, 1) -> [].



rpt_error(Reason, BeforeOrAfter, Fun, Info) ->
    Fmt = lists:flatten(
	    ["*** ERROR in parse_transform function:~n"
	     "*** Reason     = ~p~n"
	     "*** applying ~w fun (~p)~n",
	     ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, BeforeOrAfter, Fun | 
	    lists:foldr(
	      fun({K,V}, Acc) ->
		      [K, V | Acc]
	      end, [], Info)],
    io:format(Fmt, Args).



