-module(expand_rule).

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
    Forms1=parse_transform_1({refac_api, make_rule, 3},
                             fun(Form, _Context) ->
                                     case erl_syntax:application_arguments(Form) of
                                         [Before, After, Cond] ->
                                             {Before1, After1, Cond1} = expand_transform_rule(Before, After, Cond),
                                             Op = erl_syntax:application_operator(Form),
                                             NewCode=erl_syntax:application(Op, [Before1, After1, Cond1]),
                                             %%refac_io:format("Cond:\n~s\n", [erl_prettypr:format(NewCode)]),
                                             NewCode;
                                         _Args ->
                                             Msg ="Illegal application of function refac_api:make_rule/3",
                                             erlang:error(Msg)
                                     end
                             end, Forms, Options),
    Forms2=parse_transform_1({wrangler_code_inspection_api, collect, 3},
                      fun(Form, _Context) ->
                              case erl_syntax:application_arguments(Form) of
                                  [TempStr, Cond, FileOrDirs] ->
                                      {Before1, After1} = expand_collect(TempStr, Cond),
                                      Op = erl_syntax:application_operator(Form),
                                      Res=erl_syntax:application(Op, [Before1, After1, FileOrDirs]),
                                      %% refac_io:format("Res:\n~s\n",[erl_prettypr:format(Res)]),
                                      Res;
                                  _Args ->
                                      Msg ="Illegal application of function refac_api:collect/4",
                                      erlang:error(Msg)
                              end
                      end, Forms1, Options),
    Forms3=parse_transform_1({refac_api, collect, 4},
                      fun(Form, _Context) ->
                              case erl_syntax:application_arguments(Form) of
                                  [TempStr, Cond, ReturnFun, FileOrDirs] ->
                                      {Before1, After1, Cond1} = expand_collect(TempStr, Cond, ReturnFun),
                                      Op = erl_syntax:application_operator(Form),
                                      Res=erl_syntax:application(Op, [Before1, After1, Cond1, FileOrDirs]),
                                      %% refac_io:format("Res:\n~s\n",[erl_prettypr:format(Res)]),
                                      Res;
                                  _Args ->
                                      Msg ="Illegal application of function refac_api:collect/4",
                                      erlang:error(Msg)
                              end
                      end, Forms2, Options),
    parse_transform_1({refac_api, match, 2},
                      fun(Form, _Context) ->
                              case erl_syntax:application_arguments(Form) of
                                  [TempStr, Tree] ->
                                      expand_match(TempStr, Tree);
                                  _Args ->
                                      Msg ="Illegal application of function refac_api:match/2",
                                      erlang:error(Msg)
                              end
                      end, Forms3, Options).

expand_transform_rule(TempBefore, TempAfter, Cond) ->
    BindVarName=list_to_atom("_Bind"++integer_to_list(random:uniform(1000))),
    NewTempAfter = expand_temp_after(TempBefore, TempAfter, BindVarName),
    NewCond = expand_cond(Cond, BindVarName),
    %% refac_io:format("TempBefore:\n~s\n", [refac_prettypr:format(TempBefore)]),
    %% refac_io:format("NewCond:\n~s\n", [refac_prettypr:format(NewCond)]),
    %% refac_io:format("NewTempAfter:\n~s\n", [refac_prettypr:format(NewTempAfter)]),
    {TempBefore, NewTempAfter, NewCond}.
   

expand_temp_after(TempBefore, TempAfter, BindVarName) ->
    case erl_syntax:type(TempAfter) of 
        block_expr ->
            expand_temp_after_1(TempBefore, TempAfter, BindVarName);
        _ ->
            expand_temp_after_1(TempBefore, 
                                erl_syntax:block_expr([TempAfter]), BindVarName)
    end.

expand_temp_after_1(_TempBefore, TempAfter, BindVarName) ->
    TempAfter1 = refac_syntax_lib:annotate_bindings(TempAfter, []),
    FreeVars = refac_api:free_vars(TempAfter1),
    MatchExprs = [make_match_expr(V, BindVarName)||{V,_} <- FreeVars, is_meta_variable_name(V)],
    TempAfter2 = expand_macro(TempAfter1, BindVarName),
    Body = erl_syntax:block_expr_body(TempAfter2),
    make_fun_expr(MatchExprs++Body, BindVarName, refac_syntax:get_pos(TempAfter)).


expand_macro(Node, BindVarName) ->
    {Node1, _} = ast_traverse_api:stop_tdTP(fun expand_macro_1/2, Node, BindVarName),
    Node1.
expand_macro_1(Node, BindVarName) ->
    case erl_syntax:type(Node) of
        application ->
            case erl_syntax_lib:analyze_application(Node) of
                {refac_api, {quote, 1}} ->
                    {expand_quote(Node, BindVarName), true};
                _ ->
                    {Node, false}
            end;
        _ ->
            {Node, false}
    end.

expand_quote(QuoteApp, BindVarName) ->
    [Str] = erl_syntax:application_arguments(QuoteApp),
    Pos = erl_syntax:get_pos(Str),
    FreeVars = element(1, lists:unzip(refac_api:env_vars(Str))),
    NewBinds=[erl_syntax:tuple([erl_syntax:atom(VarName),
                                erl_syntax:variable(VarName)])
              ||VarName<-FreeVars],
    Binds = erl_syntax:infix_expr(erl_syntax:list(NewBinds),
                                  erl_syntax:operator('++'),
                                  erl_syntax:variable(BindVarName)),
    Op= erl_syntax:module_qualifier(erl_syntax:atom(refac_api), erl_syntax:atom(parse_annotate_expr)),
    App =erl_syntax:application(Op, [Str, erl_syntax:integer(Pos)]),
    Op1 = erl_syntax:module_qualifier(erl_syntax:atom(refac_api), erl_syntax:atom(subst)),
    erl_syntax:application(Op1,[App, Binds]).

expand_match(TempStr, Node) ->
    Pos = erl_syntax:get_pos(TempStr),
    Op= erl_syntax:module_qualifier(erl_syntax:atom(refac_api), 
                                    erl_syntax:atom(match)),
    Args=[erl_syntax:application(erl_syntax:module_qualifier(erl_syntax:atom(refac_api), 
                                      erl_syntax:atom(parse_annotate_expr)),
                                   [TempStr, erl_syntax:integer(Pos)]), Node],
    Temp = refac_api:parse_annotate_expr(erl_syntax:string_value(TempStr)),
    App =erl_syntax:application(Op, Args),
    NewVar1=list_to_atom("_Bind"++integer_to_list(random:uniform(1000))),
    NewVar2=list_to_atom("_Bind"++integer_to_list(random:uniform(1000))),
    Cs =[erl_syntax:clause([erl_syntax:atom('false')], none, 
                           [erl_syntax:application(
                              erl_syntax:module_qualifier(erl_syntax:atom(erlang), 
                                                         erl_syntax:atom(error)),
                            [erl_syntax:string("Template does not match AST node "
                                               "at line "++integer_to_list(Pos))])]),
         erl_syntax:clause([erl_syntax:tuple([erl_syntax:atom('true'), 
                             erl_syntax:variable(NewVar1)])], none, 
                           [erl_syntax:variable(NewVar1)])],                            
    MatchExpr= erl_syntax:match_expr(erl_syntax:variable(NewVar2),
                                      erl_syntax:case_expr(App, Cs)),
    %% refac_io:format("App:\n~p\n", [MatchExpr]),
    FreeVars = refac_api:free_vars(Temp),
    MatchExprs = [make_match_expr(V, NewVar2)||{V,_} <- FreeVars, 
                                               is_meta_variable_name(V)],
    NewCode=erl_syntax:block_expr([MatchExpr|MatchExprs]),
    NewCode.
  

is_meta_variable_name(VarName) ->
    lists:prefix("@", lists:reverse(atom_to_list(VarName))).
                    
expand_cond(Cond, BindVarName) ->
    Cond1 = convert_meta_atom_to_meta_var(Cond),
    Cond2 = refac_syntax_lib:annotate_bindings(Cond1, []),
    FreeVars1 = refac_api:free_vars(Cond2),
    MatchExprs = [make_match_expr(V, BindVarName)||
                     {V,_} <- FreeVars1, 
                     is_meta_variable_name(V)],
    make_fun_expr(MatchExprs ++ [Cond1], BindVarName, refac_syntax:get_pos(Cond)).

 
convert_meta_atom_to_meta_var(Node) ->
    {Node1, _} = ast_traverse_api:stop_tdTP(fun do_convert/2, Node, {}),
    Node1.

do_convert(Node, _Others) ->
    case erl_syntax:type(Node) of 
        atom ->
            case is_meta_atom(Node) of 
                true ->
                    AtomValue = erl_syntax:atom_value(Node),
                    AtomValueList=atom_to_list(AtomValue),
                    UpperAtomValue=list_to_atom(string:to_upper(AtomValueList)),
                    {erl_syntax:variable(UpperAtomValue), true};
                false ->
                    {Node, false}
            end;
        _ ->
            {Node, false}
    end.
                
is_meta_atom(Node) ->
    case erl_syntax:type(Node) of 
        atom ->
            AtomValue = erl_syntax:atom_value(Node),
            AtomValueList=atom_to_list(AtomValue),
            refac_api:is_fun_name(AtomValueList) andalso
                lists:prefix("@", lists:reverse(AtomValueList));
        _ ->
            false
    end.

%% TODO : add a sensable location here!
make_fun_expr(Body, BindVarName, StartLoc) when is_integer(StartLoc) ->
    make_fun_expr(Body,BindVarName, {StartLoc, 1});
make_fun_expr(Body, BindVarName, StartLoc={_L, _C}) ->
    Pats =[erl_syntax:variable(BindVarName)],
    C= erl_syntax:clause(Pats, none, Body),
    FunExpr=erl_syntax:fun_expr([C]),
    FunExprStr = erl_prettypr:format(FunExpr),
    parse_str(FunExprStr, StartLoc).

parse_str(Str, StartLoc) ->
    {ok, Toks, _} = erl_scan:string(Str, StartLoc),
    case erl_parse:parse_exprs(Toks++[{dot, {999,0}}]) of
        {ok, Exprs} ->
            Exprs1=erl_syntax:form_list_elements(
                     refac_syntax_lib:annotate_bindings(erl_recomment:recomment_forms(Exprs,[]))),
            hd(Exprs1);
        _Error ->
            erlang:error("cannot parse")
    end.

make_match_expr(VarName, BindVarName) ->
    VarNameStr = io_lib:write_atom(VarName),
    BindVarNameStr=atom_to_list(BindVarName),
    ValueVar = atom_to_list(VarName)++"_V",
    Str="case lists:keysearch(" ++ VarNameStr ++
        ", 1, "++BindVarNameStr++") of {value, {"++
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

expand_collect(Temp, Cond) ->
    BindVarName=list_to_atom("_Bind"++integer_to_list(random:uniform(1000))),
    NewCond = expand_cond(Cond, BindVarName),
    {Temp, NewCond}.

expand_collect(Temp, Cond, ReturnFun) ->
    BindVarName=list_to_atom("_Bind"++integer_to_list(random:uniform(1000))),
    NewReturnFun =expand_temp_after(Temp,ReturnFun, BindVarName),
    NewCond = expand_cond(Cond, BindVarName),
    {Temp, NewCond, NewReturnFun}.


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



