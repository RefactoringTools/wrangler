%%@hidden
%%@private
-module(expand_collect).

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
    parse_transform_1({wrangler_code_inspection_api, collect, 3},
                      fun(Form, _Context) ->
                              case erl_syntax:application_arguments(Form) of
                                  [Expr, Cond, FileOrDirs] ->
                                      {ok, FreeVars}= check_template_str(Expr),
                                      Cond1 = refac_syntax_lib:annotate_bindings(Cond, []),
                                      FreeVars1 = refac_util:get_free_vars(Cond1),
                                      case FreeVars1--FreeVars of 
                                          [] -> ok;
                                          Vs -> 
                                              Vs1 =[V||{V,_}<-Vs],
                                              Msg = "The condition expression contains free variable(s): "++io_lib:format("~p", Vs1),
                                              erlang:error({error, lists:flatten(Msg)})
                                      end,
                                      MatchExprs = [make_match_expr(V)||{V,_} <- FreeVars1],
                                      Cond2 =make_fun_expr(MatchExprs ++ [Cond]),
                                      Op = erl_syntax:application_operator(Form),
                                      erl_syntax:application(Op, [Expr, Cond2, FileOrDirs]);
                                  _Args ->
                                      Msg ="Illegal application of function code_inspection_api:collect/3",
                                      erlang:error(Msg)
                              end
                      end, Forms, Options).

make_fun_expr(Body) ->
    Pats =[erl_syntax:variable('_Bind')],
    C= erl_syntax:clause(Pats, none, Body),
    FunExpr=erl_syntax:fun_expr([C]),
    FunExprStr = erl_prettypr:format(FunExpr),
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



