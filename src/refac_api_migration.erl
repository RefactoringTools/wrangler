%% @hidden
%% @private
-module(refac_api_migration).

-export([do_api_migration_to_file/5,
         do_api_migration_to_dirs/5,
         generate_rule_based_api_migration_mod/2]).

-export([mk_new_var/2, mk_str/2]).

-include("../include/wrangler.hrl").

-export([behaviour_info/1]).

%%@private
-spec behaviour_info(atom()) ->[{atom(), arity()}].
behaviour_info(callbacks) ->
    [{old_apis, 0}, {meta_rule_set,0}, {simple_rule_set, 0}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                   %%
%%   Apply Rule-based API migration.                                 %%
%%                                                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec(do_api_migration_to_file(CurFile::filename(),CallBackMod::module(), SearchPaths::[filename()|dir()], 
                       Editor::atom(), TabWidth::integer()) -> 
             {ok, [filename()]} |{error, term()}). 
do_api_migration_to_file(CurFile, CallBackMod, SearchPaths, Editor, TabWidth)->
    ?wrangler_io("\nCMD: ~p:do_api_migration_to_file(~p, ~p, ~p, ~p, ~p).\n",
                   [?MODULE, CurFile, CallBackMod, SearchPaths, Editor, TabWidth]),
    do_api_migration([CurFile], CallBackMod, SearchPaths, Editor, TabWidth).

-spec(do_api_migration_to_dirs(Dirs::[dir()], CallBackMod::module(), SearchPaths::[filename()|dir()], 
                       Editor::atom(), TabWidth::integer()) -> 
             {ok, [filename()]} |{error, term()}).             
do_api_migration_to_dirs(Dirs, CallBackMod, SearchPaths, Editor, TabWidth)->
    ?wrangler_io("\nCMD: ~p:do_api_migration_to_dirs(~p, ~p, ~p, ~p, ~p).\n",
                   [?MODULE, Dirs, CallBackMod, SearchPaths, Editor, TabWidth]),
    do_api_migration(Dirs, CallBackMod, SearchPaths, Editor, TabWidth).

-spec(do_api_migration(FileOrDirs::[filename()|dir()],CallBackMod::module(), 
                       SearchPaths::[filename()|dir()], 
                       Editor::atom(), TabWidth::integer()) -> 
             {ok, [filename()]} |{error, term()}).             
do_api_migration(FileOrDirs, CallBackMod, SearchPaths, Editor, TabWidth)->
    case code:ensure_loaded(CallBackMod) of 
        {error, Reason} ->
            {error, mk_str("Wrangler failed to load module ~p; reason:~p.", [CallBackMod, Reason])};
        _ ->
            try apply(CallBackMod, old_apis, []) of 
                OldMFAs ->
                    Files = wrangler_misc:expand_files(FileOrDirs, ".erl"),
                    Res=lists:append([do_api_migration_1(CallBackMod, OldMFAs, File, SearchPaths, TabWidth)
                                      ||File <- Files]),
                    wrangler_write_file:write_refactored_files(Res, Editor, TabWidth, "")
            catch
                _E1:E2 ->
                    {error, {E2, erlang:get_stacktrace()}}
            end
    end.

-spec(do_api_migration_1(CallBackMod::module(), OldMFAs::[mfa()],
                         File::filename(),
                         SearchPaths::[filename()|dir()], 
                         TabWidth::integer()) -> 
             [{{filename(),filename()}, syntaxTree()}]).
do_api_migration_1(CallBackMod, OldMFAs, File, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} =wrangler_ast_server:parse_annotate_file(
                             File, true, SearchPaths, TabWidth),
    case lists:keysearch(module, 1, Info) of
	{value, {module, ModName}} -> ModName;
	_ -> ModName = '_'
    end,
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    Res=[do_api_migration_2(CallBackMod, OldMFAs, Form, ModName)||Form <- Forms],
    {NewForms, Changed} = lists:unzip(Res),
    case lists:member(true, Changed) of 
        true ->
            NewAST = wrangler_syntax:form_list(NewForms),
            [{{File, File}, NewAST}];
        false ->
            []
    end.

-spec(do_api_migration_2(CaMllBackMod::module(), OldMFAs::[mfa()],
                         Form::syntaxTree(),
                         ModName::module()) ->
             {syntaxTree(), boolean()}).
do_api_migration_2(CallBackMod, OldMFAs, Form, ModName) ->
    UsedVars = all_vars(Form),
    try MetaRules=apply(CallBackMod, meta_rule_set, []),
         SimpleRules = apply(CallBackMod, simple_rule_set, []),
         {ok, [{_, Form1}]}= ?FULL_TD_TP(MetaRules, [{UsedVars, Form}]),
         Form2 =case Form==Form1 of 
                    true -> 
                        do_intro_new_var_refac(OldMFAs, Form, ModName);
                    false ->
                        reparse_form(Form1, ModName)
                end,
         Form3 = do_intro_new_var_refac(OldMFAs, Form2, ModName),
         {ok, [{_,Form4}]}= ?FULL_TD_TP(SimpleRules, [{UsedVars, Form3}]),
         {Form4, Form4/=Form}
    catch
        _E1:E2 ->
            throw({error, {E2, erlang:get_stacktrace()}})
    end.
    
-spec(do_intro_new_var_refac(OldMFAs::[mfa()],Form::syntaxTree(),
                             ModName::module()) ->
             syntaxTree()).
do_intro_new_var_refac(OldMFAs, Form, ModName) ->
    case  collect_apps(OldMFAs, Form) of
        [] -> Form;
        Apps ->
            NewForm=lists:foldl(
                      fun(Expr, FunDef) ->
                              UsedVars=all_vars(FunDef),
                              NewVarName = mk_new_var("Res", UsedVars),
                              refac_intro_new_var:do_intro_new_var_in_fun(FunDef, Expr, NewVarName)
                      end, Form, Apps),
            case NewForm == Form of 
                true ->
                    Form;
                false -> 
                    reparse_form(NewForm, ModName)
            end
    end.

-spec(collect_apps(MFAs::[mfa()], FunDef::syntaxTree()) ->
             [syntaxTree()]).                   
collect_apps(MFAs, FunDef) ->
    ?STOP_TD_TU([?COLLECT(?T("catch F@(Args@@)"), _This@,
                              lists:member(api_refac:fun_define_info(F@), MFAs)),
                 ?COLLECT(?T("F@(Args@@)"), _This@,
                              lists:member(api_refac:fun_define_info(F@), MFAs) andalso
                          not lists:member(api_refac:syntax_context(_This@), 
                                           [body_expr, match_expr_body]))], FunDef).

-spec(reparse_form(Form::syntaxTree(), ModName::module()) ->syntaxTree()).                        
reparse_form(Form, ModName) ->
    FormStr = wrangler_prettypr:pp_a_form(Form, 'unix', [], 8),
    Pos = wrangler_syntax:get_pos(Form),
    {ok, Toks, _} =wrangler_scan_with_layout:string(FormStr, Pos), 
    FormAST=wrangler_misc:parse_annotate_expr(FormStr, Pos),
    FormAST1 = wrangler_ast_server:add_range(
                 wrangler_syntax:add_ann({toks, Toks}, FormAST),Toks),
    wrangler_annotate_ast:add_fun_def_info(FormAST1, ModName, [],[]).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                   %%
%%   Generate the rule-based API migration module from the API       %%
%%   interface module provided.                                      %%
%%                                                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(generate_rule_based_api_migration_mod(FileName::filename(), 
                                            NewModName::string()|atom()) ->
             {ok, filename()} | {error, term()}).
generate_rule_based_api_migration_mod(FileName, NewModName) ->
    ?wrangler_io("\nCMD: ~p:generate_rule_based_api_migration_mod(~p, ~p).\n",
                  [?MODULE, FileName, NewModName]),
    case gen_target_file_name(FileName, NewModName) of 
        {error, Reason} ->
            {error, Reason};
        NewFileName ->
            case wrangler_ast_server:parse_annotate_file(FileName, true) of 
                {ok, {AnnAST, Info}} ->
                    case lists:keysearch(module, 1, Info) of
                        {value, {module, ModName}} ->
                            try  ModName:old_api_module_name() of 
                                 M when is_atom(M) ->
                                    try do_generate_callback_funs(M, AnnAST, NewFileName) 
                                    catch
                                        throw:Error ->
                                            {error, Error}
                                    end;
                                 _ -> {error, mk_str("Invalid return value by callback function "
                                                     "~p:old_api_module_name/0",
                                                     [ModName])}
                            catch
                                _E1:E2 ->
                                    {E2, erlang:get_stacktrace()}
                            end;
                        _ -> {error, "Wrangler could not infer the module name of the file supplied"}                
                    end;
                {error, Reason} ->
                    {error, mk_str("File ~p does not compile: ~p!",[FileName, Reason])}
            end
    end.

gen_target_file_name(CurFileName, NewModName) ->
    NewModName1 =if is_list(NewModName) ->
                         list_to_atom(NewModName);
                    true ->
                         if is_atom(NewModName) ->
                                 NewModName;
                            true ->
                                 {error, "Invalid new module name."}
                         end
                 end,
    case NewModName1 of
        {error, Reason} ->
            {error, Reason};
        _ ->
            case api_refac:is_fun_name(atom_to_list(NewModName1)) of
                true -> 
                    NewFileName = filename:join(
                                    [filename:dirname(CurFileName),
                                     atom_to_list(NewModName1)++".erl"]),
                    case filelib:is_file(NewFileName) of
                        false ->
                            NewFileName;
                        true ->
                            {error, "The new module/file name has been used!"}
                    end;  
                false ->
                    {error, "Invalid new module name."}
            end
    end.
              
                              
-spec(do_generate_callback_funs(OldAPIModName::atom(),
                                AnnAST::syntaxTree(), 
                                NewFileName::filename()) 
      -> {ok, filename()} |{error, term()}).
do_generate_callback_funs(OldAPIModName, AnnAST, NewFileName) ->
    NewModName = filename:basename(NewFileName, ".erl"),
    Forms = wrangler_syntax:form_list_elements(AnnAST),
    Res = [do_process_api_interface_funs_1(OldAPIModName, Form)||
                  Form <- Forms, is_an_api_interface_fun(Form)],
    {MFAs, RuleSet1, RuleSet2} = lists:unzip3(Res),
    InitStr = 
        "-module(" ++ NewModName ++ "). \n\n"
        "-include_lib(\"wrangler/include/wrangler.hrl\"). \n\n"
        "-export([meta_rule_set/0, simple_rule_set/0, old_apis/0]).\n\n",
    OldAPIsFun=gen_old_apis_fun(lists:flatten(MFAs)),
    {Set1RuleNames, Set1Rules} = lists:unzip(lists:append(RuleSet1)),
    RuleSet1Fun = gen_rule_set_fun(meta_rule_set,Set1RuleNames),
    {Set2RuleNames, Set2Rules} = lists:unzip(lists:append(RuleSet2)),
    RuleSet2Fun = gen_rule_set_fun(simple_rule_set,Set2RuleNames),
    Code =InitStr ++ OldAPIsFun ++ "\n\n" ++ RuleSet1Fun ++ 
        "\n\n" ++ RuleSet2Fun ++ lists:append(Set1Rules) ++
        lists:append(Set2Rules),
    case file:write_file(NewFileName, list_to_binary(Code)) of
        ok ->
            {ok, NewFileName};
        _ -> Res
    end.
    

-spec(is_an_api_interface_fun(Form::syntaxTree()) -> boolean()).
is_an_api_interface_fun(Form) ->
    case wrangler_syntax:type(Form) of 
        function ->
            Name=wrangler_syntax:function_name(Form),
            case wrangler_syntax:type(Name) of 
                atom ->
                    wrangler_syntax:atom_value(Name)/=
                        old_api_module_name;
                _ -> false
            end;
        _ -> false
    end.

-spec(gen_old_apis_fun(MFAs::[{modulename(), functionname(), arity()}]) -> string()).
gen_old_apis_fun(MFAs) ->
    ?PP(wrangler_misc:parse_annotate_expr(mk_str("old_apis() -> ~p", [MFAs]))).

-spec(gen_rule_set_fun(FunName::functionname(), RuleNames::[functionname()]) ->string()).   
gen_rule_set_fun(FunName, RuleNames) ->
    Body =wrangler_syntax:list([wrangler_syntax:application(wrangler_syntax:atom(Name), [])
           ||Name<-RuleNames]),
    AST=wrangler_syntax:function(
          wrangler_syntax:atom(FunName),
          [wrangler_syntax:clause([], none, [Body])]),
    ?PP(AST).
   
-spec(do_process_api_interface_funs_1(OldAPIModName :: atom(),
                                      Form :: syntaxTree()) -> 
                                         {[{modulename(), functionname(),
                                            arity()}],
                                          [{functionname(), string()}],
                                          [{functionname(), string()}]}).             
do_process_api_interface_funs_1(OldAPIModName, Form) ->
    {M,F,A} = api_refac:fun_define_info(Form),
    Cs = wrangler_syntax:function_clauses(Form),
    case Cs of 
        [C] ->
            Pats = wrangler_syntax:clause_patterns(C),
            case lists:all(fun(P) -> api_refac:type(P) == variable end, Pats) of 
                false ->
                    throw({error,mk_str("API interface function ~p:~p/~p has non-variable parameters.",
                                 [M,F,A])});
                true ->
                    Guard = wrangler_syntax:clause_guard(C),
                    case Guard of 
                        none ->
                            Body = wrangler_syntax:clause_body(C),
                            case Body of 
                                [_] ->do_process_api_interface_funs_2(OldAPIModName,Form);
                                _ ->
                                    %% This condition could be relaxed.
                                    throw({error, mk_str("API interface function  ~p:~p/~p contains more than one expression"
                                                 " statement in the function clause body.",
                                                 [M,F,A])})
                            end;
                        _ ->throw({error, mk_str("API interface function  ~p:~p/~p contains a function clause with guard.",
                                         [M,F,A])})
                    end
            end;
        _ ->
            throw({error, mk_str("API interface function ~p:~p/~p has more than one "
                               "function clause.",
                               [M, F, A])})
    end.

-spec(do_process_api_interface_funs_2(OldAPIModName :: atom(),
                                      Form :: syntaxTree()) -> 
                                         {[{modulename(), functionname(),
                                            arity()}],
                                          [{functionname(), string()}],
                                          [{functionname(), string()}]}).
do_process_api_interface_funs_2(OldAPIModName, Form) ->
    {M,F,A} = api_refac:fun_define_info(Form),
    [C] = wrangler_syntax:function_clauses(Form),
    Pats = wrangler_syntax:clause_patterns(C),
    [B] = wrangler_syntax:clause_body(C),
    case wrangler_syntax:type(B) of 
        application ->
            do_generate_rule_for_application({OldAPIModName,F,A},Pats, B);
        case_expr->
            Cs = wrangler_syntax:case_expr_clauses(B),
            case valid_clause_body({M,F,A},Cs) of 
                true ->
                    do_generate_rule_for_case_expr({OldAPIModName,F,A}, Pats, B);
                {error, Reason} ->
                    throw({error, Reason})
            end;
        try_expr ->
            Cs = wrangler_syntax:try_expr_clauses(B),
            case valid_clause_body({M,F,A},Cs) of 
                true ->
                    do_generate_rule_for_try_expr({OldAPIModName,F,A}, Pats, B);
                {error, Reason} ->
                    throw({error, Reason})
            end;
        _ ->
            throw({error, mk_str("The last function clause body expression of the API interface function "
                         "~p:~p/~p is not supported by Wrangler.", [M,F,A])})
    end.

valid_clause_body({M, F, A},Cs) ->
    case lists:all(fun(C)-> 
                           is_valid_clause_body(C) 
                   end, Cs) of
        true ->
            true;
        false ->
            throw({error, mk_str("The API interface function ~p:~p/~p "
                         "contains an invalid return expression.", [M,F,A])})
    end.
is_valid_clause_body(C) ->
    Bs = wrangler_syntax:clause_body(C),
    LastB = to_meta_vars(lists:last(Bs)),
    ExprTypes = collect_expr_types(LastB),
    lists:all(fun(T)->
                      lists:member(T, [literal, tuple, list, variable]) 
              end, ExprTypes).

collect_expr_types(B) ->
    ?FULL_TD_TU([?COLLECT(?T("Expr@"), 
                          case wrangler_syntax:is_literal(Expr@) of 
                              true -> literal;
                              _ -> wrangler_syntax:type(Expr@)
                          end,
                          true)], B).
        

-spec(do_generate_rule_for_application({modulename(), functionname(),arity()},
                                       Pats::[syntaxTree()],Body::syntaxTree()) ->
             {[{modulename(), functionname(), arity()}], [],
              [{functionname(), string()}]}).
do_generate_rule_for_application({M,F,A}, Pats, B) -> 
    RuleFunName=list_to_atom(atom_to_list(F)++"_rule"),
    NewPats = to_meta_vars(Pats),
    NewB = to_meta_vars(B),
    M1 = wrangler_syntax:atom(M), 
    F1 = wrangler_syntax:atom(F),
    TempBefore=wrangler_syntax:macro(
                 wrangler_syntax:variable('T'),
                 [wrangler_syntax:string(?PP(wrangler_syntax:application(
                 wrangler_syntax:module_qualifier(M1, F1),NewPats)))]),
    CodeAfter= wrangler_syntax:macro(
                 wrangler_syntax:variable('TO_AST'),
                 [wrangler_syntax:string(?PP(NewB))]),
    Cond = wrangler_syntax:atom('true'),
    Body=wrangler_syntax:macro(wrangler_syntax:variable('RULE'),
                               [TempBefore, CodeAfter, Cond]),
    C = wrangler_syntax:clause([], none, [Body]),
    Form=wrangler_syntax:function(wrangler_syntax:atom(RuleFunName), [C]),
    FormStr = wrangler_prettypr:pp_a_form(Form, 'unix', [], 8),
    {[{M,F,A}], [], [{RuleFunName, FormStr}]}.


-spec(do_generate_rule_for_case_expr({modulename(), functionname(),arity()},
                                     Pats::[syntaxTree()],Body::syntaxTree()) ->
             {[{modulename(), functionname(), arity()}],
              [{functionname(),string()}],
              [{functionname(), string()}]}).
do_generate_rule_for_case_expr({M,F,A}, Pats, B) ->
    MetaRule=do_generate_meta_rule_for_case_expr({M,F,A},Pats,B,'META_RULE'),
    SimpleRule=do_generate_simple_rule_for_case_expr({M,F,A},Pats,B,'RULE'),
    {[{M,F,A}],[MetaRule], [SimpleRule]}.


-spec(do_generate_meta_rule_for_case_expr({modulename(), functionname(),arity()},
                                     Pats::[syntaxTree()],Body::syntaxTree(), 
                                          RuleNacroName::atom())
      -> string()).            
do_generate_meta_rule_for_case_expr({M,F,_A},Pats,Body,RuleMacroName) ->
    RuleFunName=list_to_atom(atom_to_list(F)++"_meta_rule"),
    NewPats = to_meta_vars(Pats),
    Cs = wrangler_syntax:case_expr_clauses(Body),
    NewCs = [make_case_clause(I, C)
             ||{C, I} <- lists:zip(Cs, lists:seq(1,length(Cs)))],
    M1 = wrangler_syntax:atom(M),
    F1 = wrangler_syntax:atom(F),
    CaseExpr=wrangler_syntax:case_expr(
               wrangler_syntax:application(
                 wrangler_syntax:module_qualifier(M1, F1),NewPats),NewCs),
    TempBefore=mk_old_code_temp(CaseExpr),
    UsedVars = all_vars(CaseExpr),
    {NewBody, Cond} = gen_new_body_and_cond(Body, UsedVars, case_expr),
    NewVars = collect_new_vars(Body, UsedVars),
    NewVarDefExprs = mk_new_var_define_exprs(NewVars),
    TempAfter = mk_new_code_temp(NewBody, NewVars, 'META_RULE'),
    NewCode = wrangler_syntax:block_expr(NewVarDefExprs ++ [TempAfter]),
    Rule=mk_rule(RuleFunName,RuleMacroName, TempBefore, Cond, NewCode),
    {RuleFunName, Rule}.

-spec(do_generate_simple_rule_for_case_expr({modulename(), functionname(),arity()},
                                            Pats::[syntaxTree()],Body::syntaxTree(),
                                           RuleMacroName::atom()) 
      -> string()).  
do_generate_simple_rule_for_case_expr({M,F,_A},Pats,Body,RuleMacroName) ->
    RuleFunName=list_to_atom(atom_to_list(F)++"_rule"),
    NewPats = to_meta_vars(Pats),
    M1 = wrangler_syntax:atom(M),
    F1 = wrangler_syntax:atom(F),
    AppExpr=wrangler_syntax:application(M1, F1,NewPats),
    TempBefore = mk_old_code_temp(AppExpr),
    UsedVars = all_vars(AppExpr),
    {NewBody, Cond} = gen_new_body_and_cond(Body, UsedVars, application),
    NewVars = collect_new_vars(Body, UsedVars),
    NewVarDefExprs = mk_new_var_define_exprs(NewVars),
    TempAfter = mk_new_code_temp(NewBody, NewVars, 'RULE'),
    NewCode = wrangler_syntax:block_expr(NewVarDefExprs ++ [TempAfter]),
    Rule=mk_rule(RuleFunName, RuleMacroName, TempBefore, Cond, NewCode),
    {RuleFunName, Rule}.

-spec(do_generate_rule_for_try_expr({modulename(), functionname(),arity()},
                                     Pats::[syntaxTree()],Body::syntaxTree()) ->
             {[{modulename(), functionname(), arity()}],
              [{functionname(),string()}],
              [{functionname(), string()}]}).
do_generate_rule_for_try_expr({M,F,A}, Pats, B) ->
    MetaRule=do_generate_meta_rule_for_try_expr({M,F,A}, Pats, B, 'META_RULE'),
    SimpleRule=do_generate_simple_rule_for_try_expr({M,F,A}, Pats, B, 'RULE'),
    {[{M,F,A}],[MetaRule], [SimpleRule]}.
  

-spec(do_generate_meta_rule_for_try_expr({modulename(), functionname(),arity()},
                                     Pats::[syntaxTree()],Body::syntaxTree(), 
                                          RuleNacroName::atom())
      -> string()).            
do_generate_meta_rule_for_try_expr({M,F,_A},Pats,Body,RuleMacroName) ->
    RuleFunName=list_to_atom(atom_to_list(F)++"_meta_rule"),
    NewPats = to_meta_vars(Pats),
    Cs = wrangler_syntax:try_expr_clauses(Body),
    NewCs = [make_case_clause(I, C)
             ||{C, I} <- lists:zip(Cs, lists:seq(1,length(Cs)))],
    M1 = wrangler_syntax:atom(M),
    F1 = wrangler_syntax:atom(F),
    CaseExpr=wrangler_syntax:case_expr(
               wrangler_syntax:application(
                 wrangler_syntax:module_qualifier(M1, F1),NewPats),NewCs),
    TempBefore=mk_old_code_temp(CaseExpr),
    UsedVars = all_vars(CaseExpr),
    {NewBody, Cond} = gen_new_body_and_cond(Body, UsedVars, try_expr),
    NewVars = collect_new_vars(Body, UsedVars),
    NewVarDefExprs = mk_new_var_define_exprs(NewVars),
    TempAfter = mk_new_code_temp(NewBody, NewVars, 'META_RULE'),
    NewCode = wrangler_syntax:block_expr(NewVarDefExprs ++ [TempAfter]),
    Rule=mk_rule(RuleFunName,RuleMacroName, TempBefore, Cond, NewCode),
    {RuleFunName, Rule}.

-spec(do_generate_simple_rule_for_try_expr({modulename(), functionname(),arity()},
                                            Pats::[syntaxTree()],Body::syntaxTree(),
                                           RuleMacroName::atom()) 
      -> string()).  
do_generate_simple_rule_for_try_expr({M,F,_A},Pats,Body,RuleMacroName) ->
    RuleFunName=list_to_atom(atom_to_list(F)++"_rule"),
    NewPats = to_meta_vars(Pats),
    M1 = wrangler_syntax:atom(M),
    F1 = wrangler_syntax:atom(F),
    AppExpr=wrangler_syntax:application(M1, F1,NewPats),
    TempBefore = mk_old_code_temp(AppExpr),
    UsedVars = all_vars(AppExpr),
    {NewBody, Cond} = gen_new_body_and_cond(Body, UsedVars, application),
    NewVars = collect_new_vars(Body, UsedVars),
    NewVarDefExprs = mk_new_var_define_exprs(NewVars),
    TempAfter = mk_new_code_temp(NewBody, NewVars, 'RULE'),
    NewCode = wrangler_syntax:block_expr(NewVarDefExprs ++ [TempAfter]),
    Rule=mk_rule(RuleFunName, RuleMacroName, TempBefore, Cond, NewCode),
    {RuleFunName, Rule}.

-spec(mk_old_code_temp(Expr::syntaxTree())-> syntaxTree()).
mk_old_code_temp(Expr) ->
    wrangler_syntax:macro(
      wrangler_syntax:variable('T'),
      [wrangler_syntax:atom(wrangler_quote_before),
       Expr,
       wrangler_syntax:atom(wrangler_quote_after)]).
   
-spec(mk_new_code_temp(Expr::syntaxTree(),NewVars::[atom()],
                      RULE::'META_RULE' |'RULE')-> syntaxTree()).
mk_new_code_temp(NewBody, NewVars, RULE) ->
    App = wrangler_syntax:application(
            wrangler_syntax:atom(refac_api_migration),
            wrangler_syntax:atom(mk_str),
            [wrangler_syntax:atom(wrangler_quote_before), NewBody,
             wrangler_syntax:atom(wrangler_quote_after),
             wrangler_syntax:list([wrangler_syntax:variable(NewVar)
                                   ||NewVar <- NewVars])]),
    case RULE of 
        'META_RULE'  ->
            wrangler_syntax:application(
              wrangler_syntax:atom(api_refac),
              wrangler_syntax:atom(anti_quote),
              [App]);
        'RULE' ->
            wrangler_syntax:macro(
              wrangler_syntax:variable('TO_AST'),
              [App])
    end.

-spec(mk_rule(RuleFunName::atom(), RuleMacroName::atom(), TempBefore::syntaxTree(),
              Cond::syntaxTree(), NewCode::syntaxTree()) -> string()).             
mk_rule(RuleFunName, RuleMacroName, TempBefore, Cond, NewCode) ->
    Body=wrangler_syntax:macro(wrangler_syntax:variable(RuleMacroName),
                               [TempBefore, NewCode, Cond]),
    C = wrangler_syntax:clause([], none, [Body]),
    Form=wrangler_syntax:function(wrangler_syntax:atom(RuleFunName), [C]),
    FormStr = wrangler_prettypr:pp_a_form(Form, 'unix', [], 8),
    replace_quote_place_holder(FormStr).

-spec(make_case_clause(I::integer(), C::syntaxTree()) -> syntaxTree()).
make_case_clause(I, C) ->
    Bs = wrangler_syntax:clause_body(C),
    LastB = to_meta_vars(lists:last(Bs)),
    Guard = wrangler_syntax:variable(list_to_atom("Guard"++integer_to_list(I)++"@@")),
    Body = wrangler_syntax:variable(list_to_atom("Body"++integer_to_list(I)++"@@")),
    wrangler_syntax:clause([LastB], Guard, [Body]).
           
to_meta_vars(Node) ->
    {ok, NewNode} = ?STOP_TD_TP([to_meta_var_rule()], Node),
    NewNode.
  
-spec(to_meta_var_rule()->function()).
to_meta_var_rule() ->    
    ?RULE(?T("V@"),
          wrangler_syntax:variable(
            list_to_atom(atom_to_list(wrangler_syntax:variable_name(V@))++"@")),
          api_refac:type(V@)==variable).

gen_new_body_and_cond(Node, UsedVars, BeforeTempType) ->
    Node1 =replace_new_var_with_placeholder(Node, UsedVars),
    case api_refac:type(Node1) of
        case_expr  ->
            Arg = to_meta_vars(wrangler_syntax:case_expr_argument(Node1)),
            Cs = to_meta_vars(wrangler_syntax:case_expr_clauses(Node1)),
            Len = length(Cs),
            Res =[gen_new_clause_and_cond(C, I, BeforeTempType)
                  ||{C, I}<-lists:zip(Cs, lists:seq(1, Len))],
            {NewCs , Conds}= lists:unzip(Res),
            {wrangler_syntax:case_expr(Arg, NewCs), 
             mk_cond(lists:append(Conds))};
        try_expr  ->
            Body = to_meta_vars(wrangler_syntax:try_expr_body(Node1)),
            Cs = to_meta_vars(wrangler_syntax:try_expr_clauses(Node1)),
            Handlers = to_meta_vars(wrangler_syntax:try_expr_handlers(Node1)),
            Len = length(Cs),
            Res =[gen_new_clause_and_cond(C, I, BeforeTempType)
                  ||{C, I}<-lists:zip(Cs, lists:seq(1, Len))],
            {NewCs , Conds}= lists:unzip(Res),
            {wrangler_syntax:try_expr(Body, NewCs, Handlers), 
             mk_cond(lists:append(Conds))}
    end.

gen_new_clause_and_cond(C, _Nth,application) ->
    Pats = wrangler_syntax:clause_patterns(C),
    Bs = wrangler_syntax:clause_body(C),
    NewC=wrangler_syntax:clause(Pats, none, Bs), %%lists:reverse([LastBody|tl(lists:reverse(Bs))])),
    {NewC, []};
gen_new_clause_and_cond(C, Nth,_BeforeTempType) ->
    Pats = wrangler_syntax:clause_patterns(C),
    GuardName = list_to_atom("Guard" ++ integer_to_list(Nth) ++ "@@"),
    Guard = wrangler_syntax:variable(GuardName),
    LastBody = wrangler_syntax:variable(list_to_atom("Body"++integer_to_list(Nth)++"@@")),
    Bs = wrangler_syntax:clause_body(C),
    NewC=wrangler_syntax:clause(Pats, Guard, lists:reverse([LastBody|tl(lists:reverse(Bs))])),
    LastB = lists:last(Bs),
    {Vars,_} = lists:unzip(api_refac:free_vars(LastB)),
    Cond=case Vars of 
             [] -> [];
             _ ->
                 [gen_cond(GuardName, Vars)]
         end,
    {NewC, Cond}.

gen_cond(GuardName, VarNames) ->
    MetaVarNames = [list_to_atom(atom_to_list(V)++"@")
                    ||V<-lists:usort(VarNames)],
    GuardFreeVars = wrangler_syntax:application(
                      wrangler_syntax:atom('api_refac'),
                      wrangler_syntax:atom('free_vars'),
                      [wrangler_syntax:variable(GuardName)]),
    BoundVars = [wrangler_syntax:application(
                   wrangler_syntax:atom('api_refac'),
                   wrangler_syntax:atom('bound_vars'),
                   [wrangler_syntax:variable(V)]) || V<-MetaVarNames],
    BoundVarAcc=case BoundVars of 
                    [V] -> V;
                    [V|Vs] ->
                        F =fun(Var, Acc) ->
                                   wrangler_syntax:infix_expr(
                                     Acc, wrangler_syntax:operator('++'), Var)
                           end,
                        lists:foldl(F, V, Vs)
                end,
    Left=wrangler_syntax:infix_expr(GuardFreeVars, wrangler_syntax:operator('--'),
                                    wrangler_syntax:parentheses(BoundVarAcc)),
    wrangler_syntax:infix_expr(Left,
                               wrangler_syntax:operator('=='), 
                               GuardFreeVars).
        
mk_cond(Conds) ->    
    case Conds of 
        [] -> wrangler_syntax:atom(true);
        [C] ->
            C;
        [C|Cs]->
            F =fun(Cond, Acc) ->
                       wrangler_syntax:infix_expr(
                         Acc, wrangler_syntax:atom('andalso'), Cond)
               end,
            lists:foldl(F, C, Cs)
    end.
             
replace_new_var_with_placeholder(Node, UsedVars)->
    F = fun(N, _) ->
                case wrangler_syntax:type(N) of 
                    variable ->
                        Name = wrangler_syntax:variable_name(N),
                        case  lists:member(
                                list_to_atom(atom_to_list(Name)++"@"),
                                UsedVars) of
                            true ->
                                {N, false};
                            false ->
                                {wrangler_misc:rewrite(Node,wrangler_syntax:text("~s")),
                                 true}
                        end;
                    _ -> 
                        {N, false}
                end
        end,
    element(1,api_ast_traverse:full_tdTP(F, Node, UsedVars)).

collect_new_vars(Node, UsedVars) ->
    F = fun(N, Acc) ->
                case wrangler_syntax:type(N) of 
                    variable ->
                        Name = wrangler_syntax:variable_name(N),
                        case  lists:member(
                                list_to_atom(atom_to_list(Name)++"@"),
                                UsedVars) of
                            true ->
                                Acc;
                            false ->
                                [Name|Acc]
                        end;
                    _ -> 
                        Acc
                end
         end,
    lists:reverse(api_ast_traverse:fold(F, [], Node)).
   
mk_new_var_define_exprs(NewVars) ->
    Vars = sets:to_list(sets:from_list(NewVars)),
    [mk_new_var_define_exprs_1(Var)||Var<-Vars].

mk_new_var_define_exprs_1(Var) ->
    wrangler_syntax:match_expr(
      wrangler_syntax:variable(Var),
      wrangler_syntax:application(
        wrangler_syntax:atom(refac_api_migration),
        wrangler_syntax:atom(mk_new_var),
        [wrangler_syntax:string(atom_to_list(Var)),
         wrangler_syntax:variable('_File@')]
         )).
    
mk_str(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).



-spec(replace_quote_place_holder(FormStr::string()) -> string()).
replace_quote_place_holder(FormStr) ->
    FormStr1 =re:replace(FormStr, "wrangler_quote_before[\t\n\r\s]*,[\t\n\r\s]*", "\"", 
                         [{return, list}, global]),
    re:replace(FormStr1, ",[\t\n\r\s]*wrangler_quote_after", "\"", 
               [{return, list}, global]).

        
mk_new_var(BaseName, UsedVars) ->
    case lists:member(list_to_atom(BaseName), UsedVars) of 
        false -> 
            BaseName;
        true ->
            mk_new_var(BaseName, UsedVars, 1)
    end.
mk_new_var(BaseName, UsedVars, N)->
    NewName=BaseName++integer_to_list(N),
    case lists:member(list_to_atom(NewName), UsedVars) of
        false ->
           NewName;
        true ->
            mk_new_var(BaseName, UsedVars, N+1)
    end.
    
all_vars(Node) ->
    Vars=?STOP_TD_TU([?COLLECT(?T("V@"), wrangler_syntax:variable_name(V@),
                               api_refac:type(V@)==variable)], Node),
    sets:to_list(sets:from_list(Vars++api_refac:env_var_names(Node))).


%% refac_api_migration:generate_rule_based_api_migration_mod("c:/cygwin/home/hl/git_repos/wrangler/src/regexp_re.erl", test3).
 %% refac_api_migration:do_api_migration([{regexp, match, 2}], ["c:/cygwin/home/hl/git_repos/wrangler/src/test.erl"], 8).

 %%refac_api_migration:generate_rule_base_api_migration_mod("c:/cygwin/home/hl/git_repos/wrangler/src/regexp_re.erl", test).
%% {ok, Apps} = ?FULL_TD_TU([?COLLECT_LOC(?T("F@(Args@@)"), 
%%                                                 lists:member(api_refac:fun_define_info(F@),
%%                                                              OldMFAs))], Form),