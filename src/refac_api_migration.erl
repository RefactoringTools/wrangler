%% Copyright (c) 2012, Huiqing Li, Simon Thompson
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

%%@author  Huiqing Li and Simon Thompson <H.Li, S.J.Thompson@kent.ac.uk>
%%
%%
%%@doc Wrangler's support for API migration. 
%% Most software evolves during its lifetime, and this will often change
%% the API of a library. A change of interface made to a library function could
%% potentially affect all client applications of the library. The  API 
%% transformations required tend to be done manually by the maintainers
%% of the client code, and this process can be both tedious and error-prone.
%%
%% Wrangler's support for API migration refactorings is  based on the template- 
%% and rule-based technique; however, unlike general refactorings, an API migration 
%% refactoring only requires the author to define adapter functions that implement 
%% the `old' API functions using the `new' API functions.  Once the adapter module 
%% has been defined, Wrangler can then take this as input, and generate the refactoring
%% code that implements the migration.
%%
%% As a design principle, we try to limit the scope of changes as much as possible, so 
%% that only the places where the `old' API function is called are affected, and 
%% the remaining part of the code is unaffected.  
%%
%% To illustrate our approach, we take the migration from `regexp:match/2'
%% to `re:run/3' as an example, which represents one of the most complex API changes 
%% we have seen in practice. Tthe change involves every aspect of the function interface, 
%% namely the module name, function name, arguments and values returned.
%%
%% An <em>adapter</em> function is a single-clause function that implements the `old' API 
%% function using the `new' API. The function shown below is the adapter function for 
%% the migration from `regexp:match/2' to `re:run/3'.
%% ``` match(String, RegExp) ->
%%       case re:run(String, RegExp, [global]) of
%%         {match, Match} ->
%%           {Start0, Len}=lists:last(lists:ukeysort(2, Match)),
%%           Start = Start0+1,
%%           {match, Start, Len};
%%         nomatch -> nomatch
%%       end.'''
%% A `case' expression is needed by the definition of the adapter function if and only 
%% if the value returned by the API function is affected by the migration, and the returned 
%% value is of a `union' type, i.e. a type consists of a number of alternatives. Within 
%% the `case' expression, each expression clause handles one possible alternative of the 
%% return value, and the clause body defines how to derive the value that should be returned 
%% by the `old' API function from the value returned by the `new' one. 
%%
%% A guard expression can be used to enures the mutual exclusiveness of expression clauses. For 
%% example, the adaptor function for the migration from lists:keysearch/3 to lists:keyfind/3 can 
%% be defined as: 
%% ``` keysearch(Key, N, TupleList) ->
%%       case lists:find(Key, N, TupleList) of
%%          Tuple when is_tuple(Tuple)->
%%              {value, Tuple};
%%          false ->
%%             false
%%       end.'''
%%
%% Obviously, for an API migration that does not affect the return value of the function, 
%% a `case' expression is not needed. For the case in which only the name of the API function
%% has been changed, the body of the adapter function could be just a function application 
%% of the `new' function. 
%%
%% A number of constraints should be satisfied by adapter functions:
%% <ul>
%% <li> The definition should have only one clause, and the name/arity should be the 
%%  same as the `old' function. </li>
%% <li> The parameters of the function should all be variables. </li>
%% <li> If the function definition is a `case' expression, then the last expression of 
%%      every clause body of the `case' expression should be a simple expression 
%%      that syntactically can be used as a pattern expression. </li>
%% </ul>
%% Apart from the adaptor functions, an adaptor module should also export a special function 
%% `old_api_module_name/0' which returns an atom representing  the name of the module to 
%% which the old API functions belong. As a result, an adaptor module can only contain adaptor 
%% functions for API functions from the same module.
%% 
%% Some example adaptor modules: 
%%<ul>
%%<li>
%%<a href="file:regexp_re.erl"> From regexp to re;</a>.
%%</li>
%%<li>
%%<a href="file:keysearch_keyfind.erl"> From lists:keysearch/3 to lists:keyfind/3.</a>.
%%</li>
%%</ul>
-module(refac_api_migration).

-export([do_api_migration/5,
         generate_rule_based_api_migration_mod/2]).

-export([mk_new_var/2, mk_str/2,
         simplify_expr/2, simplify_match_expr/2]).

-include("../include/wrangler.hrl").

-define(INTERNAL_RULE(Before, After, Cond),
        fun()->
                api_refac:check_collect_template(Before, 'RULE'),
                {rule, fun(_W_File_, _W_Node_) ->
                               _W_NewCond=fun(_W_Bind_) -> 
                                                  api_refac:make_cond(Cond, _W_Bind_)
                                          end,
                               case api_refac:match(Before, _W_Node_, _W_NewCond) of
                                   {true, _W_Bind1_} ->
                                       _This@=_W_Node_,
                                       _File@=_W_File_,
                                       api_refac:generate_bindings(Before, '_W_Bind1_'),
                                       _W_After = fun()->After end(),
                                       {wrangler_misc:reset_pos_and_range(_W_After), true};
                                   false ->{_W_Node_, false}
                               end 
                       end, Before} 
        end()).
%% %%@private
%% -spec behaviour_info(atom()) ->[{atom(), arity()}].
%% behaviour_info(callbacks) ->
%%     [{old_apis, 0}, {meta_rule_set,0}, {simple_rule_set, 0}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                   %%
%%   Apply Rule-based API migration.                                 %%
%%                                                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(do_api_migration(FileOrDirs::[filename()|dir()],CallBackMod::module(), 
                       SearchPaths::[filename()|dir()], 
                       Editor::atom(), TabWidth::integer()) -> 
             {error, string()}| {ok, [{filename(), filename(), string()}]}).
   
do_api_migration(FileOrDirs, CallBackMod, SearchPaths, Editor, TabWidth)->
    ?wrangler_io("\nCMD: ~p:do_api_migration(~p, ~p, ~p, ~p, ~p).\n",
                  [?MODULE, FileOrDirs, CallBackMod, SearchPaths, Editor, TabWidth]),
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
    ?wrangler_io("The current file under refactoring is:\n~p\n", [File]),
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
    CalledFuns = ordsets:to_list(wrangler_callgraph_server:called_funs(Form)),
    case CalledFuns -- OldMFAs of 
        CalledFuns -> 
            {Form, false};
        _ ->
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
            end
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
            try wrangler_ast_server:parse_annotate_file(FileName, true) of 
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
                                    {error, {E2, erlang:get_stacktrace()}}
                            end;
                        _ -> {error, "Wrangler could not infer the module name of the file supplied"}                
                    end
            catch
                _E1:E2 ->
                    {error, mk_str("File ~p does not compile: ~p!",[FileName, E2])}
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
        {error, Reason} -> {error, Reason}
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
                    Body = wrangler_syntax:clause_body(C),
                    case Body of 
                        [_] ->
                            do_process_api_interface_funs_2(OldAPIModName,Form);
                        _ ->
                            %% This condition could be relaxed.
                            throw({error, mk_str("API interface function  ~p:~p/~p contains more than one expression"
                                                 " statement in the function clause body.",
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
        infix_expr ->
            do_generate_rule_for_application({OldAPIModName,F,A},Pats, B);
        case_expr->
            Cs = wrangler_syntax:case_expr_clauses(B),
            case valid_clause_body({M,F,A},Cs) of 
                true ->
                    do_generate_rule_for_case_expr({OldAPIModName,F,A}, Pats,B);
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
            {error, mk_str("The API interface function ~p:~p/~p "
                           "contains an invalid return expression.", [M,F,A])}
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
    Cs = wrangler_syntax:case_expr_clauses(B),
    case need_a_rule_for_match(Cs) of 
        true ->
            MatchRule = do_generate_meta_rule_for_match_expr({M,F,A},Pats,B,'META_MATCH_RULE'),
            {[{M,F,A}],[MetaRule, MatchRule], [SimpleRule]};
        _  -> 
            {[{M,F,A}],[MetaRule], [SimpleRule]}

    end.

-spec(do_generate_meta_rule_for_case_expr({modulename(), functionname(),arity()},
                                          Pats::[syntaxTree()],Body::syntaxTree(), 
                                          RuleNacroName::atom())
      -> {atom(),string()}).            
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
    UsedVars = all_vars(CaseExpr) -- vars_in_guard(Cs),
    {NewBody, Cond} = gen_new_body_and_cond(Body, UsedVars, case_expr),
    NewVars = collect_new_vars(remove_last_expr(Body), UsedVars),
    NewVarDefExprs = mk_new_var_define_exprs(NewVars, vars_in_guard(Cs)),
    TempAfter = mk_new_code_temp(NewBody, NewVars, 'META_RULE'),
    NewCode = wrangler_syntax:block_expr(NewVarDefExprs ++ [TempAfter]),
    Rule=mk_rule(RuleFunName,RuleMacroName, TempBefore, Cond, NewCode),
    {RuleFunName, Rule}.

vars_in_guard(Cs) ->
    Vs =lists:append([begin 
                          Guard = wrangler_syntax:clause_guard(C),
                          case Guard of 
                              none -> [];
                              _ ->
                                  collect_vars(Guard)
                          end
                      end||C<-Cs]),
    [list_to_atom(atom_to_list(V)++"@")||V<-Vs].

collect_vars(Node) ->
    ?STOP_TD_TU([?COLLECT(?T("V@"), wrangler_syntax:variable_name(V@),
                          api_refac:type(V@)==variable)], Node).
                     
-spec(do_generate_simple_rule_for_case_expr({modulename(), functionname(),arity()},
                                            Pats::[syntaxTree()],Body::syntaxTree(),
                                            RuleMacroName::atom()) 
      -> {atom(), string()}).  
do_generate_simple_rule_for_case_expr({M,F,_A},Pats,Body,RuleMacroName) ->
    RuleFunName=list_to_atom(atom_to_list(F)++"_rule"),
    NewPats = to_meta_vars(Pats),
    M1 = wrangler_syntax:atom(M),
    F1 = wrangler_syntax:atom(F),
    AppExpr=wrangler_syntax:application(M1, F1,NewPats),
    TempBefore = mk_old_code_temp(AppExpr),
    UsedVars = all_vars(AppExpr),
    {NewBody, Cond} = gen_new_body_and_cond(Body, UsedVars, application),
    NewVars = collect_new_vars(remove_guard(Body), UsedVars),
    NewVarDefExprs = mk_new_var_define_exprs(NewVars,[]),
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
    Cs = wrangler_syntax:try_expr_clauses(B),
    case need_a_rule_for_match(Cs) of 
        true ->
            MatchRule = do_generate_meta_rule_for_match_expr({M,F,A},Pats,B,'META_MATCH_RULE'),
            {[{M,F,A}],[MetaRule, MatchRule], [SimpleRule]};
        _  -> 
            {[{M,F,A}],[MetaRule], [SimpleRule]}

    end.

-spec(do_generate_meta_rule_for_try_expr({modulename(), functionname(),arity()},
                                         Pats::[syntaxTree()],Body::syntaxTree(), 
                                         RuleNacroName::atom())
      -> {atom(), string()}).            
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
    NewVars = collect_new_vars(remove_last_expr(Body), UsedVars),
    NewVarDefExprs = mk_new_var_define_exprs(NewVars,[]),
    TempAfter = mk_new_code_temp(NewBody, NewVars, 'META_RULE'),
    NewCode = wrangler_syntax:block_expr(NewVarDefExprs ++ [TempAfter]),
    Rule=mk_rule(RuleFunName,RuleMacroName, TempBefore, Cond, NewCode),
    {RuleFunName, Rule}.

-spec(do_generate_simple_rule_for_try_expr({modulename(), functionname(),arity()},
                                           Pats::[syntaxTree()],Body::syntaxTree(),
                                           RuleMacroName::atom()) 
      -> {atom(), string()}).  
do_generate_simple_rule_for_try_expr({M,F,_A},Pats,Body,RuleMacroName) ->
    RuleFunName=list_to_atom(atom_to_list(F)++"_rule"),
    NewPats = to_meta_vars(Pats),
    M1 = wrangler_syntax:atom(M),
    F1 = wrangler_syntax:atom(F),
    AppExpr=wrangler_syntax:application(M1, F1,NewPats),
    TempBefore = mk_old_code_temp(AppExpr),
    UsedVars = all_vars(AppExpr),
    {NewBody, Cond} = gen_new_body_and_cond(Body, UsedVars, application),
    NewVars = collect_new_vars(remove_guard(Body), UsedVars),
    NewVarDefExprs = mk_new_var_define_exprs(NewVars,[]),
    TempAfter = mk_new_code_temp(NewBody, NewVars, 'RULE'),
    NewCode = wrangler_syntax:block_expr(NewVarDefExprs ++ [TempAfter]),
    Rule=mk_rule(RuleFunName, RuleMacroName, TempBefore, Cond, NewCode),
    {RuleFunName, Rule}.


-spec(do_generate_meta_rule_for_match_expr({modulename(), functionname(),arity()},
                                           Pats::[syntaxTree()],Body::syntaxTree(),
                                           RuleMacroName::atom()) 
      -> {atom, string()}).  
do_generate_meta_rule_for_match_expr({M,F,_A},Pats,Body,RuleMacroName) ->
    RuleFunName=list_to_atom(atom_to_list(F)++"_match_rule"),
    NewPats = to_meta_vars(Pats),
    M1 = wrangler_syntax:atom(M),
    F1 = wrangler_syntax:atom(F),
    AppExpr=wrangler_syntax:application(M1, F1,NewPats),
    MatchPat= wrangler_syntax:variable("MatchLeftExpr@"),
    TempBefore = mk_old_code_temp(
                   wrangler_syntax:match_expr(MatchPat,AppExpr)),
    UsedVars = all_vars(AppExpr),
    {NewBody0, Cond} = gen_new_body_and_cond(Body, UsedVars, application),
    NewBody = wrangler_syntax:match_expr(MatchPat, NewBody0),
    NewVars = collect_new_vars(remove_last_expr(Body), UsedVars),
    NewVarDefExprs = mk_new_var_define_exprs(NewVars,[]),
    TempAfter = mk_new_code_temp(NewBody, NewVars, 'RULE'),
    NewCode = wrangler_syntax:block_expr(NewVarDefExprs ++ [TempAfter]),
    Rule=mk_rule(RuleFunName, RuleMacroName, TempBefore, Cond, NewCode),
    {RuleFunName, Rule}.

need_a_rule_for_match(Cs) ->
    not lists:member(false, [need_a_rule_for_match_1(C)||C<-Cs]).

need_a_rule_for_match_1(C) ->
    B = wrangler_syntax:clause_body(C),
    case lists:reverse(B) of 
        [E] ->
            api_refac:free_vars(E) == [];
        [E|_]->
            wrangler_syntax:type(E) == tuple
    end.

%%-spec(mk_old_code_temp(Expr::syntaxTree())-> syntaxTree()).
mk_old_code_temp(Expr) ->
    wrangler_syntax:macro(
      wrangler_syntax:variable('T'),
      [wrangler_syntax:atom(wrangler_quote_before),
       Expr,
       wrangler_syntax:atom(wrangler_quote_after)]).

-spec(mk_new_code_temp(Expr::syntaxTree(),NewVars::[atom()],
                       RULE::'META_RULE' |'RULE')-> {tree, any(), any(), any()}). 
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

-spec(make_case_clause(I::integer(), C::syntaxTree()) -> {tree, any(), any(), any()}).
make_case_clause(I, C) ->
    Bs = wrangler_syntax:clause_body(C),
    LastB = to_meta_vars(lists:last(Bs)),
    Guard = wrangler_syntax:variable(list_to_atom("Guard"++integer_to_list(I)++"@@")),
    Body = wrangler_syntax:variable(list_to_atom("Body"++integer_to_list(I)++"@@")),
    wrangler_syntax:clause([LastB], Guard, [Body]).

to_meta_vars(Node) ->
    {ok, NewNode} = ?STOP_TD_TP([to_meta_var_rule()], Node),
    NewNode.

to_meta_var_rule() ->    
    ?INTERNAL_RULE(?T("V@"),
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
    Guard0 = wrangler_syntax:clause_guard(C),
    GuardName = list_to_atom("Guard" ++ integer_to_list(Nth) ++ "@@"),
    Guard = if Guard0 == none -> 
                    wrangler_syntax:variable(GuardName);
               true ->[Guard0, wrangler_syntax:variable(GuardName)]
            end,
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
   
mk_new_var_define_exprs(NewVars, VarsInGuard) ->
    Vars = sets:to_list(sets:from_list(NewVars)),
    [mk_new_var_define_exprs_1(Var, VarsInGuard)||Var<-Vars].

mk_new_var_define_exprs_1(Var, VarsInGuard) ->
   MetaVar=list_to_atom(atom_to_list(Var)++"@"),
   case lists:member(MetaVar, VarsInGuard) of 
       true ->
           mk_new_var_define_expr_2(Var, MetaVar);
       false ->
           mk_new_var_define_expr_2(Var)
   end.

mk_new_var_define_expr_2(Var, MetaVar) ->
    wrangler_syntax:match_expr(
      wrangler_syntax:variable(Var),
      wrangler_syntax:case_expr(wrangler_syntax:application(wrangler_syntax:atom(wrangler_syntax),
                                                            wrangler_syntax:atom(type),
                                                            [wrangler_syntax:variable(MetaVar)]),
                                [wrangler_syntax:clause([wrangler_syntax:atom(underscore)], none,
                                                        [wrangler_syntax:application(
                                                          wrangler_syntax:atom(refac_api_migration),
                                                          wrangler_syntax:atom(mk_new_var),
                                                          [wrangler_syntax:string(atom_to_list(Var)),
                                                           wrangler_syntax:variable('_File@')])]),
                                 wrangler_syntax:clause([wrangler_syntax:underscore()], none, 
                                                        [wrangler_syntax:string(atom_to_list(MetaVar))])])).

mk_new_var_define_expr_2(Var)->           
    wrangler_syntax:match_expr(
      wrangler_syntax:variable(Var),
      wrangler_syntax:application(
        wrangler_syntax:atom(refac_api_migration),
        wrangler_syntax:atom(mk_new_var),
        [wrangler_syntax:string(atom_to_list(Var)),
         wrangler_syntax:variable('_File@')]
         )).
    
%%@private
mk_str(Str, Args) ->
    lists:flatten(io_lib:format(Str, Args)).



-spec(replace_quote_place_holder(FormStr::string()) -> string()).
replace_quote_place_holder(FormStr) ->
    FormStr1 =re:replace(FormStr, "wrangler_quote_before[\t\n\r\s]*,[\t\n\r\s]*", "\"", 
                         [{return, list}, global]),
    re:replace(FormStr1, ",[\t\n\r\s]*wrangler_quote_after", "\"", 
               [{return, list}, global]).

%%@private        
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

%%@private
simplify_expr(NewExpr, OldExpr) ->
    NewExprStr = api_refac:pp(NewExpr),
    NewExpr1 = wrangler_misc:parse_annotate_expr(NewExprStr),
    {OldBoundVars,_} = lists:unzip(bound_vars(OldExpr)),
    {NewBoundVars,_}= lists:unzip(api_refac:bound_vars(NewExpr1)),
    NewVars = NewBoundVars--OldBoundVars,
    NewExpr2=if NewVars==[] -> NewExpr;
                true ->
                     case wrangler_syntax:type(NewExpr) of 
                         match_expr ->
                             simplify_match_expr(NewExpr, NewVars);
                         _ ->
                             simplify_expr_1(NewExpr, NewVars)
                     end
             end,
    {ok, NewExpr3} =?FULL_TD_TP([is_tuple_rule()], NewExpr2),
    NewExpr3.

simplify_expr_1(Expr, NewVars) ->
    NewExpr = simplify_expr_2(Expr, NewVars),
    case NewExpr==Expr of 
        true ->
            Expr;
        false ->
            simplify_expr_1(NewExpr, NewVars)
    end.

%%@private
simplify_match_expr(Expr, NewVars) ->
    Left = wrangler_syntax:match_expr_pattern(Expr),
    Right = wrangler_syntax:match_expr_body(Expr),
    case wrangler_syntax:type(Left) of
        tuple ->
            Es = wrangler_syntax:tuple_elements(Left),
            EsWithIndex =lists:zip(lists:seq(1, length(Es)), Es),
            Underscores=[Index||{Index, E}<-EsWithIndex,
                                wrangler_syntax:type(E)==underscore],
            case Underscores of 
                [] ->
                    Expr;
                _ ->
                    {NewRight, RemovedIndexes}=do_simplify_match_expr(Right, Underscores, NewVars),
                    case RemovedIndexes of 
                        [] ->
                            Expr;
                        _ ->
                            NewLeft=[E||{Index, E}<-EsWithIndex, 
                                        not lists:member(Index, RemovedIndexes)],
                            case NewLeft of 
                                [E] -> wrangler_syntax:match_expr(E, NewRight);
                                _ -> wrangler_syntax:match_expr(
                                       wrangler_syntax:tuple(NewLeft), NewRight)
                            end
                    end
            end;
        _ -> Expr
    end.

simplify_expr_2(Expr, Vars)->
    Expr1=wrangler_syntax_lib:annotate_expr(wrangler_misc:reset_ann(Expr)),
    F = fun(Node, _Others) ->
                case wrangler_syntax:type(Node) of 
                    variable ->
                        VarName = wrangler_syntax:variable_name(Node),
                        case api_refac:var_refs(Node) ==[] andalso lists:member(VarName, Vars) of 
                            true ->
                                {wrangler_syntax:underscore(), true};
                            false ->
                                {Node, false}
                        end;
                    match_expr -> 
                        Pattern = wrangler_syntax:match_expr_pattern(Node),
                        case wrangler_syntax:type(Pattern) of 
                            underscore ->
                                {wrangler_syntax:empty_node(),true};
                            tuple ->
                                Es = wrangler_syntax:tuple_elements(Pattern),
                                case lists:all(fun(E)-> 
                                                       wrangler_syntax:type(E)== underscore 
                                               end, Es) of 
                                    true ->
                                        {wrangler_syntax:empty_node(),true};
                                    false ->
                                        {Node, false}
                                end;
                            _ -> {Node, false}
                        end;
                    _ ->
                        {Node, false}
                end
        end,
    element(1,api_ast_traverse:full_tdTP(F, Expr1, [])).

                
do_simplify_match_expr(MatchExprRight, Indexes, NewVars) ->
    case wrangler_syntax:type(MatchExprRight) of
        case_expr ->
            Cs = wrangler_syntax:case_expr_clauses(MatchExprRight),
            {NewCs, RmIndexes} = do_simplify_match_expr_1(Cs, Indexes, NewVars),
            case RmIndexes of 
                [] -> {MatchExprRight, []};
                _ ->
                    Arg = wrangler_syntax:case_expr_argument(MatchExprRight),
                    {wrangler_syntax:case_expr(Arg, NewCs), RmIndexes}
            end;
        try_expr ->
            Cs = wrangler_syntax:try_expr_clauses(MatchExprRight),
            {NewCs, RmIndexes} = do_simplify_match_expr_1(Cs, Indexes, NewVars),
            case RmIndexes of 
                [] -> {MatchExprRight, []};
                _ ->
                    B = wrangler_syntax:try_expr_body(MatchExprRight),
                    H = wrangler_syntax:try_expr_handlers(MatchExprRight),
                    A = wrangler_syntax:try_expr_after(MatchExprRight),
                    {wrangler_syntax:try_expr(B, NewCs, H, A), RmIndexes}
            end;
        _ ->
            {MatchExprRight, []}
    end.
        
do_simplify_match_expr_1(Cs, Is, NewVars) ->
    do_simplify_match_expr_1(Cs, Is, NewVars, 0, []).

do_simplify_match_expr_1(Cs, [], _NewVars, _Num, Rmd) ->
    {Cs, Rmd};
do_simplify_match_expr_1(Cs, [I], NewVars, Num, Rmd) ->
    case do_simplify_match_expr_2(Cs, I, NewVars, Num) of 
        false -> {Cs, Rmd};
        NewCs ->
            {NewCs, [I|Rmd]}
    end;   
do_simplify_match_expr_1(Cs,[I|Is], NewVars, Num, Rmd) ->
    case do_simplify_match_expr_2(Cs, I, NewVars, Num) of 
        false ->
            do_simplify_match_expr_1(Cs, Is, NewVars,Num, Rmd);
        NewCs ->
            do_simplify_match_expr_1(NewCs, Is,NewVars, Num+1,[I|Rmd])
    end.
        

do_simplify_match_expr_2(Cs, I, NewVars, Num) ->
    Res=[do_simplify_match_expr_3(C,I, NewVars, Num)||C<-Cs],
    case lists:member(false, Res) of 
        true ->
            false;
        _ -> Res
    end.

do_simplify_match_expr_3(C, I, NewVars, Num) ->
    Pats = wrangler_syntax:clause_patterns(C),
    Body = wrangler_syntax:clause_body(C),
    case lists:reverse(Body) of 
        [_] ->
            false;
        [E|Es] ->
            case wrangler_syntax:type(E) of 
                tuple->
                    TupleEs = wrangler_syntax:tuple_elements(E),
                    NewTupleEs = lists:sublist(TupleEs, 1, I-Num-1)++
                        lists:nthtail(I-Num, TupleEs),
                    NewE=wrangler_syntax:tuple(NewTupleEs),
                    Body1= wrangler_syntax:block_expr(Es++[NewE]),
                    NewBody = wrangler_syntax:block_expr_body(
                                simplify_expr_1(Body1, NewVars)),
                    NewBody1 = [B||B<-NewBody, not is_empty_node(B)],
                    case length(NewBody1) == length(Es)+1 of 
                        true ->
                            false;
                        false ->
                            
                            wrangler_syntax:clause(Pats, none, NewBody1)
                    end;
                _ ->
                    false
            end
    end.
           
bound_vars(Nodes) when is_list(Nodes) ->
    lists:usort(lists:flatmap(fun (Node) -> 
                                      bound_vars(Node) 
                              end, Nodes));
bound_vars(Node) ->
    Fun = fun (N, Acc) ->
                  Ann = wrangler_syntax:get_ann(N),
                  case lists:keyfind(bound,1,Ann) of
                      {bound, Vs} ->
                          Vs++Acc;
                      false ->
                          Acc 
                  end
          end, 
    Vars=api_ast_traverse:fold(Fun, [], Node),
    lists:usort(Vars).
             
        
is_empty_node(Node) ->
    is_tree(Node) andalso wrangler_syntax:type(Node) == empty_node.
       
is_tree(Node) ->
    wrangler_syntax:is_tree(Node) orelse wrangler_syntax:is_wrapper(Node).


remove_guard(Expr) ->
    case wrangler_syntax:type(Expr) of 
        case_expr ->
            Arg=wrangler_syntax:case_expr_argument(Expr),
            Cs = wrangler_syntax:case_expr_clauses(Expr),
            NewCs = [remove_guard_1(C)||C<-Cs],
            wrangler_syntax:case_expr(Arg, NewCs);        
        try_expr ->
            B = wrangler_syntax:try_expr_body(Expr),
            H = wrangler_syntax:try_expr_handlers(Expr),
            A = wrangler_syntax:try_expr_after(Expr),
            Cs = wrangler_syntax:try_expr_clauses(B),
            NewCs = [remove_guard_1(C)||C<-Cs],                
            wrangler_syntax:try_expr(B, NewCs, H, A);
        _ ->
            Expr
    end.

remove_guard_1(C) ->
    Pats = wrangler_syntax:clause_patterns(C),
    Body = wrangler_syntax:clause_body(C),
    wrangler_syntax:clause(Pats, none, Body).


remove_last_expr(Expr) ->
    case wrangler_syntax:type(Expr) of 
        case_expr ->
            Arg=wrangler_syntax:case_expr_argument(Expr),
            Cs = wrangler_syntax:case_expr_clauses(Expr),
            NewCs = [remove_last_expr_1(C)||C<-Cs],
            wrangler_syntax:case_expr(Arg, NewCs);        
        try_expr ->
            B = wrangler_syntax:try_expr_body(Expr),
            H = wrangler_syntax:try_expr_handlers(Expr),
            A = wrangler_syntax:try_expr_after(Expr),
            Cs = wrangler_syntax:try_expr_clauses(B),
            NewCs = [remove_last_expr_1(C)||C<-Cs],                
            wrangler_syntax:try_expr(B, NewCs, H, A);
        _ ->
            Expr
    end.

remove_last_expr_1(C) ->
    Pats = wrangler_syntax:clause_patterns(C),
    Guard = wrangler_syntax:clause_guard(C),
    Body = wrangler_syntax:clause_body(C),
    Bs = lists:reverse(tl(lists:reverse(Body))),
    wrangler_syntax:clause(Pats, Guard, Bs).


is_tuple_rule()->
    ?RULE(?T("is_tuple({Es@@})"), wrangler_syntax:empty_node(), true).


