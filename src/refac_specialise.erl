%%@doc This module shows how to write refactorings 
%% use the Wrangler API. 

%% This refactoring specialise a function over a particular 
%% constant parameter of this function. 
%% To apply this refactoring, highlight the actual parameter 
%% a function application of the function under consideration,
%% then select 'Apply adhoc refactoring' from the menu, 
%% and Wrangler will prompt you to input the refactoring 
%% name, which is supposed to be the module name.
%% @private
-module(refac_specialise).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1, 
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../include/wrangler.hrl").

%% The Emacs mini-buffer prompts for the user input parameters. 
-spec (input_par_prompts/0::() -> [string()]).                           
input_par_prompts()->
    [].

%% Select the focus of interest. If no selection is neeeded, 
%% then return {ok, none}. When a proper actual parameter 
%% has been selected, this function returns the {M,F,A} of 
%% the function under consideration, the actual parameter 
%% selected, and the index the parameter selected.
-spec (select_focus/1::(#args{}) -> {ok, none}|{ok, syntaxTree()}|
                                    {error, string()}).
select_focus(#args{current_file_name=File, 
                   highlight_range={Start, End}}) ->
    case interface_api:range_to_node(File, {Start,End},fun is_expr/1) of 
        {ok, Expr} ->
            case interface_api:pos_to_node(
                   File, Start, fun(Node)->
                                        is_the_enclosing_app(Node, Expr)
                                end) of 
                {ok, App} ->
                    ?MATCH(?T("Op@(As@@)"), App),
                    {M, F, A}=refac_api:fun_define_info(Op@),
                    {As1,_As2}=lists:splitwith(fun(E)->E/=Expr end, As@@),
                    Nth = length(As1)+1,
                    {ok, {{M,F,A},Expr,Nth}};
                none ->
                    {error, "The expression selected is not "
                     "an argument of a function application."}
            end;
        none ->
            {error, "You have not selected an expression."}
    end.

%% Pre-condition checking.
-spec (check_pre_cond/1::(#args{}) -> ok).  
check_pre_cond(Args=#args{current_file_name=File, 
                          focus_sel={{M,_F,_A},_Expr,_Nth}}) ->
    case {ok, M}==refac_api:module_name(File) of 
        true ->   
            check_pre_cond_1(Args);
        false ->
            throw({error, "The function selected is not defined"
                   "in the current module."})
    end.        

check_pre_cond_1(Args=#args{focus_sel={_MFA, Expr, _Nth}}) ->
    case refac_api:free_vars(Expr) of 
        [] ->
            check_pre_cond_2(Args);
        _ ->
            throw({error, "The argument selected contains free variables."})
    end.

check_pre_cond_2(#args{current_file_name=File,
                       focus_sel={MFA,_Expr, Nth}}) ->
    FunDef= refac_api:mfa_to_fun_def(MFA, File),
    NthPars = ?FULL_TD_TU([?COLLECT(?T("f@(Args@@)-> Bs@@;"), 
                                    lists:nth(Nth, Args@@),
                                    true)],
                          FunDef),
    case lists:all(fun(P)-> 
                           refac_syntax:type(P)==variable
                   end, NthPars) of 
        true -> 
            ok;
        false ->
            throw({error, "Wrangler only supports specialisation over "
                   "a formal parameter that is a variable"})
    end.

selective()->
    false.

%% Do the actual program transformation here.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}
                                     | {error, term()}).    
transform(Args=#args{current_file_name=File,
                     focus_sel={{_M,F,A},_Expr, _Nth}})->
    InscopeFuns = refac_api:inscope_funs(File),
    NewFunName=case lists:member({F, A - 1}, InscopeFuns) of
                   true ->
                       make_new_fun_name({F,A - 1}, InscopeFuns);
                   false ->
                       atom_to_list(F)
               end,
    case refac_api:is_exported({F,A}, File) of 
        true ->
            {ok, Res}=transform_in_client_files(Args, NewFunName),
            case Res of 
                [] ->
                    %% no client files have been changed.
                    transform_in_cur_file(Args, NewFunName, false);
                _ ->
                    %% some clients files have been changed.
                    {ok, Res1}=transform_in_cur_file(Args, NewFunName, true),
                    {ok, Res1++Res}
            end;
        false ->
            %% function is not exported.
            transform_in_cur_file(Args, NewFunName, false)
    end.

transform_in_cur_file(Args=#args{current_file_name=File}, NewFunName, true)->
    ?STOP_TD_TP([rule0(Args,NewFunName),
                 rule1(Args, NewFunName), 
                 rule2(Args, NewFunName), 
                 rule3(Args, NewFunName)
                ], [File]);
transform_in_cur_file(Args=#args{current_file_name=File}, NewFunName, false) ->
    ?STOP_TD_TP([rule0(Args, NewFunName),
                 rule1(Args, NewFunName),
                 rule2(Args,NewFunName)], [File]).

transform_in_client_files(Args=#args{current_file_name=File,
                                     search_paths=SearchPaths}, 
                          NewFunName) ->
    ?FULL_TD_TP([rule0(Args, NewFunName),
                 rule1(Args, NewFunName)], 
                refac_api:client_files(File, SearchPaths)).

%% transformation rule:
%% remove the nth argument from a qualified application.
rule0(Args=#args{focus_sel={{M,F,A}, Expr, Nth}}, NewFunName) ->
    ?RULE(?T("M@:F@(Args@@)"),
          begin
              NewArgs@@=delete(Nth,Args@@),
              {ok,NewArgs1@@}=?FULL_TD_TP([rule0(Args, NewFunName),
                                           rule1(Args, NewFunName)], NewArgs@@), 
              ?QUOTE("M@:"++NewFunName++"(NewArgs1@@)")
          end,
          refac_api:fun_define_info(F@)=={M,F,A} andalso
          ?EQUAL(lists:nth(Nth, Args@@), Expr)).

%% transformation rule:
%% remove the nth argument from an unqualified application.
rule1(Args=#args{focus_sel={{M,F,A}, Expr, Nth}}, NewFunName) ->
    ?RULE(?T("F@(Args@@)"),
          begin
              NewArgs@@=delete(Nth,Args@@),
              {ok,NewArgs1@@}=?FULL_TD_TP([rule0(Args, NewFunName),
                                           rule1(Args, NewFunName)], NewArgs@@), 
              ?QUOTE(NewFunName++"(NewArgs1@@)")
          end,
          refac_api:fun_define_info(F@)=={M,F,A} andalso
          ?EQUAL(lists:nth(Nth, Args@@), Expr)).

%% transformation rule:
%% insert the new function right after the original one. 
rule2(Args=#args{focus_sel={{M,F,A},_Expr,_Nth}}, NewFunName) ->
    ?RULE(?T("F@"),
          begin
              NewFun=generate_specialised_fun(Args, F@, NewFunName),
              {ok,NewF@}= ?FULL_TD_TP([rule0(Args, NewFunName),
                                       rule1(Args, NewFunName)], F@), 
              [NewF@, NewFun]
          end,
          refac_syntax:type(F@)==function andalso 
          refac_api:fun_define_info(F@)=={M,F,A}).

%% transformation rule:
%% add the new function to export list.
rule3(_Args=#args{focus_sel={{_M,F,A},_Expr,_Nth}}, NewFunName) ->
    ?RULE(?T("F@"),
          refac_api:add_to_export_after(F@, {NewFunName, A-1}, {F,A}),
          refac_api:is_attribute(F@, export)).

generate_specialised_fun(Args, FunDef, NewFunName) ->
    {ok,NewFunDef}=?FULL_TD_TP([rule4(Args, NewFunName)], FunDef),
    NewFunDef.

rule4(Args=#args{focus_sel={{_M,F,A}, _Expr, Nth}}, NewFunName) ->
    ?RULE(?T("f@(Args@@) -> Bs@@;"), 
          begin NewArgs@@=delete(Nth, Args@@),
                NthPar = lists:nth(Nth, Args@@),
                NewBs@@=transform_in_body(Args,Bs@@,NthPar, NewFunName),
                ?QUOTE(NewFunName++"(NewArgs@@)->NewBs@@;")
          end,
          length(Args@@) == A andalso refac_syntax:is_atom(F@, F)).

transform_in_body(Args, Body, NthPar, NewFunName) ->
    {ok, Body1}=?FULL_TD_TP([rule5(Args,NthPar,NewFunName),
                             rule6(Args, NthPar, NewFunName)], Body),
    Body1.

rule5(#args{focus_sel={{M,F,A}, Expr, Nth}},NthPar, NewFunName) ->
    ?RULE(?T("F@(Args@@)"),
          begin
              NewArgs@@=delete(Nth, Args@@),
              ?QUOTE(NewFunName++"(NewArgs@@)")
          end,
          refac_api:fun_define_info(F@)=={M,F,A} andalso
          check_nth_arg(Args@@, Expr, Nth, NthPar)).

%% replace the use of the formal parameter with the expression selected.
rule6(#args{focus_sel={_MFA,Expr,_Nth}}, NthPar, _NewFunName) ->
    ?RULE(?T("V@"),
          Expr,
          refac_syntax:type(V@)==variable andalso 
          refac_api:variable_define_pos(V@) == 
              refac_api:variable_define_pos(NthPar)).

%% some utility functions.

check_nth_arg(ArgList, Expr, Nth, NthPar) ->
    NthArg=lists:nth(Nth, ArgList),
    ?EQUAL(NthArg, Expr) 
        orelse
          (refac_syntax:type(NthArg) == variable andalso
           refac_api:variable_define_pos(NthPar) ==
               refac_api:variable_define_pos(NthArg)).

is_expr(Node) ->
    refac_api:syntax_category(Node)==expression.

is_the_enclosing_app(Node, Expr) ->
    refac_syntax:type(Node) == application andalso
        lists:member(Expr, refac_syntax:application_arguments(Node)).

delete(N, List)->
    {L1,L2}=lists:split(N-1, List),
    L1++tl(L2).

make_new_fun_name({OldName, Arity},InscopeFuns) ->
    NewName=list_to_atom(atom_to_list(OldName)++"_1"),
    case lists:member({NewName,Arity}, InscopeFuns) of
	true ->
	    make_new_fun_name({NewName, Arity}, InscopeFuns);
	_ -> 
	    atom_to_list(NewName)
    end.
