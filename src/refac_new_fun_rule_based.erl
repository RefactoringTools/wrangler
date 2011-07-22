%%@private
%%@hidden
-module(refac_new_fun_rule_based).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../include/wrangler.hrl").

%% The Emacs mini-buffer prompts for the user input parameters. 
-spec (input_par_prompts/0::() -> [string()]).                           
input_par_prompts() ->
    ["New function name:"].

%% Select the focus of interest. If no selection is neeeded, 
%% then return {ok, none}.
-spec (select_focus/1::(#args{}) -> {ok, [syntaxTree()]}|{ok, none}).  
select_focus(Args=#args{current_file_name=File}) ->
    Sel =api_interface:pos_to_expr_list(File, Args#args.highlight_range),
    {ok, Sel}.

%% Pre-condition checking to ensure that the refactoring preserves the 
%% behaviour of the program.
-spec (check_pre_cond/1::(#args{}) -> ok).  
check_pre_cond(_Args=#args{current_file_name=File, focus_sel=Exprs,
                          user_inputs=[NewFunName]}) ->
    check_selection(Exprs),
    check_function_name(NewFunName),    
    check_name_conflict(File, Exprs, list_to_atom(NewFunName)),
    check_funcall_replaceable(Exprs).
   

check_selection(Exprs) when Exprs==[] ->
    throw({error, "You have not selected any expressions"});
check_selection(_Exprs) ->
    ok.

check_function_name(NewFunName) ->
    case api_refac:is_fun_name(NewFunName) of
        true ->
            ok;
        false ->
            throw({error, "Invalide function name!"})
    end.

check_name_conflict(File, Exprs, NewFunName) ->
    ModName = list_to_atom(filename:basename(File, ".erl")),
    FvVars = api_refac:free_var_names(Exprs),
    Arity = length(FvVars),
    InscopeFuns = api_refac:inscope_funs(File),
    case lists:member({ModName, NewFunName,Arity}, InscopeFuns) of 
        true ->
            throw({error, "The function is also inscope in this module."});
        false ->
            ok
    end.

check_funcall_replaceable([E]) ->
    Type= api_refac:syntax_context(E),
    case lists:member(Type, [application_op,
                             module_quailifier_argument,
                             module_qualifier_body]) of
        true ->
            throw({error, "The expression selected cannot "
             "be replaced by a function call."});
        false ->
            ok
    end;
check_funcall_replaceable([E|_Es]) ->
    P= api_refac:syntax_context(E),
    if P == body_expr -> 
            ok;
       true ->
            throw({error, "The expression sequence selected "
             "cannot be replaced by a function call."})
    end.


selective() ->
    false.

%% Do the actual program transformation here.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}).
                                     
transform(Args=#args{current_file_name=File}) ->
    ?FULL_TD_TP([rule1(Args)], [File]).

rule1(Args=#args{focus_sel=Exprs, user_inputs=[NewFunName]}) ->
    ?RULE(?T("F@"),
          begin
              ExVars = api_refac:exported_var_names(Exprs),
              Pars=format_pars(api_refac:free_var_names(Exprs)),
              {ok, F1}=?FULL_TD_TP([rule2(Args)],_This@),
              if ExVars == []-> 
                      [F1,
                       ?QUOTE(NewFunName++"("++Pars++") ->"
                              ++?SPLICE(Exprs))];
                 true->
                      [F1, ?QUOTE(NewFunName++"("++Pars++") ->"++?SPLICE(Exprs)++","
                                  ++format_tuple(ExVars)++".")]
              end 
          end,
          wrangler_syntax:type(F@) == function andalso contains(F@, Exprs)).

rule2(_Args=#args{focus_sel=Exprs, user_inputs=[NewFunName]}) ->
    ?RULE(?T("E@"), 
           begin
               ExVars=api_refac:exported_var_names(Exprs),
               FreeVars=api_refac:free_var_names(Exprs),
               if  ExVars==[] ->
                       ?QUOTE(NewFunName++"("++format_pars(FreeVars)++")");
                   true ->
                       ?QUOTE(format_tuple(ExVars)++ 
                                  "="++NewFunName++"("++format_pars(FreeVars)++")")
               end
           end,
           wrangler_misc:start_end_loc(E@) == wrangler_misc:start_end_loc(Exprs)
          ).
        
%% Some utility functions.
format_tuple([]) ->
     "{}";
format_tuple([V]) ->
     lists:flatten(io_lib:format("~s", [V]));
format_tuple(Vars) ->
     lists:flatten("{"++format_pars(Vars)++"}").


format_pars([]) -> "";
 format_pars(Vars) ->
     lists:flatten(format_pars_1(Vars)).
format_pars_1([V]) ->
     io_lib:format("~s", [V]);
format_pars_1([V|Vs]) ->
     io_lib:format("~s,", [V]) ++ format_pars_1(Vs).

contains(Expr1, Expr2) ->
     {Start1, End1} = wrangler_misc:start_end_loc(Expr1),
     {Start2, End2} = wrangler_misc:start_end_loc(Expr2),
     (Start1 =< Start2) andalso (End2 =< End1).

             
