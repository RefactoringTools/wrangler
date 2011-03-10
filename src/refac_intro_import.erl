-module(refac_intro_import).

%-behaviour(gen_refac).

-export([input_pars/0, select_focus/1, 
         pre_cond_check/1, transform/1]).

-include("../include/gen_refac.hrl").

%% The Emacs mini-buffer prompts for the user input parameters. 
-spec (input_pars/0::() -> [string()]).                           
input_pars()->
    ["Module name:"].

%% Select the focus of interest. If no selection is neeeded, 
%% then return {ok, none}.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
                           
select_focus(_Args) ->
    {ok, none}.
    
%% Pre-condition checking to ensure that the refactoring preserves the 
%% behaviour of the program.
-spec (pre_cond_check/1::(#args{}) -> ok).  
pre_cond_check(Args=#args{user_inputs=[ModuleName]}) ->
    case collect_uses(Args) of 
        [] ->
            Msg =lists:flatten(io_lib:format(
                                 "There is no qualified uses of "
                                 "functions from module '~s'.",
                                 [ModuleName])),
            throw({error, Msg});
        _-> ok
    end.
   
%%Do the actual program transformation here.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}
                                     | {error, term()}).    
transform(Args=#args{current_file_name=File,
                     user_inputs=[ModuleName]}) ->
    FAs=collect_uses(Args),
    FAs1 = imported_funs(File, ModuleName),
    FunsToImport=FAs--FAs1,
    {ok,AST} = refac_api:get_ast(File),
    case FunsToImport of 
        [] ->
            ?FOREACH([rule(Args)], {File,AST});
        _ ->
            Import=make_import_attr(ModuleName, FunsToImport),
            AST1=refac_api:insert_an_attr(AST,Import),
            ?FOREACH([rule(Args)], {File,AST1})
    end.
  
collect_uses(_Args=#args{current_file_name=File,
                         user_inputs=[ModuleName]}) ->
    ?COLLECT("M@:F@(Args@@)", ?SPLICE(M@)==ModuleName, 
             {list_to_atom(?SPLICE(F@)), length(Args@@)},
             [File]).

rule(_Args=#args{user_inputs=[ModuleName]}) ->
    ?RULE("M@:F@(Args@@)",?QUOTE("F@(Args@@)"),?SPLICE(M@)==ModuleName).

imported_funs(File, ModuleName) ->    
    {ok, ModInfo} = refac_api:get_module_info(File),
    case lists:keyfind(imports,1,ModInfo) of 
        {imports, MFAs} ->
            case lists:keyfind(list_to_atom(ModuleName), 1, MFAs) of 
                {_, FAs}->FAs;
                _ -> []
            end;
        _ -> []
    end.

make_import_attr(ModuleName, FAs) ->
    ?QUOTE("-import("++ModuleName++","++format_fa_list(FAs)++").").

format_fa_list([]) ->
    "[]";
format_fa_list(FAs) ->
    "["++lists:flatten(format_fas(FAs))++"]".

format_fas([]) ->
    "";
format_fas([{F,A}]) ->
    io_lib:format("~p/~p", [F,A]);
format_fas([{F,A}|T]) ->
    io_lib:format("~p/~p,", [F,A]) ++
        format_fas(T).
