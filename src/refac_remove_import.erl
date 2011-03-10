-module(refac_remove_import).

%-behaviour(gen_refac).

-export([input_pars/0, select_focus/1, 
         pre_cond_check/1, transform/1]).

-include("../include/gen_refac.hrl").

%% The Emacs mini-buffer prompts for the user input parameters. 
-spec (input_pars/0::() -> [string()]).                           
input_pars()->["Module name:"].

%% Select the focus of interest. If no selection is neeeded, 
%% then return {ok, none}.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
                           
select_focus(_Args) ->{ok, none}.
    
%% Pre-condition checking to ensure that the refactoring preserves the 
%% behaviour of the program.
-spec (pre_cond_check/1::(#args{}) -> ok).  
pre_cond_check(_Args=#args{current_file_name=File, 
                           user_inputs=[ModuleName]}) ->
    case is_imported(File, ModuleName) of 
        true -> 
            ok;
        false ->
            throw({error, "The module specified is not imported"})
    end.

   
%%Do the actual program transformation here.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}
                                     | {error, term()}).    
transform(Args=#args{current_file_name=File})->
    ?FOREACH([rule1(Args), rule2(Args)], File).
  
rule1(_Args=#args{user_inputs=[ModuleName]}) ->
     ?RULE("F@(Args@@)",?QUOTE(ModuleName++":F@(Args@@)"),
           ModuleName == element(1,refac_api:fun_define_info(F@))).

rule2(_Args=#args{user_inputs=[ModuleName]}) ->
    M = list_to_atom(ModuleName),
    ?RULE("-import("++ModuleName++ "FAs@)", ?QUOTE(""),
           M == element(1,refac_api:fun_define_info(F@))).


%%utility functions.
is_imported(File, ModuleName) ->    
    {ok, ModInfo} = refac_api:get_module_info(File),
    case lists:keyfind(imports,1,ModInfo) of 
        {imports, MFAs} ->
            false/=lists:keyfind(list_to_atom(ModuleName), 1, MFAs);
        _ -> false
    end.

