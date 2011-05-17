%%@doc This module shows how to write refactorings 
%% use the Wrangler API. 

%% The refactoring implemented in this module removes
%% the import attributes importing a user specified
%% module, and qualify the calls to those functions 
%% imported from that module.

-module(refac_remove_import).

-behaviour(gen_refac).

%% export of callback function.
-export([input_pars/0, select_focus/1, 
         pre_cond_check/1, selective/0,
         transform/1]).

-include("../include/gen_refac.hrl").

%% The user needs to input the module name. 
-spec (input_pars/0::() -> [string()]).                           
input_pars()->["Module name:"].

%% No focus selection is needed.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) ->{ok, none}.

%% Pre-condition checking.
-spec (pre_cond_check/1::(#args{}) -> ok|{error, term()}).  
pre_cond_check(_Args=#args{current_file_name=File, 
                           user_inputs=[ModuleName]}) ->
    case is_imported(File, ModuleName) of 
        true -> 
            ok;
        false ->
            {error, "The module specified is not imported"}
    end.

selective()->
    false.

%%Do the actual program transformation here.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}
                                     | {error, term()}).    
transform(Args=#args{current_file_name=File})->
    ?FULL_TD([rule1(Args),
              rule2(Args)], [File]).

%% qualify function calls.
rule1(_Args=#args{user_inputs=[ModuleName]}) ->
    ?RULE("F@(Args@@)",
          ?QUOTE(ModuleName++":F@(Args@@)"),
          refac_syntax:type(F@)/=module_qualifier andalso
          list_to_atom(ModuleName)== element(1,refac_api:fun_define_info(F@))).


%% remove import attributes related.
rule2(_Args=#args{user_inputs=[ModuleName]}) ->
    ?RULE("A@", ?QUOTE(""),
          refac_api:is_import(A@, list_to_atom(ModuleName))).

%%utility functions.
is_imported(File, ModuleName) ->
    refac_api:imported_funs(File, ModuleName)/=[].

