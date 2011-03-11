-module(refac_modernize_fun_call).

%-behaviour(gen_refac).

-export([input_pars/0, select_focus/1, 
         pre_cond_check/1, transform/1]).

-include("../include/gen_refac.hrl").

%% The Emacs mini-buffer prompts for the user input parameters. 
-spec (input_pars/0::() -> [string()]).                           
input_pars()->["Old function name:", "Old module name: ", "Arity: ",
               "New function name: ", "New module name: "].

%% Select the focus of interest. If no selection is neeeded, 
%% then return {ok, none}.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) ->{ok, none}.

%% Pre-condition checking to ensure that the refactoring preserves the 
%% behaviour of the program.
-spec (pre_cond_check/1::(#args{}) -> ok).  
pre_cond_check(_Args)->
    ok.

%%Do the actual program transformation here.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}
                                     | {error, term()}).    
transform(Args=#args{search_paths=SearchPaths})->
    ?FOREACH([rule1(Args),
              rule2(Args),
              rule3(Args),
              rule4(Args)], SearchPaths).
  
rule1(_Args=#args{user_inputs=[OldFunName, OldModName, Arity, 
                               NewFunName, NewModName]}) ->
    ?RULE("M@:F@(Args@@)",
          ?QUOTE(NewModName++":"++NewFunName++"(Args@@)"),
          ?SPLICE(M@)==OldModName andalso 
          ?SPLICE(F@)==OldFunName andalso
          length(Args@@) ==list_to_integer(Arity)).

rule2(_Args=#args{user_inputs=[OldFunName, OldModName, Arity, 
                               NewFunName, NewModName]}) ->
    ?RULE("F@(Args@@)",
          ?QUOTE(NewModName++":"++NewFunName++"(Args@@)"),
          refac_api:fun_define_info(F@)==
              {list_to_atom(OldModName),list_to_atom(OldFunName),Arity}).

rule3(_Args=#args{user_inputs=[OldFunName, OldModName, Arity, 
                               _NewFunName, _NewModName]}) ->
    M = list_to_atom(OldModName), 
    F = list_to_atom(OldFunName),
    A = list_to_integer(Arity),
    ?RULE("A@",
          refac_api:remove_from_import(A@, {F, A}),
          refac_api:is_import_attribute(A@, M)).

rule4(_Args=#args{user_inputs=[OldFunName, OldModName, Arity, 
                               NewFunName, NewModName]}) ->
    M = list_to_atom(OldModName), 
    F = list_to_atom(OldFunName),
    A = list_to_integer(Arity),
    ?RULE("Fun@(Args@@, M@, F@, Args2@)",
          ?QUOTE("Fun@(Args@@, "++NewModName++","++NewFunName++",Args2@)"),
          case refac_api:fun_define_info(Fun@) of
              {erlang, apply, _} -> true;
              _ -> false
          end andalso
          refac_api:fun_define_info(F@)=={M,F,A)).
         

%% more rules to be added  to handle meta fun calls and implicit funcalls
