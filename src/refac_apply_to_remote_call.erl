-module(refac_apply_to_remote_call).

%-behaviour(gen_refac).

-export([input_pars/0, select_focus/1, 
         pre_cond_check/1, transform/1]).

-include("../include/gen_refac.hrl").

%% The Emacs mini-buffer prompts for the user input parameters. 
-spec (input_pars/0::() -> [string()]).                           
input_pars()->[].

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
transform(_Args=#args{search_paths=SearchPaths})->
    ?FOREACH([rule()], SearchPaths).

rule() ->
    ?RULE("Op@(M@, F@, Args@)",
          begin
              ArgStr=?SPLICE(Args@),
              Args = lists:sublist(ArgStr,2,length(ArgStr)-2),
              ?QUOTE("M@:F@("++Args++")")
          end,
          {erlang,apply,3}==refac_api:fun_define_info(Op@) 
          andalso refac_syntax:type(Args@)== list).


     
