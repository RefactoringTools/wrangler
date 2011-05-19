-module(refac_apply_to_remote_call).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../include/wrangler.hrl").

%% The Emacs mini-buffer prompts for the user input parameters. 
-spec (input_par_prompts/0::() -> [string()]).                           
input_par_prompts() -> [].

%% Select the focus of interest. If no selection is neeeded, 
%% then return {ok, none}.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) ->{ok, none}.

%% Pre-condition checking to ensure that the refactoring preserves the 
%% behaviour of the program.
-spec (check_pre_cond/1::(#args{}) -> ok).  
check_pre_cond(_Args) ->
    ok.

selective() ->
    true.

%%Do the actual program transformation here.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}
                                     | {error, term()}).    
transform(_Args=#args{search_paths=SearchPaths})->
    ?FULL_TD([rule()], SearchPaths).

rule() ->
    ?RULE("Op@(M@, F@, Args@)",
          begin
              ArgStr=?SPLICE(Args@),
              Args = lists:sublist(ArgStr,2,length(ArgStr)-2),
              ?QUOTE("M@:F@("++Args++")")
          end,
          {erlang,apply,3}==refac_api:fun_define_info(Op@) 
          andalso (refac_api:type(Args@)== list orelse
                   refac_api:type(Args@)==nil)).
    


     
