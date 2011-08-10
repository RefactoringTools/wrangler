-module(refac_batch_inline_fun).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").

input_par_prompts() ->
    [].

%% Only an example, and is not complete yet.
%% This version only works if the expr appears only once in the function, and 
%% the function consists of only a single function clause and also that the 
%% ExprStr denotes a literal expression.

composite_refac(_Args=#args{current_file_name=CurFile, %% user_inputs=[M,F,A],
                            search_paths=SearchPaths}) ->
    Cond=fun() ->
                 collect_apps(CurFile, {test,t,0})/=[]
         end, 
    [{while, Cond,
      [{unfold_fun_app, [CurFile,fun()->
                                         Apps = collect_apps(CurFile, {test,t,0}),
                                         hd(Apps)
                                 end,
                         SearchPaths]}]}].

collect_apps(File, {M,F,A}) ->
    ?FULL_TD_TU([?COLLECT(?T("F@(Args@@)"),
                          element(1, api_refac:start_end_loc(_This@)),
                          api_refac:fun_define_info(F@)=={M,F,A})],
                [File]).
   
