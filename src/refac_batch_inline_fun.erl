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
composite_refac(_Args=#args{current_file_name=CurFile, user_inputs=[M,F,A],
                            search_paths=SearchPaths}) ->
    Apps=fun() ->
                 ?FULL_TD_TU([?COLLECT(?T("F@(Args@@)"), 
                                       wrangler_misc:get_pos(_This@),
                                       api_refac:fun_define_info(F@)=={M,F,A})],
                             [CurFile])
         end,
    [{while, Apps()/=[], 
      [{unfold_fun_app, [CurFile,hd(Apps()), SearchPaths]}]}].



%% ?LET(Apps, ?FULL_TD_TU([?COLLECT(?T("F@(Args@@)"), 
%%                                  wrangler_misc:get_pos(_This@),
%%                                  fun_define_info(F@)=={M,F,A})],
%%                        [CurFile]),
%%      {while, Apps/=[], [{unfold_fun_app, [CurFile, hd(Apps), SearchPaths]}
