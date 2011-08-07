-module(refac_batch_clone_removal_2).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").

input_par_prompts() ->
    ["Source File/Module Name: ", "Target File/Module Name: "].

%% TODO: Need to think about SCCs.    
composite_refac(_Args=#args{user_inputs=[File1, File2],search_paths=SearchPaths}) ->
    [{non_atomic, {move_fun, [{File1,
                               fun(_Fun) -> true end,
                               fun(_Arity) -> true end
                              }, 
                              File2,
                              [SearchPaths], callgraph_bottom_up]}}].
      
