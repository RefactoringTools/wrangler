%%@hidden
%%@private
-module(refac_batch_gen_fun).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").

input_par_prompts() ->
    ["Expression to be generalised over:"].


composite_refac(_Args=#args{current_file_name=CurFile,
                            user_inputs=[ExprStr],
                            search_paths=SearchPaths}) ->
    ?refac_(gen_fun, [CurFile, 
                      fun({_F,_A}) ->
                              true
                      end,
                      ExprStr,
                      {user_input, fun(_) ->"New parameter name: " end},
                      SearchPaths, bu]).

