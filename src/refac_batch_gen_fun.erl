-module(refac_batch_gen_fun).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").

input_par_prompts() ->
    [].

%% Only an example, and is not complete yet.
%% This version only works if the expr appears only once in the function, and 
%% the function consists of only a single function clause and also that the 
%% ExprStr denotes a literal expression.
composite_refac(_Args=#args{current_file_name=CurFile,
                            user_inputs=[ExprStr],
                            search_paths=SearchPaths}) ->
    [{atomic, [{gen_fun, [CurFile, 
                          fun({_F,_A}) ->
                                  true
                          end,
                          fun(Expr) ->
                                  ?SPLICE(Expr)==ExprStr
                          end, 
                          {user_input, fun(_) ->"New parameter name: " end},
                          [SearchPaths], callgraph_bottom_up]}]}].
