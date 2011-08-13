-module(refac_batch_clone_removal_1).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").
 
input_par_prompts() ->
    ["Module name: ", "Function name: ", "Arity: "].

composite_refac(_Args=#args{user_inputs=[M, F, A],search_paths=SearchPaths}) ->
    [{interactive, {rename_fun,
                    [{M, F, A}, 
                     {user_input,  fun(_)->
                                           "New funcion name: " 
                                   end},
                     SearchPaths]}},
     {rename_var,
      [{old, {M,F,A}}, 
       fun(X) ->
               re:run(atom_to_list(X), "NewVar*")/=nomatch
       end, 
       {user_input, fun({_, _, _, V}) ->
                            lists:flatten(io_lib:format("Rename variable ~p with: ", [V]))
                    end},
       SearchPaths]},
     {repeat_interactive, {swap_args, 
                           [{old, {M,F,A}},  
                            {user_input, fun()->"Index 1: " end}, 
                            {user_input, fun()->"Index 2: " end},
                            SearchPaths]}},
     {non_atomic,{fold_expr,
                  [{file, fun(_File) ->true end}, {old, {M,F,A}},1,SearchPaths]}}
    ].

