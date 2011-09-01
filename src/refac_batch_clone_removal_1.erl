%%@hidden
%%@private
-module(refac_batch_clone_removal_1).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").
 
input_par_prompts() ->
    ["Module name: ", "Function name: ", "Arity: "].

composite_refac(_Args=#args{user_inputs=[M, F, A],
                            search_paths=SearchPaths}) ->
    [?interactive(
        ?refac_(rename_fun,
                [M, {F, A}, 
                 {user_input, fun(_)->
                                      "New funcion name: " 
                              end},
                 SearchPaths])),
     ?refac_(rename_var,
             [M,
              begin
                  MFA = ?current(M,F,A),
                  {element(2, MFA),element(3, MFA)}
              end,
              fun(X) ->
                      re:run(atom_to_list(X), "NewVar*")/=nomatch
              end, 
              {user_input, fun({_, _, V}) ->
                                   lists:flatten(io_lib:format(
                                                   "Rename variable ~p to: ", [V]))
                           end},
              SearchPaths]),
     ?repeat_interactive(
        ?refac_(swap_args, 
                [M,
                 begin
                     MFA = ?current(M,F,A),
                     {element(2, MFA),element(3, MFA)}
                 end,
                 {user_input, fun(_)->"Index 1: " end}, 
                 {user_input, fun(_)->"Index 2: " end},
                 SearchPaths])),
     ?try_refac(
        ?refac_(fold_expr,
                [{file, fun(_File) ->true end}, 
                 element(1,?current(M,F,A)),
                 {element(2, ?current(M,F,A)),element(3, ?current(M,F,A))},
                 1, SearchPaths]))
    ].
    


