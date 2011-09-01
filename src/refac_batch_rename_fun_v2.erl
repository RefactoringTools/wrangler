%%@author  Huiqing Li <H.Li@kent.ac.uk>

%%@hidden
%%@private
-module(refac_batch_rename_fun_v2).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").

input_par_prompts() ->
    [].

composite_refac(_Args=#args{search_paths=SearchPaths}) ->
    ?try_refac(?refac_(rename_fun, 
                       [{file, fun(_File)-> true end},
                        fun({F, _A}) ->
                                camelCase_to_camel_case(F) /= F
                        end,
                        {generator, fun({_File, F,_A}) ->
                                            camelCase_to_camel_case(F)
                                    end},
                        SearchPaths])).


%% transform camelCase atom to camel_case.
camelCase_to_camel_case(Name) ->
    list_to_atom(camelCase_to_camel_case_1(
                   atom_to_list(Name),[])).

camelCase_to_camel_case_1([], Acc) ->
    lists:reverse(Acc);
camelCase_to_camel_case_1([H|T], Acc) 
  when  (H >= 65) and (90 >= H)->
    case Acc of 
        [95|_] ->
            camelCase_to_camel_case_1(T, [H+(97-65)|Acc]);
        _ ->
            camelCase_to_camel_case_1(T, [H+(97-65), 95|Acc])
    end;
camelCase_to_camel_case_1([H|T], Acc) ->
    camelCase_to_camel_case_1(T, [H|Acc]).
    
