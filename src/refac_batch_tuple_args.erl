-module(refac_batch_tuple_args).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").

%% User inputs.
input_par_prompts() ->
    [].

%% This evalution of this function returns a composite refactoring script.
composite_refac(_Args=#args{search_paths=SearchPaths}) ->
    Pars = ?STOP_TD_TU(
              [?COLLECT(?T("f@(As1@@, Line@, Col@, AS2@@) when G@@ -> Body@@."),
                        {api_refac:fun_define_info(f@), length(As1@@)+1},
                        re:run(?SPLICE(Line@), "Line*")/=nomatch andalso
                        re:run(?SPLICE(Col@), "Col*") /=nomatch)], 
              SearchPaths),
    Refacs=[wrangler_gen:tuple_args(M, {F, A}, Index, Index + 1, true,
                                    SearchPaths)||
               {{M, F, A}, Index} <- Pars],
    %% ?non_atomic(lists:append(Refacs))
    ?interactive(non_atomic, lists:append(Refacs)).
    
