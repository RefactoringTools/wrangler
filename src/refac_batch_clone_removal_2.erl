%%@hidden
%%@private
-module(refac_batch_clone_removal_2).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").
 
input_par_prompts() ->
    ["Source File Name: ", "Target File Name: "].


composite_refac(_Args=#args{user_inputs=[FromFile, ToFile],
                            search_paths=SearchPaths}) ->
    FAs1 = api_refac:defined_funs(filename:join([FromFile])),
    FAs2 = api_refac:defined_funs(filename:join([ToFile])),
    CommonFAs = FAs1 -- (FAs1 -- FAs2),
    ?non_atomic([?interactive(?refac_(move_fun, [FromFile, {F, A}, ToFile, SearchPaths]))||
                   {_M,F,A} <- api_refac:get_mfas(FromFile, bu),
                   lists:member({F,A}, CommonFAs)]).
