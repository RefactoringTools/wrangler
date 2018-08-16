%%@hidden
%%@private
%%
%% inline variable definitions in the format of Var1 = Var2.
-module(refac_batch_inline_vars).

-export([composite_refac/1, input_par_prompts/0, select_focus/1]).

-behaviour(gen_composite_refac).

-include("wrangler.hrl").

input_par_prompts() ->
    [].

select_focus(_Args) -> 
    {ok, none}.
   
%% This evalution of this function returns a composite refactoring script.
composite_refac(_Args=#args{current_file_name=File, 
                            search_paths=SearchPaths}) ->
    Refacs=?refac_(inline_var, [File, 
                                fun({_F,_A}) -> true end,
                                fun(MatchExpr) ->
                                        ?MATCH(?T("Var@=Expr@"),MatchExpr),
                                        api_refac:type(Expr@)==variable
                                end, SearchPaths]),
    ?non_atomic(Refacs).

