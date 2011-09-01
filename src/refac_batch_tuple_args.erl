%%@hidden
%%@private
-module(refac_batch_tuple_args).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").

input_par_prompts() ->
    [].
     

composite_refac(_Args=#args{current_file_name=File,
                            search_paths=SearchPaths}) ->
    M=list_to_atom(filename:basename(File, ".erl")),
    [?if_then(
        begin
            FunDef = api_refac:mfa_to_fun_def({M,F,A}, File),
            Cond=?MATCH(?T("f@(Args1@@, Line@, Col@, Args2@@) when Guard@@->Body@@."),
                        FunDef,
                        re:run(?SPLICE(Line@), "Line*")/=nomatch andalso
                        re:run(?SPLICE(Col@), "Col*") /=nomatch),
            if Cond ->
                    {Cond, length(Args2@@)};
               true ->
                    false
            end
        end,
        fun(Index) ->
                ?try_refac(?interactive(?refac_(tuple_args, [File, {F, A}, Index + 1,
                                                             Index+2,SearchPaths])))
        end)                    
     ||{F,A} <- api_refac:defined_funs(File)].
                   
