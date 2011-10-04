%%@hidden
%%@private
-module(refac_batch_tuple_args).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").

-compile(export_all).

%% User inputs.
input_par_prompts() ->
    [].

%% This evalution of this function returns a composite refactoring script.
composite_refac_1(_Args=#args{search_paths=SearchPaths}) ->
    Pars = ?STOP_TD_TU(
              [?COLLECT(?T("f@(As1@@, Line@, Col@, AS2@@) when G@@ -> Body@@."),
                        {api_refac:fun_define_info(f@), length(As1@@)+1},
                        re:run(?SPLICE(Line@), "Line*")/=nomatch andalso
                        re:run(?SPLICE(Col@), "Col*") /=nomatch)], 
              SearchPaths),
    Refacs=[wrangler_gen:tuple_args(M, {F, A}, Index, Index + 1, false, SearchPaths)||
                                      {{M, F, A}, Index} <- Pars],
    ?interactive(non_atomic, lists:append(Refacs)).
    
%% Alternately.  
composite_refac(_Args=#args{search_paths=SearchPaths}) ->
    ?non_atomic([?if_then(
                    begin
                        M=list_to_atom(filename:basename(File, ".erl")),
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
                            {refactoring, tuple_args, 
                             [File, {F, A}, Index + 1, Index+2,SearchPaths]}
                    end)
                 ||File<-wrangler_misc:expand_files(SearchPaths, ".erl"),
                   {F, A}<-api_refac:defined_funs(File)]).
                   

