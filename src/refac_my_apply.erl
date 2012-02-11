%%@hidden
%%@private
-module(refac_my_apply).

-include("../include/wrangler.hrl").

-compile(export_all).

%% Note: fuctions needed to redefined: apply/2, apply/3, spawn/1/2/3/4, 
%% spawn_link/1/2/3/4, ...

-spec test/1::(string()) ->string().              
test(Str) -> transform(Str, {"my_mod", "my_apply", "my_spawn"}).

-spec transform/2::(string(), {string(), string(), string()}) -> 
                           {ok, string()} |{error, term()}.
transform(Str, {MyMod, MyApply, MySpawn}) ->
    try 
        Str1 =rm_dot(Str),
        Exprs =parse_annotate_expr(Str1), 
        {ok, NewExprs}=do_transform(Exprs, {MyMod, MyApply, MySpawn}),
        {ok, ?PP(NewExprs)++"."}
    catch
        _E1:E2 ->
            {error, {E2, erlang:get_stacktrace()}}
    end.
            
do_transform(Exprs, {MyMod, MyApply, MySpawn}) ->
    ?FULL_BU_TP([rule1(MyMod, MyApply, MySpawn)], Exprs).
 
rule1(MyMod, MyApply, MySpawn) ->
    ?RULE(?T("F@(Args@@)"), 
          case api_refac:fun_define_info(F@) of 
              {erlang, apply, _} ->
                  ?TO_AST(MyMod++":"++MyApply++"("++?PP(Args@@)++")");
              {erlang, spawn, _} ->
                  ?TO_AST(MyMod++":"++MySpawn++"("++?PP(Args@@)++")");
              {erlang, spawn_link, _} ->
                  ?TO_AST(MyMod++":"++MySpawn++"_link"++"("++?PP(Args@@)++")");
              {erlang, _, _} ->
                  _This@;
              {M, F, _A} when M/='_' andalso F/='_' ->
                        ?TO_AST(MyMod++":" ++ MyApply++"("++atom_to_list(M)++","
                               ++atom_to_list(F)++", ["++?PP(Args@@)++"])");
              _ ->
                 case api_refac:type(F@) of 
                      module_qualifier ->
                          M = wrangler_syntax:module_qualifier_argument(F@),
                          F= wrangler_syntax:module_qualifier_body(F@),
                          ?TO_AST(MyMod++":"++MyApply++"("++?PP(M)++","
                                  ++?PP(F)++", ["++?PP(Args@@)++"])");
                      _ -> _This@
                  end
            end,
          true).
          
parse_annotate_expr(Str) ->
    Expr= wrangler_misc:parse_annotate_expr(Str),
    Expr1 = if is_list(Expr) -> Expr; true -> [Expr] end,
    [wrangler_annotate_ast:add_fun_def_info(E, '_', [], [])||E<-Expr1].
     

rm_dot(Str) ->
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str),
    Toks1 = lists:takewhile(fun(T) ->element(1, T) /= dot end, Toks),
    wrangler_misc:concat_toks(Toks1).
                                   
   
                                  

