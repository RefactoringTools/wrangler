%%@hidden
%%@private
-module(refac_my_apply).

-include("../include/wrangler.hrl").

-compile(export_all).

%% Note: test/1 does not accept a string ended with a dot, so 
%% please remove the dot at the end of the string.
%% eg: refac_my_apply("F=atom_to_list("aa)", F:bar()").
test(Str) -> transform(Str, {"my_mod", "my_apply", "my_spawn"}).

transform(Str, {MyMod, MyApply, MySpawn}) ->
    try 
        Exprs =parse_annotate_expr(Str), 
        {ok, NewExprs}=do_transform(Exprs, {MyMod, MyApply, MySpawn}),
        {ok, ?PP(NewExprs)}
    catch
        _E1:E2 ->
            {error, {E2, erlang:get_stacktrace()}}
    end.
            
do_transform(Exprs, {MyMod, MyApply, MySpawn}) when is_list(Exprs)->
    ?FULL_BU_TP([rule1(MyMod, MyApply, MySpawn)], Exprs);
do_transform(Expr, {MyMod, MyApply, MySpawn}) ->
    ?FULL_BU_TP([rule1(MyMod, MyApply, MySpawn)], [Expr]).

  
rule1(MyMod, MyApply, MySpawn) ->
    ?RULE(?T("F@(Args@@)"), 
          case api_refac:fun_define_info(F@) of 
              {erlang, apply, _} ->
                  ?TO_AST(MyMod++":"++MyApply++"("++?PP(Args@@)++")");
              {erlang, spawn, _} ->
                  ?TO_AST(MyMod++":"++MySpawn++"("++?PP(Args@@)++")");
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
    case is_list(Expr) of 
        true ->
            [wrangler_annotate_ast:add_fun_def_info(E, '_', [], [])||
                E<-Expr];
        false ->
            wrangler_annotate_ast:add_fun_def_info(Expr, '_', [], [])
    end.
     

