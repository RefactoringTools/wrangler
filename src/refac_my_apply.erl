%%@hidden
%%@private
-module(refac_my_apply).

-include("../include/wrangler.hrl").

-compile(export_all).

%% Note: fuctions needed to redefined: apply/2, apply/3, spawn/1/2/3/4, 
%% spawn_link/1/2/3/4, ...

-spec test/1::(string()) ->{ok, string()} |{error, term()}.              
test(Str) -> transform(Str, "my_mod").

-spec transform/2::(string(), string()) -> 
                           {ok, string()} |{error, term()}.
transform(Str, MyMod) ->
    try 
        Str1 =rm_dot(Str),
        Exprs =parse_annotate_expr(Str1), 
        {ok, NewExprs}=do_transform(Exprs, MyMod),
        {ok, ?PP(NewExprs)++"."}
    catch
        _E1:E2 ->
            {error, {E2, erlang:get_stacktrace()}}
    end.
            
do_transform(Exprs, MyMod) ->
    ?FULL_BU_TP([rule0(MyMod), rule1(MyMod)], Exprs).
 
rule0(MyMod) ->
    ?RULE(?T("F@"),
          begin
              {_M,F,A} = api_refac:fun_define_info(F@),
              mk_implicit_fun(MyMod, F, A)
          end,
          api_refac:type(F@) == implicit_fun andalso
          lists:member(api_refac:fun_define_info(F@), special_funs())).

mk_implicit_fun(MyMod, F, A) ->
    wrangler_syntax:implicit_fun(
      wrangler_syntax:module_qualifier(
        wrangler_syntax:atom(list_to_atom(MyMod)),
        wrangler_syntax:arity_qualifier(
          wrangler_syntax:atom(F), wrangler_syntax:integer(A)))).

      
special_funs() ->[{erlang, apply, 2}, {erlang, apply, 3}, {erlang, spawn, 1}, 
                  {erlang, spawn, 2}, {erlang, spawn, 3}, {erlang, spawn, 4},
                  {erlang, spawn_link, 1}, {erlang, spawn_link, 2}, 
                  {erlang, spawn_link, 3}, {erlang, spawn_link, 4}].

rule1(MyMod) ->
    ?RULE(?T("F@(Args@@)"), 
          case api_refac:fun_define_info(F@) of 
              {erlang, apply, _} ->
                  ?TO_AST(MyMod++":apply("++?PP(Args@@)++")");
              {erlang, spawn, _} ->
                  ?TO_AST(MyMod++":spawn("++?PP(Args@@)++")");
              {erlang, spawn_link, _} ->
                  ?TO_AST(MyMod++":spawn_link("++?PP(Args@@)++")");
              {erlang, _, _} ->
                  _This@;
              {M, F, _A} when M/='_' andalso F/='_' ->
                                     Str = MyMod++":apply("++atom_to_list(M)++","
                                         ++atom_to_list(F)++", ["++?PP(Args@@)++"])",
                                     ?TO_AST(Str);
               _ ->
                  case api_refac:type(F@) of 
                      module_qualifier ->
                          M = wrangler_syntax:module_qualifier_argument(F@),
                          F= wrangler_syntax:module_qualifier_body(F@),
                          ?TO_AST(MyMod++":apply("++?PP(M)++","
                                  ++?PP(F)++", ["++?PP(Args@@)++"])");
                      _ -> 
                          ?TO_AST(MyMod++":apply("++?PP(F@)++","
                                 ++ "["++?PP(Args@@)++"])")
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
                                   
   
                                  

