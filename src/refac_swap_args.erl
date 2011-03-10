-module(refac_swap_args).

%% -behaviour(gen_refac).

-export([input_pars/0, select_focus/1, 
         pre_cond_check/1, transform/1]).

-include("../include/gen_refac.hrl").

-import(refac_api, [fun_define_info/1]).

input_pars()->
    ["Parameter Index 1: ",
     "Parameter Index 2: "].

select_focus(_Args=#args{current_file_name=File, cursor_pos=Pos}) ->
    interface_api:pos_to_fun_def(File, Pos).

pre_cond_check(_) ->
    ok.

transform(Args=#args{current_file_name=File,focus_sel=FunDef, 
                     user_inputs=[I, J]}) ->
    {M,F,A} = fun_define_info(FunDef),
    I1 = list_to_integer(I),
    J1 = list_to_integer(J),
    {ok, Res}=transform_in_cur_file(Args, {M,F,A}, I1, J1),
    case refac_api:is_exported({F,A}, File) of
        true ->
            {ok, Res1}=transform_in_client_files(Args, {M,F,A}, I1, J1),
            {ok, Res++Res1};
        false ->
            {ok, Res}
    end.
    

transform_in_cur_file(_Args=#args{current_file_name=File},MFA, I, J)->
    ?FOREACH([rule1(MFA, I, J),
              rule2(MFA, I, J),
              rule3(MFA, I, J),
              rule4(MFA, I, J),
              rule5(MFA, I, J),
              rule6(MFA, I, J)],
             [File]).

transform_in_client_files(_Args=#args{current_file_name=File,
                                     search_paths=SearchPaths}, 
                          MFA, I, J) ->
    ?FOREACH([rule2(MFA, I, J),
              rule3(MFA, I, J),
              rule4(MFA, I, J),
              rule5(MFA, I, J),
              rule6(MFA, I, J)],
             refac_api:client_files(File, SearchPaths)).
                  

rule1({_M,F,A}, I, J) ->
    ?RULE("f@(Args@@) -> Bs@@;", begin NewArgs@@=swap(Args@@,I,J),
                                      ?QUOTE("f@(NewArgs@@)->Bs@@;")
                                end,
          length(Args@@) == A andalso refac_syntax:is_atom(f@, F)).

rule2({M,F,A}, I, J) ->
    ?RULE("F@(Args@@)", begin NewArgs@@=swap(Args@@, I, J),
                              ?QUOTE("F@(NewArgs@@)")
                        end,
          fun_define_info(F@) == {M, F, A}).

rule3({M,F,A}, I, J)->
     ?RULE("Fun@(Args@@, M@, F@, Args2@)",
           begin
               NewArgs2@=refac_syntax:list(swap(refac_syntax:list_elements(Args2@), I, J)),
               ?QUOTE("Fun@(Args@@, M@, F@,NewArgs2@)")
           end,
           case fun_define_info(Fun@) of
               {erlang, apply, _} -> true;
               _ -> false
           end andalso
           refac_syntax:is_atom(M@, M) andalso
           refac_syntax:is_atom(F@, F) andalso
           refac_syntax:type(Args2@) == list andalso
           refac_syntax:list_length(Args2@) == A).

rule4({M,F,A}, I, J) ->
    ?RULE("Fun@(Args@@, M@, F@, Args2@)",
          begin
              AfterStr =lists:flatten(
                          io_lib:format(
                            "Fun@(Args@@, M@, F@, 
                                    fun(List) ->
                                       Ith = lists:nth(~p, List),
                                       Jth = lists:nth(~p, List),
                                       T = list_to_tuple(List),
                                       T1=setelement(~p, setelement(~p, T, Jth), Ith),
                                       tuple_to_list(T1)
                                    end(Args2@))", [I, J,J ,I])),
              ?QUOTE(AfterStr)
          end,
          case fun_define_info(Fun@) of 
              {erlang,apply, _} -> true;
              _ -> false
          end andalso
          refac_syntax:is_atom(M@, M) andalso 
          fun_define_info(F@) == {M,F,A} andalso
          refac_syntax:type(Args2@) =/= list).

rule5({M,F,A}, I, J)->
    ?RULE("Fun@(F@, Args@)", 
          begin
              AfterStr =lists:flatten(
                          io_lib:format(
                            "Fun@(F@, 
                                    fun(List) ->
                                       Ith = lists:nth(~p, List),
                                       Jth = lists:nth(~p, List),
                                       T = list_to_tuple(List),
                                       T1=setelement(~p, setelement(~p, T, Jth), Ith),
                                       tuple_to_list(T1)
                                    end(Args@))", [I, J,J ,I])),
              ?QUOTE(AfterStr)
          end,
           fun_define_info(Fun@)=={erlang, apply, 2} andalso 
           refac_syntax:type(F@)==implicit_fun       andalso
           fun_define_info(F@)=={M,F,A}       andalso
           refac_syntax:type(Args@) =/= list).

rule6({M,F,A}, I, J) ->
     ?RULE("Fun@(F@, Args@)", 
           begin
               NewArgs2@=refac_syntax:list(
                           swap(refac_syntax:list_elements(Args@), I,J)),
               ?QUOTE("Fun@(F@, NewArgs2@)")
           end, 
           fun_define_info(Fun@)=={erlang, apply, 2} andalso 
           refac_syntax:type(F@)==implicit_fun       andalso
           fun_define_info(F@)=={M,F,A}       andalso
           refac_syntax:type(Args@) == list).


%% utility functions.
swap(List, I, J) ->
    Ith = lists:nth(I, List),
    Jth = lists:nth(J, List),
    T = list_to_tuple(List),
    T1=setelement(J, setelement(I, T, Jth), Ith),
    tuple_to_list(T1).
 

