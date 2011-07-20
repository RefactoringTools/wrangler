%%@doc This module shows how to write refactorings 
%% use the Wrangler API. 

%% The refactoring implemented in this module
%% swaps the Ith and Jth arguments of a function 
%% selected by the user. The value of Ith and Jth 
%% are user input. 
%% To perform this refactoring, point the cursor to
%% the function definition, and then select 
%% 'Apply adhoc refactoring' from the menu, after 
%% that, Wrangler will prompts you input the 
%% the refactoring rename, which is supposed to be 
%% the module name, and then you will prompted to 
%% input the values of Ith and Jth.

%% @private
-module(refac_swap_args).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../include/wrangler.hrl").

-import(refac_api, [fun_define_info/1]).

%% The user needs to input the indexes of 
%% the parameters to swap.
input_par_prompts() ->
    ["Parameter Index 1: ",
     "Parameter Index 2: "].

%% The user needs to point the cursor to the 
%% function definition whose parameters is to 
%% be re-arranged. 
%% Note: the function: pos_to_fun_def will be 
%% moved to module refac_api.
select_focus(_Args=#args{current_file_name=File, 
                         cursor_pos=Pos}) ->
    api_interface:pos_to_fun_def(File, Pos).

%% Check that the values of Ith and Jth inputted 
%% by the user are valid. 
check_pre_cond(_Args=#args{focus_sel=FunDef,
                          user_inputs=[I, J]}) ->
    Ith=list_to_integer(I),
    Jth=list_to_integer(J),
    {_M,_F,A} = fun_define_info(FunDef),
    case Ith /=Jth of 
        true ->
            case Ith>=1 andalso Ith=<A of 
                true ->
                    case Jth>=1 andalso Jth=<A of 
                        true ->
                            ok;
                        false ->
                            {error, "Index 2 is invalid."}
                    end;
                false ->
                    {error, "Index 1 is invalid."}
            end;
        false ->
            {error, "Index 1 and Index 2 are the same."}
    end.
                
selective() ->
    false.
%% Do the program transformation here.
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
    
%% transform the current file.
transform_in_cur_file(_Args=#args{current_file_name=File},MFA, I, J)->
    ?FULL_TD_TP([rule1(MFA, I, J),
              rule2(MFA, I, J),
              rule3(MFA, I, J),
              rule4(MFA, I, J),
              rule5(MFA, I, J),
              rule6(MFA, I, J)],
             [File]).

%% transform the client files.
transform_in_client_files(_Args=#args{current_file_name=File,
                                     search_paths=SearchPaths}, 
                          MFA, I, J) ->
    ?FULL_TD_TP([rule2(MFA, I, J),
              rule3(MFA, I, J),
              rule4(MFA, I, J),
              rule5(MFA, I, J),
              rule6(MFA, I, J)],
             refac_api:client_files(File, SearchPaths)).
                  

%% transform the function definition itself.
rule1({M,F,A}, I, J) ->
    ?RULE(?T("f@(Args@@) when Guard@@ -> Bs@@;"), 
          begin NewArgs@@=swap(Args@@,I,J),
                ?QUOTE("f@(NewArgs@@) when Guard@@->Bs@@;")
          end,
          refac_api:fun_define_info(f@)=={M, F,A}).

%% the following rules transform the different kinds of 
%% application senarioes of the function.
rule2({M,F,A}, I, J) ->
    ?RULE(?T("F@(Args@@)"), 
          begin NewArgs@@=swap(Args@@, I, J),
                ?QUOTE("F@(NewArgs@@)")
          end,
          fun_define_info(F@) == {M, F, A}).

rule3({M,F,A}, I, J)->
    ?RULE(?T("Fun@(N@@, M@, F@, [Args@@])"),
          begin
              NewArgs@@=swap(Args@@, I, J),
              ?QUOTE("Fun@(N@@, M@, F@,[NewArgs@@])")
          end,
          case fun_define_info(Fun@) of
              {erlang, apply, _} -> 
                  refac_api:fun_define_info(F@)=={M,F,A};
              _ -> false
          end).

rule4({M,F,A}, I, J) ->
    ?RULE(?T("Fun@(N@@, M@, F@, Args@)"),
          begin
              AfterStr =lists:flatten(
                          io_lib:format(
                            "Fun@(N@@, M@, F@, 
                                    fun(List) ->
                                       Ith = lists:nth(~p, List),
                                       Jth = lists:nth(~p, List),
                                       T = list_to_tuple(List),
                                       T1=setelement(~p, setelement(~p, T, Jth), Ith),
                                       tuple_to_list(T1)
                                    end(Args@))", [I, J,J,I])),
              ?QUOTE(AfterStr)
          end,
          case fun_define_info(Fun@) of 
              {erlang,apply, _} -> 
                  refac_api:fun_define_info(F@)=={M,F,A};
              _ -> false
          end).

rule5({M,F,A}, I, J) ->
    ?RULE(?T("Fun@(F@, [Args@@])"), 
          begin
              NewArgs@@= swap(Args@@, I,J),
              ?QUOTE("Fun@(F@, [NewArgs@@])")
          end, 
          fun_define_info(Fun@)=={erlang, apply, 2} andalso 
          fun_define_info(F@)=={M,F,A}).

rule6({M,F,A}, I, J)->
    ?RULE(?T("Fun@(F@, Args@)"), 
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
          fun_define_info(F@)=={M,F,A}).


%% utility functions.
swap(List, I, J) ->
    Ith = lists:nth(I, List),
    Jth = lists:nth(J, List),
    T = list_to_tuple(List),
    T1=setelement(J, setelement(I, T, Jth), Ith),
    tuple_to_list(T1).
 
 
