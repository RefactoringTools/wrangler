%% Copyright (c) 2013, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%@doc This refactoring can be used when a new WS operation has been added to a
%%     WS. To invoke this refactoring, select 'add a WS operation' from the 
%%     'Refactorings for QuickCheck' sub-menu.
%%    This refactoring does not work with ```eqc_statem''' group syntax yet.
%%@hidden
%%@private
-module(refac_add_op).

-behaviour(gen_refac).

-compile(export_all).

%% Include files
-include("../../include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
         check_pre_cond/1, selective/0, 
         transform/1]).

%%%===================================================================
%%% gen_refac callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prompts for parameter inputs
%%
%% @spec input_par_prompts() -> [string()]
%% @end
%%--------------------------------------------------------------------
input_par_prompts() ->
    ["Operation name: ", "Parameters: "].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Select the focus of the refactoring.
%%
%% @spec select_focus(Args::#args{}) ->
%%                {ok, syntaxTree()} |
%%                {ok, none}
%% @end
%%--------------------------------------------------------------------
select_focus(_Args) -> 
    {ok, none}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec check_pre_cond(Args::#args{}) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
check_pre_cond(_Args=#args{
                 user_inputs=[OpName, OpArgs]}) ->
    case api_refac:is_fun_name(OpName) orelse 
        api_refac:is_var_name(OpName) of 
        true ->
            Args =string:tokens(OpArgs, [$,]),
            Args1 = lists:filter(fun(A)->
                                         api_refac:is_var_name(A)/=true
                                 end, Args),
            case Args1 of
                [] -> ok;
                _ ->
                    Msg = lists:flatten(io_lib:format("Invalid argument name(s):~p\n", [Args1])),
                    {error, Msg}
            end;
        false ->
            {error, "Invalid operation name:" ++ OpName++"."}
    end.
      
      
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Selective transformation or not.
%%
%% @spec selective() -> boolean()
%% @end
%%--------------------------------------------------------------------
selective() ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function does the actual transformation.
%%
%% @spec transform(Args::#args{}) -> 
%%            {ok, [{filename(), filename(), syntaxTree()}]} |
%%            {error, Reason}
%% @end
%%--------------------------------------------------------------------


transform(_Args=#args{current_file_name=File, 
                      user_inputs=[OpName, OpArgs]})->
    Params =string:tokens(OpArgs, [$,]),
    Style = check_style(File),
    StateCode = gen_state_code(OpName, Params, Style),
    PreCondCode = gen_precondition_code(OpName, Params, Style),
    PostCondCode = gen_postcondition_code(OpName,Params, Style),
    CmdCode = gen_cmd_generator_code(OpName, Params, Style),
    WrapperCode = gen_adaptor_fun(OpName, Params, Style),
    {ok, [{F, AST}]}=?STOP_TD_TP([rule1(StateCode),
                                  rule2(PreCondCode),
                                  rule3(PostCondCode),
                                  rule4(CmdCode)
                                 ],[File]),
    NewAST=insert_wrapper_fun(AST, WrapperCode),
    {ok, [{F, NewAST}]}.

rule1(NextStateCode) ->
    ?RULE(?T("next_state(S@, R@,Cmd@)-> Body@@;"),
           (api_refac:make_fake_block_expr([?TO_AST(NextStateCode),_This@])),
          (lists:member(api_refac:type(Cmd@), [variable,underscore]))).
        

rule2(PreCondCode) ->
    ?RULE(?T("precondition(S@, Cmd@)-> Body@@;"),
           (api_refac:make_fake_block_expr([?TO_AST(PreCondCode),_This@])),
           (lists:member(api_refac:type(Cmd@), [variable,underscore]))).

rule3(PostCondCode) ->
    ?RULE(?T("postcondition(S@, Cmd@, _Res@)-> Body@@;"),
          (api_refac:make_fake_block_expr([?TO_AST(PostCondCode),_This@])),
          (lists:member(api_refac:type(Cmd@), [variable,underscore]))).

rule4(CmdCode) ->
    ?RULE(?T("[Cmds@@,{call, M@, F@, Args@@}]"),
          (wrangler_syntax:list(
             wrangler_syntax:list_elements(_This@),
             ?TO_AST(CmdCode,
                     (element(2, api_refac:start_end_loc(_This@)))))),
          (api_refac:is_expr(_This@))).

gen_state_code(OpName, Args, Style) ->
    ArgStr=gen_arg_string(Args, Style, "_"),
    lists:flatten(
       io_lib:format(
         "next_state(S, _R, {call, ?MODULE, ~s, ~s}) ->S;\n",[OpName, ArgStr])).

gen_precondition_code(OpName, Args, Style) ->
    ArgStr=gen_arg_string(Args, Style, "_"),
    lists:flatten(
      io_lib:format(
        "precondition(_S, {call, ?MODULE, ~s, ~s}) ->true;\n",[OpName, ArgStr])).

gen_postcondition_code(OpName, Args, Style) ->
    ArgStr=gen_arg_string(Args, Style, "_"),
    lists:flatten(
      io_lib:format(
        "postcondition(_S, {call, ?MODULE, ~s, ~s}, _Res) ->true;\n",[OpName, ArgStr])).

gen_arg_string([], _Style, _Prefix)->
    "[]";
gen_arg_string([A], tuple, Prefix) ->
    "[{"++Prefix++A++"}]";
gen_arg_string([A], non_tuple, Prefix) ->
    "["++Prefix++A++"]";
gen_arg_string([A|As], tuple, Prefix) ->
    "[{"++Prefix++A++gen_arg_string_1(As, Prefix)++"}]";
gen_arg_string([A|As], non_tuple, Prefix) ->
    "["++Prefix++A++gen_arg_string_1(As, Prefix)++"]".

gen_arg_string_1([], _Prefix) ->
    "";
gen_arg_string_1([A|As], Prefix) ->
    ","++Prefix++A++gen_arg_string_1(As, Prefix).
    
gen_adaptor_fun(APIName, ParamNames, _Style)->
    APIName1=camelCase_to_camel_case(APIName),
    ParamStr =gen_param_string(ParamNames, false),              
    APIName1++"("++ParamStr++")->\n"++
        "      ?SUT:"++APIName1++"("++ParamStr++").\n\n".

    
%% utils funs.
gen_param_string([], _) ->
    "";
gen_param_string([P], WithUnderScore) ->
    if is_atom(P) ->
            to_upper(atom_to_list(P), WithUnderScore);
       true ->
            to_upper(P, WithUnderScore)
    end;
gen_param_string([H|T], WithUnderScore) ->
    if is_atom(H) ->
            to_upper(atom_to_list(H), WithUnderScore)
                ++", "++gen_param_string(T, WithUnderScore);
       true ->
             to_upper(H, WithUnderScore)
                ++", "++gen_param_string(T,WithUnderScore)
    end.
to_upper([H|T],WithUnderScore) -> 
    case WithUnderScore of 
        true ->
            "_"++normalise([string:to_upper(H)|T]);
        false ->
            normalise([string:to_upper(H)|T])
    end.

normalise([H|T]) ->
    case   (is_upper(H) or is_lower(H) or 
            is_digit(H) or (H == 64) or (H == 95)) of
        true ->
            [H|normalise(T)];
        false ->
            [95|normalise(T)]
    end;
normalise([]) ->[].



is_upper(L) -> (L >= 65) and (90 >= L).

is_lower(L) -> (L >= 97) and (122 >= L).

is_digit(L) -> (L >= 48) and (57 >= L).


%% transform camelCase atom to camel_case.
-spec(camelCase_to_camel_case(Name::string()) ->string()).
camelCase_to_camel_case(Name) ->
    case Name of 
        [H|T] when (H >= 65) and (90 >= H)->
            camelCase_to_camel_case_1([H+32|T],[]);
        _ ->
            camelCase_to_camel_case_1(Name,[])
    end.

camelCase_to_camel_case_1([], Acc) ->
    lists:reverse(Acc);
camelCase_to_camel_case_1([H|T], Acc)
  when  (H >= 65) and (90 >= H)->
    case Acc of 
        [95|_] ->
            camelCase_to_camel_case_1(T, [H + (97 - 65) |Acc]);
        _ ->
            camelCase_to_camel_case_1(T, [H + (97 - 65), 95|Acc])
    end;
camelCase_to_camel_case_1([H|T], Acc) ->
    camelCase_to_camel_case_1(T, [H|Acc]).
    

gen_cmd_generator_code(OpName, Args, Style)->
    Args1=case Style of 
               tuple->
                   ["gen_"++camelCase_to_camel_case(OpName)++"()"];
               non_tuple->
                   ["gen_"++camelCase_to_camel_case(A)++"()"||A<-Args]
           end,
    ArgStr= gen_arg_string(Args1, non_tuple, ""),
    lists:flatten(
      io_lib:format(
        "{call, ?MODULE, ~s, ~s}\n",[OpName, ArgStr])).
    


add_op(File, OpName, OpArgs, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=File, 
                user_inputs=[OpName, OpArgs],
                search_paths=SearchPaths,
                tabwidth=TabWidth},
    case check_pre_cond(Args) of
        ok -> 
            {ok, Res}=transform(Args),
            wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"");
        {error, Reason} ->
            {error, Reason}
    end.

insert_wrapper_fun(AST, AdaptorFun) ->
    Forms = wrangler_syntax:form_list_elements(AST),
    {Forms1, Forms2} = lists:splitwith(fun(F)->
                                               not is_a_wrapper_fun(F)
                                       end, lists:reverse(Forms)),
    case Forms2 of 
        [] ->
            wrangler_syntax:form_list(
              lists:reverse(
                [?TO_AST(AdaptorFun)|lists:reverse(Forms)]));
        _ ->
            wrangler_syntax:form_list(
              lists:reverse(Forms2)++
                  [?TO_AST(AdaptorFun)|lists:reverse(Forms1)])
    end.


is_a_wrapper_fun(F) ->
    Res=?STOP_TD_TU(
           [?COLLECT(?T("M@:F@(Args@@)"),
                     true,
                     ?PP(M@)=="?SUT"
                    )],
           F),
    lists:usort(Res)==[true].
        

check_style(File) ->
    Styles=?STOP_TD_TU(
              [?COLLECT(?T("{call, M@, F@, [{Args@@}]}"),
                           tuple, api_refac:is_pattern(_This@)),
               ?COLLECT(?T("{call, M@, F@, [Args@@]}"),
                           non_tuple, api_refac:is_pattern(_This@))],
              [File]),
    case lists:usort(Styles) of 
        [tuple] -> tuple;
        [non_tuple] -> non_tuple;
        [non_tuple, tuple] ->
            throw({error, "Mixed coding style used."});
        [] ->
            tuple
    end.
                 
