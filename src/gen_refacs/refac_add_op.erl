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
%% @hidden
%% @private
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
check_pre_cond(_Args) ->
    ok.
      
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
    StateCode = gen_state_code(OpName, Params),
    PreCondCode = gen_precondition_code(OpName, Params),
    PostCondCode = gen_postcondition_code(OpName,Params),
    CmdCode = gen_cmd_generator_code(OpName, Params),
    ?STOP_TD_TP([%% rule0(CmdCode),
                 rule0(StateCode),
                 rule1(PreCondCode),
                 rule2(PostCondCode)
                ],
                [File]).

%% This is only prototype, and will be improved.
rule0(Code) ->
    ?RULE(?T("next_state(S@, R@,Cmd@)-> Body@@;"),
           api_refac:make_fake_block_expr([?TO_AST(Code),_This@]),
          lists:member(api_refac:type(Cmd@), [variable,underscore])).
        
rule1(Code) ->
    ?RULE(?T("precondition(S@, Cmd@)-> Body@@;"),
           api_refac:make_fake_block_expr([?TO_AST(Code),_This@]),
           lists:member(api_refac:type(Cmd@), [variable,underscore])).

rule2(Code) ->
    ?RULE(?T("postcondition(S@, Cmd@, _Res@)-> Body@@;"),
          api_refac:make_fake_block_expr([?TO_AST(Code),_This@]),
          lists:member(api_refac:type(Cmd@), [variable,underscore])).

rule3(Code, OpName) ->
    ?RULE(?T("[Cmds@@,{call, M@, F@, Args@@}]"),
          wrangler_syntax:list(
            wrangler_syntax:list_elements(_This@),
            ?TO_AST("\n[{call, ?MODULE, aa, []}]\n", 
                    element(2, api_refac:start_end_loc(_This@)))),
          api_refac:is_expr(_This@)).

gen_state_code(OpName, Args) ->
    ArgStr=gen_arg_string(Args),
    lists:flatten(
       io_lib:format(
         "next_state(S, _R, {call, ?MODULE, ~s, ~s}) ->S;\n",[OpName, ArgStr])).

gen_precondition_code(OpName, Args) ->
    ArgStr=gen_arg_string(Args),
    lists:flatten(
      io_lib:format(
        "precondition(_S, {call, ?MODULE, ~s, ~p}) ->true;\n",[OpName, ArgStr])).

gen_postcondition_code(OpName, Args) ->
    ArgStr=gen_arg_string(Args),
    lists:flatten(
      io_lib:format(
        "postcondition(_S, {call, ?MODULE, ~s, ~p}, _Res) ->true;\n",[OpName, ArgStr])).

gen_arg_string([])->
    "[]";
gen_arg_string([A]) ->
    "["++A++"]";
gen_arg_string([A|As]) ->
    "["++A++gen_arg_string_1(As)++"]".

gen_arg_string_1([]) ->
    "";
gen_arg_string_1([A|As]) ->
    ","++A++gen_arg_string_1(As).
    

gen_an_adaptor_fun(APIName, ParamNames)->
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
    

gen_cmd_generator_code(OpName, Args)->
    lists:flatten(
      io_lib:format(
        "{call, ?MODULE, ~s, ~p},\n",[OpName, Args])).


add_op(File, NextStateIndex, NextStateCode, PreCondIndex, PreCondCode, 
           PostCondIndex, PostCondCode, CmdsIndex, CmdsCode, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=File, 
                user_inputs=[NextStateIndex, NextStateCode, 
                             PreCondIndex, PreCondCode, 
                             PostCondIndex, PostCondCode,
                             CmdsIndex, CmdsCode],
                search_paths=SearchPaths,
                tabwidth=TabWidth},
    case check_pre_cond(Args) of
        ok -> 
            {ok, Res}=transform(Args),
            wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"");
        {error, Reason} ->
            {error, Reason}
    end.
