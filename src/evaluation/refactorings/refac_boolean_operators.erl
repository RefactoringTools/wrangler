%%%-------------------------------------------------------------------
%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%% Boolean Operators - Simplifies the defined boolean operators to <i>true</i> or <i>false</i>. This refactoring uses Wrangler API and can be found in <em>Wrangler -> Refactor -> gen_refac Refacs -> Symbolic Evaluation -> Boolean Operators</em>.
%% <p>
%% The following list of boolean operators that receives parameters of any type was implemented: 
%% <i>
%% "`>'",
%% "`<'",
%% "`>='",
%% "`=<'", 
%% "`/='", 
%% "`=/='", 
%% "`=='", 
%% "`=:='", 
%% is_atom/1, is_boolean/1, is_integer/1, is_float/1, is_function/1, is_function/2, is_list/1, is_number/1, is_tuple/1.
%% </i>
%% </p>
%% <p>
%% Furthermore, the following list of operators that only simplify boolean expressions was created: <i>and, andalso, or, orelse, xor, not/1</i>. 
%% </p>
%% Examples of usage:
%% <ul>
%% <li>
%% <em>2 > 1</em> is simplified to <em>true</em>.
%% </li>
%% <li>
%% <em>is_list({1,2,3})</em> is simplified to <em>false</em>.
%% </li>
%% <li>
%% <em>true andalso false</em> is simplified to <em>false</em>.
%% </li>
%% </ul>
%%@end
%%
%%

-module(refac_boolean_operators).

-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

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
input_par_prompts() -> refac:input_par_prompts().

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
select_focus(Args) -> refac:select_focus(Args).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec(check_pre_cond(Args::args{}) -> ok | {error, Reason})
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
transform(Args)-> 
    refac:try_call_transform(Args, fun core_boolean_operators:rules/2).


    

   





