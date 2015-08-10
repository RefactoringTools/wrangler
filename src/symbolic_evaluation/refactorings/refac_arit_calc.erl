%%%-------------------------------------------------------------------
%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%%This module covers refactorings with arithmetic expressions. These expressions may contain <b>variables and/or integers</b>.
%%<p>
%% Unfortunately, there are some cases that are still not being  covered.  
%%<ul><li>For instance, if an expression contains integers and variables interchanged, as <i>"1 + X + 1"</i>. In this case, this expression is not simplified anymore.</li>
%%<li> Multiplication and division between variables is also not covered.</li> </ul></p>
%%<p>However, if the expression contains integers and variables but they are not mixed, as in <i>"1 + 2 + 3 + X + 2*X + Y"</i>, the refactoring can be done and the expression would become <i>"6 + 3 * X + Y"</i> for this example.</p>

%%%
%%% @end
%%%-------------------------------------------------------------------
-module(refac_arit_calc).

-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1]).

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
%% @spec(selective() -> boolean())
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
    refac:try_call_transform(Args, fun core_arit_calc:rules/2).
