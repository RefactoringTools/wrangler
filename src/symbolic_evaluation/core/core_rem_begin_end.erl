%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc
%% Remove Begin/End Core - Removes begin/end blocks when a single steps is inside them.
%%% @private
%%% @end
%%%-------------------------------------------------------------------
-module(core_rem_begin_end).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).
%%--------------------------------------------------------------------
%% @doc
%% List of rules with single rule to remove begin/end blocks with single step.
%% @end
%% @private
%%--------------------------------------------------------------------
rules(_,_) ->
    [remove_begin_end_rule()]. 

%% @private
remove_begin_end_rule() ->
    ?RULE(?T("begin Arg@ end"),
	  Arg@,
	  true
	).







