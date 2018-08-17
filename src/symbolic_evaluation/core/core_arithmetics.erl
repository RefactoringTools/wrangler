%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%% Arithmetic Operators Core - Covers arithmetic expressions. 
%% 
%% The simplifications performed are divided into two minor cores: <a href="core_arit_simpl.html">Arithmetic Simplifications Core</a> and  <a href="core_arit_calc.html">Arithmetic Calculations Core</a>.
%% @end
%%%-------------------------------------------------------------------
-module(core_arithmetics).

%% Include files
-include("wrangler.hrl").
-export([rules/2]).

%%--------------------------------------------------------------------
%% @doc
%% Returns the list of arithmetics rules. The returned list of rules, contains the rules from <a href="core_arit_simpl.html">Arithmetic Simplifications Core</a> first and then the rules of <a href="core_arit_calc.html">Arithmetic Calculations Core</a>.
%% @spec rules(term(), term()) -> [rule()]
%% @end
%%--------------------------------------------------------------------
-spec(rules(_,_) -> [{'rule',fun(),list() | tuple()},...]).
rules(_A, _B) ->
    core_arit_simpl:rules(_A, _B) ++ core_arit_calc:rules(_A, _B).
  




    

