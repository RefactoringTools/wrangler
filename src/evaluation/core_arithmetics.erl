%%%-------------------------------------------------------------------
%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%%This module was created with the aim of <b>composing the arithmetic refactorings and applying all their rules toghether</b>. Thus, this refactoring just calls the other arithmetic refactorings (arithmetics simplification and arithmetic calculations). 
%%<p>For instance, the function clause <i>"f(X) -> 0 + X + 1 + 2;"</i> would be transformed into <i>"f(X) -> X + 3;"</i>. This would be done by the refactoring that does arithmetic simplifications (i.e.: "0 + X" is modified to "X") and also by the arithmetic calculations refactoring (this refactoring would substitute <i>"1 + 2"</i> by <i>"3"</i> obtaining <i>"f(X) -> X + 3;"</i>). </p>
%%<p>For the same example, if the arithmetic simplifications refactoring was used separetely the result would be <i>"f(X) -> X + 1 + 2;"</i>. Additionaly, if the arithmetic calculations refactoring was used individually the return would be <i>"f(X) -> 0 + X + 3;"</i>. That's why the composite refactoring is important, to integrate the others two arithmetic refactorings and create a more powerful single refactoring that contains the other two.</p>  
%%%
%%% @end
%%% Created : 31 Jan 2014 Roberto S. M. de Barros Filho <>
%%%-------------------------------------------------------------------
-module(core_arithmetics).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function calls the rules from the other refactorings.
%%--------------------------------------------------------------------
rules(_A, _B) ->
    core_arit_simpl:rules(_A, _B) ++ core_arit_calc:rules(_A, _B).
  




    

