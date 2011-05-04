%%@doc This module contains some transformation 
%% examples related to list operations. Please 
%% note that not all of these transformations 
%% are desirable in practice, the major purpose 
%% of these examples is just to show how to write 
%% transformation rules using the Wrangler 
%% API, and how to use the `gen_refac' behaviour for 
%% writing transformations each of which is local, but 
%% may apply to many instances.

%%@author  Huiqing Li <H.Li@kent.ac.uk>

-module(refac_keysearch_to_keyfind).

-behaviour(gen_refac).

-export([input_pars/0, select_focus/1, 
         pre_cond_check/1, transform/1]).

-include("../include/gen_refac.hrl").

%% No parameter input is required.
-spec (input_pars/0::() -> [string()]).                           
input_pars()->[].

%% No focus selection is required.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) ->{ok, none}.

%% No precondition checking is required.
-spec (pre_cond_check/1::(#args{}) -> ok).  
pre_cond_check(_Args)->
    ok.

%% Apply the transformation rules to all the Erlang files included in the 
%% SearchPaths.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}
                                     | {error, term()}).    
transform(_Args=#args{search_paths=SearchPaths})->
    ?FULL_TD([rule_keysearch_to_keyfind_1(),
              rule_keysearch_to_keyfind_2()
            ], SearchPaths).

%%=========================================================
%% replace the use of lists:append/2 with the use of ++.
rule_keysearch_to_keyfind_1() ->
    ?RULE("case lists:keysearch(Key@, N@, TupleList@) of
              {value, Tuple@} ->
                   Body1@@;
                false->
                   Body2@@
          end",
          case ?SPLICE(Tuple@) of 
             "_" ->
                  ?QUOTE("case lists:keyfind(Key@, N@, TupleList@) of 
                          false ->
                            Body2@@;
                          _ ->
                           Body1@@
                        end");
              _ ->
                  ?QUOTE("case lists:keyfind(Key@, N@, TupleList@) of 
                             Tuple@ ->
                               Body1@@;
                            false ->
                              Body2@@
                          end")  
              end,
          true).

rule_keysearch_to_keyfind_2() ->
    ?RULE("case lists:keysearch(Key@, N@, TupleList@) of
              false ->
                   Body1@@;
              {value, Tuple@} ->
                   Body2@@
           end",
           ?QUOTE("case lists:keyfind(Key@, N@, TupleList@) of 
                  false ->
                     Body1@@;
                  Tuple@ ->
                     Body2@@
                end"), 
          true).


