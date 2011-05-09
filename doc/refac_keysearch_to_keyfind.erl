%%@doc This module shows how to write refactorings 
%% use the Wrangler API. 

%% This refactoring replace the use of lists:keysearch/3 with
%% the use of lists:keyfind/3 whenever it is safe to do so.
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
-spec (transform/1::(#args{}) -> 
                          {ok, [{filename(), filename(), syntaxTree()}]}
                              | {error, term()}).    
transform(_Args=#args{search_paths=SearchPaths})->
    ?FULL_TD([rule_keysearch_to_keyfind()], SearchPaths).

rule_keysearch_to_keyfind() ->
    ?RULE(?T("case lists:keysearch(Key@, N@, TupleList@) of 
                         Pats@@@ when Guards@@@ ->
                             Body@@@
                    end"),
          begin
              try make_new_pats(Pats@@@) of 
                  NewPats@@@ ->
                      ?QUOTE(?T("case lists:keyfind(Key@, N@, TupleList@) of 
                         NewPats@@@ when Guards@@@ ->
                             Body@@@
                         end"))
              catch
                  _E1:_E2 ->
                      _This@
              end
          end,
          true).

make_new_pats(ListOfPats) ->
    [make_new_pat(Pats)||Pats<-ListOfPats].
make_new_pat([Pat]) ->
    case ?MATCH(?T("{value, T@}"), Pat) of
        true ->
            case refac_syntax:type(T@) of
                variable ->
                    [?QUOTE(?SPLICE(T@)++"={_,_}")];
                underscore->
                    [?QUOTE("{_,_}")];
                _ ->
                    [T@]
            end;
        false ->
            case ?MATCH(?T("false"), Pat) of 
                true ->
                    [Pat];
                false ->
                    case ?MATCH(?T("_"), Pat) of 
                        true ->
                            [Pat];
                        false ->
                            throw({error, "Transformation aborted."})
                    end
            end
    end.
