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
%% @hidden
%% @private
-module(refac_list).

-behaviour(gen_refac).

-export([input_par_prompts/0, select_focus/1,
         check_pre_cond/1, selective/0,
         transform/1]).

-include("../include/wrangler.hrl").

%% No parameter input is required.
-spec (input_par_prompts/0::() -> [string()]).                           
input_par_prompts() -> [].

%% No focus selection is required.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) ->{ok, none}.

%% No precondition checking is required.
-spec (check_pre_cond/1::(#args{}) -> ok).  
check_pre_cond(_Args) ->
    ok.

selective() ->
    true.

%% Apply the transformation rules to all the Erlang files included in the 
%% SearchPaths.
-spec (transform/1::(#args{}) -> {ok, [{filename(), filename(), syntaxTree()}]}).
                                        
transform(_Args=#args{search_paths=SearchPaths})->
    ?FULL_TD_TP([rule_replace_append(),
              rule_replace_substract(),
              rule_map_to_list_comp_1(),
              rule_map_to_list_comp_2(),
              rule_map_to_list_comp_3(),
              rule_filter_to_list_comp_1(),
              rule_filter_to_list_comp_2(),
              rule_filter_to_list_comp_3()
             ], SearchPaths).

%%=========================================================
%% replace the use of lists:append/2 with the use of ++.
rule_replace_append() ->
    ?RULE(?T("F@(L1@, L2@)"),
          ?QUOTE("L1@++L2@"),
          {lists, append, 2} == api_refac:fun_define_info(F@)).

%%=========================================================
%% replace the use of lists:subtract with the use of --.
rule_replace_substract() ->
    ?RULE(?T("F@(L1@, L2@)"), 
          ?QUOTE("L1@--L2@"),
          {lists, subtract, 2} == api_refac:fun_define_info(F@)).

%%=============================================================
%% Transform certain uses of lists:map/2 to list comprehension.
%%
%% Rule 1: handles the case when the first argument of lists:map/2
%% is a fun expression whose only argument is a variable.

% In this example, Body@@ maps a list of expressions
rule_map_to_list_comp_1()->
    ?RULE(?T("Op@(fun(V@)->Body@@ end, L@)"), 
          begin
              case length(Body@@)==1 of 
                  true ->
                      E@ = hd(Body@@),
                      ?QUOTE("[E@||V@<-L@]");
                  false ->
                      ?QUOTE("[begin Body@@ end||V@<-L@]")
              end
          end,
          {lists, map, 2} == api_refac:fun_define_info(Op@)
         andalso
          api_refac:type(V@) == variable).

%% Rule 2: handles the case when the first argument of 
%% lists:map/2 is an implicit fun expression.

%% In this example, '_This@' maps to the whose node that 
%% matches the template, and f@ represents a meta atom.
rule_map_to_list_comp_2()->
    ?RULE(?T("Op@(fun f@/1, L@)"),
          begin
              VisibleNames = api_refac:env_var_names(_This@),
              V = atom_to_list(api_refac:make_new_name('NewVar', VisibleNames)),
              ?QUOTE("[f@("++V++")||" ++ V++"<-L@]")
          end,
          {lists, map, 2} == api_refac:fun_define_info(Op@)).

%%Rule 3:  handles the case when the first argument of lists:map/2
%% is a variable.
rule_map_to_list_comp_3()->
    ?RULE(?T("Op@(F@, L@)"),
          begin
              VisibleNames = api_refac:env_var_names(_This@),
              V = atom_to_list(api_refac:make_new_name('NewVar', VisibleNames)),
              ?QUOTE("[F@("++V++")||" ++ V++"<-L@]")
          end,
          {lists, map, 2} == api_refac:fun_define_info(Op@) andalso
          api_refac:type(F@) == variable).
        

%%=============================================================
%% Transform certain uses of lists:filter/2 to list comprehension.
%%
%% Rule 1: handles the case when the first argument of lists:filter/2
%% is a fun expression whose only argument is a variable.
rule_filter_to_list_comp_1()->
    ?RULE(?T("Op@(fun(V@)-> Body@@ end, L@)"),
          begin
              case length(Body@@)==1 of 
                  true ->
                      E@ = hd(Body@@),
                      ?QUOTE("[V@||V@<-L@, Body@@]");
                  false ->
                      ?QUOTE("[V@||V@<-L@, begin Body@@ end]")
              end
          end,
          {lists, filter, 2} == api_refac:fun_define_info(Op@) andalso
          api_refac:type(V@) == variable). 

%% Rule 2: handles the case when the first argument of 
%% lists:filter/2 is an implicit fun expression.
rule_filter_to_list_comp_2()->
    ?RULE(?T("Op@(fun f@/1, L@)"),
          begin
              VisibleNames = api_refac:env_var_names(_This@),
              V = atom_to_list(api_refac:make_new_name('NewVar', VisibleNames)),
              ?QUOTE("["++V++"||"++ V++"<-L@,f@("++V++")]")
          end,
          {lists, filter, 2} == api_refac:fun_define_info(Op@)).

%% Rule 3: handles the case when the first argument of lists:filter/2
%% is an implicit fun expression or a variable.
rule_filter_to_list_comp_3()->
    ?RULE(?T("Op@(F@, L@)"),
          begin
              VisibleNames = api_refac:env_var_names(_This@),
              V = atom_to_list(api_refac:make_new_name('NewVar', VisibleNames)),
              ?QUOTE("["++V++"||"++ V++"<-L@,F@("++V++")]")
          end,
          {lists, filter, 2} == api_refac:fun_define_info(Op@) andalso
          api_refac:type(F@) == variable).

