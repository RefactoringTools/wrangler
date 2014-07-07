-module(core_boolean_operators).
-include_lib("wrangler/include/wrangler.hrl").
-export([rules/2]).

%% andalso, and, orelse, or, xor
%% not, is_list, is_integer, is_float, is_number, is_boolean, is_atom, is_tuple, is_function
%%

rules(_,_) ->
    [   
     gt_rule(),
     gte_rule(),
     lt_rule(),
     lte_rule(),
     equal_rule(),
     different_rule(),
     exactly_equal_rule(),
     exactly_different_rule()
    ]. 
%%------------------------------------------------------------------
equal_rule() ->
    ?RULE(
         ?T("Op1@ == Op2@"),
         begin
	     Op1_conv = utils_convert:convert_elem(Op1@),
	     Op2_conv = utils_convert:convert_elem(Op2@),
	     wrangler_syntax:atom(Op1_conv == Op2_conv)
	 end,
         utils_convert:convertable_type(Op1@) andalso utils_convert:convertable_type(Op2@)
	).

different_rule() ->
    ?RULE(
         ?T("Op1@ /= Op2@"),
         begin
	     Op1_conv = utils_convert:convert_elem(Op1@),
	     Op2_conv = utils_convert:convert_elem(Op2@),
	     wrangler_syntax:atom(Op1_conv /= Op2_conv)
	 end,
         utils_convert:convertable_type(Op1@) andalso utils_convert:convertable_type(Op2@)
	).

exactly_equal_rule() ->
    ?RULE(
         ?T("Op1@ =:= Op2@"),
         begin
	     Op1_conv = utils_convert:convert_elem(Op1@),
	     Op2_conv = utils_convert:convert_elem(Op2@),
	     wrangler_syntax:atom(Op1_conv =:= Op2_conv)
	 end,
         utils_convert:convertable_type(Op1@) andalso utils_convert:convertable_type(Op2@)
	).

exactly_different_rule() ->
    ?RULE(
         ?T("Op1@ =/= Op2@"),
         begin
	     Op1_conv = utils_convert:convert_elem(Op1@),
	     Op2_conv = utils_convert:convert_elem(Op2@),
	     wrangler_syntax:atom(Op1_conv =/= Op2_conv)
	 end,
         utils_convert:convertable_type(Op1@) andalso utils_convert:convertable_type(Op2@)
	).

gt_rule() ->
    ?RULE(
         ?T("Op1@ > Op2@"),
         begin
	     Op1_conv = utils_convert:convert_elem(Op1@),
	     Op2_conv = utils_convert:convert_elem(Op2@),
	     wrangler_syntax:atom(Op1_conv > Op2_conv)
	 end,
         utils_convert:convertable_type(Op1@) andalso utils_convert:convertable_type(Op2@)
	).

gte_rule() ->
    ?RULE(
         ?T("Op1@ >= Op2@"),
         begin
	     Op1_conv = utils_convert:convert_elem(Op1@),
	     Op2_conv = utils_convert:convert_elem(Op2@),
	     wrangler_syntax:atom(Op1_conv >= Op2_conv)
	 end,
         utils_convert:convertable_type(Op1@) andalso utils_convert:convertable_type(Op2@)
	).

lt_rule() ->
    ?RULE(
         ?T("Op1@ < Op2@"),
         begin
	     Op1_conv = utils_convert:convert_elem(Op1@),
	     Op2_conv = utils_convert:convert_elem(Op2@),
	     wrangler_syntax:atom(Op1_conv < Op2_conv)
	 end,
         utils_convert:convertable_type(Op1@) andalso utils_convert:convertable_type(Op2@)
	).

lte_rule() ->
    ?RULE(
         ?T("Op1@ =< Op2@"),
         begin
	     Op1_conv = utils_convert:convert_elem(Op1@),
	     Op2_conv = utils_convert:convert_elem(Op2@),
	     wrangler_syntax:atom(Op1_conv =< Op2_conv)
	 end,
         utils_convert:convertable_type(Op1@) andalso utils_convert:convertable_type(Op2@)
	).
    
