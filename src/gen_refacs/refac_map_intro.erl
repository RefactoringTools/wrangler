%% @private
-module(refac_map_intro).

-behaviour(gen_refac).

-compile(export_all).

-include("../..//include/wrangler.hrl").

-export([input_par_prompts/0, select_focus/1, check_pre_cond/1, selective/0, transform/1]).

%%% Will need to input the number of Workers (NW)
input_par_prompts() -> ["Name of Partitioner: ", "Name of Combiner: ", "Name of new Worker: "].
    
select_focus(_Args=#args{current_file_name=File,
			 highlight_range={Start, End}}) ->
	% api_interface:pos_to_fun_def(File, Pos).
	api_interface:pos_to_expr1(File, Start, End).
	% {Start, End}.
    
check_pre_cond(_) -> 
   ok.

selective() -> true.

transform(Args=#args{current_file_name=_File, focus_sel=Expr, user_inputs=[Comp, DeComp, Worker]}) ->
	%{M,F,A} = api_refac:fun_define_info(FunDef),
    transform_in_cur_file(Args, Expr, Comp, DeComp, Worker).

% transform(Args=#args{current_file_name=_File, focus_sel={Expr}, user_inputs=[Comp, DeComp, Worker]}) ->
	% {M,F,A} = api_refac:fun_define_info(FunDef),
%    transform_in_cur_file(Args, Expr, Comp, DeComp, Worker).

transform_in_cur_file(_Args=#args{current_file_name=File}, Expr, Comp, DeComp, Worker) ->
    ?FULL_TD_TP([in_seq(Expr, Comp, DeComp, Worker), in_comp(Expr, Comp, DeComp, Worker)], [File]).

% 
in_seq(Expr, Comp, DeComp, Worker) ->
    ?RULE(?T("{seq, Ex@}"),
        begin
          % ?MATCH(?T("{seq, f@}"), Expr@),
          ?TO_AST("{map,[{seq, fun ?MODULE:"++Worker++"/1}], fun ?MODULE:"++Comp++"/1, fun ?MODULE:"++DeComp++"/1}")
        end,
        begin
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
 % io:fwrite("inComp~p~n",[Start1, Start2]),
           Start1 == Start2
        end
         ).
         
in_seq2(Expr, Comp, DeComp, Worker) ->
    ?RULE(?T("{seq, fun ?MODULE:georef/1}"),
        begin
          % ?MATCH(?T("{seq, f@}"), Expr@),
          ?TO_AST("{map,[{seq, fun ?MODULE:"++Worker++"/1}], fun ?MODULE:"++Comp++"/1, fun ?MODULE:"++DeComp++"/1}")
        end,
        begin
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
 % io:fwrite("inComp~p~n",[Start1, Start2]),
           Start1 == Start2
        end
         ).
         
in_comp(Expr, Comp, DeComp, Worker) ->
    ?RULE(?T("[f@(Input@) || Input@ <- Inputs@]"),
        begin
          % ?MATCH(?T("{seq, f@}"), Expr@),
          %?TO_AST("blah")
          ?TO_AST("skel:run([{map,[{seq, fun ?MODULE:"++Worker++"/1}], fun ?MODULE:"++Comp++"/1, fun ?MODULE:"++DeComp++"/1}], Inputs@)")
        end,
        begin
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
 % io:fwrite("inComp~p~n",[Start1, Start2]),
           Start1 == Start2
        end
         ).


% ====================

is_expr(Node) ->
    api_refac:syntax_category(Node) == expression.
        
is_the_enclosing_tuple(Node, _Expr) ->
    wrangler_syntax:type(Node) == tuple.
