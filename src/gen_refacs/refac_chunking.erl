-module(refac_chunking).

-behaviour(gen_refac).

-compile(export_all).

-include("../../include/wrangler.hrl").

-export([input_par_prompts/0, select_focus/1, check_pre_cond/1, selective/0, transform/1]).

%%% Will need to input the number of Workers (NW)
input_par_prompts() -> ["Chunk Size: "].

%%%% Get the funtion at the current cursor position
select_focus(_Args=#args{current_file_name=File,
			 highlight_range={Start, End}}) ->
	% api_interface:pos_to_fun_def(File, Pos).
	api_interface:pos_to_expr1(File, Start, End).
    
check_pre_cond(_) -> ok.

selective() -> true.

transform(Args=#args{current_file_name=_File, focus_sel=Expr, user_inputs=[ChunkSize]}) ->
	% {M,F,A} = api_refac:fun_define_info(FunDef),
    transform_in_cur_file(Args, Expr, ChunkSize).

transform_in_cur_file(_Args=#args{current_file_name=File}, Expr, ChunkSize) ->
    ?STOP_TD_TP([in_comp(Expr, ChunkSize), in_skel(Expr, ChunkSize), in_skel2(Expr, ChunkSize)], [File]).

% 
in_comp(Expr, ChunkSize) ->
    ?RULE(?T("[f@(I@) || I@ <- Is@]"),
        begin
          % ?MATCH(?T("{seq, f@}"), Expr@),
          ?TO_AST("skel:run([{map,[{seq,fun ?MODULE:f@/1}], fun skel:partition/1, fun skel:combine/1}], skel:chunk(Is@,"++ChunkSize++"))")
        end,
        begin
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
           Start1 == Start2
        end
         ).

in_skel(Expr, ChunkSize) ->
    ?RULE(?T("skel:run([{farm, [{seq, fun m@:f@/1}], Nw@}], Is@)"),
        begin
          % ?MATCH(?T("{seq, f@}"), Expr@),
          ?TO_AST("skel:run([{map,[{seq,fun m@:f@/1}], fun skel:partition/1, fun skel:combine/1}], skel:chunk(Is@,"++ChunkSize++"))")
        end,
        begin
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
           Start1 == Start2
        end
         ).
         
in_skel2(Expr, ChunkSize) ->
    ?RULE(?T("skel:run([{farm, [{seq, fun ?MODULE:f@/1}], Nw@}], Is@)"),
        begin
          % ?MATCH(?T("{seq, f@}"), Expr@),
          ?TO_AST("skel:run([{map,[{seq,fun ?MODULE:f@/1}], fun skel:partition/1, fun skel:combine/1}], skel:chunk(Is@,"++ChunkSize++"))")
        end,
        begin
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
           Start1 == Start2
        end
         ).

% ====================

is_expr(Node) ->
    api_refac:syntax_category(Node) == expression.
        
is_the_enclosing_tuple(Node, _Expr) ->
    wrangler_syntax:type(Node) == tuple.
