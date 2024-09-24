%% @private
-module(refac_farm_intro).

-behaviour(gen_refac).

-compile(export_all).
-compile(nowarn_export_all).

-include("wrangler.hrl").

-export([input_par_prompts/0, select_focus/1, check_pre_cond/1, selective/0, transform/1]).

%%% Will need to input the number of Workers (NW)
input_par_prompts() -> ["Number of Workers: "].

%%%% Get the funtion at the current cursor position
%select_focus(#args{current_file_name=File, 
%                   highlight_range={Start, End}}) ->
%    {ok, Expr}=api_interface:range_to_node(File, {Start,End}, fun is_expr/1),
    %{ok, L}= api_interface:pos_to_node(
    %             File, Start, fun(Node)->
    %                                  is_the_enclosing_tuple(Node, Expr)
    %                          end),
    %?MATCH(?T("{seq, fun ?MODULE:f@/1}"), L),
    %?MATCH(?T("[ lists:map(fun(X) -> mult_prime(R,X) end, Cols) || R <- Rows]"), L),
%    true,
%    io:fwrite("After condition~n",[]),
%    {ok, {Expr}}.
    
select_focus(_Args=#args{current_file_name=File,
			 highlight_range={Start, End}}) ->
	% api_interface:pos_to_fun_def(File, Pos).
	api_interface:pos_to_expr1(File, Start, End).

check_pre_cond(_) -> ok.

selective() -> true.

transform(Args=#args{current_file_name=_File, focus_sel=Expr, user_inputs=[NW]}) ->
	% {M,F,A} = api_refac:fun_define_info(FunDef),
    transform_in_cur_file(Args, Expr, NW).

transform_in_cur_file(_Args=#args{current_file_name=File}, Expr, NW) ->
    ?STOP_TD_TP([in_seq(Expr, NW), in_map(Expr, NW), in_comp(Expr, NW), in_comp2(Expr, NW)], [File]).

% 
in_seq(Expr, NW) ->
    ?RULE(?T("{seq, E@}"),
        begin
          %?MATCH(?T("{seq, fun ?MODULE:f@/1}"), Expr@),
          ?TO_AST("{farm,[{seq, E@}],"++NW++"}")
        end,
        begin
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
 % io:fwrite("inComp~p~n",[Start1, Start2]),
           Start1 == Start2  
        end
    ).
    
in_map(Expr, NW) ->
    ?RULE(?T("lists:map(fun ?MODULE:f@/1, List@)"),
       begin
         ?TO_AST("skel:run([{farm, [{seq, fun ?MODULE:f@/1}],"++NW++"}],List@)")
       end,
       begin
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
 % io:fwrite("inComp~p~n",[Start1, Start2]),
           Start1 == Start2  
        end ).
        
in_comp(Expr, NW) ->
    ?RULE(?T("[f@(Input@) || Input@ <- Inputs@]"),
       begin
         ?TO_AST("skel:run([{farm, [{seq, fun ?MODULE:f@/1}],"++NW++"}],Inputs@)")
       end,
       begin
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
 % io:fwrite("inComp~p~n",[Start1, Start2]),
           Start1 == Start2  
        end ).
        
in_comp2(Expr, NW) ->
    ?RULE(?T("[Ex@ || X@ <- Xs@]"),
       begin
         Argument = getArg(Ex@),
         Stages = getStages(Ex@, Argument),
         ?TO_AST("skel:run([{farm, [{seq, fun("++Argument++") -> "++Stages++" end}],"++NW++"}],Xs@)")
       end,
       begin
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
 % io:fwrite("inComp~p~n",[Start1, Start2]),
           Start1 == Start2  
        end ).

getStages(Args, X) ->
    case (?MATCH(?T("F@(Args2@)"), Args)) of
        true ->  ?PP(F@)++"(" ++ getStages(Args2@, X)++")" ;
        false -> X 
    end.    

getArg(Args) ->     
   case (?MATCH(?T("F@(Args3@)"), Args)) of
        true ->  getArg(Args3@);
        false -> case (?MATCH(?T("Args4@"), Args)) of
                    true -> ?PP(Args4@);
                    false -> ""
                 end
    end.    


% ====================

is_expr(Node) ->
    api_refac:syntax_category(Node) == expression.
        
is_the_enclosing_tuple(Node, _Expr) ->
    wrangler_syntax:type(Node) == tuple.
