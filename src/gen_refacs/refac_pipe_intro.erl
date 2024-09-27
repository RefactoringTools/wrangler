%% @private
-module(refac_pipe_intro).

-behaviour(gen_refac).

-compile(export_all).
-compile(nowarn_export_all).

-include("wrangler.hrl").

-export([input_par_prompts/0, select_focus/1, check_pre_cond/1, selective/0, transform/1]).

%%% No input parameters...
input_par_prompts() -> [].

%%%% Get the funtion at the current cursor position
select_focus(_Args=#args{current_file_name=File,
			 highlight_range={Start, End}}) ->
	% api_interface:pos_to_fun_def(File, Pos).
	api_interface:pos_to_expr1(File, Start, End).
	

%select_focus(#args{current_file_name=File, 
%                   highlight_range={Start, End}}) ->
%    {ok, Expr}=api_interface:range_to_node(File, {Start,End}, fun is_expr/1),
%    {ok, L}= api_interface:pos_to_node(
%                 File, Start, fun(Node)-> true
%                                      % is_the_enclosing_listComp(Node, Expr)
%                              end),
%    io:fwrite("Before condition~n",[]),
%    Data = [1,2,3,{car, ?PP(Expr)}],
%    file:write_file("/Users/chrisb/output.txt", io_lib:fwrite("~p.\n", [Data])),
%    %?MATCH(?T("[ f@(g@(X@)) || X@ <- List@]"), L),
%    io:fwrite("After condition~n",[]),
%    {ok, {{Expr},L}}.

% need to check the functions have arity 1 ... 
% although that's probably not a pre-condition.
check_pre_cond(_) -> ok.

selective() -> true.

transform(Args=#args{current_file_name=_File, focus_sel=Expr}) ->
	%{M,F,A} = api_refac:fun_define_info(FunDef),
    transform_in_cur_file(Args, Expr).

transform_in_cur_file(_Args=#args{current_file_name=File}, Expr) ->
    _Start1 = api_refac:start_end_loc(Expr),
    %Start2 = api_refac:start_end_loc(Args@),
    %Data2 = [1,2,3,{in_comp2, Start1}],
    %io:fwrite("transform~p~n",[Data2]),
    ?FULL_TD_TP([in_map(Expr), in_comp(Expr), in_seq(Expr)], [File]). %, in_seq(Expr)],[File]).


%in_seq_parent({M,F,A}) ->
%    ?RULE(?T("f@(Args@@) -> Exprs@@."),
%        begin
%          NewExprs@@ = transform_in_body_seq(Exprs@@),
%          ?TO_AST("f@(Args@@) -> NewExprs@@.")
%        end,
%      api_refac:fun_define_info(f@) == {M,F,A}
%      ).

%transform_in_body_seq(Body) ->
%  {ok, Body1} = ?FULL_TD_TP([in_comp(), in_seq()], Body),
%  Body1.

in_seq(Expr) ->
    ?RULE(?T("{seq, fun(X@) -> Ex@ end}"),
    %?RULE(?T("Args@"),
        begin
          Stages = getStages2(getStages(Ex@)),
          ?TO_AST("{pipe, ["++(lists:reverse(Stages))++"]}")
        end,
        begin
           %{S1, S2} = api_refac:start_end_loc(Expr), 
           %{S11, S22} = api_refac:start_end_loc(Ex@),
%           % io:fwrite("inComp~p~n",[Start1, Start2]),
%           Start1 == Start2
           %(S11 >= S1) and (S22 =< S2)
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
 % io:fwrite("inComp~p~n",[Start1, Start2]),
           Start1 == Start2
        end).
        %).
        
in_map(Expr) ->
    ?RULE(?T("lists:map(fun(X@) -> Ex@ end, Xs@)"),
    %?RULE(?T("Args@"),
        begin
          Stages = getStages2(getStages(Ex@)),
          ?TO_AST("skel:run([{pipe, ["++(lists:reverse(Stages))++"]}], Xs@)")

        end,
        begin
         %  {S1,S2} = api_refac:start_end_loc(Expr), 
         %  {S11, S22} = api_refac:start_end_loc(Ex@),
           % io:fwrite("inComp~p~n",[Start1, Start2]),
           % ?MATCH(?T("[Ex1@ || X1@ <- Xs1@]"), Args@),
         %  (S11 >= S1) and (S22 =< S2)
      %     ?EQUAL(Expr, ?TO_AST("[Ex@ || X@ <- Xs@]"))
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
 % io:fwrite("inComp~p~n",[Start1, Start2]),
           Start1 == Start2
        end
           ).


in_comp(Expr) ->
    ?RULE(?T("[Ex@ || X@ <- Xs@]"),
    %?RULE(?T("Args@"),
        begin
          Stages = getStages2(lists:reverse(getStages(Ex@))),
          ?TO_AST("skel:run([{pipe, ["++Stages++"]}], Xs@)")

        end,
        begin
         %  {S1,S2} = api_refac:start_end_loc(Expr), 
         %  {S11, S22} = api_refac:start_end_loc(Ex@),
           % io:fwrite("inComp~p~n",[Start1, Start2]),
           % ?MATCH(?T("[Ex1@ || X1@ <- Xs1@]"), Args@),
         %  (S11 >= S1) and (S22 =< S2)
      %     ?EQUAL(Expr, ?TO_AST("[Ex@ || X@ <- Xs@]"))
           Start1 = api_refac:start_end_loc(_This@),
           Start2 = api_refac:start_end_loc(Expr),
 % io:fwrite("inComp~p~n",[Start1, Start2]),
           Start1 == Start2
        end
           ).
        %).
        
checkExpr(Args) -> 
    case (?MATCH(?T("f@(Args2@)"), Args)) of 
       true -> checkExpr(Args2@);
       false -> true
    end.
 
getStages2(X) ->
  string_join(",", X). 
 
string_join(Join, L) ->
    string_join(Join, L, fun(E) -> E end).

string_join(_Join, L=[], _Conv) ->
    L;
string_join(Join, [H|Q], Conv) ->
    lists:flatten(lists:concat(
        [Conv(H)|lists:map(fun(E) -> [Join, Conv(E)] end, Q)]
    )). 
 
getStages(Args) ->
    case (?MATCH(?T("F@(Args2@)"), Args)) of
        true ->  ["{seq, fun ?MODULE:"++?PP(F@)++"/1}" | getStages(Args2@) ];
        false -> "" 
    end.    

% =======================

is_expr(Node) ->
    api_refac:syntax_category(Node) == expression.
        
is_the_enclosing_listComp(Node, _Expr) ->
    wrangler_syntax:type(Node) == list_comp.
    % andalso
    %    lists:member(Expr, wrangler_syntax:application_arguments(Node)).
	






