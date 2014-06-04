%% Copyright (c) 2010, Huiqing Li
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


%% Program slicing is the computation of the set of program statements, 
%% i.e. the program slice, that may affect the values at some point of 
%% interest, referred to as a slicing criterion. 
%%
%% This module implements intra-function_claus backward slicing.
%% Things used: data-dependency and control dependency.
%% Things need to be considers: implicit data depdendency caused by 
%% SIDE EFFECT!
%% TO check: slicing of recursive functions; pattern 
%% matching of bound variables.
%% Timeout.
%% Infix expression.
-module(wrangler_slice_new). 

-export([backward_intra_fun_slice/7, backward_slice/2]).
%%-compile(export_all).

-include("../include/wrangler_internal.hrl").


-spec(backward_intra_fun_slice(file:filename(), pos_integer(), pos_integer(),
                               pos_integer(), pos_integer(), [dir()|filename()], pos_integer()) ->
             {ok, [{pos(), pos()}], [{pos(), pos()}], [pos()]} | {error, string()}).
backward_intra_fun_slice(File, StartLine, StartCol, EndLine, EndCol, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD:~p:backward_intra_fun_slice(~p,~p,~p,~p,~p,~p,~p).\n",
                 [?MODULE, File, StartLine, StartCol, EndLine, EndCol,
                  SearchPaths, TabWidth]),
    Start={StartLine, StartCol},
    End={EndLine,EndCol},
    {ok, {AnnAST, _Info}}= wrangler_ast_server:parse_annotate_file(File, true, SearchPaths, TabWidth),
    ExprsOrPats = api_interface:pos_to_expr_or_pat_list(AnnAST, Start, End),
    case ExprsOrPats of
        [] ->
            {error, "You have not selected an expression or a list"
             " of expressions to start the slicing with."};
        _ ->
            ExprRange={ExprStartPos, _} = wrangler_misc:start_end_loc(ExprsOrPats),
            {ok, FunDef} = api_interface:expr_to_fun(AnnAST, hd(ExprsOrPats)),
            %% PARALLELISATION IS PROBLEMATIC BECAUSE OF SIDEEFFECT!!!
            Ranges=[backward_slice(FunDef, E)||E <- ExprsOrPats],  
            {Ranges1, MaybeRanges}=lists:unzip(Ranges),
            {ok, [ExprRange|lists:usort(lists:append(Ranges1))], 
             lists:usort(lists:append(MaybeRanges)), [ExprStartPos]}
    end.

%% intra-clause backward slicing.
backward_slice(Node, Expr) ->
    start_locs_keeper_process(),
    case wrangler_syntax:type(Node) of
        function ->
            Clauses = wrangler_syntax:function_clauses(Node),
            {S, E} = wrangler_misc:start_end_loc(Expr),
            Pred = fun (N) ->
                           {StartPos, EndPos} = wrangler_misc:start_end_loc(N),
                           S >= StartPos andalso E =< EndPos
                   end,
            case [C||C<-Clauses, Pred(C)] of 
                [EncloseClause] ->
                    process_a_clause(EncloseClause,Expr);
                _ ->
                    throw ({error, "Invalid expression selection."})
            end;
        clause ->
            process_a_clause(Node,Expr)
    end,
    Ranges = get_all_ranges(),
    stop_locs_keeper_process(),
    Ranges.

process_clauses(Clauses, Expr) ->
    {S, E} = wrangler_misc:start_end_loc(Expr),
    Pred = fun (Node) ->
		   {StartPos, EndPos} = wrangler_misc:start_end_loc(Node),
		   S >= StartPos andalso E =< EndPos
	   end,
    lists:append([case Pred(C) of 
                      true ->
                          [process_a_clause(C, Expr)];
                      false -> 
                          []
                  end||C <- Clauses]).

       
process_a_clause(C, Expr) ->
    Patterns = wrangler_syntax:clause_patterns(C),
    Guard = wrangler_syntax:clause_guard(C),
    Body = wrangler_syntax:clause_body(C),
    NewBody = process_body(Body, Expr),
    %% io:format("NewBody:~p\n", [NewBody]),
    %% io:format("Expr:~p\n", [Expr]),
    add_ranges([wrangler_misc:start_end_loc(Guard)]
               --[{{0,0},{0,0}}]),
    {NewBodyBound, NewBodyFree} = get_bound_free_vars(NewBody),
    GuardFree  = api_refac:free_vars(Guard),
    Free = lists:usort(GuardFree++NewBodyFree),
    NewPatterns= make_new_patterns(Patterns, Free),
    C1 = wrangler_syntax:clause(NewPatterns, Guard, NewBody),  
    wrangler_misc:update_ann(
      wrangler_misc:update_ann(
        C1, {bound, NewBodyBound}), {free, Free}).

make_new_patterns(Patterns, Free) ->
    [element(1,make_new_pattern_1(P, Free))
     ||P<-Patterns].

make_new_pattern_1(Pattern, Free) ->
    api_ast_traverse:stop_tdTP(
      fun(N,_Others) ->
              case api_refac:type(N) of 
                  variable ->
                      case Free -- api_refac:bound_vars(N) == Free of 
                          true ->
                              {wrangler_syntax:underscore(), true};
                          false -> 
                              add_ranges([wrangler_misc:start_end_loc(N)]),
                              {N, true}
                      end;
                  _ -> 
                      add_ranges([wrangler_misc:start_end_loc(N)]),
                      {N, false}
              end
      end, Pattern, []).

process_body(Body, Expr) when is_list(Body)->
    {S, E} = wrangler_misc:start_end_loc(Expr),
    Exprs1 = lists:takewhile(
               fun (B) ->
                       {StartPos, EndPos} = wrangler_misc:start_end_loc(B),
                       (EndPos =< S) or (S >= StartPos andalso E =< EndPos)
               end,
               Body),
    case lists:suffix([Expr], Exprs1) of
        true -> 
            case Exprs1 of 
                [Expr] -> [Expr];
                _ -> 
                    rm_unused_exprs(Exprs1)
            end;
        false when Exprs1==[]->
            [];
        false ->
            LastExpr = process_expr(lists:last(Exprs1), Expr),
            NewExprs = lists:reverse(tl(lists:reverse(Exprs1))) ++ [LastExpr],
            rm_unused_exprs(NewExprs)
    end.
 
process_expr(Expr, SubExpr) ->
    case wrangler_syntax:type(Expr) of
        match_expr -> 
            Body=get_match_expr_body(Expr),
            Pat =wrangler_syntax:match_expr_pattern(Expr),
            case enclose(Pat, SubExpr) of 
                true ->
                    process_match_expr(Expr, SubExpr);
                false ->
                    process_expr_1(Body, SubExpr)
            end;
        _ -> process_expr_1(Expr, SubExpr)
    end.

process_match_expr(Expr, SubExpr) ->
    Body=get_match_expr_body(Expr),
    Pat =wrangler_syntax:match_expr_pattern(Expr),
    case api_refac:bound_vars(SubExpr) of
        [] ->
            SubExpr;
        _ -> case strict_enclose(Pat, SubExpr) of
                 true ->
                     process_match_expr_1({Pat, Body}, SubExpr);
                 false ->
                     add_ranges([wrangler_misc:start_end_loc(Body)]),
                     Expr
             end
    end.

process_match_expr_1({Pat, Body}, SubExpr) ->
    case {api_refac:type(Pat), api_refac:type(Body)} of
        {tuple, tuple} ->
            PatElems = wrangler_syntax:tuple_elements(Pat),
            BodyElems = wrangler_syntax:tuple_elements(Body),
            process_tuple_elements(SubExpr, lists:zip(PatElems, BodyElems), []);
        {list, list} ->
            PatElems= list_elements(Pat),
            BodyElems = list_elements(Body),
            PatLen = length(PatElems),
            BodyLen = length(BodyElems),
            if PatLen == BodyLen  ->
                    process_list_elements(SubExpr, lists:zip(PatElems, BodyElems), []);
               PatLen <BodyLen ->
                    BodyElems1 = lists:sublist(BodyElems, PatLen-1)
                        ++[lists:sublist(BodyElems, PatLen, BodyLen)],
                    process_list_elements(SubExpr, lists:zip(PatElems, BodyElems1), []);
               true ->
                    PatElems1 = lists:sublist(PatElems, BodyLen-1)
                        ++[lists:sublist(PatElems, BodyLen, PatLen)],
                    process_list_elements(SubExpr, lists:zip(PatElems1, BodyElems), [])
            end;                    
        {_, _} ->
            add_ranges([wrangler_misc:start_end_loc(Body)]),
            Free1 = api_refac:free_vars(Pat),
            Free2 = api_refac:free_vars(Body),
            FreeVars = ordsets:union(Free1, Free2),
            wrangler_misc:update_ann(wrangler_syntax:match_expr(Pat, Body),
                                 {free, FreeVars})
        end.
     

process_tuple_elements(_, [], []) ->
    wrangler_syntax:empty();
process_tuple_elements(_SubExpr, [], Acc) ->
    {Pats, Exprs} = lists:unzip(lists:reverse(Acc)),
    Expr=case {Pats, Exprs} of 
             {[P], [E]} ->
                 wrangler_syntax:match_expr(P, E);
             _ ->
                 Pattern = wrangler_syntax:tuple(Pats),
                 Body = wrangler_syntax:tuple(Exprs),
                 wrangler_syntax:match_expr(Pattern, Body)
         end,
    Free1 = api_refac:free_vars(Pats),
    Free2 = api_refac:free_vars(Exprs),
    FreeVars = ordsets:union(Free1, Free2),
    wrangler_misc:update_ann(Expr,{free, FreeVars});
process_tuple_elements(SubExpr,[{P,E}|PEs], Acc) ->
    case enclose(SubExpr, P) of 
        true ->
            add_ranges([wrangler_misc:start_end_loc(E)]),
            process_tuple_elements(SubExpr, PEs, [{P, E}|Acc]);
        false->
            case strict_enclose(P, SubExpr) of 
                true -> 
                    process_match_expr_1({P, E}, SubExpr);
                false ->
                    process_tuple_elements(SubExpr, PEs, Acc)
            end
    end.


process_list_elements(_, [], []) ->
    wrangler_syntax:empty();
process_list_elements(_SubExpr, [], Acc) ->
    {Pats, Exprs} = lists:unzip(lists:reverse(Acc)),
    Expr=case {Pats, Exprs} of 
             {[P], [E]} ->
                 wrangler_syntax:match_expr(P, E);
             _ ->
                 Pattern = wrangler_syntax:list(Pats),
                 Body = wrangler_syntax:list(Exprs),
                 wrangler_syntax:match_expr(Pattern, Body)
         end,
    Free1 = api_refac:free_vars(Pats),
    Free2 = api_refac:free_vars(Exprs),
    FreeVars = ordsets:union(Free1, Free2),
    wrangler_misc:update_ann(Expr,{free, FreeVars});
process_list_elements(SubExpr,[{P,E}|PEs], Acc) ->
    case enclose(SubExpr, P) of 
        true ->
            add_ranges([wrangler_misc:start_end_loc(E)]),
            E1 = case is_list(E) of 
                     true ->
                         Free1 = api_refac:free_vars(P),
                         Free2 = api_refac:free_vars(E),
                         FreeVars = ordsets:union(Free1, Free2),
                         wrangler_misc:update_ann(wrangler_syntax:list(E),
                                                  {free, FreeVars});
                     _ -> E
                 end,
            process_list_elements(SubExpr, PEs, [{P, E1}|Acc]);
        false->
            case strict_enclose(P, SubExpr) of 
                true -> 
                    process_match_expr_1({P, E}, SubExpr);
                false ->
                    process_list_elements(SubExpr, PEs, Acc)
            end
    end.

list_elements(Node) ->
    lists:reverse(list_elements(Node, [])).
list_elements(Node,As) ->
    case wrangler_syntax:type(Node) of 
        list ->
            As1 = lists:reverse(wrangler_syntax:list_prefix(Node)) ++ As,
	    case wrangler_syntax:list_suffix(Node) of
                none -> As1;
                Tail ->
                    list_elements(Tail, As1)
            end;
        nil ->
            As;
        _ -> 
            [Node|As]
    end.

process_expr_1(Container, Expr) ->
    case Container of 
        Expr -> Expr;
        _ ->
            process_expr_2(Container, Expr)
    end.

process_expr_2(Container, Expr) ->
    case wrangler_syntax:type(Container) of
        case_expr ->
            Args = wrangler_syntax:case_expr_argument(Container),
            {Bound1, Free1} = {api_refac:bound_vars(Args), api_refac:free_vars(Args)},
	    Clauses= wrangler_syntax:case_expr_clauses(Container),
            case process_clauses(Clauses,Expr) of
                [] -> process_expr_2(Args, Expr); %% the expression selected is the args of case.
                [NewClause] ->
                    {Bound2, Free2} ={api_refac:bound_vars(NewClause), api_refac:free_vars(NewClause)},
                    case Bound2==[] andalso Bound1--Free2==Bound1 of
                        true ->
                            E1 = wrangler_syntax:case_expr(Args, [NewClause]),
                            wrangler_misc:update_ann(
                              wrangler_misc:update_ann(E1, {bound, Bound2}), {free, Free2});
                        false ->
                            %% The annotation information is needed for removing used expressions!.
                            add_ranges([wrangler_misc:start_end_loc(Args)]),
                            Bound = ordsets:union(Bound1, Bound2),
                            Free = ordsets:union(Free1, Free2),
                            E1 = wrangler_syntax:case_expr(Args, [NewClause]),
                            wrangler_misc:update_ann(wrangler_misc:update_ann(E1, {bound, Bound}), {free, Free})
                    end
            end;
        block_expr ->
	    Body = wrangler_syntax:block_expr_body(Container),
	    NewBody = process_body(Body, Expr),
            {Bound, Free} = get_bound_free_vars(NewBody),
	    BE = wrangler_syntax:block_expr(NewBody),
	    wrangler_misc:update_ann(wrangler_misc:update_ann(BE, {bound, Bound}), {free, Free});
	if_expr ->
	    Clauses = wrangler_syntax:if_expr_clauses(Container),
	    NewClauses = lists:flatmap(fun (C) -> process_a_clause(C, Expr) end, Clauses),
	    {Bound, Free} = lists:foldl(fun (C, {Bd, Fr}) ->
						{Bd1, Fr1} = {api_refac:bound_vars(C), api_refac:free_vars(C)},
						{ordsets:intersection(Bd, Bd1), ordsets:union(Fr, Fr1)}
					end,
					{[], []}, NewClauses),
	    IE = wrangler_syntax:if_expr(NewClauses),
	    wrangler_misc:update_ann(wrangler_misc:update_ann(IE, {bound, Bound}), {free, Free});
        receive_expr ->
            Clauses = wrangler_syntax:receive_expr_clauses(Container),
            Action  = wrangler_syntax:receive_expr_action(Container),
            TimeOut = wrangler_syntax:receive_expr_timeout(Container),
            case enclose(Clauses, Expr) of
                true -> 
                    process_clauses(Clauses,Expr);
                false ->
                    case enclose(Action, Expr) of 
                        true ->
                            process_body(Action, Expr);
                        false ->
                            process_expr(TimeOut, Expr)
                    end
            end;
        fun_expr ->
            process_fun_expr(Container, Expr);
        list_comp -> 
            process_list_comp(Container, Expr);
        application -> 
            process_application(Container, Expr);
        list ->
            process_list_expr(Container, Expr);
        tuple ->
            process_tuple_expr(Container, Expr);
        _ ->
            Expr
    end.
 
process_fun_expr(Container, Expr) ->
    Cs = wrangler_syntax:fun_expr_clauses(Container),
    process_clauses(Cs,Expr).

process_list_comp(Container, Expr) ->
    Bodys = wrangler_syntax:list_comp_body(Container),
    case lists:filter(fun(B) ->enclose(B, Expr) end, Bodys) of 
        [] -> Container;
        [B] ->
            process_expr_1(B, Expr)
    end.
    
process_application(Container, Expr) ->
    Args = wrangler_syntax:application_arguments(Container),
    process_elems_expr(Container, Args, Expr).

process_list_expr(Container, Expr) ->
    TupleElems = wrangler_syntax:list_elements(Container),
    process_elems_expr(Container, TupleElems, Expr).

process_tuple_expr(Container, Expr) ->
    TupleElems = wrangler_syntax:tuple_elements(Container),
    process_elems_expr(Container, TupleElems, Expr).

process_elems_expr(Container, ContainerElems, Expr) ->
     case lists:member(Expr, ContainerElems) of
         true ->
             Expr;
         _ ->
             case lists:filter(fun(E)
                               ->
                                 strict_enclose(E, Expr)
                            end, ContainerElems) of
                 [E1] ->
                     process_expr_1(E1, Expr);
                 [] ->
                     Container
             end
     end.

%% this is the function that does the backward slicing.
%% Need to think about side effects.
%% make only use of def-use relation.
rm_unused_exprs([]) -> [];
rm_unused_exprs(Exprs) ->
    LastExpr = lists:last(Exprs),
    FreeVars = api_refac:free_vars(LastExpr),
    ReversedPrevExprs = tl(lists:reverse(Exprs)),
    rm_unused_exprs_1(ReversedPrevExprs, FreeVars, [LastExpr]).
    

rm_unused_exprs_1([], _FreeVars, Acc) -> Acc;
rm_unused_exprs_1([E| Exprs], FreeVars, Acc) ->
    ExportedVars = wrangler_misc:exported_vars(E),
    case FreeVars -- ExportedVars =/= FreeVars of
	true ->
            %% some of the exported variables by E are used.
	    FreeVarsInE = api_refac:free_vars(E),
	    NewFreeVars = lists:usort((FreeVars -- ExportedVars) ++ FreeVarsInE),
            add_ranges([api_refac:start_end_loc(E)]),
	    rm_unused_exprs_1(Exprs, NewFreeVars, [E| Acc]);
	false when ExportedVars/=[]->
            rm_unused_exprs_1(Exprs, FreeVars, Acc);
        false ->
            rm_unused_exprs_1(Exprs, FreeVars, Acc) 
    end.

get_match_expr_body(E) ->
    Body = wrangler_syntax:match_expr_body(E),
    case Body of
        match_expr -> get_match_expr_body(Body);
        _ -> Body
    end.

get_bound_free_vars(Body) ->
    Fun = fun (E, {Bd, Fr}) ->
		  {Bd1, Fr1} = {api_refac:bound_vars(E), api_refac:free_vars(E)},
		  {ordsets:union(Bd, Bd1), ordsets:union(Fr, ordsets:subtract(Fr1, Bd))}
	  end,
    lists:foldl(Fun,{[], []}, Body).

enclose(Expr1, Expr2) ->
    {S1, E1} = api_refac:start_end_loc(Expr1),
    {S2, E2} = api_refac:start_end_loc(Expr2),
    S1=<S2 andalso E2=<E1.
    
strict_enclose(Expr1, Expr2) ->
    {S1, E1} = api_refac:start_end_loc(Expr1),
    {S2, E2} = api_refac:start_end_loc(Expr2),
    (S1=<S2 andalso E2<E1) 
        orelse (S1<S2 andalso E2=<E1).
  

%% A process keeps tracking of locations.
start_locs_keeper_process() ->    
    case erlang:whereis(backward_slicer) of 
	undefined -> ok;
	_         -> erlang:unregister(backward_slicer)
    end,
    register(backward_slicer, spawn(fun() -> locs_loop({[], []}) end)).

stop_locs_keeper_process() ->
    backward_slicer!stop.

get_all_ranges() ->
    backward_slicer!{self(), get_all},
    receive 
	{all, Ranges} ->
	    Ranges
    end.


add_ranges(Ranges)->
    backward_slicer ! {add, Ranges}.

add_maybe_ranges(Ranges) ->
    backward_slicer ! {add_maybe, Ranges}.

locs_loop(State={Ranges, MaybeRanges}) ->
    receive
	{add, Rs} ->
            locs_loop({Rs++Ranges, MaybeRanges});
        {add_maybe, Rs} ->
            locs_loop({Ranges, Rs++MaybeRanges});
        {From, get_all} ->
	   From ! {all, State},
	   locs_loop(State);
	stop ->
            ok
    end.

