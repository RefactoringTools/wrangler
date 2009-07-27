%% Copyright (c) 2009, Huiqing Li, Simon Thompson
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
%% ===========================================================================================
%% Refactoring: Search an user-selected expression/expression sequence from the current buffer.
%%
%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
-module(refac_sim_code).

-export([sim_code_detection/6, sim_code_detection_in_buffer/6]).

-include("../include/wrangler.hrl").

-define(DefaultSimiScore, 0.8).

-import(refac_duplicated_code, [get_clones_by_suffix_tree/5, display_clone_result/2]).

-compile(export_all).

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.

-define(DEFAULT_LEN, 5).
-define(DEFAULT_FREQ, 2).
-define(DEFAULT_SIMI_SCORE, 0.8).



sim_code_detection_in_buffer(FileName, MinLen, MinFreq, SimiScore, SearchPaths, TabWidth) ->
    sim_code_detection([FileName],MinLen, MinFreq, SimiScore, SearchPaths, TabWidth).

    
sim_code_detection(DirFileList, MinLen1, MinFreq1, SimiScore1, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_code_detection(~p,~p,~p,~p,~p,~p).\n", 
		 [?MODULE, DirFileList, MinLen1, MinFreq1, SimiScore1, SearchPaths, TabWidth]),
    MinLen = case MinLen1 == [] orelse list_to_integer(MinLen1) =< 1 of
		 true -> 
		     ?DEFAULT_LEN;
		 _ -> list_to_integer(MinLen1)
	     end,
    MinFreq = case MinFreq1 == [] orelse list_to_integer(MinFreq1) < ?DEFAULT_FREQ of
		  true -> ?DEFAULT_FREQ;
		  _ -> list_to_integer(MinFreq1)
	      end,
    SimiScore = case SimiScore1 of 
		     [] -> ?DefaultSimiScore;
		    _ -> S = list_to_float(SimiScore1),
			 case (S>=0.1) andalso (S =<1.0) of 
			     true ->S;
			      _ -> ?DefaultSimiScore
			 end
		end,			  
    Pid = start_hash_process(),
    ASTTab = ets:new(ast_tab, [set, public]),
    Files = refac_util:expand_files(DirFileList, ".erl"),
    case Files of
	[] -> ?wrangler_io("Warning: No files found in the searchpaths specified.",[]);
	_ -> generalise_and_hash_ast(Files, Pid, SearchPaths, TabWidth, ASTTab),
	     Dir = filename:dirname(hd(Files)),
	     Cs = get_clones(Pid, MinLen, MinFreq, Dir),
	     Cs1= lists:sort(fun({_, L1, F1}, {_, L2, F2}) ->
				     {L1, F1}>={L2, F2}
			     end, Cs),
	     ?debug("Result1:\n~p\n", [Cs1]),
	     Cs2 = examine_clone_classes(Cs1, MinFreq, SimiScore, ASTTab,[]),
	     ?debug("Result2:\n~p\n", [Cs2]),
	     stop_hash_process(Pid),
	     ets:delete(ASTTab),
	     display_clone_result(remove_fun_info(Cs2), "Similar")
    end,
    {ok, "Similar code detection finished."}.
    

generalise_and_hash_ast(Files, Pid, SearchPaths, TabWidth, ASTTab) ->
    lists:foreach(fun(F) ->
			  generalise_and_hash_ast_1(F, Pid, SearchPaths, TabWidth, ASTTab)
		  end, Files).


generalise_and_hash_ast_1(FName, Pid, SearchPaths, TabWidth, ASTTab) ->
    Fun = fun(Form) ->
		  case refac_syntax:type(Form) of 
		      function -> 
			  FunName = refac_syntax:atom_value(refac_syntax:function_name(Form)),
			  Arity = refac_syntax:function_arity(Form),
			  ets:insert(ASTTab, {{FName, FunName, Arity}, refac_util:update_ann(Form,{toks, []})}),
			  {Form1, _} = refac_util:full_tdTP(fun generalise_and_hash_ast_2/2, 
							    Form, {FName, FunName, Arity, Pid}),
			  [Form1];
		      _ -> [Form]
		  end
	  end,
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, SearchPaths, TabWidth),
    Forms = refac_syntax:form_list_elements(AnnAST),
    refac_syntax:form_list([Fun(F)||F<-Forms]).
  
generalise_and_hash_ast_2(Node, {FName, FunName, Arity, Pid}) ->
    F0 = fun(T, _Others) ->
		case variable_replaceable(T) of 
		    true ->
			{refac_syntax:variable('Var'), true};
		    false -> {T, false}
		end
	end,
    F1 = fun(T) ->
		 {T1, _} =refac_util:stop_tdTP(F0, T, []),
		 HashVal = erlang:md5(refac_prettypr:format(T1)),
		 {S, E} = refac_util:get_range(T),
		 insert_hash(Pid, HashVal, {{FName, FunName, Arity}, S, E}),
		 T1
	 end,
    case refac_syntax:type(Node) of 
	clause -> 
	   %%  P = refac_syntax:clause_patterns(Node),
%% 	    G = refac_syntax:clause_guard(Node),
	    Body = refac_syntax:clause_body(Node),
	    insert_dummy_entry(Pid),
	    _NewBody = [F1(E) || E <-Body],
%% 	    {refac_util:rewrite(Node, refac_syntax:clause(P, G, NewBody)), true};
	    {Node, true};
	block_expr ->
	    insert_dummy_entry(Pid),
	    Body = refac_syntax:block_expr_body(Node),
	    _NewBody = [F1(E) || E <-Body],
	   %% Node1 =refac_util:rewrite(Node, refac_syntax:block_expr(NewBody)),
	    {Node,true};
	try_expr  ->
	    insert_dummy_entry(Pid),
	    Body = refac_syntax:try_expr_body(Node),
	    _NewBody=[F1(E) || E <-Body],
	    %% Cs = refac_syntax:try_expr_clauses(Node),
%% 	    Handlers = refac_syntax:try_expr_handlers(Node),
%% 	    After = refac_syntax:try_expr_after(Node),
%% 	    Node1 = refac_util:rewrite(Node, refac_syntax:try_expr(NewBody, Cs, Handlers, After)),
	    {Node, true};
	_ -> {Node, false}
    end.



variable_replaceable(Exp) ->
    case lists:keysearch(category,1, refac_syntax:get_ann(Exp)) of 
	{value, {category, record_field}} -> false;
	{value, {category, record_type}} -> false;	 
	{value, {category, guard_expression}} -> false;
	{value, {category, macro_name}} -> false;
	{value, {category, pattern}} -> 
 	    %%refac_syntax:is_literal(Exp) orelse 
	    refac_syntax:type(Exp)==variable;
	_ -> T = refac_syntax:type(Exp),
	     (not (lists:member(T, [match_expr, operator, case_expr, 
				    block_expr, catch_expr, fun_expr,
				    receive_expr, try_expr, clause,
				    binary_field, query_expr])))
    end.
    


examine_clone_classes([], _MinFreq, _SimiScore, _ASTTab, Acc) -> 
    Acc;
examine_clone_classes([C|Cs], MinFreq, SimiScore, ASTTab, Acc) ->
    {Ranges, Len, Freq} = C, 
    case lists:any(fun(A) ->is_sub_clone({Ranges, Len, Freq, none}, A) end, Acc) of 
	true ->
	    examine_clone_classes(Cs, MinFreq, SimiScore, ASTTab, Acc);
	_ ->
            Res = examine_a_clone_class(C, MinFreq, SimiScore, ASTTab),
	    Res1 = [R||R <-Res, not(lists:any(fun(A)-> is_sub_clone(R, A) end, Acc))],
	    examine_clone_classes(Cs, MinFreq, SimiScore, ASTTab, Res1++Acc)
    end.
    
remove_fun_info(Cs) ->
    [remove_fun_info_1(C) || C <-Cs].
remove_fun_info_1({Ranges, Len, Freq, C}) ->
    Ranges1=[{{F, SLine, SCol}, {F, EndLine, EndCol}} || {{F,_, _},{SLine, SCol}, {EndLine, EndCol}} <-Ranges],
    {Ranges1, Len, Freq, C}.

remove_sub_clones(Cs) ->
    Cs1 = lists:sort(fun({_Ranges1, Len1, Freq1, _}, {_Ranges2, Len2, Freq2,_})
			-> {Len1, Freq1} >= {Len2, Freq2}
		     end, Cs),
    remove_sub_clones(Cs1,[]).

remove_sub_clones([], Acc_Cs) ->
    Acc_Cs;
remove_sub_clones([C|Cs], Acc_Cs) ->
    R = lists:any(fun(C1)-> is_sub_clone(C, C1) end, Acc_Cs),
    case R of 
	true ->remove_sub_clones(Cs, Acc_Cs);
	_ -> remove_sub_clones(Cs, Acc_Cs++[C])
    end.

is_sub_clone({Ranges1, Len1, Freq1,_}, {Ranges2, Len2, Freq2,_}) ->
    case Freq1=<Freq2 andalso Len1=<Len2 of 
	true ->
	    lists:all(fun ({FMA,S, E}) -> 
			      lists:any(fun ({FMA1,S1, E1}) ->
						FMA == FMA1 andalso
						    S1 =< S andalso 
						    E =< E1 
					end, Ranges2) 
		      end, Ranges1);
	false ->
	    false
    end.
    
examine_a_clone_class(C={Ranges, _Len, _Freq}, MinFreq, SimiScore, ASTTab) ->
    Res = examine_clone_members(Ranges, C, MinFreq, SimiScore, ASTTab, []),
    remove_sub_clones(Res).
 
examine_clone_members([], _,_, _, _, Acc) ->
    Acc;    
examine_clone_members([R|Rs], C={Ranges,Len, Freq}, MinFreq, SimiScore, ASTTab, Acc) ->
    Res = examine_a_clone_member(R, C,  MinFreq, SimiScore, ASTTab),
    case Res of 
	[] -> 
	    Ranges1 =Ranges--[R],
	    examine_clone_members(Rs, {Ranges1, Len, Freq}, MinFreq, SimiScore, ASTTab, Acc);
	{_Rs, _L, Freq, _Str} ->
	    [Res|Acc];
	_ ->
	    examine_clone_members(Rs, C, MinFreq, SimiScore, ASTTab, [Res|Acc])
    end.

examine_a_clone_member(Range={_FName, _Start, End}, {Rs, Len, _Freq},  MinFreq, SimiScore, ASTTab) ->
    {FunDef, Exprs1} = get_expr_list(Range, ASTTab),
    Res =[find_anti_unifier(Exprs1, Range1, SimiScore, ASTTab)||Range1 <-Rs, Range1=/=Range],
    Res1 = lists:append(Res),
    case length(Res1) < MinFreq-1 of
	true ->[];
	_ ->
	    {Ranges, ExportVars, SubSt} = lists:unzip3(Res1),
	    ExportVars1 = {element(1,lists:unzip(refac_sim_expr_search:vars_to_export(FunDef, End, Exprs1))), lists:usort(lists:append(ExportVars))},
	    AntiUnifier = refac_sim_expr_search:generalise_expr(Exprs1,SubSt, ExportVars1),
	    {[Range|Ranges], Len, length(Ranges)+1, refac_prettypr:format(AntiUnifier)}
    end.
    
find_anti_unifier(Exprs1, Range,SimiScore, ASTTab)->
    {FunDef2, Exprs2} = get_expr_list(Range, ASTTab),
    Res =refac_sim_expr_search:find_anti_unifier(Exprs1, Exprs2, SimiScore, FunDef2),
    case Res of 
	[] ->
	     Res;
	[{_, EVs, SubSt}] -> [{Range, EVs, SubSt}]
    end.


    
    
get_expr_list({{FName, FunName, Arity}, StartLoc, EndLoc}, ASTTab) ->
    [{_, FunDef}] = ets:lookup(ASTTab, {FName, FunName, Arity}),
    case refac_util:once_tdTU(fun get_expr_list_1/2, FunDef, {StartLoc, EndLoc}) of 
	{_, false} -> {FunDef, []};
	{R, true} -> {FunDef, R}
    end.

get_expr_list_1(Node, {StartLoc, EndLoc}) ->
    case refac_syntax:type(Node) of
	clause ->
	    Exprs = refac_syntax:clause_body(Node),
	    Exprs1 = [E ||E<-Exprs, {SLoc, ELoc}<-[refac_util:get_range(E)],
			 StartLoc =< SLoc, ELoc=<EndLoc],
	    {Exprs1, Exprs1=/=[]};
	block_expr ->
	    Exprs = refac_syntax:block_expr_body(Node),
	    Exprs1 = [E ||E<-Exprs, {SLoc, ELoc}<-[refac_util:get_range(E)],
			  StartLoc =< SLoc, ELoc=<EndLoc],
	    {Exprs1, Exprs1=/=[]};	       
	try_expr -> 
	    Exprs = refac_syntax:try_expr_body(Node),
	    Exprs1 = [E ||E<-Exprs, {SLoc, ELoc}<-[refac_util:get_range(E)],
			  StartLoc =< SLoc, ELoc=<EndLoc],
	    {Exprs1, Exprs1=/=[]};
	_ ->{[], false}
    end.
 
    
start_hash_process() ->		     
    HashTab = ets:new(hash_tab, [set, public]),
    spawn_link(fun()->hash_loop({1,HashTab,[]}) end).

stop_hash_process(Pid) ->
    Pid!stop.

insert_hash(Pid, HashVal,Range) ->
    Pid ! {add, HashVal, Range}.

insert_dummy_entry(Pid) ->
    Pid ! add_dummy.

get_clones(Pid, MinLen, MinFreq, Dir) ->
    Pid! {get_clones, self(), MinLen, MinFreq, Dir},
    receive
	{Pid, Cs} -> 
	    Cs
    end.
    
hash_loop({Index, HashTab, Data}) ->
    receive
      {add, Key, Range} ->
	  case ets:lookup(HashTab, Key) of
	    [{Key, I}] ->
		hash_loop({Index, HashTab, [{I, Range}| Data]});
	    [] -> ets:insert(HashTab, {Key, Index}),
		  hash_loop({Index + 1, HashTab, [{Index, Range}| Data]})
	  end;
      add_dummy ->
	  hash_loop({Index, HashTab, [{'#', {'_', {0, 0}, {0, 0}}}| Data]});
      {get_clones, From, MinLen, MinFreq, Dir} ->
	  Cs = search_for_clones(Dir,lists:reverse(Data), MinLen, MinFreq),
	  From ! {self(), Cs},
	  hash_loop({Index, HashTab, Data});
      stop ->
	  ets:delete(HashTab),
	  ok
    end.


search_for_clones(Dir, Data, MinLen, MinFreq) ->
    F0 = fun(I) ->
		 case is_integer(I) of 
		     true -> integer_to_list(I) ++ ",";
		     false -> atom_to_list(I)
		 end
	 end,
    F =fun({I, Range}) ->
	       lists:duplicate(length(F0(I)), {I, Range})
       end,
    IndexStr = lists:append([F0(I)|| {I, _}<-Data]),
    NewData =lists:append([F(Elem) ||Elem <-Data]),
    ?debug("Databeforeduplication:\n~p\n", [Data]),
    ?debug("DataWithDuplicates:\n~p\n", [NewData]),
    Cs= get_clones_by_suffix_tree(Dir, IndexStr++"&",MinLen, MinFreq,"0123456789,#&"),
    ?debug("Cs:\n~p\n", [Cs]),	 	
    Cs1 = lists:append([strip_a_clone({[{S,E} |Ranges], Len, Freq}, SubStr, MinLen)
			|| {[{S,E} |Ranges], Len, Freq} <- Cs, 
			      SubStr <-[lists:sublist(IndexStr, S, E-S+1)]]),
    ?debug("Cs1:\n~p\n", [Cs1]),
    Cs3 = get_clones_in_ranges(Cs1, NewData, MinLen, MinFreq),
    ?debug("Cs3:\n~p\n", [Cs3]),
    Cs3.    

strip_a_clone({Ranges, Len, F}, Str, MinLen) ->
    {Str1, Str2} = lists:splitwith(fun(C) ->C==$# orelse C ==$, end, Str),
    {Str21, Str22} = lists:splitwith(fun(C) ->C==$# orelse C ==$, end, lists:reverse(Str2)),
    case Str22=="" of 
	true -> [];
	_ -> ?debug("NewStr:\n~p\n", [lists:reverse(Str22)]),
	     NewRanges = [{S+length(Str1), E-length(Str21)}|| {S, E} <-Ranges],
	     NewLen = Len-length(Str1) -length(Str21),
	     case NewLen >= MinLen*2-1 of
		 true ->
		     split_a_clone({NewRanges,NewLen, F},lists:reverse(Str22), MinLen);
		 _ -> []
	     end	    
    end.

split_a_clone(_, "", _)->[];
split_a_clone({Ranges, Len, F}, Str, MinLen) ->
    ?debug("Str:\n~p\n", [Str]),
    {Str1, Str2} = lists:splitwith(fun(C) -> C=/=$# end, Str),
    Len1 = length(Str1),
    {NewRanges, RemainedRanges} = lists:unzip([{{S, S+Len1-1}, {S+Len1+1, E}}
					       ||{S, E} <-Ranges]),
    ?debug("NewRanges:\n~p\n", [NewRanges]),
    ?debug("RemaindRanges:\n~p\n", [RemainedRanges]),
    ?debug("Str1:\n~p\n", [Str1]),
    ?debug("Str2:\n~p\n", [Str2]),
    case Str2 of 
	"" ->
	    case Len1>= MinLen*2-1  of
		true -> [{NewRanges, Len1, F}];
		false -> []
	    end;
	[$#|Str3] ->
	    case Len1>= MinLen*2-1 of
		true ->
		    [{NewRanges, Len1, F} | split_a_clone({RemainedRanges, Len, F}, Str3, MinLen)];
		false ->
		    split_a_clone({RemainedRanges, Len, F}, Str3, MinLen)
	    end	    
    end.
   
%% get_clones_in_ranges(Cs, Data, MinLen, MinFreq) ->
%%     F0 = fun({S, E}) ->
%% 		 {_, Ranges}=lists:unzip(lists:sublist(Data, S, E-S+1)),
%% 		 {F1, S1, _E1}=hd(Ranges),
%% 		 {_F2, _S2, E2} =lists:last(Ranges),
%% 		 {F1, S1, E2}
%% 	 end,
%%     F= fun({Ranges=[{S, E}|_], _Len, Freq}) ->
%% 	       NewRanges = [F0(R)|| R <- Ranges],
%% 	       {_, Rs}=lists:unzip(lists:sublist(Data, S, E-S+1)),
%% 	       NewLen = length(refac_util:remove_duplicates(Rs)),
%% 	       [{NewRanges, NewLen, Freq}]
%%        end,       
%%     [{NewRanges, NewLen, Freq}||C <-Cs, 
%% 				{NewRanges, NewLen, Freq}<-F(C), 
%% 				NewLen>=MinLen, Freq>=MinFreq].

get_clones_in_ranges(Cs, Data, MinLen, MinFreq) ->
    F0 = fun({S, E}) ->
 		 {_, Ranges}=lists:unzip(lists:sublist(Data, S, E-S+1)),
		 Ranges1 =refac_util:remove_duplicates(Ranges),
		 sub_list(Ranges1, MinLen, length(Ranges1))
	 end,
    F1 = fun(Rs) ->
		 [begin {MFA, S1, _E1} = hd(R),
			{_, _, E2} = lists:last(R),
			{MFA, S1, E2}
		  end ||R<-Rs]
	 end,		 
    F= fun({Ranges, _Len, Freq}) ->
	       case Freq>=MinFreq of 
		   true -> NewRanges0 = [F0(R)|| R <- Ranges],
			   NewRanges = zip(NewRanges0),
			   [{F1(Rs),length(hd(Rs)), Freq}|| Rs <-NewRanges];
		   _ ->[]
	       end
       end,    
    lists:append([F(C) || C<-Cs]).
    
zip(L) ->[nested_tuple_to_list(E)||E<-zip_1(L)].
zip_1([]) ->[];
zip_1([L]) ->L;
zip_1([L,H|T]) ->
    zip_1([lists:zip(L,H)|T]).

nested_tuple_to_list({A,B}) ->
			  nested_tuple_to_list(A)++[B];
nested_tuple_to_list(A) ->
    [A].

sub_list(List, Len, MaxLen) ->
   sub_list_1(List, Len, MaxLen, []).
sub_list_1(_List, Len, MaxLen, Acc) when Len>MaxLen ->
    Acc;
sub_list_1(List, Len, MaxLen, Acc) when Len==MaxLen ->
    [List|Acc];
sub_list_1(List, Len, MaxLen, Acc) ->
   SubLists = [lists:sublist(List, I, Len)||I<- lists:seq(1, length(List)-Len+1)],    
   sub_list_1(List, Len+1, MaxLen, SubLists++Acc).

     
    
    
