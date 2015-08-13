%% @private
-module(refac_intro_parallel).

-compile(export_all).

-include("../include/wrangler.hrl").

%% This function assumes that the function 
%% is an acummulative tail-recursive function.

test() ->
    parallel_acc_tail_recursion("./gen_refacs/test.erl", {22, 12}).

test1() ->
    parallel_acc_tail_recursion("./gen_refacs/test.erl", {186, 12}).

test2() ->
    parallel_acc_tail_recursion("./gen_refacs/test.erl", {65, 12}).


parallel_acc_tail_recursion(File, Pos) ->
    {ok, FunDef} = api_interface:pos_to_fun_def(File, Pos),
    MFA={_, FunName, Arity}= api_refac:fun_define_info(FunDef),
    case is_accumative(FunDef, MFA) of 
        false -> {error, "The function selected is not an accumulative tail-recursion function."};
        {true, [{Index, Expr, _SliceLocs}]} ->
            Cs = wrangler_syntax:function_clauses(FunDef),
            BaseCs = [C||C<-Cs, is_accumulative_clause(C, MFA)
                             =={true, base_case}],
            [C] = Cs--BaseCs, %% remove the limitation!!!
            NewPars = generate_new_parameters([C], Index, Arity),
            %% VERY STRONG CONSTRAINT HERE!
            {AccSlice, WorkerSlice, _DispatchSlice} = partition_clause_body(C, Index, Expr),
            TopFun = do_create_par_fun(FunName, NewPars), 
            io:format("\n\n"),
            ?wrangler_io((wrangler_prettypr:format(TopFun)), []),
            io:format("\n\n"),
            {Worker, AccPars, FreeVars} =
                do_intro_worker_loop(FunName, C, {WorkerSlice, AccSlice}, Index, Expr),
            ?wrangler_io((wrangler_prettypr:format(Worker)), []),
            DispatchFun = do_create_dispatch_and_collect_loop(
                            FunDef, MFA, Index, FreeVars, AccSlice, AccPars),
            io:format("\n\n"),
            ?wrangler_io((wrangler_prettypr:format(DispatchFun)), []),
            io:format("\n\n"),
            OneOf = oneof_fun(),
            io:format("\n\n"),
            ?wrangler_io((wrangler_prettypr:format(OneOf)),[]),
            io:format("\n\n")
    end.

generate_new_parameters(Cs, Index, Arity) ->
    Arity1 = case Index of 
                 0 -> Arity;
                 _ -> Arity-1
             end,
    NonAccPats=[begin
                    Pats = wrangler_syntax:clause_patterns(C),
                    case Index of
                        0 -> Pats;
                        _ -> Pats -- [lists:nth(Index, Pats)]
                    end
                end ||C<-Cs],
    [begin
         AllIthPats=[lists:nth(I, P)||P<-NonAccPats],
         Vars=lists:filter(fun(P) ->
                                   api_refac:type(P)==variable
                           end, AllIthPats),
         case Vars of 
             [] -> wrangler_syntax:variable(
                     list_to_atom("Var_" ++ integer_to_list(I)));
             [V|_] -> V
                   end
     end
     ||I<-lists:seq(1, Arity1)].
       
 
%% This function needs to be refined.
partition_clause_body(C, AccIndex, AccExpr) ->
    Body0 = wrangler_syntax:clause_body(C),
    Pats = wrangler_syntax:clause_patterns(C),
    Body = lists:reverse(tl(lists:reverse(Body0))),
    {AccSlice, _FreeVars} = wrangler_slice_new:backward_slice(Body0, AccExpr),
    %% io:format("Accslice:~p\n", [AccSlice]),
    LastExpr = lists:last(Body0), 
    Args = wrangler_syntax:application_arguments(LastExpr),
    NonAccArgs =case AccIndex of 
                    0 -> Args;
                    _ ->lists:sublist(Args,1, AccIndex-1)++
                            lists:sublist(Args, AccIndex+1, length(Args))
                end,
    MainLoopSlice=sets:to_list(
                    sets:from_list(lists:append(
                             [begin
                                  Res=element(1,wrangler_slice_new:backward_slice(Body0, Arg)),
                                  Res--[Arg]
                              end
                              ||Arg <-NonAccArgs]))),
    AccPat = case AccIndex of 
                 0 -> wrangler_syntax:empty_node();
                 _ ->lists:nth(AccIndex, Pats)
             end,
    AccPatVars=wrangler_misc:exported_vars(AccPat),
    {WorkerSlice, AccSlice1}=case AccIndex of 
                                 0 ->{Body --MainLoopSlice, []};
                                 _ -> lists:splitwith(fun(B) ->
                                                              F = wrangler_misc:free_vars(B),
                                                              F -- AccPatVars == F
                                                      end, AccSlice)
                             end,
    {AccSlice1, WorkerSlice, MainLoopSlice}.
    
    
    
do_create_dispatch_and_collect_loop(FunDef, MFA={_M, FunName, _A}, Index, AccSliceFreeVars,
                                    AccBody, AccPars) ->
    DispatchFunName= wrangler_syntax:atom(
                       list_to_atom(atom_to_list(FunName)
                                    ++"_dispatch_and_collect_loop")),
    Cs = wrangler_syntax:function_clauses(FunDef),
    RecvCs = lists:append(
               [wrangler_syntax:receive_expr_clauses(
                  gen_receive_clause(C, MFA, Index, AccSliceFreeVars))||C<-Cs]),
    case AccBody of 
        [] ->
            CollectCs = 
                api_refac:subst(
                  ?T("receive
                       {{worker, _Pid}, RecvIndex, AccPars@@} ->
                          f@(
                             Parent,Workers, RecvIndex+1, CurIndex)
                         end"), [{'_W_f@', DispatchFunName},
                           {'AccPars@@', AccPars}]);
        _ ->
            {NewAccBody, NewAcc} = case length(AccBody)>1 of 
                                       true -> {lists:sublist(AccBody,length(AccBody)-1),
                                                lists:last(AccBody)};
                                       false  ->
                                           {[wrangler_syntax:empty_node()], lists:last(AccBody)}
                           end,
            CollectCs = api_refac:subst(
                              ?T("receive
                     {{worker, _Pid}, RecvIndex, AccPars@@} ->
                           NewAccSlice@@,
                           f@(
                             Parent, NewAcc@, Workers, RecvIndex+1, CurIndex)
                   end"), [{'_W_f@', DispatchFunName},
                           {'AccPars@@', AccPars},
                           {'NewAccSlice@@', NewAccBody},
                           {'NewAcc@', NewAcc}])
    end,
    AllRecvCs = RecvCs++wrangler_syntax:receive_expr_clauses(CollectCs),
    RecvExpr = wrangler_syntax:receive_expr(AllRecvCs),
    Code = case Index /=0 of 
               true ->?T("f@(Parent, Acc, Workers, RecvIndex, CurIndex) ->
                          Body@.");
               false ->
                   ?T("f@(Parent, Workers, RecvIndex, CurIndex) ->
                          Body@.")
           end,
    api_refac:subst(Code, [{'Body@',RecvExpr},
                           {'_W_f@', DispatchFunName}]).
                         
gen_receive_clause(C, MFA={_,FunName,_}, AccIndex, AccSliceFreeVars) ->
    DispatchFunName= wrangler_syntax:atom(
                        list_to_atom(atom_to_list(FunName)
                                     ++"_dispatch_and_collect_loop")),
    Pats = wrangler_syntax:clause_patterns(C),
    Guard = wrangler_syntax:clause_guard(C),
    Body = wrangler_syntax:clause_body(C),
    NewPats = case AccIndex of 
                  0 -> Pats;
                  _ ->
                      lists:sublist(Pats, 1, AccIndex-1)++
                          lists:sublist(Pats, AccIndex+1, length(Pats))
              end,
    RevPat = case length(NewPats) >1 of 
                 true -> wrangler_syntax:tuple(NewPats);
                 false -> hd(NewPats)
             end,
    case is_accumulative_clause(C, MFA) of 
        {true, base_case} ->
            Body1 = case length(Body) of 
                        1 -> [wrangler_syntax:empty_node()];
                        _ ->lists:sublist(Body,1, length(Body)-1)
                    end,
            Code = case AccIndex ==0 of 
                       true when Guard==none->
                           ?T("receive
                             RevPat@ when RecvIndex==CurIndex ->
                              Body@@,
                              Parent!{self(), LastExpr@};
                             RevPat@ when RecvIndex < CurIndex ->
                              f@(
                              Parent,Workers, RecvIndex, CurIndex)
                           end");
                       true ->
                           ?T("receive
                             RevPat@ when Guard@@ andalso RecvIndex==CurIndex ->
                              Body@@,
                              Parent!{self(), LastExpr@};
                             RevPat@ when RecvIndex < CurIndex ->
                              f@(
                              Parent,Workers, RecvIndex, CurIndex)
                           end");
                       false when Guard==none->
                        ?T("receive
                         RevPat@ when RecvIndex==CurIndex ->
                            Body@@,
                            Parent!{self(), LastExpr@};
                         RevPat@ when RecvIndex < CurIndex ->
                           f@(
                              Parent, Acc, Workers, RecvIndex, CurIndex)
                        end");
                       false ->
                           ?T("receive
                         RevPat@ when Guard@@ andalso RecvIndex==CurIndex ->
                            Body@@,
                            Parent!{self(), LastExpr@};
                         RevPat@ when RecvIndex < CurIndex ->
                           f@(
                              Parent, Acc, Workers, RecvIndex, CurIndex)
                        end")   
                   end,
            api_refac:subst(Code, [{'Body@@', Body1}, {'LastExpr@', lists:last(Body)},
                                   {'RevPat@', RevPat},
                                   {'_W_f@', DispatchFunName},
                                   {'Guard@@', Guard}]);
        {true, [{I, _A, _Ls}]} -> %% assume only one accumulator!
            LastExpr = lists:last(Body),  %% This is the recursive call.
            Args = wrangler_syntax:application_arguments(LastExpr),
            NonAccArgs =case AccIndex of 
                            0 -> Args;
                            _ ->lists:sublist(Args,1, AccIndex-1)++
                                    lists:sublist(Args, AccIndex+1, length(Args))
                        end,
            NextLoopPars = case length(NonAccArgs) of 
                               1 -> hd(NonAccArgs);   
                               _ ->wrangler_syntax:tuple(NonAccArgs)
                           end,
            Body = wrangler_syntax:clause_body(C),
            %% NEED TO REFINED HERE!
            Slice=sets:to_list(
                    sets:from_list(lists:append(
                                     [begin
                                          {NewBody, _FreeVars} = wrangler_slice_new:backward_slice(Body, Expr),
                                          NewBody --[Expr]
                                      end
                                      ||Expr <-NonAccArgs,Expr/=none]))),
            Code= case I of 
                      0 when Guard==none->
                          ?T("receive
                           RevPat@ ->
                              Slice@@,
                              Pid = oneof(Workers),
                              Pid!{self(), Pars@@},
                              self()!NextLoopPars@,
                              f@(
                                Parent, Workers, RecvIndex, CurIndex+1)
                           end");
                      0 ->
                          ?T("receive
                           RevPat@ when Guard@@->
                              Slice@@,
                              Pid = oneof(Workers),
                              Pid!{self(), Pars@@},
                              self()!NextLoopPars@,
                              f@(
                                Parent, Workers, RecvIndex, CurIndex+1)
                           end");
                      _ when Guard ==none ->
                          ?T("receive
                           RevPat@ ->
                             Slice@@,
                             Pid = oneof(Workers),
                             Pid!{self(), Pars@@},
                             self()!NextLoopPars@,
                             f@(
                                Parent, Acc, Workers, RecvIndex, CurIndex+1)
                          end");
                      _ ->
                          ?T("receive
                           RevPat@ when Guard@@->
                             Slice@@,
                             Pid = oneof(Workers),
                             Pid!{self(), Pars@@},
                             self()!NextLoopPars@,
                             f@(
                                Parent, Acc, Workers, RecvIndex, CurIndex+1)
                          end")
                      end,
            api_refac:subst(Code, [{'RevPat@', RevPat}, 
                                   {'NextLoopPars@', NextLoopPars},
                                   {'_W_f@', DispatchFunName},
                                   {'Slice@@', Slice},
                                   {'Pars@@', AccSliceFreeVars},
                                   {'Guard@@', Guard}])
    end.

                              
                              
    
do_create_par_fun(FunName, FreeVars) ->
    NewFunName = wrangler_syntax:atom(
                   list_to_atom(atom_to_list(FunName)++
                                    "_parallel")),
    LoopFunName= wrangler_syntax:atom(
                   list_to_atom(atom_to_list(FunName)
                                ++"_loop")),
    DispatchFunName= wrangler_syntax:atom(
                       list_to_atom(atom_to_list(FunName)
                                    ++"_dispatch_and_collect_loop")),
    NewCode =
        ?T("f1@(Pars@@, Initial) ->
            Parent = self(),
            Workers = [spawn_link(fun()->
                                    f2@()
                                  end)
                       || _I<-lists:seq(1, erlang:system_info(schedulers))],
            Pid = spawn_link(fun()-> 
                              f3@(Parent, Initial, Workers, 0, 0)
                             end),
            Pid!IntialPars@,
            receive
              {Pid, Acc} ->
                [P!stop||P<-Workers],
                Acc
            end."),
    InitialPar =  case length(FreeVars) >1 of   %% TOFIX: []
                      true -> wrangler_syntax:tuple(FreeVars);
                      _ -> hd(FreeVars)
                  end,
    api_refac:subst(NewCode, [{'Pars@@', FreeVars},
                              {'IntialPars@', InitialPar},
                              {'_W_f1@', NewFunName},
                              {'_W_f2@', LoopFunName},
                              {'_W_f3@', DispatchFunName}]).
                
  
do_intro_worker_loop(FunName, C, {WorkerSlice, AccSlice}, AccIndex, _AccExpr) ->
    NewFunName = wrangler_syntax:atom(
                   list_to_atom(atom_to_list(FunName)
                   ++"_loop")),
    Args = wrangler_syntax:clause_patterns(C),
    AccPat = case AccIndex of 
                 0 -> wrangler_syntax:empty_node();
                 _ ->lists:nth(AccIndex, Args)
             end,
    AccPatVars=wrangler_misc:exported_vars(AccPat),
    FreeVars = [wrangler_syntax:variable(V)||
                    V<-element(1, 
                               lists:unzip(
                                 lists:keysort(2,
                                               wrangler_misc:free_vars(WorkerSlice))))],  
    AccPars0 =element(1, lists:unzip(wrangler_misc:free_vars(AccSlice)
                                    --AccPatVars)),
    AccPars = [wrangler_syntax:variable(V)||V<-AccPars0],
    NewCodeTemp = 
       ?T("f@()-> 
          receive 
             {Parent, Pars@@, Index} ->
                  Body@@,
                  Parent ! {{worker, self()}, Index, AccPars@@}, 
                  f@();
              stop -> ok
          end"),
    NewCode=api_refac:subst(NewCodeTemp,
                            [{'Body@@', WorkerSlice},
                             {'Pars@@', FreeVars},
                             {'AccPars@@', AccPars},
                             {'_W_f@', NewFunName}]),
    {NewCode, AccPars, FreeVars}.
    
                  
oneof_fun() ->
    ?T("oneof(Workers) ->
          ProcInfo = [{Pid, process_info(Pid, message_queue_len)}
                      ||Pid<-Workers],
          [{Pid, _}|_] = lists:keysort(2, ProcInfo),
          Pid.").

                  

acc_tail_recursive(File) ->
    ?wrangler_io("\nCMD: ~p:acc_tail_recursive(~p).\n",
		 [?MODULE, File]),
    MFAs = lists:flatmap(fun (F) ->
				 acc_tail_recursive_1(F, [],8)
			 end, [File]),
    {ok, MFAs}.

acc_tail_recursive_1(FName, SearchPaths, TabWidth) ->
    {ok, {AnnAST, Info}} = wrangler_ast_server:parse_annotate_file(
                             FName, true, SearchPaths, TabWidth),
    ModName = get_module_name(FName, Info),
    Fun = fun (T, S) ->
		  case wrangler_syntax:type(T) of
		      function ->
			  FunName = wrangler_syntax:atom_value(wrangler_syntax:function_name(T)),
			  Arity = wrangler_syntax:function_arity(T),
			  case has_receive_expr(T) of
			      {true, _Line} -> S;
                              false ->
				  case is_acc_tail_recursive(T, {ModName, FunName, Arity}) of
				      {true, I} -> [{{ModName, FunName, Arity, I}}| S];
				      _ -> S
				  end
                          end;
		      _ -> S
		  end
	  end,
    api_ast_traverse:fold(Fun, [], AnnAST).

is_acc_tail_recursive(FunDef, MFA) ->
    case is_tail_recursive(FunDef, MFA) of 
        true ->
            is_accumative(FunDef, MFA);
        false ->
            false
    end.
                  
is_accumative(FunDef, MFA) ->
    Cs = wrangler_syntax:function_clauses(FunDef),
    Res=[is_accumulative_clause(C, MFA)||C <- Cs],
    case lists:member(false, Res) of 
        true ->
            false;
        false ->
            Accs=lists:usort(Res) --[{true, base_case}],
            io:format("Accs:~p\n", [Accs]),
            case Accs of 
                [I] ->
                    I;
                _ ->false
            end
    end.
    
    
is_accumulative_clause(C, MFA) ->
    Body = wrangler_syntax:clause_body(C),
    Last = lists:last(Body),
    case is_application(MFA, Last) of 
        true ->
            Args = wrangler_syntax:application_arguments(Last),
            Slices=[{Index, A, wrangler_slice_new:backward_slice(C, A)} || 
                       {Index, A}<-lists:zip(lists:seq(1, length(Args)),Args)],
            Accs=[Slice||Slice={_Index, _A, _Locs}<-Slices, 
                                  is_accumulator(C, Slice, Slices--[Slice])],
            case Accs /=[] of 
               true -> {true, Accs};
               false ->{true, [{0, none, []}]}
            end;
        false ->
            {true, base_case}
    end.

is_accumulator(Clause, {Index, _A, {Locs1, Locs2}}, OtherSlices) ->
    Locs = Locs1++Locs2,
    Pats = wrangler_syntax:clause_patterns(Clause),
    PatLocs = [{I, wrangler_misc:start_end_loc(P)}||
                  {I,P}<-lists:zip(lists:seq(1, length(Pats)),
                                       Pats)],
    {Index, PatLoc} = lists:keyfind(Index,1, PatLocs),
    case lists:any(fun({_I, _P, {L1, L2}})->
                           lists:member(PatLoc, L1) orelse
                               lists:member(PatLoc, L2)
                   end, OtherSlices) of
        true -> false;
        false ->
            ParSlices = [I||{I, P}<-PatLocs, 
                                 lists:member(P, Locs), I/=Index],
            case ParSlices of 
                [] -> false;
                _ ->
                  Locs--[PatLoc|ParSlices] /=[] 
            end
    end.

is_application(MFA, Node) ->
    case api_refac:type(Node) of
        application ->
            Op = wrangler_syntax:application_operator(Node),
            MFA==api_refac:fun_define_info(Op);
        _ -> false
    end.



is_tail_recursive(FunDef, MFA) ->
    Cs = wrangler_syntax:function_clauses(FunDef),
    Cs1 = [C||C<-Cs, is_direct_recursive(C, MFA)],
    case Cs1 of 
        [] -> false;
        _ -> lists:all(fun(C) ->
                               is_tail_recursive_clause(C, MFA)
                       end, Cs1)
    end.

is_direct_recursive(C, MFA) ->
    CalledFuns = ordsets:to_list(wrangler_callgraph_server:called_funs(C)),
    lists:member(MFA, CalledFuns).
    
is_tail_recursive_clause(C, MFA) ->
    Body = wrangler_syntax:clause_body(C),
    Last = lists:last(Body),
    case api_refac:type(Last) of 
        application ->
            Op = wrangler_syntax:application_operator(Last),
            MFA==api_refac:fun_define_info(Op);
        _ -> false
    end.
                
has_receive_expr(FunDef) ->
    F = fun (T, S) ->
		case wrangler_syntax:type(T) of
		    receive_expr ->
			{{StartLine, _}, _} = wrangler_misc:start_end_loc(T),
			[StartLine| S];
		    _ -> S
		end
	end,
    LineNums = api_ast_traverse:fold(F, [], FunDef),
    case LineNums of
	[] -> false;
	_ -> {true, lists:min(LineNums)}
    end.

get_module_name(FName, Info) ->
    case lists:keysearch(module, 1, Info) of
	{value, {module, Mod}} -> Mod;
	_ -> list_to_atom(filename:basename(FName, ".erl"))
    end.

