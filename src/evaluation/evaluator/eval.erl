-module(eval).
-export([start_evaluation/8,keep_temp_info/3,try_evaluate/11]).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").   

%%--------------------------------------------------------------------
%% @doc
%% This function represents a rule that tries to perform substitution rules in a function body only if the function information matches the function chosen by the user in the case of a Composite Refactoring.
%% @end
%%--------------------------------------------------------------------   
start_evaluation({DefFiles,Scope},Info,Pid,RulesFun,Expression,NSteps,TypedF,Timeout) ->
	    if Pid /= "" ->
		    	Steps = get_temp_info_steps(Pid),
                        NRefacsDone = get_temp_info_nSteps(Pid),
		    	Body = lists:nth(NRefacsDone+1,Steps),
		            NewBody = evaluate(Pid,Body,{DefFiles,Scope},Info,RulesFun,false,NSteps,TypedF,0,Steps),
                            if TypedF == false ->
					    NSteps_Int = list_to_integer(NSteps),
				            if   length(Steps) > NSteps_Int + NRefacsDone -> Change = true;
				                 true -> Change = ?PP(NewBody) /= ?PP(Body)
				            end;
                               true -> if length(Steps) - 1 > NRefacsDone -> Change = true;
                                          true -> Change = ?PP(NewBody) /= ?PP(Body)
                                       end
                            end,
                            if
				    Change ->
                                             PidEvaluation = spawn(eval, try_evaluate, [Pid,Body,{DefFiles,Scope},Info,RulesFun,true,NSteps,TypedF,0,Steps,self()]),
                                             case Timeout of
                                                  0 -> timeout_manager(PidEvaluation,Body,1000);
                                                  _ -> timeout_manager(PidEvaluation,Body,Timeout)
                                             end;
				    true ->
                                              {error,"No evaluation done"}
                            end;
               true ->  Body = ?TO_AST(Expression),
                       Change = ?PP(evaluate("",Body,{DefFiles,Scope},Info,RulesFun,false,NSteps,TypedF,0,[Body])) /= Expression,
                       if
			       Change ->
                                        PidEvaluation = spawn(eval, try_evaluate, [Pid,Body,{DefFiles,Scope},Info,RulesFun,true,NSteps,TypedF,0,[Body],self()]),
                                        case Timeout of
                                        	0 -> timeout_manager(PidEvaluation,Body,1000);
                                                _ -> timeout_manager(PidEvaluation,Body,Timeout)
                                        end;
				true -> 
                          		{error,"No evaluation done"}   
       	    		end
            end.

try_evaluate(Pid,Body,{File,Scope},Info,RulesFun,true,NSteps,TypedF,NRefacsDone,Steps,MainPid) ->
            Result = evaluate(Pid,Body,{File,Scope},Info,RulesFun,true,NSteps,TypedF,NRefacsDone,Steps),
	    MainPid ! {result, Result}.

timeout_manager(Pid,Node,TimeOut) -> 
		receive
		     {result, _} -> {ok,[]}
                after 
		     TimeOut -> 
		     exit(Pid,kill),
		     io:format("TIMEOUT in expression: ~p~n",[?PP(Node)]),
		     {error, timeOut}
		end.
               

%%--------------------------------------------------------------------
%% @doc
%% This function computes how many steps were done, so that if the user wants to undo, this information will be used. A new thread was created to do this math.
%% @end
%%--------------------------------------------------------------------
keep_temp_info(NSteps,Steps,Text) -> 
	receive
    		{refactoring_finished, NRefacsDone, NewSteps,NewText} -> 
                                          keep_temp_info(NRefacsDone, NewSteps,NewText);
    		{ask_steps,MainPid} -> begin
                                          MainPid ! {value,Steps},
                                          keep_temp_info(NSteps,Steps,Text)
                                      end;
                 {ask_nSteps,MainPid} -> begin
                                           MainPid ! {value,NSteps},
                                           keep_temp_info(NSteps,Steps,Text)
                                        end;
                {ask_text,MainPid} ->  begin
                                           MainPid ! {value,Text},
                                           keep_temp_info(NSteps,Steps,Text)
                                        end;
        stop -> true
    	end.

%%--------------------------------------------------------------------
%% @doc
%% This function gets the number of total steps applied in the evaluation. To do this, a message is sent to the thread and a message with the number is received.
%% @end
%%--------------------------------------------------------------------
get_temp_info_steps(Pid) ->   Pid ! {ask_steps,self()},
           		 receive 
             			{value,Steps} -> Steps
                         end. 

get_temp_info_nSteps(Pid) -> Pid ! {ask_nSteps,self()},
           		 receive 
             			{value,NSteps} -> NSteps
                         end. 
get_temp_info_text(Pid) -> Pid ! {ask_text,self()},
                         receive 
             			{value,Text} -> Text
                         end. 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function tries to transform the body as much as possible in the case of a composite refactoring.
%% @spec(transform_body_two_ways([syntaxTree()] | syntaxTree(),string(),[syntaxTree()] | syntaxTree(),[{{modulename(),functionname(),arity()},syntaxTree(),syntaxTree() | [syntaxTree()]}], boolean(), string(), boolean(), string(),integer(),string()) -> [syntaxTree()] | syntaxTree()).
%% @end
%%--------------------------------------------------------------------  
-spec(evaluate(string(),[syntaxTree()] | syntaxTree(),string(),[{{modulename(),functionname(),arity()},syntaxTree(),syntaxTree() | [syntaxTree()]}],RulesFun::fun((_) -> any()), boolean(), string(), boolean(),integer(),[syntaxTree]) -> [syntaxTree()] | syntaxTree()).
evaluate(Pid,CurrentNode,{DefFiles,Scope},Info,RulesFun,Transform,Input_Steps,TypedF,NRefacsDone,Steps) ->
    RemoveUnrefInfo = core_unreferenced_assign:collector_variable_occurrences(CurrentNode),
    VarsInfo = core_unreferenced_assign:collector_var_expr_value(CurrentNode),
    {ok, NewNode0} = ?STOP_TD_TP((RulesFun({DefFiles,VarsInfo,api_refac:bound_vars(CurrentNode)},{Info,RemoveUnrefInfo})), CurrentNode),
    {ok,NewNode} = 
	case eval_rem_begin_end:collector(CurrentNode) of
	    [] ->
		?FULL_TD_TP((core_rem_begin_end:rules(empty,empty)),NewNode0);
	    _ -> 
		{ok,NewNode0}
	 end,
    Changed = ?PP(CurrentNode) /= ?PP(NewNode),
    if
       Changed ->  NRefacsDoneI = NRefacsDone + 1,
                   NewSteps = Steps ++ [NewNode];
       true -> NRefacsDoneI = NRefacsDone,
               NewSteps = Steps
    end,
    if TypedF ->
                if
                    Transform andalso length(Steps) - 1 > NRefacsDone -> 
                         OldText = get_temp_info_text(Pid),
                         evaluation_ended(Transform,length(Steps)-1, Pid,lists:last(Steps), [],OldText ++ get_text(Steps,NRefacsDone,Input_Steps));
   	            Transform andalso Changed -> 
                     evaluate(Pid,NewNode,{DefFiles,Scope},Info,RulesFun,Transform,Input_Steps,TypedF,NRefacsDoneI,NewSteps);
                    true ->
                          NRefacsDoneBef = get_temp_info_nSteps(Pid),
                          OldText = get_temp_info_text(Pid),
                          evaluation_ended(Transform,NRefacsDoneBef + NRefacsDoneI,Pid,NewNode,NewSteps,OldText ++ get_text(NewSteps))
                end;
       true ->
                Input_Steps_Int = list_to_integer(Input_Steps),
                if
                    Transform andalso (length(Steps) - 1 >= (NRefacsDone + Input_Steps_Int)) andalso Input_Steps_Int > 0 -> 
                              NRefacsDoneBef = get_temp_info_nSteps(Pid),
                              OldText = get_temp_info_text(Pid),
                              evaluation_ended(Transform,NRefacsDoneBef + NRefacsDone + Input_Steps_Int, Pid,lists:last(Steps), [],OldText ++ get_text(Steps,NRefacsDone,Input_Steps));
   	            Transform andalso Changed andalso Input_Steps_Int > 1 ->
                     evaluate(Pid,NewNode,{DefFiles,Scope},Info,RulesFun,Transform,integer_to_list(Input_Steps_Int - 1),TypedF,NRefacsDoneI,NewSteps);
                    Input_Steps_Int < 0 -> 
                                   go_back_steps(Pid,Input_Steps_Int,Transform);
                    true -> NRefacsDoneBef = get_temp_info_nSteps(Pid),
                            OldText = get_temp_info_text(Pid),
                            evaluation_ended(Transform,NRefacsDoneBef+NRefacsDoneI,Pid,NewNode,NewSteps,OldText++get_text(NewSteps))
    		end
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function start a new evaluation when the user chooses to undo steps.
%% @end
%%--------------------------------------------------------------------
go_back_steps(Pid,Input_Usr,Transform) ->
   TotalSteps = get_temp_info_nSteps(Pid),  
   Steps = get_temp_info_steps(Pid),
                         if
                              TotalSteps >= Input_Usr * (-1)  -> Input = Input_Usr;
                              true -> Input = TotalSteps * (-1)
                         end,					
                         OldText = get_temp_info_text(Pid),
		         [_|NewSteps] = get_back_steps(lists:reverse(Steps),Input * (-1) + 1,TotalSteps),
                         if Transform ->
                                       Pid ! {refactoring_finished,TotalSteps + Input,Steps,OldText++get_text(NewSteps)},
                                   	write_result(Pid,lists:last(NewSteps));
                            true -> lists:last(NewSteps)
                         end.

get_back_steps(_, 0,_) -> [];
get_back_steps([H|T],NSteps,TotalSteps) -> if length(T) =< TotalSteps -> [H|get_back_steps(T,NSteps -1,TotalSteps)];
                                              true -> get_back_steps(T,NSteps,TotalSteps)
                                           end.  

%%--------------------------------------------------------------------
%% @doc
%% This function returns the current node and updates the number of evaluations done.
%% @end
%%--------------------------------------------------------------------
evaluation_ended(Transform,NRefacsDone,Pid,NewBody,Steps,Text) -> 
                                if Transform ->
		                                if Pid /= "" ->  OldSteps = get_temp_info_steps(Pid),
		                                                 LengthOld = length(OldSteps),
		                                                 if LengthOld == 1 ->
				                                         Pid ! {refactoring_finished,NRefacsDone,Steps,Text},
                                                                         write_result(Pid, NewBody);
		                                                 true -> 
		                                                 	 Pid ! {refactoring_finished,NRefacsDone,OldSteps ++ Steps,Text},
                                                                         write_result(Pid, NewBody)
		                                                 end,
		                                                 NewBody;
		                                   true -> 
		                                           write_result(Pid, NewBody)
		                                end;
                                    true ->
                                        NewBody
                                 end.

get_text([]) -> "";
get_text([H|T]) -> "> " ++?PP(H) ++ "\n" ++ get_text(T).

get_text([],_,_) -> [];
get_text(_,_,"0") -> [];
get_text([H|T],0,Input) -> if Input /= "f" andalso Input /= "F" -> "> "++"\n" ++ get_text(T,0,integer_to_list(list_to_integer(Input)-1));
                              true -> "> " ++ ?PP(H)++"\n" ++ get_text(T,0,Input)
                           end;
get_text([_|T],NSteps,Input) -> get_text(T,NSteps-1,Input).

write_result(Pid,NewBody) -> 
                  Text = get_temp_info_text(Pid),
		  io:fwrite(Text),
                  %%io:format("~p~n",[?PP(NewBody)]),
                  NewBody.












