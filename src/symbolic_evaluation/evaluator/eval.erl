%% @private
-module(eval).
-export([start_evaluation/8,keep_temp_info/3,try_evaluate/12]).

%% Include files
-include("wrangler.hrl").   

%%--------------------------------------------------------------------
%% @doc
%% This function represents a rule that tries to perform substitution rules in a function body only if the function information matches the function chosen by the user in the case of a Composite Refactoring.
%% @end
%%--------------------------------------------------------------------   
start_evaluation({DefFiles,Scope},Info,Pid,RulesFun,_,NSteps,TypedF,Timeout) ->
		            continue_evaluation(DefFiles, Scope, Info, Pid,
                                                RulesFun, NSteps, TypedF,
		                                Timeout).

call_try_evaluate(DefFiles, Scope, Info, Pid, RulesFun, NSteps, TypedF,
	          Timeout, Steps, NRefacsDone, Body,IText) ->
    PidEvaluation = spawn(eval, try_evaluate, [Pid,Body,{DefFiles,Scope},Info,RulesFun,true,NSteps,TypedF,NRefacsDone,Steps,self(),IText]),
    case Timeout of
	0 -> timeout_manager(PidEvaluation,Body,1000);
        _ -> timeout_manager(PidEvaluation,Body,Timeout)
    end.

continue_evaluation(DefFiles, Scope, Info, Pid, RulesFun, NSteps,
                    TypedF, Timeout) ->
    Steps = get_temp_info_steps(Pid),
    NRefacsDone = get_temp_info_nSteps(Pid),
    Body = lists:nth(NRefacsDone + 1,Steps),
		           
    Change = if TypedF == false ->
		     NSteps_Int = list_to_integer(NSteps),
		     if
			NSteps_Int < 0 andalso NRefacsDone /= 0 -> go_back_steps(Pid,NSteps_Int,true),ok;
			NSteps_Int < 0 -> false;
		        length(Steps) - 1 > NRefacsDone andalso NSteps_Int > 0 -> true;
			true ->
				   checkForTransformation(DefFiles,Scope,Info,Pid,RulesFun,NSteps,TypedF,
                                                          Steps,NRefacsDone,Body)
                     end;
                true -> if 
			   length(Steps) - 1 > NRefacsDone -> true;
                           true -> checkForTransformation(DefFiles,Scope,Info,Pid,RulesFun,NSteps,TypedF,
                                                          Steps,NRefacsDone,Body)
                        end
             end,
    if
	    Change ->
                     evaluate_if_necessary(DefFiles, Scope, Info, Pid, RulesFun, NSteps,
                                           TypedF, Timeout, Steps, NRefacsDone, Body, []);
	    Change == ok -> {ok,[]};
	    true ->
                      {error,"No evaluation done"}
    end.

evaluate_if_necessary(DefFiles, Scope, Info, Pid, RulesFun, NSteps, TypedF,
                      Timeout, Steps, NRefacsDone, Body, ChangedSteps) ->
    if
	TypedF andalso length(Steps) - 1 > NRefacsDone ->
	       NRefacsDoneI = NRefacsDone + 1,
	       NewBody = lists:nth(NRefacsDoneI + 1,Steps),
	       NewChangedSteps = ChangedSteps ++ [NewBody],
	       evaluate_if_necessary(DefFiles, Scope, Info, Pid, RulesFun, NSteps, TypedF, Timeout, Steps, NRefacsDoneI, NewBody, NewChangedSteps);
	TypedF -> call_try_evaluate(DefFiles, Scope, Info, Pid, RulesFun,
		                       NSteps, TypedF, Timeout, Steps,
			               NRefacsDone, Body,get_text(ChangedSteps));
	true ->
		     NSteps_Int = list_to_integer(NSteps),
	   if
		     NSteps_Int == 0 ->
		     evaluation_ended(true,NRefacsDone,Pid, Body,Steps,get_text(ChangedSteps)),
		     {ok,[]};
			   length(Steps) - 1 > NRefacsDone andalso NSteps_Int > 0 ->
		     NewNSteps_Int = NSteps_Int - 1,
		     NRefacsDoneI = NRefacsDone + 1,
		     NewBody = lists:nth(NRefacsDoneI + 1,Steps),
		     NewChangedSteps = ChangedSteps ++ [NewBody],
		         evaluate_if_necessary(DefFiles, Scope, Info, Pid, RulesFun, integer_to_list(NewNSteps_Int), TypedF, Timeout, Steps, NRefacsDoneI, NewBody, NewChangedSteps);
			       true ->
		             call_try_evaluate(DefFiles, Scope, Info, Pid, RulesFun,
		                           NSteps, TypedF, Timeout, Steps,
			                   NRefacsDone, Body,get_text(ChangedSteps))
		     end
    end.

checkForTransformation(DefFiles, Scope, Info, Pid, RulesFun, NSteps, TypedF, Steps,
                       NRefacsDone, Body) ->
    NewBody = evaluate(Pid,Body,{DefFiles,Scope},Info,RulesFun,false,NSteps,TypedF,NRefacsDone,Steps),
    ?PP(NewBody) /= ?PP(Body).

try_evaluate(Pid,Body,{File,Scope},Info,RulesFun,true,NSteps,TypedF,NRefacsDone,Steps,MainPid,IText) ->
            Result = evaluate(Pid,Body,{File,Scope},Info,RulesFun,true,NSteps,TypedF,NRefacsDone,Steps,IText),
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

evaluate(Pid,CurrentNode,{DefFiles,Scope},Info,RulesFun,Transform,Input_Steps,TypedF,NRefacsDone,Steps) ->
     evaluate(Pid,CurrentNode,{DefFiles,Scope},Info,RulesFun,Transform,Input_Steps,TypedF,NRefacsDone,Steps,"").

-spec(evaluate(string(),[syntaxTree()] | syntaxTree(),string(),[{{modulename(),functionname(),arity()},syntaxTree(),syntaxTree() | [syntaxTree()]}],RulesFun::fun((_) -> any()), boolean(), string(), boolean(),integer(),[syntaxTree],[syntaxTree]) -> [syntaxTree()] | syntaxTree()).
evaluate(Pid,CurrentNode,{DefFiles,Scope},Info,RulesFun,Transform,Input_Steps,TypedF,NRefacsDone,Steps,IText) when is_list(Steps) andalso length(Steps) == 1 ->
    evaluate(Pid,CurrentNode,{DefFiles,Scope},Info,RulesFun,Transform,Input_Steps,TypedF,NRefacsDone,[],IText,Steps);
evaluate(Pid,CurrentNode,{DefFiles,Scope},Info,RulesFun,Transform,Input_Steps,TypedF,NRefacsDone,Steps,IText) -> 
    evaluate(Pid,CurrentNode,{DefFiles,Scope},Info,RulesFun,Transform,Input_Steps,TypedF,NRefacsDone,Steps,IText,[]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function tries to transform the body as much as possible in the case of a composite refactoring.
%% @end
%%--------------------------------------------------------------------
evaluate(Pid,CurrentNode,{DefFiles,Scope},Info,RulesFun,Transform,Input_Steps,TypedF,NRefacsDone,Steps,IText,NewSteps) ->
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
                  NewStepsMod = NewSteps ++ [NewNode];
       true -> NRefacsDoneI = NRefacsDone,
               NewStepsMod = NewSteps
    end,
    NewCompleteSteps = Steps ++ NewStepsMod, 
    if TypedF ->
                if                    
   	            Transform andalso Changed -> 
                     evaluate(Pid,NewNode,{DefFiles,Scope},Info,RulesFun,Transform,Input_Steps,TypedF,NRefacsDoneI,Steps,IText,NewStepsMod);
                    true ->
                          evaluation_ended(Transform,NRefacsDoneI,Pid,NewNode,NewCompleteSteps, IText ++ get_text(NewStepsMod))
                end;
       true ->
                Input_Steps_Int = list_to_integer(Input_Steps),
                if                    
   	            Transform andalso Changed andalso Input_Steps_Int > 1 ->
                     evaluate(Pid,NewNode,{DefFiles,Scope},Info,RulesFun,Transform,integer_to_list(Input_Steps_Int - 1),TypedF,NRefacsDoneI,Steps,IText,NewStepsMod);
                    true ->
                        evaluation_ended(Transform,NRefacsDoneI,Pid,NewNode,NewCompleteSteps,IText ++ get_text(NewStepsMod))
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
		                                if Pid /= "" ->  
							OldText = get_temp_info_text(Pid),	  
				                        Pid ! {refactoring_finished,NRefacsDone,Steps,OldText ++ Text},
                                                        write_result(Pid, NewBody),
		                                        NewBody;
		                                   true -> 
		                                           write_result(Pid, NewBody)
		                                end;
                                    true ->
                                        NewBody
                                 end.

get_text([]) -> "";
get_text([H|T]) -> "> " ++?PP(H) ++ "\n" ++ get_text(T).

write_result(Pid,NewBody) -> 
                  Text = get_temp_info_text(Pid),
		  io:fwrite(Text),
                  %%io:format("~p~n",[?PP(NewBody)]),
                  NewBody.
