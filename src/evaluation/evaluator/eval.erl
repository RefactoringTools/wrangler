-module(eval).
-export([start_evaluation/8,keep_temp_info/2,try_evaluate/12]).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").   

%%--------------------------------------------------------------------
%% @doc
%% This function represents a rule that tries to perform substitution rules in a function body only if the function information matches the function chosen by the user in the case of a Composite Refactoring.
%% @end
%%--------------------------------------------------------------------   
start_evaluation(SearchPaths,{File,Scope},Info,Pid,RulesFun,Expression,NSteps, TypedF) ->
	    if Pid /= "" ->
		    	Steps = get_temp_info_steps(Pid),
		    	Body = lists:last(Steps),
		   	NewBody = evaluate(SearchPaths,Pid,Body,{File,Scope},Info,RulesFun,false,NSteps,TypedF,0,[Body]),
                        Change = ?PP(NewBody) /= ?PP(Body);
               true ->  Body = ?TO_AST(Expression),
           		Change = ?PP(evaluate(SearchPaths,"",Body,{File,Scope},Info,RulesFun,false, NSteps,TypedF,0,[Body])) /= Expression
            end,
            if
		Change -> PidEvaluation = spawn(eval,try_evaluate, [SearchPaths,Pid,Body,{File,Scope},Info,RulesFun,true,NSteps,TypedF,0,[Body],self()]),
                          timeout_manager(PidEvaluation,Body,1000);
		true -> 
                          {error,"No evaluation done"}   
       	    end.  

try_evaluate(SearchPaths,Pid,Body,{File,Scope},Info,RulesFun,true,NSteps,TypedF,NRefacsDone,Steps,MainPid) ->
            Result = evaluate(SearchPaths,Pid,Body,{File,Scope},Info,RulesFun,true,NSteps,TypedF,NRefacsDone,Steps),
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
keep_temp_info(NSteps,Steps) -> 
	receive
    		{refactoring_finished, NRefacsDone, NewSteps} -> 
                                          keep_temp_info(NRefacsDone + NSteps, NewSteps);
    		{ask_steps,MainPid} -> begin
                                          MainPid ! {value,Steps},
                                          keep_temp_info(NSteps,Steps)
                                      end;
                 {ask_nSteps,MainPid} -> begin
                                           MainPid ! {value,NSteps},
                                           keep_temp_info(NSteps,Steps)
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function tries to transform the body as much as possible in the case of a composite refactoring.
%% @spec(transform_body_two_ways([syntaxTree()] | syntaxTree(),string(),[syntaxTree()] | syntaxTree(),[{{modulename(),functionname(),arity()},syntaxTree(),syntaxTree() | [syntaxTree()]}], boolean(), string(), boolean(), string(),integer(),string()) -> [syntaxTree()] | syntaxTree()).
%% @end
%%--------------------------------------------------------------------  
-spec(evaluate(string(),string(),[syntaxTree()] | syntaxTree(),string(),[{{modulename(),functionname(),arity()},syntaxTree(),syntaxTree() | [syntaxTree()]}],RulesFun::fun((_) -> any()), boolean(), string(), boolean(),integer(),[syntaxTree]) -> [syntaxTree()] | syntaxTree()).
evaluate(SearchPaths,Pid, CurrentNode,{File,Scope},Info,RulesFun, Transform, Input_Steps,TypedF,NRefacsDone,Steps) -> 
    {ok, NewNode} = ?STOP_TD_TP(RulesFun({File,Scope},Info), CurrentNode),
    Changed = ?PP(CurrentNode) /= ?PP(NewNode),
    if
       Changed ->  NRefacsDoneI = NRefacsDone + 1,
                   NewSteps = Steps ++ [NewNode];
       true -> NRefacsDoneI = NRefacsDone,
               NewSteps = Steps
    end,
    if TypedF ->
                if
   	            Transform andalso Changed -> 
                        
                	evaluate(SearchPaths,Pid, NewNode,{File,Scope},Info,RulesFun, Transform, Input_Steps, TypedF,NRefacsDoneI,NewSteps);
   		    true -> 
                             evaluation_ended(SearchPaths,Transform,NRefacsDoneI,Pid,NewNode,NewSteps)
    		end;
       true ->
                Input_Steps_Int = list_to_integer(Input_Steps),
                if
   	            Transform andalso Changed andalso Input_Steps_Int > 1 -> 
                	evaluate(SearchPaths,Pid, NewNode,{File,Scope},Info,RulesFun, Transform, integer_to_list(Input_Steps_Int - 1), 					TypedF,NRefacsDoneI,NewSteps);
                    Input_Steps_Int < 0 -> 
                                   go_back_steps(SearchPaths,Pid,Input_Steps_Int,Transform);
                    true ->  evaluation_ended(SearchPaths,Transform,NRefacsDoneI,Pid,NewNode,NewSteps)
    		end
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function start a new evaluation when the user chooses to undo steps.
%% @end
%%--------------------------------------------------------------------
go_back_steps(SearchPaths,Pid,Input_Int,Transform) ->
   TotalSteps = get_temp_info_nSteps(Pid), 
   case SearchPaths of 
        [FirstPath|_] -> 
                         if
                              TotalSteps >= Input_Int * (-1)  ->					
		                   Steps = get_temp_info_steps(Pid),
		                   [_|NewSteps] = get_back_steps(lists:reverse(Steps),Input_Int * (-1) + 1),
                                   OldSteps = get_old_steps(lists:reverse(Steps),Input_Int * (-1)),
                                   FileName = FirstPath ++ "/results.txt",
                                   if Transform ->
                                                Pid ! {refactoring_finished,Input_Int,OldSteps},
                                   		write_result(NewSteps,FileName,lists:last(NewSteps));
                                      true -> lists:last(NewSteps)
                                   end
                            end;
        _ -> {error, "Invalid Wrangler Search Path"}
   end.

get_back_steps(_, 0) -> [];
get_back_steps([H|T],NSteps) -> [H|get_back_steps(T,NSteps -1)].  

get_old_steps(T,0) -> lists:reverse(T);
get_old_steps([_|T],NSteps) -> get_old_steps(T,NSteps-1).

%%--------------------------------------------------------------------
%% @doc
%% This function returns the current node and updates the number of evaluations done.
%% @end
%%--------------------------------------------------------------------
evaluation_ended(SearchPath,Transform,NRefacsDone,Pid,NewBody,Steps) -> 
                                if Transform ->
                                        case SearchPath of
					    [File | _] ->
		                                FileName = File ++ "/results.txt",
		                                if Pid /= "" ->  OldSteps = get_temp_info_steps(Pid),
		                                                 LengthOld = length(OldSteps),
		                                                 if LengthOld == 1 ->
				                                         write_result(Steps, FileName, NewBody),
				                                         Pid ! {refactoring_finished,NRefacsDone,Steps};
		                                                 true -> write_result(OldSteps ++ Steps, FileName, NewBody),
		                                                 	 Pid ! {refactoring_finished,NRefacsDone,OldSteps ++ Steps}
		                                                 end,
		                                                 NewBody;
		                                   true -> 
		                                           write_result(Steps, FileName, NewBody)
		                                end;
                                             _ -> {error, "Invalid Wrangler Search Paths"}
                                         end;
                                    true ->
                                        NewBody
                                 end.

get_text([]) -> "";
get_text([H|T]) -> ">" ++ ?PP(H) ++ "\n" ++ get_text(T).

write_result(Steps, FileName, NewBody) -> 
                  Text = get_text(Steps),
                  {ok,_} = file:open(FileName, [append]),
                  file:write_file(FileName, Text,[append]),
                  NewBody.












