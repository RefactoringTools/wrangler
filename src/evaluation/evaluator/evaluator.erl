-module(evaluator).

-export([start/1,start/2]).
-include_lib("wrangler/include/wrangler.hrl").

%%--------------------------------------------------------------------
%% @doc
%% This function applies the evaluation iteratively, i.e., every time that
%% the evaluation is done the user can repeat it. It calls the four evaluators:
%% function calls, arithmetic expressions, arithmetic simplifications and also
%% the combination of the last three.
%% @end
%%--------------------------------------------------------------------
start(ModName) -> start(ModName,0).
start(ModName,Timeout) -> 
        {ok,CurrDir} = file:get_cwd(),
        SearchPaths = [CurrDir],
        File = SearchPaths ++ "/"++ atom_to_list(ModName) ++ ".erl",
	evaluate(ModName,File,SearchPaths,"","",Timeout).
evaluate(ModName,File,SearchPaths,OldExpression,OldPid,Timeout) ->
	case SearchPaths of
	    [_ | _] ->
                    if OldExpression == "" ->
                    			 Expression = io:get_line("Type an expression: ");
                    true -> Expression = OldExpression
                    end,
                    {ok, Input2} = io:fread("Type N to execute N steps or 'f' to execute all steps: ", "~s"),
                    [NSteps|_] = Input2,
                    if OldPid == "" -> Pid = spawn(eval, keep_temp_info, [0,[?TO_AST(Expression)],""]);
                       true -> Pid = OldPid
                    end,
                    Result = eval_all:eval_all(File,Expression,Pid,NSteps,SearchPaths,Timeout),
                    case Result of
                          {error,_} -> io:format("~p~n",[Result]);
                           _ -> io:format("~n")
                    end,
                    {ok, [Answer]} = io:fread("Do you wish to continue this evaluation? [y/n] ", "~s"),
                    if Answer == "Y" orelse Answer == "y" orelse Answer == "Yes" -> evaluate(ModName,File,SearchPaths,Expression,Pid,Timeout); 
                       Answer == "N" orelse Answer == "n" orelse Answer == "No" -> 
                                  {ok, [Answer2]} = io:fread("Do you wish to start a new evaluation? [y/n] ", "~s"),
                                  if Answer2 == "Y" orelse Answer2 == "y" orelse Answer2 == "Yes" -> start(ModName,Timeout); 
                       		     Answer2 == "N" orelse Answer2 == "n" orelse Answer2 == "No" ->  {ok,"Evaluator Stopped"};
                                     true -> {error, "Please answer yes or no"}
                                  end;
                       true -> {error, "Please answer yes or no"}
                   end;
    	     _ -> {error, "Invalid Wrangler Search Paths"}
    end.

