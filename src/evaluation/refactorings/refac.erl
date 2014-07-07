%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Roberto S. M. de Barros Filho, Simon  Thompson
%% Auxiliar Module that is used when the refactorings contains similaraties.
%% This module is responsible for the timeout for example.
-module(refac).
-export([try_call_transform/2, try_call_transform/3, body_rules/4, try_transform_manager/5, checkTimeOut/1, str_to_bool/1, select_focus/1, input_par_prompts/0]).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

input_par_prompts() ->
   ["Please, choose a timeout value in miliseconds (default is 1000ms):", "Would you like to apply this refactoring to the entire file? (y/n)"].

select_focus(_Args=#args{current_file_name=File, 
                         cursor_pos=Pos, user_inputs=InputsList}) ->
    EntireFileStr = lists:nth(2, InputsList),
    RefacBool = str_to_bool(EntireFileStr),
    case RefacBool of
	true -> {ok, none};
	_ -> api_interface:pos_to_fun_def(File, Pos)
    end.

try_call_transform(Args, RulesFun) ->
    try_call_transform(Args, RulesFun, []).

try_call_transform(_Args=#args{current_file_name=File,
		     user_inputs=InputsList, focus_sel=FunDef}, RulesFun, FunArgs) ->  
    TimeOutStr = lists:nth(1, InputsList),
    EntireFileStr = lists:nth(2, InputsList),
    TimeOut = checkTimeOut(TimeOutStr),
    case TimeOut of
	{error, _} -> {error, "invalid timeout input!"};
	_ ->
	    CheckedFileBool = str_to_bool(EntireFileStr),
	    case CheckedFileBool of
		true ->       		    
		    ?FULL_TD_TP((body_rules(RulesFun, {CheckedFileBool, unknown}, TimeOut, FunArgs)),[File]);
		false ->
		    MFA = api_refac:fun_define_info(FunDef),
		    if
			MFA /= unknown ->
			     ?FULL_TD_TP((body_rules(RulesFun, {CheckedFileBool, MFA}, TimeOut, FunArgs)),[File]);
			 true -> io:format("The reason is unknown~n"),{error, "Please, place the mouse cursor on the desired function!"}
		    end;
		_ -> {error, "Please, answer 'y' or 'n'!"}
	    end
    end.

body_rules(RulesFun, RefacType, TimeOut, Info) -> [body_rule(RulesFun, RefacType, TimeOut, Info)].

str_to_bool("y") -> true;
str_to_bool(Str) when Str == "n" orelse Str == "" -> false; 
str_to_bool(_) -> maybe. 
    

%%--------------------------------------------------------------------
%% @doc
%% This function represents a rule that tries to perform substitution rules in a function body only if the function information matches the function chosen by the user.
%% @end
%%--------------------------------------------------------------------   
body_rule(RulesFun, {RefacWholeFile, MFA}, TimeOut, FunArgs) ->
    ?RULE(
       ?T("f@(Args@@) when Guards@@ -> Body@@;"),
       begin	   
	   NewBody@@ = utils_transform:transform_body(Body@@,RulesFun,{FunArgs, _This@},api_refac:fun_define_info(_This@)),%%api_refac:fun_define_info(f@)),
           ?TO_AST("f@(Args@@) when Guards@@ -> NewBody@@;")
       end, 
       begin
	   FunInfo = api_refac:fun_define_info(_This@),%%FunInfo = api_refac:fun_define_info(f@),
	   RefacWholeFile == true orelse	   
	   FunInfo == MFA andalso
	   begin
	       Result = try_transform_body(Body@@, RulesFun, {FunArgs, _This@}, FunInfo, TimeOut),
	       case Result of
		    {error, _Reason} -> false;
		    _ -> true
	       end andalso
		(?PP(Result) /= ?PP(Body@@))
	   end
      end
       ).

try_transform_body(Node, RulesFun, FunArgs, FunDefInfo, TimeOut) ->
	    Pid = spawn(refac, try_transform_manager, [Node, RulesFun, FunArgs, FunDefInfo, self()]),
	    receive
		{result, NewNode} -> NewNode
	    after 
		TimeOut -> 
		    exit(Pid,kill),
		    io:format("TIMEOUT in expression: ~p~n",[?PP(Node)]),
		    {error, timeOut}
	    end.

checkTimeOut("") -> 1000;
checkTimeOut(TimeOutStr) ->
    try (list_to_integer(TimeOutStr)) of
	TimeOut when TimeOut > 0 -> TimeOut; 
	_ -> {error, badarg}
    catch
	error:Error -> {error, Error}
    end.

try_transform_manager(Node, RulesFun, FunArgs, FunDefInfo, MainPid) ->
            NewBody = utils_transform:transform_body(Node, RulesFun, FunArgs, FunDefInfo),
	    MainPid ! {result, NewBody}.




    


