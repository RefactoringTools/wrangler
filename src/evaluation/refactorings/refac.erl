%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Roberto S. M. de Barros Filho, Simon  Thompson
%% Auxiliar Module that is used when the refactorings contains similaraties.
%% This module is responsible for the timeout for example.
-module(refac).
-export([try_call_transform/2, try_call_transform/3, body_rules/4, try_transform_manager/5, checkTimeOut/1, get_refac_scope/1, select_focus/1, input_par_prompts/0,get_files/3,filterError/1,input_refac_scope_message/0]).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

input_par_prompts() ->
   ["Please, choose a timeout value in miliseconds (default is 1000ms):",input_refac_scope_message()].

input_refac_scope_message() ->
     "Please choose y/n to apply this refactoring to this whole file or Y to apply it to the whole project (y,n or Y)".

select_focus(Args=#args{user_inputs=InputsList}) ->
    EntireFileStr = lists:nth(2, InputsList),
    select_focus(Args, EntireFileStr).

select_focus(_Args=#args{current_file_name=File, 
                         cursor_pos=Pos},EntireFileStr) ->
    RefacScope = get_refac_scope(EntireFileStr),
    if 
	RefacScope == file orelse RefacScope == project -> {ok, none};
	true -> api_interface:pos_to_fun_def(File, Pos)
    end.

try_call_transform(Args, RulesFun) ->
    try_call_transform(Args, RulesFun, []).

try_call_transform(_Args=#args{current_file_name=File,
		     user_inputs=InputsList, focus_sel=FunDef,search_paths=SearchPaths}, RulesFun, FunArgs) -> 
   
    TimeOutStr = lists:nth(1, InputsList),
    EntireFileStr = lists:nth(2, InputsList),
    TimeOut = checkTimeOut(TimeOutStr),
    case TimeOut of
	{error, _} -> {error, "invalid timeout input!"};
	_ ->
	    RefacScope = get_refac_scope(EntireFileStr),
	    case RefacScope of
		file ->       		    
		    ?FULL_TD_TP((body_rules(RulesFun, {RefacScope, unknown}, TimeOut, FunArgs)),[File]);
		function ->
		    MFA = api_refac:fun_define_info(FunDef),
		    if
			MFA /= unknown ->
			     ?FULL_TD_TP((body_rules(RulesFun, {RefacScope, MFA}, TimeOut, FunArgs)),[File]);
			true -> {error, "Please, place the mouse cursor on the desired function!"}
		    end;
		project ->		 
		     Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
		     ?FULL_TD_TP((body_rules(RulesFun, {RefacScope, unknown}, TimeOut, FunArgs)),Files);
		_ -> {error, "Please, answer 'y', 'Y' or 'n'!"}
	    end
    end.

body_rules(RulesFun, RefacType, TimeOut, Info) -> [body_rule(RulesFun, RefacType, TimeOut, Info)].

get_refac_scope("y") -> file;
get_refac_scope("Y") -> project;
get_refac_scope(Str) when Str == "n" orelse Str == "" -> function;
get_refac_scope(_) -> maybe. 
    

%%--------------------------------------------------------------------
%% @doc
%% This function represents a rule that tries to perform substitution rules in a function body only if the function information matches the function chosen by the user.
%% @end
%%--------------------------------------------------------------------   
body_rule(RulesFun, {RefacScope, MFA}, TimeOut, FunArgs) ->
    ?RULE(
       ?T("f@(Args@@) when Guards@@ -> Body@@;"),
       begin	   
	   NewBody@@ = utils_transform:transform_body(Body@@,RulesFun,{FunArgs, _This@, api_refac:bound_vars(Body@@)},api_refac:fun_define_info(f@)),	      
           ?TO_AST("f@(Args@@) when Guards@@ -> NewBody@@;")
       end, 
       begin
           FunInfo = api_refac:fun_define_info(f@),
	   (RefacScope /= function orelse FunInfo == MFA) andalso
	 begin
	     Result = try_transform_body(Body@@, RulesFun, {FunArgs, _This@, api_refac:bound_vars(Body@@)}, FunInfo, TimeOut),
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

get_files(RefacScope,SearchPaths,File) ->
     case RefacScope of
	project ->
	     wrangler_misc:expand_files(SearchPaths, ".erl");
	_ ->	    
	     [File]
    end.

filterError({error,_}) -> true;
filterError(_) -> false.



    


