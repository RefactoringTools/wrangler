%% @private
%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Roberto S. M. de Barros Filho, Simon  Thompson
%% Auxiliar Module that is used when the refactorings contains similaraties.
%% This module is responsible for the timeout for example.
-module(refac).
-export([try_call_transform/2, try_call_transform/3, body_rules/5, try_transform_manager/5, checkTimeOut/1, get_refac_scope/1, select_focus/1, select_focus/2, input_par_prompts/0,get_files/3,get_files/4,filterError/1,input_refac_scope_message/0,validation_with_timeout/3,validate_refac_scope/2,start_transformation/5,start_transformation/6,validate_all/4,validate_definitions_str/2,collectFile/3,fun_define_info/2,get_definitions_tuplelist/2,timeout_manager/1]).

%% Include files
-include_lib("../../../include/wrangler.hrl").

input_par_prompts() ->
   ["Please, choose a timeout value in miliseconds (default is 1000ms):",input_refac_scope_message()].

input_refac_scope_message() ->
     "Do you want to apply this refactoring to the whole project (Y), the whole file (y), or just the selected function (n)?".

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

try_call_transform(Args=#args{user_inputs=InputsList}, RulesFun, FunArgs) -> 
    TimeOutStr = lists:nth(1, InputsList),
    RefacScopeStr = lists:nth(2, InputsList),
    Validation = validation_with_timeout(TimeOutStr,RefacScopeStr,Args),
    case Validation of
	{error, Reason} -> {error, Reason};
	_ ->
	    start_transformation(RefacScopeStr,RulesFun,TimeOutStr,FunArgs,Args)
    end.

timeout_manager(List) when is_list(List) ->
    receive
	{timeout, {File, MFA, Exp}} ->
	    MFAList = get_list(lists:keyfind(File,1,List)),
	    ExpList = get_list(lists:keyfind(MFA,1,MFAList)),
	    NewExpList = lists:keystore(Exp,1,ExpList,{Exp}),
	    NewMFAList = lists:keystore(MFA,1,MFAList,{MFA,NewExpList}),
	    NewList = lists:keystore(File,1,List,{File,NewMFAList}),
	    timeout_manager(NewList);
	finished ->
	    case List of
		[_ | _] ->
		    io:format("TIMEOUTS SUMMARY~n"),
		    lists:foreach(fun({File,FList}) ->
					  io:format("Timeouts in file: ~p~n",[File]),
					  lists:foreach(fun({{M,F,A},MFAList}) ->
								MFA = atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A),
								io:format("   Timeouts in function: ~p~n",[MFA]),
								lists:foreach(fun({Exp}) ->
										     io:format("      Timeout in expression: ~p~n",[Exp])
									      end, MFAList)
							end,FList) 
				  end,List);
		_ -> io:format("No timeout occurred!~n")
	    end
		     
    end.

get_list({_,List}) when is_list(List) -> List;
get_list(_) -> [].

					 
start_transformation(RefacScopeStr,RulesFun,TimeOutStr,FunArgs,Args=#args{current_file_name=File,search_paths=SearchPaths}) ->
     Files = get_files(RefacScopeStr,SearchPaths,File),
     start_transformation(RefacScopeStr,RulesFun,TimeOutStr,FunArgs,Args,Files).

start_transformation(RefacScopeStr,RulesFun,TimeOutStr,FunArgs,_Args=#args{current_file_name=File, focus_sel=FunDef},Files) ->
    Pid = spawn(refac, timeout_manager, [[]]),
    TimeOut = checkTimeOut(TimeOutStr),
    RefacScope = get_refac_scope(RefacScopeStr),
    MFA = fun_define_info(RefacScope,FunDef),
    Result = if
		RefacScope == file orelse RefacScope == function ->       	    		
		     ?FULL_TD_TP((body_rules(RulesFun, {RefacScope, MFA}, TimeOut, FunArgs, Pid)),[File]);
		RefacScope == project ->		 
		     ?FULL_TD_TP((body_rules(RulesFun, {RefacScope, MFA}, TimeOut, FunArgs, Pid)),Files)
    end,
    Pid ! finished,
    Result.

fun_define_info(RefacScope, FunDef) ->
    case RefacScope of
	function ->
	    api_refac:fun_define_info(FunDef);
	_ -> unknown
    end.
    
validate_refac_scope(RefacScopeStr, _Args=#args{focus_sel=FunDef,search_paths=SearchPaths}) ->
    case get_refac_scope(RefacScopeStr) of
	file -> ok;
	function ->
	    MFA = api_refac:fun_define_info(FunDef),
	    if 
		MFA /= unknown -> ok;
		true -> {error, "Please, place the mouse cursor on the desired function!"}
	    end;
	project ->
	    Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
	    RefacModuleInfoList = lists:map(fun(CurFile) -> api_refac:module_name(CurFile) end,Files),
	    case lists:filter(fun(Tuple) -> filterError(Tuple) end, RefacModuleInfoList) of
		[_ | _] -> {error, "Invalid file!"};
		_ -> ok
	    end;
	_ -> {error, "Please, answer 'y', 'Y' or 'n' to define the scope of the refactoring!"}
    end.
			
validation_with_timeout(TimeOutStr,RefacScopeStr,Args) ->
    case checkTimeOut(TimeOutStr) of
	{error, _} -> {error, "invalid timeout input!"};
	_ ->
	    validate_refac_scope(RefacScopeStr,Args)
    end.

validate_all(TimeOutStr,RefacScopeStr,DefinitionsStr, Args) ->
    case validation_with_timeout(TimeOutStr,RefacScopeStr,Args) of
	{error, Reason} -> {error, Reason};
	_ -> validate_definitions_str(DefinitionsStr, Args)
    end.

validate_definitions_str([],_) -> ok;
validate_definitions_str(DefinitionsStr, _Args=#args{search_paths=SearchPaths}) -> 
    CollectFiles = get_definitions_tuplelist(DefinitionsStr,SearchPaths),
    WrongFiles = lists:filter(fun(X) -> filterError(X) end, CollectFiles),
    case WrongFiles of
	[H | _] -> H;
	_ -> 
	    ok
    end.

get_definitions_tuplelist("",_) -> [];
get_definitions_tuplelist(DefinitionsStr,SearchPaths) ->
    Definitions = string:tokens(DefinitionsStr, " "),
    lists:map(fun(DefinitionFile) -> collectFile(DefinitionFile,[],SearchPaths) end, Definitions).

get_definitions_list("",_) -> [];
get_definitions_list(DefinitionsStr, SearchPaths) -> 
    Definitions = string:tokens(DefinitionsStr, " "),
    lists:map(fun(DefFileName) -> 
		      {ok, DefinitionsFile} = core_funApp:getCollectFile(DefFileName,[],SearchPaths),
		      DefinitionsFile
    end, Definitions).

collectFile(ModName,File,SearchPaths) ->
    CollectFile = core_funApp:getCollectFile(ModName,File,SearchPaths),
    case CollectFile of
	{ok, DefinitionsFile} -> {ok, DefinitionsFile, ModName};
	_ -> {error, "Definitions file '" ++ ModName ++ "' doesn't exist!"}
    end.

body_rules(RulesFun, RefacType, TimeOut, Info,ManagerPid) -> [body_rule(RulesFun, RefacType, TimeOut, Info,ManagerPid)].

get_refac_scope("y") -> file;
get_refac_scope("Y") -> project;
get_refac_scope(Str) when Str == "n" orelse Str == "" -> function;
get_refac_scope(_) -> maybe. 
    

%%--------------------------------------------------------------------
%% @doc
%% This function represents a rule that tries to perform substitution rules in a function body only if the function information matches the function chosen by the user.
%% @end
%%--------------------------------------------------------------------   
body_rule(RulesFun, {RefacScope, MFA}, TimeOut, FunArgs,ManagerPid) ->
    ?RULE(
       ?T("f@(Args@@) when Guards@@ -> Body@@;"),
       begin	   
	   NewBody@@ = utils_transform:transform_body(Body@@,RulesFun,{FunArgs, _This@, api_refac:bound_vars(Body@@)},api_refac:fun_define_info(f@)),	      
           ?TO_AST("f@(Args@@) when Guards@@ -> NewBody@@;")
       end, 
       begin
           FunInfo = api_refac:fun_define_info(f@),
	   FunInfo /= unknown andalso
	   (RefacScope /= function orelse FunInfo == MFA) andalso
	 begin
	     Result = try_transform_body(Body@@, RulesFun, {FunArgs, _This@, api_refac:bound_vars(_This@)}, FunInfo, TimeOut,get_file(_File@),ManagerPid),
	     case Result of
		  {error, _Reason} -> false;
		  _ -> true
	     end andalso
		(?PP(Result) /= ?PP(Body@@))
	   end
      end
       ).

get_file({FileName,FileName}) -> FileName;
get_file(FileName) -> FileName. 

try_transform_body(Node, RulesFun, FunArgs, FunDefInfo, TimeOut,File,ManagerPid) ->   
    Pid = spawn(refac, try_transform_manager, [Node, RulesFun, FunArgs, FunDefInfo, self()]),
	    receive
		{result, NewNode} -> NewNode
	    after 
		TimeOut -> 
		    exit(Pid,kill),
		    Exp = ?PP(Node),
		    ManagerPid ! {timeout,{File,FunDefInfo,Exp}},
		    io:format("TIMEOUT in expression: ~p~n",[Exp]),
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

get_files(RefacScopeStr,SearchPaths,File) ->
    get_files(get_refac_scope(RefacScopeStr),SearchPaths,File,"").

get_files(RefacScope,SearchPaths,File, DefinitionsStr) ->
     case RefacScope of
	project ->
	     DefinitionsList = get_definitions_list(DefinitionsStr,SearchPaths),
	     Files = wrangler_misc:expand_files(SearchPaths, ".erl"),
	     lists:filter(fun(FileName) -> lists:any(fun(FileName2) -> FileName == FileName2 end,DefinitionsList) == false end,Files);
	_ ->	    
	     [File]
    end.

filterError({error,_}) -> true;
filterError(_) -> false.



    


