%%% @author Gabriela Sampaio<>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Simon  Thompson

-module(evaluator).

-export([start/1,start/2]).
-include_lib("../../../include/wrangler.hrl").

%%--------------------------------------------------------------------
%% @doc
%% This function starts the evaluator. <br/>
%% - <b>DefFilesInp</b>: list of atoms representing the names of the Erlang modules containing definitions. It is assumed, in this function, 
%%   that these files are in the current directory.
%% @end
%%--------------------------------------------------------------------
start(DefFilesInp) when is_list(DefFilesInp) -> start(DefFilesInp, []);
start(_) -> "Invalid input, please inform a list of definitions files!". 

%%--------------------------------------------------------------------
%% @doc
%% This function starts the evaluator. <br/>
%% - <b>DefFilesInput</b>: list of atoms representing the names of the Erlang modules containing definitions.  <br/>
%% - <b>Options</b>: a list of tuples. One tuple should represent the search paths and the other the timeout.  They can be defined in any order
%%   and each tuple should contain an atom specifying if is search paths or timeout and the respective value. If a timeout is defined, the 
%%   evaluation will not stop before this time. The search paths allow the user to choose which directory(ies) contain the Erlang file(s).<br/>
%% A possible call would be: <br/>
%% <i><b>evaluator:start([file1,file2,...,fileN],[{timeout, Timeout},{search_paths,[Search_Path1,Search_Path2,...,Search_PathN]}]</b></i>, <br/>
%% where <i>file1,file2,...,fileN, timeout</i> and <i>search_paths</i> are <b>atoms</b>, <i>Timeout</i> is an <b>integer</b> representing the 
%% timeout and <i>Search_Path1, Search_Path2,..., Search_PathN</i> are <b>strings</b> representing the search paths.  
%% @end
%%--------------------------------------------------------------------
start(DefFilesInput,Options) when is_list(DefFilesInput) andalso is_list(Options) ->
       SearchPathsInput = case lists:keyfind(search_paths, 1, Options) of
	   {search_paths, UserInput} -> UserInput;
	   _ -> 
	       	{ok,CurrDir} = file:get_cwd(),
	        [CurrDir]
       end,
       TimeoutInput = case lists:keyfind(timeout, 1, Options) of
	{timeout, UserInput2} -> UserInput2;
	_ -> 0
       end,
    start(DefFilesInput,SearchPathsInput,TimeoutInput);
start(_,_) -> {error,"Invalid input! Please inform a list of definitions files and optionally a list of options"}.

start(DefFilesNames,SearchPaths,Timeout) ->
    case validateSearchPaths(SearchPaths) of
	ok ->
	    case get_def_files(DefFilesNames,SearchPaths) of
		{ok,DefFiles} ->
		    case is_integer(Timeout) of
			    true -> 
			         case application:start(wrangler) of
				     ok -> 
					 evaluate(DefFiles,SearchPaths,"","",Timeout);
				     {error, {already_started,wrangler}} ->
					 evaluate(DefFiles,SearchPaths,"","",Timeout);
				     _ -> {error, "Please start Wrangler!"}
				 end;
			    _ -> {error,"Invalid Timeout! Please inform an integer for the timeout (or use the default timeout of 1000ms)!"}
		    end;
	        {error,Reason} -> {error,Reason}
	    end;
       {error,Reason} -> {error,Reason}
    end.

get_def_files(DefFiles,SearchPaths) ->
    get_def_files(DefFiles,SearchPaths, []).

get_def_files([],_,Acc) -> {ok,Acc};
get_def_files([FileName | T], SearchPaths,Acc) when is_atom(FileName) -> 
    case wrangler_misc:modname_to_filename(FileName, SearchPaths) of
	{ok, FullFileName} ->
	    get_def_files(T,SearchPaths, Acc ++ [{ok,FullFileName,FileName}]);
	_ ->  {error,"Definitions file not found in the provided list of search paths: " ++ atom_to_list(FileName)}
    end;
get_def_files(_,_,_) -> {error,"Invalid input for the list of definitions files, please inform a list of atoms corresponding to valid definitions files"}. 
			 
validateSearchPaths([]) -> ok;    
validateSearchPaths(List) when is_list(List) ->
    case io_lib:printable_list(List) of
	true -> {error,"Invalid list of search paths. Please inform a list of strings containg valid directories!"};
	_ ->
	    [H | T] = List,
	    case filelib:is_dir(H) of
		true -> 
		    validateSearchPaths(T);
		_ -> {error, H ++ " is not a directory"}
	    end
    end;
validateSearchPaths(_) -> {error,"Invalid list of search paths. Please inform a list of strings containg valid directories!"}.

get_expression_input() ->
    Line = io:get_line("Type an expression: "),
    case Line of
	"\n" -> get_expression_input();
	".\n" -> get_expression_input();
	Result -> 
	    LenLine = string:len(Line),
	    case string:substr(Line,LenLine - 1) of
		".\n" ->  		    
		    string:substr(Line,1,LenLine - 2) ++ "\n";
		_ ->
		    Result
	    end		
    end.
	    		       
evaluate(DefFiles,SearchPaths,OldExpression,OldPid,Timeout) ->
	case SearchPaths of
	    [_ | _] ->
                    Expression = if 
				     OldExpression == "" ->
                    			  get_expression_input();
				     true -> OldExpression
                    end,
                    {ok, Input2} = io:fread("Type N to execute N steps or 'f' to execute all steps: ", "~s"),
                    [NSteps|_] = Input2,
                    if OldPid == "" -> 
			       Pid = spawn(eval, keep_temp_info, [0,[?TO_AST(Expression)],""]);
                       true -> Pid = OldPid
                    end,
                    Result = eval_all:eval_all(DefFiles,Expression,Pid,NSteps,SearchPaths,Timeout),
                    case Result of
                          {error,_} -> io:format("~p~n",[Result]);
                           _ -> io:format("~n")
                    end,
                    {ok, [Answer]} = io:fread("Do you wish to continue this evaluation? [y/n] ", "~s"),
                    if Answer == "Y" orelse Answer == "y" orelse Answer == "Yes" -> evaluate(DefFiles,SearchPaths,Expression,Pid,Timeout);
                       Answer == "N" orelse Answer == "n" orelse Answer == "No" -> 
                                  {ok, [Answer2]} = io:fread("Do you wish to start a new evaluation? [y/n] ", "~s"),
                                  if Answer2 == "Y" orelse Answer2 == "y" orelse Answer2 == "Yes" -> evaluate(DefFiles,SearchPaths,"","",Timeout);
                                     Answer2 == "N" orelse Answer2 == "n" orelse Answer2 == "No" -> {ok,"Evaluator Stopped"};
                                     true -> {error, "Please answer yes or no"}
                                  end;
                       true -> {error, "Please answer yes or no"}
                   end;
    	     _ -> {error, "Invalid Wrangler Search Paths"}
    end.

