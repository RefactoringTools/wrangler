-module(refac_ast_server).

-export([start_ast_server/1, stop_ast_server/0, get_ast/1]).

-export([ast_server/2]).


start_ast_server(SearchPaths) ->
    Pid = spawn_link(refac_ast_server, ast_server, [[], SearchPaths]),
    register(ast_server, Pid).

get_ast(FileName) ->
    ast_server ! {self(), get, FileName},
    receive 
	{ast_server, Res} ->
	    Res
    end.
	    

stop_ast_server() ->
    ast_server ! stop.

ast_server(Env, SearchPaths) ->
    receive
	{From, get, FileName} ->
	    case lists:keysearch(FileName, 1, Env) of
		{value, {FileName, {AnnAST, Info, FileModifiedTime}}} ->
		    case FileModifiedTime < filelib:last_modified(FileName) of 
			false ->
			    From ! {ast_server, {ok, {AnnAST, Info}}},
			    case lists:keysearch(errors,1, Info) of 
				{value, {errors, Error}} ->
				    error_logger ! {add, {FileName, Error}};
				false -> ok
			    end,
			    ast_server(Env, SearchPaths);
			true ->
			    case refac_util:parse_annotate_file(FileName, true, SearchPaths) of 
				{ok, {AnnAST1, Info1}} ->
				    From ! {ast_server, {ok, {AnnAST1, Info1}}},
				    case lists:keysearch(errors,1, Info) of 
 					{value, {errors, Error}} ->
 					    error_logger ! {add, {FileName, Error}};
 					false -> ok
 				    end,
				    ast_server([{FileName, {AnnAST1, Info1, filelib:last_modified(FileName)}}|Env], SearchPaths);
				{error, Reason} ->
				    From ! {ast_server, {error, Reason}},
				    ast_server(Env, SearchPaths)
			    end
		    end;
		false -> case refac_util:parse_annotate_file(FileName, true, SearchPaths) of 
			     {ok, {AnnAST, Info}} ->
				 From ! {ast_server, {ok, {AnnAST, Info}}},
				 case lists:keysearch(errors,1, Info) of 
				     {value, {errors, Error}} ->
					 error_logger ! {add, {FileName, Error}};
				     false -> ok
				 end,
				 ast_server([{FileName, {AnnAST, Info, filelib:last_modified(FileName)}}|Env], SearchPaths);
			     {error, Reason} ->
				 From ! {ast_server, {error, Reason}},
				 ast_server(Env, SearchPaths)
			 end
	    end;
		%% 	{update, {FileName, {AnnAST, Info}}} ->
		%% 	    Env1 =case lists:keysearch(FileName, 1, Env) of
		%% 		       {value, {FileName,  {_AnnAST1, _Info1}}} ->
		%% 			   lists:keyreplace(FileName, 1, Env, {FileName, {AnnAST, Info}});
		%% 		       false ->
		%% 			   [{FileName, {AnnAST, Info}} |Env]
		%% 		   end,
		%% 	    ast_server(Env1, SearchPaths);
	stop ->
	    ok
    end.
