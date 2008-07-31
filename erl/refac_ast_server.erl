-module(refac_ast_server).

-export([start_ast_server/1, stop_ast_server/0, get_ast/1, update_ast/2]).

-include("../hrl/wrangler.hrl").

-spec(start_ast_server/1::([dir()]) -> true).	     
start_ast_server(SearchPaths) ->
    Pid = spawn_link(fun() ->ast_server([], SearchPaths) end),
    register(ast_server, Pid).

-spec(get_ast/1::(filename()) ->
	     {ok, {syntaxTree(), moduleInfo()}} | {error, string()}).
get_ast(FileName) ->
    ast_server ! {self(), get, FileName},
    receive 
	{ast_server, Res} ->
	    Res
    end.
-type(modifyTime()::{{integer(), integer(), integer()},{integer(), integer(), integer()}}).
-spec(update_ast/2::(filename(), {syntaxTree(), moduleInfo(), modifyTime()}) ->
	     ok).
update_ast(FileName, {AnnAST, Info, Time}) ->
    ast_server ! {update,{FileName, {AnnAST, Info, Time}}},
    ok.

-spec(stop_ast_server/0::()-> stop).	     
stop_ast_server() ->
    ast_server ! stop.

ast_server(Env, SearchPaths) ->
    receive
	{From, get, FileName} ->
	    case lists:keysearch(FileName, 1, Env) of
		{value, {FileName, {AnnAST, Info, FileModifiedTime}}} ->
		    case FileModifiedTime >= filelib:last_modified(FileName) of 
			true ->
			    From ! {ast_server, {ok, {AnnAST, Info}}},
			    case lists:keysearch(errors,1, Info) of 
				{value, {errors, Error}} ->
				    wrangler_error_logger ! {add, {FileName, Error}};
				false -> ok
			    end,
			    ast_server(Env, SearchPaths);
			false ->
			    {ok, {AnnAST1, Info1}} = refac_util:parse_annotate_file_1(FileName, true, SearchPaths),
			    From ! {ast_server, {ok, {AnnAST1, Info1}}},
			    case lists:keysearch(errors,1, Info) of 
				{value, {errors, Error}} ->
				    wrangler_error_logger ! {add, {FileName, Error}};
				false -> ok
			    end,
			    ast_server([{FileName, {AnnAST1, Info1, filelib:last_modified(FileName)}}|Env], SearchPaths)
			end;
		false -> 
		    {ok, {AnnAST, Info}} = refac_util:parse_annotate_file_1(FileName, true, SearchPaths),
		    From ! {ast_server, {ok, {AnnAST, Info}}},
		    case lists:keysearch(errors,1, Info) of 
			{value, {errors, Error}} ->
			    wrangler_error_logger ! {add, {FileName, Error}};
			false -> ok
		    end,
		    ast_server([{FileName, {AnnAST, Info, filelib:last_modified(FileName)}}|Env], SearchPaths)
	    end;
	{update, {FileName, {AnnAST, Info, Time}}} ->
	    Env1 =case lists:keysearch(FileName, 1, Env) of
		      {value, {FileName,  {_AnnAST1, _Info1, _Time}}} ->   %% IS THIS CORRECT?
			  lists:keyreplace(FileName, 1, Env, {FileName, {AnnAST, Info, Time}});
		      false ->
			  [{FileName, {AnnAST, Info, Time}} |Env]
		  end,
	    ast_server(Env1, SearchPaths);
	stop ->
	    ok
    end.
