%% =====================================================================
%% A temporary callgraph server. This module will be refactored to use 
%% the genserver behavioure..
%%
%% Copyright (C) 2006-2009  Huiqing Li, Simon Thompson
%%======================================================================

-module(refac_callgraph_server).


-export([start_callgraph_server/0, stop_callgraph_server/0, get_callgraph/1]).

-include("../hrl/wrangler.hrl").


-spec(start_callgraph_server/0::() -> true).	     
start_callgraph_server() ->
    Pid = spawn_link(fun()->callgraph_server([]) end),
    register(callgraph_server, Pid).

-spec(stop_callgraph_server/0::() -> stop).
stop_callgraph_server() ->
    callgraph_server!stop.


-spec(get_callgraph/1::([dir()])-> {[{{atom(), atom(), integer()}, [{atom(), atom(), integer()}]}], scc_order(), external_calls()}).
get_callgraph(SearchPaths) ->
    callgraph_server ! {self(), get, SearchPaths},
    receive
	{callgraph_server, CallGraph} ->
	    CallGraph
    end.
    
callgraph_server(State) ->    
    receive
	{From, get, SearchPaths} ->
	   case lists:keysearch(SearchPaths, 1, State) of 
	       {value, {SearchPaths, CallGraph}} ->
		   From ! {callgraph_server, CallGraph},
		   callgraph_server(State);
	       false ->
		   {CallerCallee, Sccs, E} = build_callgraph(SearchPaths),
		   From ! {callgraph_server,  {CallerCallee, Sccs, E}},
		   callgraph_server([{SearchPaths, {CallerCallee, Sccs, E}}|State])
	   end;
	stop -> 
		ok
    end.
    
build_callgraph(DirList) ->
    Files = refac_util:expand_files(DirList, ".erl"),
    CallGraph = build_callgraph(Files, []),
    CallerCallee = lists:map(fun ({{Caller, _CallerDef}, Callee}) -> {Caller, Callee} end, CallGraph),
    #callgraph{scc_order = Sccs, external_calls = E} = refac_callgraph:construct(CallGraph),
    {CallerCallee, Sccs, E}.
     
   
build_callgraph([FileName | Left], Acc) ->
    {ok, {AnnAST, Info}}= refac_util:parse_annotate_file(FileName, true, []),
    G1 = refac_util:build_callgraph(AnnAST, Info, FileName),
    Acc1 = Acc ++ G1,
    build_callgraph(Left, Acc1);
build_callgraph([], Acc) -> Acc.			   
			    


