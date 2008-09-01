%%%-------------------------------------------------------------------
%%% File    : wrangler_callgraph_server.erl
%%% Author  :  <Huiqing>
%%% Description : A gen server manageing the callgraph of the program under refactoring.
%%%
%%% Created : 28 Aug 2008 by  <Huiqing>
%%%-------------------------------------------------------------------
-module(wrangler_callgraph_server).

-behaviour(gen_server).

-include("../hrl/wrangler.hrl").

%% API
-export([start_callgraph_server/0, get_callgraph/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {callgraph=[]}).
%%====================================================================
%% API
%%====================================================================

-spec(start_callgraph_server/0::() -> {ok, pid()} | ignore | {error, string()}).
start_callgraph_server() ->
    gen_server:start_link({local, wrangler_callgraph_server}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec(init/1::(any()) ->{ok, #state{}}).
init(_Args) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
-spec(handle_call/3::({atom(), [dir()]}, any(), #state{}) ->
	     {reply, {callercallee(), scc_order(), external_calls()}, #state{}}).
handle_call({get, SearchPaths}, _From, State) ->
    {Reply, State1} = get_callgraph(SearchPaths, State),
    {reply, Reply, State1}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
-spec(handle_cast/2::(any(), #state{}) ->
	      {noreply, #state{}}).
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
-spec(handle_info/2::(any(), #state{}) ->
	      {noreply, #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-spec(terminate/2::(any(), #state{}) -> ok).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
-spec(code_change/3::(any(), #state{}, any()) ->
	      {ok, #state{}}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec(get_callgraph/1::([dir()])-> {callercallee(), scc_order(), external_calls()}).
get_callgraph(SearchPaths) ->
    gen_server:call(wrangler_callgraph_server, {get, SearchPaths}).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_callgraph(SearchPaths, State) ->
    case lists:keysearch(SearchPaths, 1, State#state.callgraph) of 
	{value, {SearchPaths, CallGraph}} ->
	    {CallGraph, State};	
	false ->
	    CallGraph = build_callgraph(SearchPaths),
	    {CallGraph, #state{callgraph=[{SearchPaths, CallGraph}|State#state.callgraph]}}
    end.

    
build_callgraph(SearchPaths) ->
    Files = refac_util:expand_files(SearchPaths, ".erl"),
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
			    


