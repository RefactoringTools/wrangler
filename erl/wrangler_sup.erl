%%%-------------------------------------------------------------------
%%% File    : wrangler_sup.erl
%%% Author  :  <Huiqing Li>
%%% Description : 
%%%
%%% Created : 15 Aug 2008 by  <Huiqing Li>
%%%-------------------------------------------------------------------
-module(wrangler_sup).

-behaviour(supervisor).

%% API
-export([start/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------

init(_Args) ->
    ASTServer = {wrangler_ast_server, {wrangler_ast_server, start_ast_server, []},
		 permanent,10000,worker,[wrangler_ast_server]},
    CallGraphServer={wrangler_callgraph_server, {wrangler_callgraph_server, start_callgraph_server, []},
		     permanent, 10000, worker, [wrangler_callgraph_server]},
    ErrorLogger={wrangler_error_logger, {wrangler_error_logger, start_wrangler_error_logger, []},
		  permanent, 10000, worker, [wrangler_error_logger]},
    UndoServer={wrangler_undi_server, {wrangler_undo_server, start_undo_server, []}, 
	       permanent, 10000, worker, [wrangler_undo_server]},
           
    {ok,{{one_for_one,3,60}, [ASTServer, CallGraphServer, ErrorLogger, UndoServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
