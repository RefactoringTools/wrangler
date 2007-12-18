%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : wrangler.hrl
%%%-------------------------------------------------------------------

-record(options, {search_paths=[],
		  include_dirs=[],
		  plt_libs= [kernel,stdlib]
		  }).

-record(callgraph, {scc_order, external_calls}).

-record(attr, {pos = {0,0}, ann = [], com = none}).

%% Will be edited by Makefile 
-define(WRANGLER_DIR, "C:/cygwin/home/hl/wrangler/share/distel/wrangler").


-define(DEFAULT_LOC, 
        {0, 0}).  %% default defining location.
-define(DEFAULT_MODULE,
	unknown).  %% default module name.

