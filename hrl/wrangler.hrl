%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : wrangler.hrl
%%%-------------------------------------------------------------------

-define(DEFAULT_LIBS, [compiler,gs,hipe,kernel,mnesia,stdlib]).

-record(analysis, {analysis_pid, core_transform=cerl_typean,
		   defines=[], doc_plt=none, module_graph = none,
		   files, fixpoint, granularity, include_dirs=[],
		   init_plt, mcg=none, plt_info=none, 
		   supress_inline, start_from, user_plt}).

-record(options, {files=[],
		  files_rec=[],
		  core_transform=cerl_typean,
		  defines=[],
		  from=src_code, 	  
		  init_plt = none,  
		  include_dirs=[],
		  output_plt,
		  module_graph = none,
		  plt_libs=none,
		  supress_inline=false,
		  output_file=""}).

-record(callgraph, {scc_order, external_calls}).

%% Will be edited by Makefile 
-define(WRANGLER_DIR, "/home/hl/wrangler/share/distel/wrangler/plt").
