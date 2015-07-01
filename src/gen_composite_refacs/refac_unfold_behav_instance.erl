%%%-------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela
%%% @doc
%%% Unfold an instance of a behaviour into its behaviour definition
%%% @end
%%% Created : 16 Jun 2015 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(refac_unfold_behav_instance).

-behaviour(gen_composite_refac).

-include("../../include/wrangler.hrl").

%%%===================================================================
%% gen_composite_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 composite_refac/1]).

%%%===================================================================
%%% gen_composite_refac callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prompts for parameter inputs
%%
%% @spec input_par_prompts() -> [string()]
%% @end
%%--------------------------------------------------------------------
input_par_prompts() ->
    ["Name for the output module : "].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Select the focus of the refactoring.
%%
%% @spec select_focus(Args::#args{}) ->
%%                {ok, syntaxTree()} |
%%                {ok, none}
%% @end
%%--------------------------------------------------------------------
select_focus(_Args) ->
    {ok, none}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function defines the composite refactoring script.
%%
%% @spec composite_refac(Args::#args{}) -> composite_refac()|[]. 
%% @end
%%--------------------------------------------------------------------
composite_refac(#args{current_file_name = FileName,
		      user_inputs = [NewModuleNameStr],
		      search_paths = SearchPaths,
		      tabwidth = _TabWidth} = _Args) ->
    DefModuleStr = ?PP(hd(find_behaviour_module(FileName))),
    DefModule = list_to_atom(DefModuleStr),
    NewModule = list_to_atom(NewModuleNameStr),
    %[DefModuleFile] = wrangler_gen:gen_file_names(DefModule, SearchPaths),
    %% [ASTFuncs|_] = collect_callbacks(DefModuleFile),
    %% Funcs = hd(lists:map(fun wrangler_syntax:concrete/1, ASTFuncs)),
    ?atomic([?refac_(copy_mod,
		     [DefModule,
	     	      NewModule,
	     	      [FileName],
	     	      true,
	     	      SearchPaths]),
	     ?interactive(atomic,
	     	[?refac_(instantiate_calls,
	     	  [NewModuleNameStr ++ ".erl",
	     	   {generator, fun (_) -> filename:basename(FileName, ".erl") end},
	     	   true, SearchPaths])]),
	     ?refac_(move_fun,
		     [FileName,
		      fun (_FA) -> true end,
		      NewModule,
		      true,
		      SearchPaths
		     ])
	    %% , ?interactive(
	    %% 	[?refac_(unfold_fun_app,
	    %% 		 [NewModule,
	    %% 		  fun (FA) -> not lists:member(FA, Funcs) end,
	    %% 		  fun ({D, F, A}) -> D =:= NewModule andalso
	    %% 					 lists:member({F, A}, Funcs) end,
	    %% 		  true,
	    %% 		  SearchPaths
	    %% 		 ])])
	    ]).

%% collect_callbacks(File) ->
%%     ?FULL_TD_TU([?COLLECT(?T("behaviour_info(callbacks) -> Funcs@@;"),
%% 			  Funcs@@, true)],
%% 		[File]).

find_behaviour_module(File) ->
    ?FULL_TD_TU([?COLLECT(?T("-behaviour(module@)."),
			  module@, true),
		 ?COLLECT(?T("-behavior(module@)."),
			  module@, true)],
		[File]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
