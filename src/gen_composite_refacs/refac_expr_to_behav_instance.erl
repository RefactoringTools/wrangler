%%%-------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Parametrises an expression by moving it to another module as
%%% a behaviour callback. If the destination does not exist it is
%%% created. If the destination module is not a behaviour instance
%%% of this module, it is transformed into one.
%%% The new callback is added to the behaviour_info(callback)
%%% function, which is created if it does not exist.
%%% @end
%%% Created :  5 Jun 2015 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(refac_expr_to_behav_instance).

-behaviour(gen_composite_refac).

-include("wrangler.hrl").

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
    ["Destination module : ", "Name for the new callback : "].

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
select_focus(#args{current_file_name=File,
		   highlight_range={Sta, End},
		   tabwidth = TabWidth,
		   user_inputs = [_DestModule, CallbackName]} = _Args) ->
    {ok, {AnnASTD, _InfoD}} = wrangler_ast_server:parse_annotate_file(File, true, [], TabWidth),
    case wrangler_misc:funname_arities(AnnASTD, list_to_atom(CallbackName)) of
	[] -> api_interface:pos_to_expr1(File,Sta,End);
	_  -> {error, "There exists a function with the provided name in this module, please choose another name!"}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function defines the composite refactoring script.
%%
%% @spec composite_refac(Args::#args{}) -> composite_refac()|[]. 
%% @end
%%--------------------------------------------------------------------
composite_refac(#args{current_file_name = FileName,
		      highlight_range = {Start, End},
		      user_inputs = [DestModule, CallbackName],
		      search_paths = SearchPaths,
		      tabwidth = _TabWidth} = _Args) ->
    TupleCallbackName = list_to_atom(CallbackName),
    ?atomic([
	     {refactoring, create_behav_instance,
	      [FileName, list_to_atom(DestModule), SearchPaths, composite_emacs]},
	     {refactoring, fun_extraction,
	      [FileName, tuple_to_list(Start), tuple_to_list(End),
	       CallbackName, composite_emacs]},
	     {refactoring, add_callbacks,
	      [FileName, CallbackName, SearchPaths, composite_emacs]},
	     ?refac_(move_fun,
		     [FileName,
		      fun ({Name, _}) -> Name =:= TupleCallbackName;
			  (_Else) -> false
		      end,
		      list_to_atom(DestModule),
		      SearchPaths])
	    ]).

