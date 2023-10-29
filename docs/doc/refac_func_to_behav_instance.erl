%%%-------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas>
%%% @author Simon Thompson <S.J.Thompson@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela, Simon Thompson
%%% @doc
%%% Moves the function to another module as
%%% a behaviour callback. If the destination does not exist it is
%%% created. If the destination module is not a behaviour instance
%%% of this module, it is transformed into one.
%%% The new callback is added to the behaviour_info(callback)
%%% function, which is created if it does not exist.
%%% @end
%%% Created :  5 Jun 2015 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(refac_func_to_behav_instance).

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
    ["Destination module : "].

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
		   cursor_pos=Pos,
		   tabwidth = _TabWidth,
		   user_inputs = [_DestModule]} = _Args) ->
    api_interface:pos_to_fun_def(File, Pos).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function defines the composite refactoring script.
%%
%% @spec composite_refac(Args::#args{}) -> composite_refac()|[]. 
%% @end
%%--------------------------------------------------------------------
composite_refac(#args{current_file_name = FileName,
		      focus_sel = FunDef,
		      user_inputs = [DestModule],
		      search_paths = SearchPaths,
		      tabwidth = _TabWidth} = _Args) ->
    {_M, F, A} = api_refac:fun_define_info(FunDef),
    ?atomic([
	     {refactoring, create_behav_instance,
	      [FileName, list_to_atom(DestModule), SearchPaths, composite_emacs]},
	     {refactoring, add_callback,
	      [FileName, atom_to_list(F), integer_to_list(A), SearchPaths, composite_emacs]},
	     ?refac_(move_fun,
		     [FileName,
		      fun ({Name, Arity}) -> Name =:= F andalso Arity =:= A;
			  (_Else) -> false
		      end,
		      list_to_atom(DestModule),
		      SearchPaths])
	    ]).

