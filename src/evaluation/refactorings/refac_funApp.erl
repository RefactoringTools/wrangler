%%%-------------------------------------------------------------------
%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc This module contains refactorings that substitute function calls by its application. 
%%<p>There are three refactoring rules on this module:
%%<ul> 
%%<li> <b>length_rule/1</b> -  Replaces function calls to the function <i>length/1</i> from the standard by the length of the list passed as parameter.</li>
%%<li> <b>functionCall_rule/1</b> - Function application for function calls from external modules.</li>
%%<li> <b>functionCall_rule_2/1</b> - Function application for function calls from internal modules. </li>
%%</ul>
%%</p> 
%%%
%%% @end
%%% Created : 05 Dec 2013 by Roberto S. M. de Barros Filho <>
%%%-------------------------------------------------------------------
-module(refac_funApp).
-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1,rules/2, transform_funApp/2]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prompts for parameter inputs
%%
%% @spec input_par_prompts() -> [string()]
%% @end
%%--------------------------------------------------------------------
input_par_prompts() -> refac:input_par_prompts() ++ ["Please, inform the name of the file with the definitions:"].

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
select_focus(Args) -> refac:select_focus(Args).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec(check_pre_cond(Args::args{}) -> ok | {error, Reason})
%% @end
%%--------------------------------------------------------------------
check_pre_cond(_Args) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Selective transformation or not.
%%
%% @spec selective() -> boolean()
%% @end
%%--------------------------------------------------------------------
selective() ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function does the actual transformation.
%%
%% @spec transform(Args::#args{}) -> 
%%            {ok, [{filename(), filename(), syntaxTree()}]} |
%%            {error, Reason}
%% @end
%%--------------------------------------------------------------------
transform(_Args)-> 
    transform_funApp(_Args, fun refac_funApp:rules/2).

transform_funApp(Args=#args{current_file_name=File, search_paths=SearchPaths,user_inputs=[_,_,ModuleName]}, Fun) ->
    CollectFile = core_funApp:getCollectFile(ModuleName, File, SearchPaths),
    case CollectFile of
	{ok, DefinitionsFile} ->
	    RefacModuleInfo = api_refac:module_name(File),
	    case RefacModuleInfo of
		{ok, RefacModule} ->
		    DefinitionsModule = list_to_atom(ModuleName),
		    Info = core_funApp:collect(DefinitionsFile, ModuleName == "" orelse DefinitionsModule == RefacModule),
		    refac:try_call_transform(Args, Fun, {DefinitionsModule,RefacModule, Info});
		_ -> {error, "Refactoring failed!"}
	    end;
	_ -> {error, "Definitions file doesn't exist!"}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
rules({{ModuleName,RefacModule,Info},_}, FunDefInfo) ->
    [   
    	core_funApp:length_rule(),
	core_funApp:functionCall_rule(Info, FunDefInfo, module_rules(ModuleName, RefacModule) ,true),
	functionCall_rule_2(Info, FunDefInfo),
	core_funApp:anonymousCall_rule()
    ]. 

module_rules(ModuleName, RefacModule) ->
    [core_funApp:addModuleName_rule(ModuleName), removeModuleName_rule(RefacModule)].
  
%%--------------------------------------------------------------------
%% @doc
%%This function represents a rule that substitutes a function call from a function in the same module by the appropriate body. 
%% <p>The parameter <i>Info</i> is the list returned by the collector.
%%</p>
%% <p>
%% This rule only applies a rewriting if exists a matching between the function clause being evaluated and any element from <i>Info</i>. Otherwise, nothing is done. </p>
%% @end
%%--------------------------------------------------------------------
functionCall_rule_2(Info, FunDefInfo) ->
    ?RULE(
          ?T("F@(Args@@)"),
	  begin
	      {M,F,A} = api_refac:fun_define_info(F@),
	      {match,Patt,Body} = utils_match:firstMatch(Info,{M,F,A},Args@@),
	      utils_subst:subst(Body, Patt, Args@@)
	  end,
	  begin	     	      	      
	      FunInfo = api_refac:fun_define_info(F@),
	      case FunInfo of
		  {M,F,A} ->
		      FunInfo /= FunDefInfo andalso
		      begin
			  FirstMatch = utils_match:firstMatch(Info,{M,F,A},Args@@),
			  if
			      FirstMatch == noMatch -> false;
			      true -> true
			  end
		      end;
		  _ -> false
	      end	      
	  end
	  ).

removeModuleName_rule(RefacModule) ->  
      ?RULE(
          ?T("M@:F@(Args@@)"),
	  ?TO_AST("F@(Args@@)"),
      begin
	  FunInfo = api_refac:fun_define_info(F@),
	  case FunInfo of
	      {M,_,_} ->
		  M == RefacModule;
	      _ -> false
	  end	  
      end
).

















