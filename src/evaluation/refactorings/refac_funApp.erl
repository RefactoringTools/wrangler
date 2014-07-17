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
input_par_prompts() -> refac:input_par_prompts() ++ ["Please, inform the name of the files with the definitions (the names should by separated spaces):"].

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

transform_funApp(Args=#args{current_file_name=File, search_paths=SearchPaths,user_inputs=[_,_,ModuleNamesStr]}, Fun) ->
    RefacModuleInfo = api_refac:module_name(File),
    case RefacModuleInfo of
	  {ok, RefacModule} -> 
	                Result = getInfoList(ModuleNamesStr, File, SearchPaths,RefacModule),
	                case Result of
		             {error, _} -> Result;
		             InfoList -> 
				refac:try_call_transform(Args, Fun, {RefacModule, InfoList})
	                end;
        _ -> {error, "Refactoring failed!"}
    end.

collector()->
    ?COLLECT(
       ?T("f@(ArgPatt@@) when Guard@@ -> Body@@;"),
       {api_refac:fun_define_info(f@),ArgPatt@@,Guard@@,Body@@},
       api_refac:fun_define_info(f@) /= unknown 
     ).

collect(File) ->
    ?FULL_TD_TU(    
       [collector()],
       [File]
      ).

filterError({error,_}) -> true;
filterError(_) -> false.

getInfoList([],File,_,_) -> {collect(File),[]};
getInfoList(ModuleNamesStr,File, SearchPaths,RefacModule) ->
    ModuleNames = string:tokens(ModuleNamesStr, " "),
    CollectFiles = lists:map(fun(ModName) -> collectFile(ModName,File,SearchPaths) end, ModuleNames),
    WrongFiles = lists:filter(fun(X) -> filterError(X) end, CollectFiles),
	                case WrongFiles of
		             [H | _] -> H;
		             _ -> 
				{getInternalInfo(CollectFiles,RefacModule),{list,lists:map(fun(X) -> getExternalInfoElem(X) end, CollectFiles)}}
	                end.
getExternalInfoElem({ok, DefinitionsFile,ModName}) ->
    Info = core_funApp:collect(DefinitionsFile),
    {list_to_atom(ModName), Info}.

getInternalInfo([],_) -> [];
getInternalInfo([{ok, DefinitionsFile,ModName} | T],RefacModule) ->
    DefinitionsModule = list_to_atom(ModName),    
    case DefinitionsModule == RefacModule of
	    true ->
		collect(DefinitionsFile);
	    _ -> getInternalInfo(T,RefacModule)
    end.

collectFile(ModName,File,SearchPaths) ->
    CollectFile = core_funApp:getCollectFile(ModName,File,SearchPaths),
    case CollectFile of
	{ok, DefinitionsFile} -> {ok, DefinitionsFile, ModName};
	_ -> {error, "Definitions file '" ++ ModName ++ "' doesn't exist!"}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
rules({{RefacModule,{InternalInfo,ExternalInfo}},_,BoundVars}, FunDefInfo) ->
    [   
    	core_funApp:length_rule(),
	core_funApp:functionCall_rule(ExternalInfo, FunDefInfo, RefacModule ,true, BoundVars),
	functionCall_rule_2(InternalInfo, FunDefInfo,BoundVars),
	core_funApp:anonymousCall_rule()
    ].
    
  
%%--------------------------------------------------------------------
%% @doc
%%This function represents a rule that substitutes a function call from a function in the same module by the appropriate body. 
%% <p>The parameter <i>Info</i> is the list returned by the collector.
%%</p>
%% <p>
%% This rule only applies a rewriting if exists a matching between the function clause being evaluated and any element from <i>Info</i>. Otherwise, nothing is done. </p>
%% @end
%%--------------------------------------------------------------------
functionCall_rule_2(InfoList, FunDefInfo,BoundVars) ->
    ?RULE(
          ?T("F@(Args@@)"),
	  begin
	      {M,F,A} = api_refac:fun_define_info(F@),
	      {match,Patt,Body} = utils_match:firstMatch(InfoList,{M,F,A},Args@@),
	      utils_subst:subst(Body, Patt, Args@@)
	  end,
	  begin	     
	      FunInfo = api_refac:fun_define_info(F@),
	      api_refac:is_fun_name(?PP(F@)) andalso core_funApp:functionCall_cond(FunInfo,FunDefInfo,InfoList,Args@@,BoundVars,api_refac:bound_vars(_This@))
	  end
	  ).



    















