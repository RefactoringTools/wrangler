%%%-------------------------------------------------------------------
%%% @author Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc Unfold Function Application - Substitute function calls by its application. This refactoring uses Wrangler API and can be found in <em>Wrangler -> Refactor -> gen_refac Refacs -> Symbolic Evaluation -> Unfold Function Application</em>.
%%
%% Most of the transformation in this module are from <a href="core_funApp.html">Unfold Function Application Core</a>.
%%
%% Nevertheless, the following simplification is also done in this module: 
%%
%%<b>Internal calls</b> - Function application for function calls from internal modules. To illustrate, consider the following definition of the function <em>double/1</em>: <br/><br/>
%%<em>double(X) when is_number(X) -> 2 * X.</em><br/><br/>
%%A call to <em>double(2)</em>, within the same module that <em>double/1</em> was defined, is modified to <em>2 * 2</em>.
%%% @end
%%%-------------------------------------------------------------------
-module(refac_funApp).
-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1, rules/2, transform_funApp/2, getInfoList/2, getDefinitionsInfo/1, getInternalFiles/3]).

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

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
transform_funApp(Args=#args{current_file_name=File,search_paths=SearchPaths,user_inputs=[TimeOutStr,RefacScopeStr,DefinitionsStr]}, Fun) ->
    case refac:validate_all(TimeOutStr,RefacScopeStr, DefinitionsStr, Args) of
	{error, Reason} -> {error,Reason};
	_ ->
	    RefacScope = refac:get_refac_scope(RefacScopeStr),
	    Files = refac:get_files(RefacScope,SearchPaths,File,DefinitionsStr),
            start_transformation(Files,Fun,RefacScope,Args)
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start_transformation(Files,Fun,RefacScope,Args=#args{search_paths=SearchPaths,user_inputs=[TimeOutStr,RefacScopeStr,DefinitionsStr]}) ->
    DefsTupleList = refac:get_definitions_tuplelist(DefinitionsStr,SearchPaths),
    InternalFiles = getInternalFiles(Files,DefsTupleList,RefacScope),
    case createInfoList(InternalFiles, DefsTupleList) of
	{error, Reason} -> {error,Reason};
	 InfoList ->
	         refac:start_transformation(RefacScopeStr,Fun,TimeOutStr,InfoList,Args,Files)
	    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
collector()->
    ?COLLECT(
       ?T("f@(ArgPatt@@) when Guard@@ -> Body@@;"),
       {api_refac:fun_define_info(f@),ArgPatt@@,Guard@@,Body@@},
       api_refac:fun_define_info(f@) /= unknown 
     ).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
collect(Files) ->
    ?FULL_TD_TU(    
       [collector()],
       Files
      ).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
createInfoList(InternalFiles,DefsTupleList) ->
    {collect(InternalFiles),getDefinitionsInfo(DefsTupleList)}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
getInfoList(InternalFiles,DefinitionsInfo) ->
    {collect(InternalFiles),DefinitionsInfo}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
getInternalFiles(_,_,project) -> [];
getInternalFiles(Files, DefsTupleList,_) when is_list(Files) andalso length(Files) == 1 ->
    lists:filter(fun(File) -> lists:keyfind(File,2,DefsTupleList) /= false end, Files);
getInternalFiles(Files,_,_) -> Files.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
getDefinitionsInfo([]) -> [];
getDefinitionsInfo(DefsTupleList) ->
    {list,lists:map(fun(X) -> getExternalInfoElem(X) end, DefsTupleList)}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
getExternalInfoElem({ok, DefinitionsFile,ModName}) ->
    Info = core_funApp:collect(DefinitionsFile),
    {list_to_atom(ModName), Info}.

%%--------------------------------------------------------------------
%%@doc
%% Return the list of rules for the function application refactoring. This is formed by:
%%<ul>
%%<li>core_funApp:length_rule/0</li>
%%<li>core_funApp:functionCall_rule/4</li>
%%<li>Internal calls rule</li>
%%<li>core_funApp:anonymousCall_rule/0</li>
%%</ul>
%% @spec rules({{InternalInfo::[{mfa(),syntaxTree(),[syntaxTree()],syntaxTree()}],ExternalInfo::[{mfa(),syntaxTree(),[syntaxTree()],syntaxTree()}] | [{list, [{modulename(),[{mfa(),syntaxTree(),[syntaxTree()],syntaxTree()}]}]}]},term(),BoundVars::[{atom(), pos()}]},FunDefInfo::mfa() | unknown) -> [rule()]
%% @end
%%--------------------------------------------------------------------
-spec(rules({{InternalInfo::[{mfa(),syntaxTree(),[syntaxTree()],syntaxTree()}],ExternalInfo::[{mfa(),syntaxTree(),[syntaxTree()],syntaxTree()}] | [{list, [{modulename(),[{mfa(),syntaxTree(),[syntaxTree()],syntaxTree()}]}]}]},_,BoundVars::[{atom(), pos()}]},FunDefInfo::mfa() | unknown) -> [{'rule',fun(),list() | tuple()},...]).
rules({{InternalInfo,ExternalInfo},_,BoundVars}, FunDefInfo) ->
    [   
    	core_funApp:length_rule(),
	core_funApp:functionCall_rule(ExternalInfo, FunDefInfo, true, BoundVars),
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
%% @private
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



    















