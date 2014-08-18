%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc Unfold Function Application Core - Substitute function calls by its application. 
%%
%% There are three types of transformations in this module:
%%<ul> 
%%<li> <b>Length</b> -  Replaces function calls to the function <i>length/1</i> from the standard by the length of the list passed as parameter. For instance, <em>length([10,20,30])</em> is transformed to <em>3</em>.</li>
%%<li> 
%%<b>External calls</b> - Function application for function calls from external modules. For example, consider the following module <em>def</em>:<br/><br/>
%%<em>
%%module(def).<br/>
%%export([sumList/1]).<br/>
%%<br/>
%%sumList([]) -> 0;<br/>
%%sumList([H | T]) when is_number(H) -> H + sumList(T).<br/><br/>
%%</em>
%%A call to <em>def:sumList([1,2,3])</em> can be simplified to <em>1 + (2 + (3 + 0))</em>.
%%</li>
%%<li>
%%<b>Parametrized Anonymous Calls</b> - This transformation is responsible for modifications in parametrized function calls of anonymous functions. For example:
%%<em>fun(X) -> 2 * X end(1)</em> is simplified to <em>2 * 1</em>.
%%</li>
%%</ul>
%%% @end
%%%-------------------------------------------------------------------
-module(core_funApp).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([getCollectFile/3,collect/1,length_rule/0,anonymousCall_rule/0,addModuleName_rule/1,functionCall_rule/4,functionCall_cond/6,collectFromDefsList/1]).
%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Auxiliary function to get the definitions file.
%%
%% @spec getCollectFile(modulename(), filename(), [paths()]) -> {ok, filename()} | {error, Reason} 
%% @end
%%--------------------------------------------------------------------
getCollectFile([], File, _) -> {ok, File};
getCollectFile(ModuleName, _, SearchPaths) -> 
    wrangler_misc:modname_to_filename(list_to_atom(ModuleName), SearchPaths).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function represents the collects info from the file.
%% All collectors defined in this file should be called here.
%% @end
%%--------------------------------------------------------------------
collect(File) ->
    ExportedFuns = api_refac:exported_funs(File),
    ExportedAll = exported_all(File),
    ?FULL_TD_TU(    
       [collector({ExportedAll, ExportedFuns})],
       [File]
      ).
%%--------------------------------------------------------------------
%% @doc
%% For all eligible Function Clauses in the file containing guards collects a tuple containing:
%%   -The function info: Module name, function name and arity
%%   -The AST representation of the pattern clause
%%   -The guards expression
%%   -The AST representation of the body
%% @end
%%--------------------------------------------------------------------
collector(ExportTuple)->
    ?COLLECT(
       ?T("f@(ArgPatt@@) when Guard@@ -> Body@@;"),
       {api_refac:fun_define_info(f@),ArgPatt@@,Guard@@,Body@@},
       api_refac:fun_define_info(f@) /= unknown andalso funIsExported(f@, ExportTuple)
     ).
%%--------------------------------------------------------------------
%% @doc
%% Auxiliary function that checks if a function is exported in a module.
%% @private
%% @end
%%--------------------------------------------------------------------
funIsExported(Fun, {ExportedAll, ExportedFuns}) ->
    {_, F, A} = api_refac:fun_define_info(Fun),
    ExportedAll orelse
    lists:any(fun({F2, A2}) ->  F == F2 andalso A == A2 end, ExportedFuns).

%%--------------------------------------------------------------------
%% @doc
%% Substitutes a call to the length rule, of the standard, with a list as a parameter
%% by the length of the list.
%% @end
%% @private
%%--------------------------------------------------------------------
length_rule() ->
    ?RULE(?T("length(List@)"),
	  begin
	     Length = length(utils_convert:ast_to_list(List@,[])),
	     ?TO_AST(integer_to_list(Length))
	  end,
	  api_refac:type(List@) == list andalso
	  utils_convert:ast_to_list(List@,[]) /= error
	).

%%@private
anonymousCall_rule() ->
    ?RULE(
      ?T("fun(Patt@@) -> Body@@ end(Args@@)"),
      utils_subst:subst(Body@@, Patt@@, Args@@),
     utils_match:match(Args@@, Patt@@)
    ).

%%--------------------------------------------------------------------
%% @doc
%%This function represents a rule that substitutes a function call from a external module by the appropriate body. 
%% <p>The parameter <i>Info</i> is the list returned by the collector.
%%</p>
%% <p>
%% This rule only applies a rewriting if exists a matching between the function clause being evaluated and any element from <i>Info</i>. Otherwise, nothing is done. </p>
%% @end
%% @private
%%--------------------------------------------------------------------
functionCall_rule(InfoList, FunDefInfo, IsRefactoring, BoundVars) ->
    ?RULE(
          ?T("M@:F@(Args@@)"),
	  begin
	      {M,F,A} = getFunDefineInfo(IsRefactoring, M@, F@, Args@@),
	      {match,Patt,Body} = utils_match:firstMatch(InfoList,{M,F,A},Args@@),
	      NewBody = utils_subst:subst(Body, Patt, Args@@),
	      Result = ?FULL_TD_TP(module_rules(utils_convert:convert_elem(M@)), NewBody),
	      case Result of
		  {ok, FinalNode} -> FinalNode;
		  _ -> {error, getErrorMsg(IsRefactoring)}
	      end
	  end,
	  begin	   
	      FunInfo = getFunDefineInfo(IsRefactoring, M@, F@, Args@@),
	      functionCall_cond(FunInfo,FunDefInfo,InfoList,Args@@,BoundVars,api_refac:bound_vars(_This@))
	  end
	  ).

%%@private
functionCall_cond(FunInfo,FunDefInfo,InfoList,Args@@,BoundVars,BoundVarsThis) ->
    case FunInfo of
		  {M,F,A} ->
		      FunInfo /= FunDefInfo andalso
		      begin
			  FirstMatch = utils_match:firstMatch(InfoList,{M,F,A},Args@@),
			  case FirstMatch of
			      {match,Patt,Body} -> 
				  Subst = utils_subst:subst(Body,Patt,Args@@),
				  variablesAreValid(BoundVars,api_refac:bound_vars(Subst),BoundVarsThis);
			      _ -> false
			  end
		      end;
		  _ -> false
	      end.
    

getFunDefineInfo(false, M@, F@, Args@@) -> {list_to_atom(?PP(M@)),list_to_atom(?PP(F@)),length(Args@@)};
getFunDefineInfo(true,_,F@,_) -> api_refac:fun_define_info(F@);			   
getFunDefineInfo(_, _,_,_) -> unknown.

getErrorMsg(false) -> 'No simplification was done!';
getErrorMsg(true) -> 'No refactoring was done!';
getErrorMsg(_) -> 'Unexpected Error!'.

-spec(exported_all/1::(File::filename()) -> boolean()).
exported_all(File) ->
    {ok, {_, Info}} = wrangler_ast_server:parse_annotate_file(File, true),
    case lists:keysearch(attributes, 1, Info) of
	{value, {attributes, ListOfAttributes}} ->
	    case lists:keysearch(compile, 1, ListOfAttributes) of
		{value, {compile, CompileDirective}} ->
		    CompileDirective == export_all orelse 
		    CompileDirective == [export_all];
		_ -> false
	    end;
	_ -> false
   end.

variablesAreValid([],_,_) -> true;
variablesAreValid(_,[],_) -> true; 
variablesAreValid([{Var,DefPos} | T],NewBoundVars,OldBoundVars) ->
    case lists:keyfind(Var,1,NewBoundVars) of
	false -> variablesAreValid(T,NewBoundVars,OldBoundVars);
	{Var,DefPos} ->
	    case lists:keyfind(Var,1,OldBoundVars) of
		false -> false;
		_ -> variablesAreValid(T,NewBoundVars,OldBoundVars)
	    end;
	_ -> false
    end.

module_rules(ModuleName) ->
  [addModuleName_rule(ModuleName)].

%%@private
addModuleName_rule(Module) ->  
      ?RULE(
          ?T("F@(Args@@)"),
	  ?TO_AST(atom_to_list(Module) ++  ":F@(Args@@)"),
	  begin
	     FunInfo = api_refac:fun_define_info(F@),
	     case FunInfo of
		 {M,_,_} ->
		     M == Module;
		 _ -> false
	     end
	  end
).

%%@private
collectFromDefsList([]) -> [];
collectFromDefsList(DefsTupleList) ->
    {list,lists:map(fun(X) -> getExternalInfoElem(X) end, DefsTupleList)}.

%%@private
getExternalInfoElem({ok, DefinitionsFile,ModName}) ->
    Info = collect(DefinitionsFile),
    {ModName, Info}.









