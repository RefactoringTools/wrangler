-module(core_funApp).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-compile([export_all]).
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
collect(File, InternalDefinitions) ->
    ExportedFuns = api_refac:exported_funs(File),
    ?FULL_TD_TU(    
       [collector(ExportedFuns, InternalDefinitions)],
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
collector(ExportedFuns, InternalDefinitions)->
    ?COLLECT(
       ?T("f@(ArgPatt@@) when Guard@@ -> Body@@;"),
       {api_refac:fun_define_info(f@),ArgPatt@@,Guard@@,Body@@},
       InternalDefinitions orelse funIsExported(f@, ExportedFuns)
     ).

%%--------------------------------------------------------------------
%% @doc
%% Auxiliary function that checks if a function is exported in a module.
%% @private
%% @end
%%--------------------------------------------------------------------
funIsExported(Fun, ExportedFuns) ->
    {_, F, A} = api_refac:fun_define_info(Fun),
    lists:any(fun({F2, A2}) ->  F == F2 andalso A == A2 end, ExportedFuns).

%%--------------------------------------------------------------------
%% @doc
%% Substitutes a call to the length rule, of the standard, with a list as a parameter
%% by the length of the list.
%% @end
%%--------------------------------------------------------------------
length_rule() ->
    ?RULE(?T("length(List@)"),
	  begin
	     Length = length(ast_to_list(List@,[])),
	     ?TO_AST(integer_to_list(Length))
	  end,
	  api_refac:type(List@) == list andalso
	  ast_to_list(List@,[]) /= error
	).

%%--------------------------------------------------------------------
%% @doc
%% Auxiliar function that receives an AST and returns a list.
%% @end
%% @private
%%--------------------------------------------------------------------
ast_to_list(none, List) -> List;
ast_to_list(ListNode, List) -> 
    Head = convert_elem(wrangler_syntax:list_prefix(ListNode)),
    case Head of
	error -> error;
	_ ->  
	    ast_to_list(wrangler_syntax:list_suffix(ListNode), 
			List ++ [Head])
    end.


%%--------------------------------------------------------------------
%% @doc
%% Auxiliar function that converts an element depending on its type.
%% @end
%% @private
%%--------------------------------------------------------------------
convert_elem([Elem | _T]) ->
    ElemStr = ?PP(Elem),
    ElemType = api_refac:type(Elem),
    case ElemType of
	integer -> list_to_integer(ElemStr);
	atom -> list_to_atom(ElemStr);
	tuple -> list_to_tuple(ElemStr);
	list -> ElemStr;
	variable -> ElemStr;
	_ -> error
    end;
convert_elem(_) -> error.

anonymousCall_rule() ->
    ?RULE(
      ?T("fun(Patt@@) -> Body@@ end(Args@@)"),
      utils_subst:subst(Body@@, Patt@@, Args@@),
      utils_match:match(Args@@, Patt@@) == true
    ).

addModuleName_rule(Module) ->  
      ?RULE(
          ?T("F@(Args@@)"),
      ?TO_AST(atom_to_list(Module) ++ ":F@(Args@@)"),
      begin
	  FunInfo = api_refac:fun_define_info(F@),
	  case FunInfo of
	      {M,_,_} ->
		  M == Module;
	      _ -> false
	  end
      end
).

%%--------------------------------------------------------------------
%% @doc
%%This function represents a rule that substitutes a function call from a external module by the appropriate body. 
%% <p>The parameter <i>Info</i> is the list returned by the collector.
%%</p>
%% <p>
%% This rule only applies a rewriting if exists a matching between the function clause being evaluated and any element from <i>Info</i>. Otherwise, nothing is done. </p>
%% @end
%%--------------------------------------------------------------------
functionCall_rule(Info, FunDefInfo, RulesList, IsRefactoring) ->
    ?RULE(
          ?T("M@:F@(Args@@)"),
	  begin
	      {M,F,A} = getFunDefineInfo(IsRefactoring, M@, F@, Args@@),
	      {match,Patt,Body} = utils_match:firstMatch(Info,{M,F,A},Args@@),
	      NewBody = utils_subst:subst(Body, Patt, Args@@),
	      Result = ?FULL_TD_TP(RulesList,NewBody),
	      case Result of
		  {ok, FinalNode} -> FinalNode;
		  _ -> {error, getErrorMsg(IsRefactoring)}
	      end
	  end,
	  begin	     
	      FunInfo = getFunDefineInfo(IsRefactoring, M@, F@, Args@@),
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

getFunDefineInfo(false, M@, F@, Args@@) -> {list_to_atom(?PP(M@)),list_to_atom(?PP(F@)),length(Args@@)};
getFunDefineInfo(true,_,F@,_) -> api_refac:fun_define_info(F@);			   
getFunDefineInfo(_, _,_,_) -> unknown.

getErrorMsg(false) -> 'No simplification was done!';
getErrorMsg(true) -> 'No refactoring was done!';
getErrorMsg(_) -> 'Unexpected Error!'.
    












