%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2013, Simon  Thompson
%%% @doc 
%%This module was created with the aim of <b>mixing some refactorings and applying all their rules toghether</b>. Thus, this refactoring just call the others refactorings (arithmetics, identity and function applications). <p>For instance, the code <i>"f(X) ->  X + 0 + 1 + 2"</i> would be transformed into <i>"f(X) -> X + 3"</i>. This would be done by the refactorings that substitute identity expressions (E.g.: "X + 0", which return "X") and also the refactoring which manipulates with operations between integers and variables (this refactoring would substitute <i>"1 + 2"</i> to <i>"3"</i>. Then the final result is <i>"f(X) -> X + 3"</i>. </p><p>That's why this refactoring is important, to integrate the others three refactorings and create a more powerful single refactoring that contains the other three.</p>  
%%%
%%% @end
%%% Created : 18 Oct 2013 by Gabriela Cunha, Roberto Souto <>
%%%-------------------------------------------------------------------
-module(eval_all).

-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1,transform/3,transform/5,rules/2, eval_all/7]).

%%%===================================================================
%%% gen_refac callbacks
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
    ["Type the expression you want to apply the refactoring: ",
     "Type N to execute N steps or 'f' to execute all steps: "].

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
select_focus(_Args=#args{}) -> {ok,none}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring. The user will type a number (the number of steps that will be executed in the refactoring or the letter <i>f</i> to execute all of the steps in one time. This entry needs to be checked, because it can be only a integer or the letter f. If the user types anything different, an error message is given and the refactoring is not executed.
%%
%% @spec(check_pre_cond(_Args::args{}) -> ok | {error, Reason})
%% @end
%%--------------------------------------------------------------------
check_pre_cond(_Args=#args{user_inputs=[_,I]}) -> 
    check_valid_number_steps(I);
check_pre_cond(_Args=#args{user_inputs=[I]}) ->
    check_valid_number_steps(I).

check_valid_number_steps(I) -> 
    if 
         I == "f" orelse I == "F" orelse I == "" -> ok;
         true -> try  
                     _ = list_to_integer(I),
                     ok
                  catch 
                        _:_ -> {error, "The user input is not valid."}
                  end
    end.

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
transform(Args=#args{current_file_name=_File, user_inputs=[E,I], search_paths=_SearchPaths})->
    transform(Args,E,I,refac_eval_single,nil).

transform(Args=#args{current_file_name=_File,user_inputs=[I], search_paths=_SearchPaths}, OriginalNode, Pid)->
    transform(Args,OriginalNode,I,refac_eval_compos,Pid).

transform(Args=#args{current_file_name=File, user_inputs=_, search_paths=SearchPaths},E,I,TypeRefac,Pid) ->
	    Node = ?TO_AST(E),
	    Match = ?MATCH(?T("M@:F@(Args@@)"),Node),
            {ok,Scope} = api_refac:get_ast(File),
	    case Match of
	       true ->  ModuleName = ?PP(M@),
		        CollectFile = core_funApp:getCollectFile(ModuleName,File,SearchPaths),
		        case CollectFile of 
					{ok,DefinitionsFile} -> 
				 				     DefinitionsModule = list_to_atom(ModuleName),
								     Info = core_funApp:collect(DefinitionsFile,false),
								     checkNumberSteps(Args,E,I,{{DefinitionsModule,Info},Scope},TypeRefac,Pid);
		                        _ -> {error,"Definitions file does not exist."}
		 
		        end;
	       _ ->     
                        case TypeRefac of
                              funApp -> {error,"Invalid expression."};
                              _ -> %%Info = core_funApp:collect(File,false),
                                   checkNumberSteps(Args,E,I,{{File,empty},Scope},TypeRefac,Pid)
                        end
	    end.   

checkNumberSteps(_Args=#args{current_file_name=_,user_inputs=_, search_paths=SearchPaths},E,I,{{File,Info},Scope},TypeRefac,Pid) -> 
   if I == "" -> NSteps = "1";
      true -> NSteps = I
   end,
   TypedF = NSteps == "f" orelse NSteps == "F",
   case TypeRefac of 
             refac_eval_single ->  
		     eval:start_evaluation(SearchPaths,{File,Scope},Info,"",fun eval_all:rules/2,E,NSteps, TypedF);
             refac_eval_compos -> 
		     eval:start_evaluation(SearchPaths,{File,Scope},Info,Pid,fun eval_all:rules/2, "",NSteps, TypedF);
             eval_funApp -> 
		      eval:start_evaluation(SearchPaths,{File,Scope},Info,"",fun eval_funApp:rules/2,E,NSteps, TypedF);
             eval_arit_calc ->
                      eval:start_evaluation(SearchPaths,{File,Scope},Info,"",fun core_arit_calc:rules/2, E, NSteps, TypedF);
             eval_arit_simpl ->
                      eval:start_evaluation(SearchPaths,{File,Scope},Info,"",fun core_arit_simpl:rules/2,E, NSteps, TypedF);
             eval_boolean_operators ->
                      eval:start_evaluation(SearchPaths,{File,Scope},Info,"",fun core_boolean_operators:rules/2,E, NSteps, TypedF);
             eval_arithmetics ->
                      eval:start_evaluation(SearchPaths,{File,Scope},Info,"",fun core_arithmetics:rules/2,E, NSteps, TypedF);
             eval_if ->
                      eval:start_evaluation(SearchPaths,{File,Scope},Info,"",fun core_if:rules/2,E, NSteps, TypedF);
             eval_lists -> 
                      eval:start_evaluation(SearchPaths,{File,Scope},Info,"",fun core_lists_concat:rules/2,E,NSteps,TypedF);
             _ -> {error, "Invalid refactoring type."}
    end.
    
       
%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function calls all the rules from the other refactorings.
%%--------------------------------------------------------------------
rules({File,Scope},Info) ->
     
     core_arithmetics:rules(empty,empty) ++
     core_lists_concat:rules(empty,empty) ++
     eval_funApp:rules({File,Scope},Info) ++
     core_boolean_operators:rules(nil,nil) ++
     core_if:rules({File,Scope},Info)      
   .    


%%Composite Refactoring
eval_all(FileName, OriginalNode, Pid, Input, SearchPaths,Editor, TabWidth) ->
            Args=#args{current_file_name=FileName,
                       user_inputs=[Input],
                       search_paths=SearchPaths,
                       tabwidth=TabWidth},
            case check_pre_cond(Args) of
                ok ->
                    {ok, Res}=transform(Args,OriginalNode,Pid),
                    wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"");
                {error, Reason} ->
                    {error, Reason}
    end.
    

