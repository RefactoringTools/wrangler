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
	 transform/1,transform/4,transform/6,rules/2, eval_all/6]).

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

transform(Args=#args{current_file_name=_DefFiles, user_inputs=[E,I], search_paths=_SearchPaths}) ->
    transform(Args,E,I,refac_eval_single,nil,0).

transform(Args=#args{current_file_name=_DefFiles,user_inputs=[I], search_paths=_SearchPaths}, OriginalNode, Pid,Timeout)->
    transform(Args,OriginalNode,I,refac_eval_compos,Pid,Timeout).

transform(Args=#args{current_file_name=DefFiles},E,I,TypeRefac,Pid,Timeout) ->
	    Node = ?TO_AST(E),
	    Match = ?MATCH(?T("M@:F@(Args@@)"),Node),
            Scope = nil,
	    case Match of
	       true ->  
			Info = core_funApp:collectFromDefsList(DefFiles),
			checkNumberSteps(Args,I,{{empty,Info},empty},TypeRefac,Pid,Timeout);
		               
	       _ ->     
                        case TypeRefac of
                              funApp -> {error,"Invalid expression."};
                              _ %%Info = core_funApp:collect(File),
                                ->
                                   checkNumberSteps(Args,I,{{DefFiles,empty},Scope},TypeRefac,Pid,Timeout)
                        end
	    end.   

checkNumberSteps(_Args=#args{current_file_name=_, user_inputs=_, search_paths=_SearchPaths},I,{{DefFiles,Info},Scope},TypeRefac,Pid,Timeout) ->
   if I == "" -> NSteps = "1";
      true -> NSteps = I
   end,
   TypedF = NSteps == "f" orelse NSteps == "F",
   case TypeRefac of 
             refac_eval_compos -> 
		     eval:start_evaluation({DefFiles,Scope},Info,Pid,fun eval_all:rules/2,"",NSteps,TypedF,Timeout);
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
rules(RulesArgs,{Info,RemoveInfo}) ->
     eval_inline_variable:rules(RulesArgs,Info) ++
     core_unreferenced_assign:rules(empty,RemoveInfo) ++
     core_rem_begin_end:rules(empty,empty) ++
     core_arithmetics:rules(empty,empty) ++
     core_lists_concat:rules(empty,empty) ++
     eval_funApp:rules(RulesArgs,Info) ++
     core_boolean_operators:rules(nil,nil) ++
     core_if:rules({empty,[],empty},Info) ++
     core_case:rules({empty,[],empty},Info).    


eval_all(DefFiles, OriginalNode, Pid, Input, SearchPaths, Timeout) ->
            Args=#args{current_file_name=DefFiles,
                       user_inputs=[Input],
                       search_paths=SearchPaths},
            case check_pre_cond(Args) of
                ok ->
                    transform(Args,OriginalNode,Pid,Timeout);
                {error, Reason} ->
                    {error, Reason}
    end.
    

