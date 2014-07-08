-module(evaluator).

-export([composite_refac/1, input_par_prompts/0, select_focus/1]).
-include_lib("wrangler/include/wrangler.hrl").

-behaviour(gen_composite_refac).

input_par_prompts() ->
    ["Type the expression to be evaluated: "].

select_focus(_Args=#args{}) -> {ok,none}.

%%--------------------------------------------------------------------
%% @doc
%% This function applies the evaluation iteratively, i.e., every time that
%% the evaluation is done the user can repeat it. It calls the four evaluators:
%% function calls, arithmetic expressions, arithmetic simplifications and also
%% the combination of the last three.
%% @end
%%--------------------------------------------------------------------

composite_refac(_Args=#args{
                  current_file_name = File,
                  user_inputs = [E],
                  search_paths=SearchPaths}) ->
    ResultFile = SearchPaths ++ "/results.txt",
    file:delete(ResultFile),
    {ok,_} = file:open(ResultFile, [append]),
    Pid = spawn(eval, keep_temp_info, [0,[?TO_AST(E)]]),
    Term = [ ?repeat_interactive(
               ?refac_(eval_all,
                       [File,
                        E,
                        Pid,
                        {user_input, fun() ->"Type N to execute N steps or 'f' to execute all steps: " end},
                        SearchPaths]
                      )
                )
          ],
          ?atomic(Term).

