%%@hidden
%%@private
-module(refac_batch_inline_fun).

-export([composite_refac/1, input_par_prompts/0]).

-behaviour(gen_composite_refac).

-include("../include/wrangler.hrl").


input_par_prompts() ->
    ["Name of function to be inlined: ", "Arity of function to be inline: "].

composite_refac(_Args=#args{current_file_name=File, user_inputs=[F,A],
                            search_paths=SearchPaths}) ->
    MFA ={list_to_atom(filename:basename(File, ".erl")),
          list_to_atom(F), list_to_integer(A)},
    ?if_then(is_not_recursive(File,MFA),
             ?while(begin Apps=collect_apps(File, MFA),
                          {Apps/=[], Apps}
                    end,
                    fun(Apps)->
                            ?refac_(unfold_fun_app, [File,hd(Apps), SearchPaths])
                    end)).


collect_apps(File, MFA) ->
    ?FULL_TD_TU([?COLLECT(?T("F@(Args@@)"),
                          element(1, api_refac:start_end_loc(_This@)),
                          api_refac:fun_define_info(F@)==MFA)],
                [File]).
   
is_not_recursive(File,MFA)->
    FunDef=api_refac:mfa_to_fun_def(MFA, File),
    {M,F,A} = MFA,
    ?FULL_TD_TU([?COLLECT_LOC(?FUN_APPLY(M,F,A), true)], FunDef)==[].
