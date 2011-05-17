
%%@doc This module shows how to write refactorings 
%% use the Wrangler API. 

%% The refactoring implemented in this module adds an 
%% import attribute which explicitly imports the 
%% functions which are defined in a user specified 
%% module, and used in the current module by remote 
%% function calls.
-module(refac_intro_import).

-behaviour(gen_refac).

%% export of callback function.
-export([input_pars/0, select_focus/1, 
         pre_cond_check/1, selective/0,
         transform/1]).

-include("../include/gen_refac.hrl").

%% Ask the user which module to import. 
-spec (input_pars/0::() -> [string()]).                           
input_pars()->
    ["Module name:"].

%% no focus selection is needed.
-spec (select_focus/1::(#args{}) -> {ok, syntaxTree()}|{ok, none}).  
select_focus(_Args) ->
    {ok, none}.
    
%% Pre-condition checking. 
-spec (pre_cond_check/1::(#args{}) -> ok | {error, term()}).  
pre_cond_check(Args=#args{user_inputs=[ModuleName]}) ->
    case collect_uses(Args) of 
        [] ->
            Msg =io_lib:format(
                   "There is no remote calls to "
                   "functions from module '~s'.",
                   [ModuleName]),
            {error, lists:flatten(Msg)};
        _-> ok
    end.

selective() ->
    false.

%%Do the actual program transformation here.
-spec (transform/1::(#args{}) -> 
                          {ok, [{filename(), filename(), syntaxTree()}]}
                              | {error, term()}).    
transform(Args=#args{current_file_name=File,
                     user_inputs=[ModuleName]}) ->
    %% collect the functions that are defined 
    %% in ModuleNaem, and are remotely called
    %% in the current module.
    FAs=lists:usort(collect_uses(Args)),
    FAs1 = refac_api: imported_funs(File, ModuleName),
    %% Functions that need to be imported.
    FunsToImport=FAs--FAs1,
    {ok,AST} = refac_api:get_ast(File),
    case FunsToImport of 
        [] ->
            [NewAST]=?FULL_TD([rule(Args)], [AST]),
            {ok, [{{File, File}, NewAST}]};
        _ ->
            Import=make_import_attr(ModuleName, FunsToImport),
            AST1=refac_api:insert_an_attr(AST,Import),
            [NewAST]=?FULL_TD([rule(Args)], [AST1]),
            {ok, [{{File, File}, NewAST}]}
    end.

collect_uses(_Args=#args{current_file_name=File,
                         user_inputs=[ModuleName]}) ->
    ?COLLECT("M@:F@(Args@@)", ?SPLICE(M@)==ModuleName, 
             {list_to_atom(?SPLICE(F@)), length(Args@@)},
             [File]).

rule(_Args=#args{user_inputs=[ModuleName]}) ->
    ?RULE("M@:F@(Args@@)",?QUOTE("F@(Args@@)"),
          ?SPLICE(M@)==ModuleName).

make_import_attr(ModuleName, FAs) ->
    ?QUOTE("-import("++ModuleName++","++
               format_fa_list(FAs)++").").

format_fa_list([]) ->
    "[]";
format_fa_list(FAs) ->
    "["++lists:flatten(format_fas(FAs))++"]".

format_fas([]) ->
    "";
format_fas([{F,A}]) ->
    io_lib:format("~p/~p", [F,A]);
format_fas([{F,A}|T]) ->
    io_lib:format("~p/~p,", [F,A]) ++
        format_fas(T).
