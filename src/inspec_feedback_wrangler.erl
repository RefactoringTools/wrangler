%% =====================================================================
%%      Wrangler's feedback interface module to Erlang E-learning
%% =====================================================================
%%
%% @author Huiqing Li
%%   [http://www.cs.kent.ac.uk/projects/wrangler]
%%
%%
%% @doc Wrangler's feedback interface module to Erlang E-learning

%%@private
-module(inspec_feedback_wrangler).

-export([do_code_inspection/2]).

-export([long_functions/2, 
         calls_to_specific_function/2,
         test/1]).

-include("../include/wrangler.hrl").

-type (message()::string()).
-type (tag() :: atom()).
-spec do_code_inspection([dir()|filename()], [{atom(), [any()]}]) ->
             [{message(), [{tag, list()}]}].
do_code_inspection(SearchPaths, Options) ->
    do_code_inspection(SearchPaths, Options, []).

do_code_inspection(_SearchPaths, [], Acc) ->
    lists:append(lists:reverse(Acc));
do_code_inspection(SearchPaths, [{FunName, Args}|Options], Acc) ->
    Res= try_inspector(?MODULE, FunName, 
                       Args++[SearchPaths]),
    do_code_inspection(SearchPaths, Options, [Res|Acc]).

try_inspector(Mod, Fun, Args) -> 
    case try_to_apply(Mod, Fun, Args) of
        {error, {Reason, StackTrace}} ->
            wrangler_io:format("Wrangler failed to run '~p':\n{~p, \n~s}\n",
                               [Fun, Reason, StackTrace]),
            [];
        {ok, Res} ->
            Res
    end.

try_to_apply(Mod, Fun, Args) -> 
    try apply(Mod, Fun, Args)
    catch
        throw:Error -> 
            Error;    
        _E1:E2->
            Reason=lists:flatten(
                     io_lib:format("~p",[erlang:get_stacktrace()])),
            {error, {E2,Reason}}
    end.

-spec long_functions(integer(), [dir()|filename()]) ->
                            {ok, [{message(), [{tag(), list()}]}]}.
long_functions(Lines, SearchPaths) ->
    {ok, LongFuns} = wrangler_code_inspector_lib:long_functions(
                       SearchPaths, Lines, SearchPaths, 8),
    Res=[begin
             Msg=lists:flatten(
                   io_lib:format("Function ~p:~p/~p has more than ~p "
                                 "lines of code.", [M, F, A, Lines])),
             {Msg, [{file, File}, 
                    {line, integer_to_list(Line)}]}
         end ||{{M, F, A}, {File, Line}}<-LongFuns],
    {ok, Res}.

-spec calls_to_specific_function(mfa(), [dir()|filename()]) ->    
                                        {ok, [{message(), [{tag(), list()}]}]}.
calls_to_specific_function({M, F, A}, SearchPaths) ->
    {ok, Calls} = wrangler_code_inspector_lib:calls_to_specific_function(
                    {M,F,A}, SearchPaths),
    Res=[begin
             Msg=lists:flatten(
                   io_lib:format("Oops. You've called the blacklisted function ~p:~p/~p at line ~p.",
                                 [M, F, A, Line])),
             {Msg, [{file, File}, 
                    {line, integer_to_list(Line)}]}
         end ||{File,{{Line, _}, _}}<-Calls],
    {ok, Res}.


%% In this test, I try to detect two code code smells. One is 
%% function definitions with 40 or more lines of code, and the
%% other is the use of outdated function lists:keysearch/3.
test(SearchPaths)->
    Options=[{long_functions, [40]},
             {calls_to_specific_function, [{lists, keysearch, 3}]}],
    do_code_inspection(SearchPaths, Options).


