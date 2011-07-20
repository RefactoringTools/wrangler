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
-module(feedback_wrangler).

%%-behaviour(feedback).

-export([do/2]).

-export([test/0, long_functions/2, calls_to_specific_function/4]).

%% Since I don't have the .hrl file, I define the record types here.
-record(fTFeedbackMessage, {feedback_type  :: atom(),
                            message       :: string(),
                            options       :: [tuple()],
                            custom        :: [any()]
                           }).

-record(fTFeedback, {status = true  ::boolean(),
                     messages =[]   ::[#fTFeedbackMessage{}]
                    }).

%% the callback function.
-spec do(SearchPaths::[string()], Options::[tuple()])->
                {ok, #fTFeedback{}}. 
do(SearchPaths, Options) ->
    api_wrangler:start(),
    Messages = do_code_inspection(SearchPaths, Options),
    api_wrangler:stop(),
    {ok, #fTFeedback{status =true, messages=Messages}}.


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
            refac_io:format("Wrangler failed to run '~p':\n{~p, \n~s}\n",
                            [Fun,Reason, StackTrace]),
            [];
        {ok, Res} ->
            [create_one_feedback_message(R)||R<-Res]
    end.

try_to_apply(Mod, Fun, Args) -> 
    try apply(Mod, Fun, Args)
    catch
        throw:Error -> 
            Error;    
        _E1:E2->
            Reason=lists:flatten(io_lib:format(
                                   "~p", 
                                   [erlang:get_stacktrace()])),
            {error, {E2,Reason}}
    end.

create_one_feedback_message({Msg, Options}) ->
    #fTFeedbackMessage{feedback_type=wrangler,
                       message = Msg,
                       options= Options}.

%% In this test, I try to detect two code code smells. One is 
%% function definitions with 40 or more lines of code, and the
%% other is the use of outdated function lists:keysearch/3.
test()->
    SearchPaths=["c:/cygwin/home/hl/test"],
    Options=[{long_functions, ["40"]},
             {calls_to_specific_function,   ["lists", "keysearch", "3"]}],
    do(SearchPaths, Options).

long_functions(Lines, SearchPaths) ->
    Lines1=list_to_integer(Lines),
    {ok, LongFuns} = wrangler_code_inspector_lib:long_functions(
                       SearchPaths, Lines1, SearchPaths, 8),
    Res=[begin
             Msg=lists:flatten(
                   io_lib:format("Function ~p:~p/~p has more than ~p "
                                 "lines of code.", [M, F, A, Lines1])),
             {Msg, [{file, File}, {line, Line}]}
         end ||{{M, F, A}, {File, Line}}<-LongFuns],
    {ok, Res}.
    
calls_to_specific_function(M, F, A, SearchPaths) ->
    M1=list_to_atom(M),
    F1=list_to_atom(F),
    A1=list_to_integer(A),
    {ok, Calls} = wrangler_code_inspector_lib:calls_to_specific_function(
                    {M1,F1,A1}, SearchPaths),
    Res=[begin
             Msg=lists:flatten(
                   io_lib:format("Function ~p:~p/~p is called in ~s, line ~p.",
                               [M1,F1,A1, File, Line])),
             {Msg, [{file, File}, {line, Line}]}
         end ||{File,{{Line, _}, _}}<-Calls],
    {ok, Res}.


%% Here is the result returned by running feedback_wrangler:test().
%% {ok,{fTFeedback,true,
%%         [{fTFeedbackMessage,wrangler,
%%              "Function brchcp_vig_calls_SUITE:set_topology_2/1 has more than 40 lines of code.",
%%              [{file,"c:/cygwin/home/hl/test/brchcp_vig_calls_SUITE.erl"},
%%               {line,445}],
%%              undefined},
%%          {fTFeedbackMessage,wrangler,
%%              "Function brchcp_vig_calls_SUITE:set_topology_3/1 has more than 40 lines of code.",
%%              [{file,"c:/cygwin/home/hl/test/brchcp_vig_calls_SUITE.erl"},
%%               {line,516}],
%%              undefined},
%%          {fTFeedbackMessage,wrangler,
%%              "Function brchcp_vig_calls_SUITE:set_topology_4/1 has more than 40 lines of code.",
%%              [{file,"c:/cygwin/home/hl/test/brchcp_vig_calls_SUITE.erl"},
%%               {line,624}],
%%              undefined},
%%          {fTFeedbackMessage,wrangler,
%%              "Function ct_expand:xform/4 has more than 40 lines of code.",
%%              [{file,"c:/cygwin/home/hl/test/ct_expand.erl"},{line,142}],
%%              undefined},
%%          {fTFeedbackMessage,wrangler,
%%              "Function lists:keysearch/3 is called in c:/cygwin/home/hl/test/ch.erl, line 8.",
%%              [{file,"c:/cygwin/home/hl/test/ch.erl"},{line,8}],
%%              undefined},
%%          {fTFeedbackMessage,wrangler,
%%              "Function lists:keysearch/3 is called in c:/cygwin/home/hl/test/ch.erl, line 49.",
%%              [{file,"c:/cygwin/home/hl/test/ch.erl"},{line,49}],
%%              undefined},
%%          {fTFeedbackMessage,wrangler,
%%              "Function lists:keysearch/3 is called in c:/cygwin/home/hl/test/ch.erl, line 56.",
%%              [{file,"c:/cygwin/home/hl/test/ch.erl"},{line,56}],
%%              undefined},
%%          {fTFeedbackMessage,wrangler,
%%              "Function lists:keysearch/3 is called in c:/cygwin/home/hl/test/ch.erl, line 64.",
%%              [{file,"c:/cygwin/home/hl/test/ch.erl"},{line,64}],
%%              undefined},
%%          {fTFeedbackMessage,wrangler,
%%              "Function lists:keysearch/3 is called in c:/cygwin/home/hl/test/ch.erl, line 90.",
%%              [{file,"c:/cygwin/home/hl/test/ch.erl"},{line,90}],
%%              undefined}]}}
                
