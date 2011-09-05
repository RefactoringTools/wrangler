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
         test/1,
	 classify_pattern_match/1,
	 collect_function_apps/2,
	 collect_function_apps2/2]).

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

%% Give the type (according to syntax_tools) of the patterns
%% in a function definition.

classify_pattern_match(File) ->
    Classify = ?FULL_TD_TU([?COLLECT(?T("f@(Args@@@) when _Guard@@@ -> _Body@@@."), 
				     {api_refac:fun_define_info(f@),
				      api_refac:start_end_loc(_This@),
				      overall_classify([pattern_type(Pat) || Pat <- lists:flatten(Args@@@)])},
				     true
				    )],
			   [File]),
    MsgFun = fun (mixed)    -> " patterns made up entirely of variables or of literals.";
		 (variable) -> " mixed patterns or patterns of literals only.";
		 (literal)  -> " mixed patterns or patterns of variables only."
	  end,
    Res=[begin
             Msg=lists:flatten(
                   io_lib:format("This function could be defined differently: try it with~s~n", [MsgFun(Kind)])),
             {Msg, [{file, File}, 
                    {line_from, integer_to_list(LineFrom)},
		    {line_to, integer_to_list(LineTo)}]}
         end ||{_, {{LineFrom,LineTo},{_,_}}, Kind}<-Classify, Kind /= none],
    {ok, Res}.

-spec pattern_type(syntaxTree()) -> variable | literal | mixed.

pattern_type(Pat) ->
    case wrangler_syntax:is_literal(Pat) of
	true -> literal;
	false -> case api_refac:type(Pat) of
		     variable -> variable;
		     _ -> mixed
		 end
    end.
    
-spec overall_classify([atom()]) -> atom().

overall_classify([]) ->
     none;
overall_classify([X|Xs]) ->
    overall_classify(X,Xs).

-spec overall_classify(atom(),[atom()]) -> atom().

overall_classify(X,[]) ->
     X;
overall_classify(X,[Y|Ys]) ->
    case X==Y of
	true ->
	    overall_classify(X,Ys);
	false ->
	     mixed
    end.
    


%% Collect all the function applications within a function definition.
%% Returns {erlang,apply,3} for uses of apply.

collect_function_apps({M, F, A}, SearchPaths) ->
    ?FULL_TD_TU([?COLLECT(?T("f@(Args@@) when _Guard@@ -> Body@@;"), 
                          collect_apps_within(_This@),
			  {M, F, A} == api_refac:fun_define_info(f@))
			 ],
                [SearchPaths]).

collect_apps_within(Node) ->
    ?FULL_TD_TU([?COLLECT(?T("F@(Argss@@)"),
			  api_refac:fun_define_info(F@),
			  true)
		],
		Node).

collect_function_apps2({M, F, A}, SearchPaths) ->
    ?FULL_TD_TU([?COLLECT(?T("f@(Args@@) when _Guard@@ -> Body@@;"), 
                          collect_apps_within2(_This@),
			  {M, F, A} == api_refac:fun_define_info(f@))
			 ],
                [SearchPaths]).

collect_apps_within2(Node) ->
    ?FULL_TD_TU([?COLLECT(?T("F@(Argss@@)"),
			  ?SPLICE(api_refac:get_app_mod(_This@)),
			  true)
		],
		Node).




%% In this test, I try to detect two code code smells. One is 
%% function definitions with 40 or more lines of code, and the
%% other is the use of outdated function lists:keysearch/3.
test(SearchPaths)->
    Options=[{long_functions, [40]},
             {calls_to_specific_function, [{lists, keysearch, 3}]}],
    do_code_inspection(SearchPaths, Options).


