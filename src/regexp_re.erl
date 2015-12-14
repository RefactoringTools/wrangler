%% @private
%% @hidden
-module(regexp_re).

-export([old_api_module_name/0]).

-export([match/2, 
         first_match/2, 
         matches/2,
         sub/3, 
         gsub/3,
         split/2,
         parse/1
        ]).

old_api_module_name() ->
    regexp.

match(String, RegExp) ->
    try re:run(String, RegExp, [global]) of
         {match, Match} ->
            {Start0, Len} = lists:last(
                              lists:ukeysort(
                                2, lists:append(Match))),
            Start = Start0+1,
            {match, Start, Len};
        nomatch -> nomatch
    catch
        error:_->
            {error, Error}=re:compile(RegExp),
            {error, Error}
    end.

first_match(String, RegExp) ->
    try re:run(String, RegExp) of
        {match, [{Start0, Len}]} ->
            Start = Start0 +1,
            {match, Start, Len};
        nomatch -> nomatch
    catch
        error:_->
            re:compile(RegExp)
    end.

matches(String, RegExp) ->
    try re:run(String, RegExp, [global]) of
        {match, Res} ->
            Matches=[{Start+1, Len}||
                        {Start, Len}<-lists:append(Res)],
            {match, Matches};
        nomatch -> 
            {match, []}
    catch
        error:_->
            re:compile(RegExp)
    end.


sub(String, RegExp, New) ->
    try re:replace(String, RegExp, New, [{return, list}]) of 
        NewString  ->
            Count = case re:run(String,RegExp) of 
                        nomatch -> 0;
                        _ -> 1
                    end,
            {ok, NewString, Count}
    catch
        error:_ ->
            re:compile(RegExp)
    end.


gsub(String, RegExp, New) ->
    try re:replace(String, RegExp, New, [{return, list}]) of 
        NewString  ->
            Count = case re:run(String,RegExp, [global]) of 
                        nomatch -> 0;
                        {match, Matches}-> 
                            length(lists:append(Matches))
                    end,
            {ok, NewString, Count}
    catch
        error:_ ->
            re:compile(RegExp)
    end.


split(String, RegExp) ->
    try re:split(String, RegExp, [{return, list}]) of 
        SplitList ->
            {ok, SplitList}
    catch
        error:_ ->
            re:compile(RegExp)
    end.
 
parse(RegExp) ->
    re:compile(RegExp).

 
%% inspect() -> 
%%     ["regexp:split(String, "")",
%%      "regexp:split(String, " ")"].

