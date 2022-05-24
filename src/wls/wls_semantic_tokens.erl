-module(wls_semantic_tokens).
-export([token_types/0]).
-export([token_modifiers/0]).
-export([semantic_tokens/1]).


-spec token_types() -> [binary()].
token_types() -> [<<"enumMember">>]. %% enumMember`s color is blue
-spec token_modifiers() -> [binary()].
token_modifiers() -> [].

-spec semantic_tokens(els_core:uri()) -> [integer()].
semantic_tokens(Uri) ->
    Path = wls_utils:path(Uri),
    case wls_server:get_state(Path) of
        {under_refactoring, #{refactor := _Refactor, regions := Regions}} ->
            calculate_tokens(Regions, 0);
        _ ->
            []
    end.

%% Output format: [deltaLine, deltaStartChar, length, tokenTypeIndex, tokenModifierBitFlag, ...]
-spec calculate_tokens([{integer(), integer(), integer(), integer()}], number()) -> [integer()].
calculate_tokens([], _) -> [];
calculate_tokens([#{range := {SLine, SCol, SLine, ECol}} | Regions], PrevLines) -> 
    [SLine-1-PrevLines, SCol-1, ECol-SCol+1, 0, 0] ++ calculate_tokens(Regions, SLine-1);
calculate_tokens([#{range := {SLine, SCol, ELine, ECol}} | Regions], PrevLines) -> 
    [SLine-1-PrevLines, SCol-1, 100, 0, 0] %% First line of multiline expression
    ++ calculate_multiline_token(SLine + 1, ELine, ECol) %% Remaining lines of multiline expression
    ++ calculate_tokens(Regions, ELine-1). %% Remaining regions

calculate_multiline_token(SLine, SLine, ECol) ->
    [1, 0, ECol, 0, 0];
calculate_multiline_token(SLine, ELine, ECol) ->
    [1, 0, 100, 0, 0]
    ++ calculate_multiline_token(SLine + 1, ELine, ECol).