-module(wls_highlight).
-export([get_highlights/2]).

-define(document_highlight_kind_write, 3).

-include("wls_core.hrl").

-spec get_highlights(uri(), _) -> 'null' | [map()].
get_highlights(Uri, _Pos) ->
    Path = wls_utils:path(Uri),
    case wls_server:get_state(Path) of
        {under_refactoring, #{refactor := _Refactor, regions := Regions}} ->
            calculate_highlights(Regions);
        _ ->
            null
    end.


calculate_highlights([]) -> [];
calculate_highlights([#{range := {SLine, SCol, ELine, ECol}} | Regions]) -> 
    document_highlight({SLine, SCol}, {ELine, ECol}) ++ calculate_highlights(Regions).

document_highlight({StartLine, StartCol}, {EndLine, EndCol}) ->
  [#{ range => 
        #{ start => #{line => StartLine - 1, character => StartCol - 1}
         , 'end' => #{line => EndLine - 1,   character => EndCol}
        },
     kind => ?document_highlight_kind_write %% highlighted with blue background
   }].