%% @private
-module(eval_rem_begin_end).

%% Include files
-include("wrangler.hrl").
-export([collector/1]).

collector(Scope) ->
    ?FULL_TD_TU([collect()],Scope).

collect() ->
    ?COLLECT(
       ?T("begin Args@@ end"),
       true,
       true
   ).
