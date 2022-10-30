-module(wls_code_action_inline_var).

-behaviour(wls_code_actions).

-export([ title/0
        , id/0
        , command_args/3
        , precondition/2
        , execute_command/1
        ]).

-export([ calculate_regions/2
        , recalculate_regions/2]).


-include_lib("wls_core.hrl").
-include_lib("kernel/include/logger.hrl").

-spec title() -> binary().
title() -> <<"Inline variable">>.

-spec id() -> action_id().
id() -> <<"inline-var">>.

-spec command_args(els_core:uri(), els_core:range(), state()) -> map().
command_args(Uri, Range, _State) ->
  #{ 'range' => Range
   , 'uri' => Uri
  }.

-spec precondition(els_core:uri(), els_core:range()) -> boolean().
precondition(Uri, Range) -> 
  Path = wls_utils:path(Uri),
  {StartPos, _EndPos} = wls_utils:range(Range),
  refac_inline_var:is_available_at(Path, StartPos).



%%==============================================================================
%% Calculate regions for the Wrangler Form
%%==============================================================================

calculate_regions(Path, Pos) -> 
  recalculate_regions(Path, Pos).

recalculate_regions(Path, Pos) -> 
  {ok, {AnnAST, _Info}} = wrangler_ast_server:parse_annotate_file(Path, true, wls_utils:search_paths(), wls_utils:tab_width()),
  Form = refac_inline_var:pos_to_form(AnnAST, Pos),
  case api_interface:pos_to_var(Form, Pos) of
    {ok, VarNode} ->
      case refac_inline_var:get_var_define_match_expr(Form, VarNode) of
        {ok, MatchExpr} ->
          Cands = refac_inline_var:search_for_unfold_candidates(Form, MatchExpr, VarNode),
          {ok, make_regions(Cands), Pos};
        _ -> {ok, [], Pos}
      end;
    _ -> % Wrangler deletes the variable declaration after the last inline 
      {ok, [], Pos}
  end.

make_regions([{{SLine, SCol}, {ELine, ECol}}| Rem]) -> 
  [#{range => {SLine, SCol, ELine, ECol}, data => {}}] ++ make_regions(Rem);
make_regions([]) -> [].

%%==============================================================================
%% Execute commands
%% - initiate the refactoring with Wrangler Form
%% - Execute the selected refactoring 
%%==============================================================================

-spec execute_command([any()]) -> [map()].
execute_command([#{ <<"range">> := Range,
                      <<"uri">> := Uri
                   }]) ->
  Path = wls_utils:path(Uri),
  {{Line, Col}, _EndPos} = wls_utils:range(Range),
  case wls_server:start_refactoring(Path, inline_var, {Line, Col}) of
    ok ->  wls_utils:send_info("Select the highlighted inline candidates you want to refactor.");
    {error, Msg} -> wls_utils:send_warning(Msg)
  end,
  [];

execute_command([#{ <<"uri">> := Uri,
                    <<"index">> := Index}]) ->
  Path = wls_utils:path(Uri),
  case wls_server:get_state(Path) of
    {under_refactoring, #{refactor := Refactor, regions := Regions, data := {Line, Col}}} ->
      case Refactor of
        inline_var ->
          try lists:nth(Index, Regions) of
            Region ->
              #{range := {StartLine, StartCol, EndLine, EndCol}} = Region,
              Candidate = {{StartLine, StartCol}, {EndLine, EndCol}},
              try refac_inline_var:inline_var_1(Path, Line, Col, [Candidate],wls_utils:search_paths(), wls_utils:tab_width(), "", wls) of
                  {ok, [{OldPath, _NewPath, Text}]} ->
                    file:write_file(OldPath, Text),
                    wls_server:refresh(OldPath);
                  Err ->
                    wls_utils:send_error("Unknown error occurred. See logs for details1."),
                    ?LOG_INFO("Error while doing inline: ~p", [Err])
              catch
                  _:{error, Message} -> wls_utils:send_warning(Message);
                  _:E -> 
                  wls_utils:send_error("Unknown error occurred. See logs for details2."),
                  ?LOG_INFO("Error while doing inline: ~p", [E])
              end
          catch 
            _:_ -> wls_utils:send_info("Please wait while the form is being updated.")
          end;
        _ -> ?LOG_WARNING("Unknown refactoring: ~p", [Refactor])
      end;
    State -> ?LOG_WARNING("Unknown state: ~p", [State])
  end,
  [].