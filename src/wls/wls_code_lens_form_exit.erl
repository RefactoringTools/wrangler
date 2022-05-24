-module(wls_code_lens_form_exit).

-behaviour(wls_code_lens).

-export([command/3
        ,pois/1
        ]).

-spec command(els_dt_document:item(), els_core:poi(), els_code_lens:state()) ->
                 els_command:command().
command(Document, _POI, _State) ->
  Title = title(),
  #{uri := Uri} = Document,
  Argument = #{ <<"uri">>  => Uri},
  els_command:make_command(Title, <<"wrangler-form-exit">>, [Argument]).

-spec pois(els_dt_document:item()) -> [els_core:poi()].
pois(Document) ->
  #{uri := Uri} = Document,
  Path = wls_utils:path(Uri),
  case wls_server:get_state(Path) of
    {under_refactoring, #{regions := Regions}} ->
      getPois(Regions);
    _ -> []
  end.

-spec title() -> binary().
title() ->
  <<"Exit">>.

getPois([]) -> [els_poi:new(#{from => {1, 1}, to => {1, 2}}, dummy, dummy, dummy)];
getPois([#{range := {SLine, SCol, ELine, ECol}} | Regions]) ->
  [els_poi:new(#{from => {SLine, SCol}, to => {ELine, ECol}}, dummy, dummy, dummy)]
  ++ getPois(Regions).