-module(wls_code_lens_form_refactor).

-behaviour(wls_code_lens).

-export([command/3
         ,pois/1
        ]).

-spec command(els_dt_document:item(), els_core:poi(), els_code_lens:state()) ->
                 els_command:command().
command(Document, POI, _State) ->
  Title = title(),
  #{uri := Uri} = Document,
  #{data := #{counter := Num, refactor := Refactor}} = POI,
  Argument = #{ <<"uri">>  => Uri
              , <<"index">> => Num},
  els_command:make_command(Title, re:replace(atom_to_binary(Refactor), "_", "-", [global, {return, binary}]), [Argument]).

-spec pois(els_dt_document:item()) -> [els_core:poi()].
pois(Document) ->
  #{uri := Uri} = Document,
  Path = wls_utils:path(Uri),
  case wls_server:get_state(Path) of
    {under_refactoring, #{refactor := Refactor, regions := Regions}} ->
      getPois(Refactor, Regions, 1);
    _ -> []
  end.

-spec title() -> binary().
title() ->
  <<"Refactor this instance">>.

getPois(_, [], _ ) -> [];
getPois(Refactor, [#{range := {SLine, SCol, ELine, ECol}} | Regions], Index) ->
  [els_poi:new(
    #{from => {SLine, SCol}, to => {ELine, ECol}},
    dummy,
    dummy,
    #{counter => Index, refactor => Refactor}
  )] 
 ++ getPois(Refactor, Regions, Index + 1).