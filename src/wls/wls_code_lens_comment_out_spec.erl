-module(wls_code_lens_comment_out_spec).

-behaviour(wls_code_lens).

-export([ pois/1
        , command/3
        ]).

command(Document, _POI, _State) ->
    Title = <<"Comment out spec">>,
    CommandId = <<"comment_out_spec">>,
    #{uri := Uri} = Document,
    CommandArgs = [#{ <<"uri">>  => Uri}],
    els_command:make_command(Title, CommandId, CommandArgs).

pois(_Document) ->
%% Return a dummy POI on the first line
[els_poi:new(#{from => {1, 1}, to => {2, 1}}, dummy, dummy)].
