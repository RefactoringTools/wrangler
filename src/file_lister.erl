-module(file_lister).
-export([recursively_list_dir/1,
         recursively_list_dir/2,
         filter_erl_files/1]).


% @type name() = string() | atom() | binary().
-type name() :: string() | atom() | binary().




%% API

% @spec (Dir::name()) -> {ok, [string()]} | {error, atom()}
% @equiv recursively_list_dir(Dir, false)
%
% @doc Lists all the files in a directory and recursively in all its
% sub directories. Returns {ok, Paths} if successful. Otherwise,
% it returns {error, Reason}. Paths is a list of Paths of all the
% files and directories in the input directory's subtree. The paths are not
% sorted in any order.

-spec recursively_list_dir(Dir::name()) ->
        {ok, [string()]} | {error, atom()}.

recursively_list_dir(Dir) ->
    recursively_list_dir(Dir, false). % default value of FilesOnly is false




% @spec (Dir::name(), FilesOnly::boolean()) -> {ok, [string()]} |
%                                                   {error, atom()}
%
% @doc Lists all the files in a directory and recursively in all its
% sub directories. Returns {ok, Paths} if successful. Otherwise,
% it returns {error, Reason}. If FilesOnly is false, Paths is a list of paths
% of all the files <b>and directories</b> in the input directory's subtree.
% If FilesOnly is true, Paths is a list of paths of only the files in the
% input directory's subtree. The paths are not sorted in any order.

-spec recursively_list_dir(Dir::name(), FilesOnly::boolean()) ->
        {ok, [string()]} | {error, atom()}.

recursively_list_dir(Dir, FilesOnly) ->
    case filelib:is_file(Dir) of
        true ->
            case filelib:is_dir(Dir) of
                true -> {ok, recursively_list_dir([Dir], FilesOnly, [])};
                % if we pass a file, the file's directory will be the root
                false -> {ok, recursively_list_dir([filename:dirname(Dir)], FilesOnly, [])}
            end;
        false -> {error, enoent}
    end.




%% Internal

recursively_list_dir([], _FilesOnly, Acc) -> Acc;
recursively_list_dir([Path|Paths], FilesOnly, Acc) ->
    recursively_list_dir(Paths, FilesOnly,
        case filelib:is_dir(Path) of
            false -> [Path | Acc];
            true ->
                {ok, Listing} = file:list_dir(Path),
                SubPaths = [filename:join(Path, Name) || Name <- Listing],
                recursively_list_dir(SubPaths, FilesOnly,
                    case FilesOnly of
                        true -> Acc;
                        false -> [Path | Acc]
                    end)
        end).


filter_erl_files({ok, FileList}) ->
    {ok, lists:filter(fun (X) -> filename:extension(X) == ".erl" end, FileList)};
filter_erl_files({error, X}) ->
    {error, X}.

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

non_existing_file_returns_error_test() ->
    ?assertEqual({error, enoent},
                 recursively_list_dir("UnUSuAalfIlEnaMe")),
    ok.

non_directory_input_returns_error_test() ->
    cleanup(),
    file:write_file("f1.test", <<"temp test file">>),
    ?assertEqual({error, enotdir},
                 recursively_list_dir("f1.test")),
    cleanup(),
    ok.

simple_test() ->
    cleanup(),
    filelib:ensure_dir("a/b/c/"),
    ?assertEqual({ok, ["a/b/c", "a/b", "a"]},
                 recursively_list_dir("a")),
    file:write_file("a/b/f.test", <<"temp test file">>),
    ?assertEqual({ok, ["a/b/c","a/b/f.test","a/b","a"]},
                 recursively_list_dir("a")),
    cleanup(),
    ok.

filesonly_test() ->
    cleanup(),
    filelib:ensure_dir("a/b/f.test"),
    file:write_file("a/b/f.test", <<"hello">>),
    ?assertEqual({ok, ["a/b/f.test"]},
                 recursively_list_dir("a", true)),
    cleanup(),
    ok.

cleanup() ->
    file:delete("f1.test"),
    file:delete("a/b/f.test"),
    file:del_dir("a/b/c"),
    file:del_dir("a/b"),
    file:del_dir("a"),
    ok.

-endif.
