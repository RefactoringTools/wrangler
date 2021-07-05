-module(gen_refac_2_utils).

-export([generate_unique_vars/1,
         transform_extend1/1, transform_extend2/1,
         transform_simplify1/2, transform_simplify2/2,
         transform_exportlist/2,
         read_orig_functions/1, check_for_difference/2,
         create_backups/1, restore_backups/1, clean_backups/1]).

-include("wrangler.hrl").

% utility functions
generate_unique_vars(N) ->
    generate_unique_vars(N, []).
generate_unique_vars(0, L) ->
    L;
generate_unique_vars(N, L) ->
    Cur = list_to_atom("A" ++ integer_to_list(N)),
    generate_unique_vars(N - 1, [Cur] ++ L).

% These transformations need to be called in the context of a gen_refac_2 behavior

% in CurrentFile
transform_extend1(_Args=#args{current_file_name=File,
                 user_inputs=UserInputs}) ->
     M0 = lists:nth(1, UserInputs),
     F0 = lists:nth(2, UserInputs),
     A0 = lists:nth(3, UserInputs),

     M = list_to_atom(M0),
     F = list_to_atom(F0),
     A = list_to_integer(A0),
    ?FULL_TD_TP([rule_extend({M,F,A})],
                [File]).

% in client_files
transform_extend2(_Args=#args{current_file_name=File,
                 search_paths=SearchPaths,
                 user_inputs=UserInputs}) ->
     M0 = lists:nth(1, UserInputs),
     F0 = lists:nth(2, UserInputs),
     A0 = lists:nth(3, UserInputs),

     M = list_to_atom(M0),
     F = list_to_atom(F0),
     A = list_to_integer(A0),
    ?FULL_TD_TP([rule_extend({M,F,A})],
                api_refac:client_files(File, SearchPaths)).

% in CurrentFile
transform_simplify1({NewF,NewA}, _Args=#args{current_file_name=File,
                 user_inputs=UserInputs}) ->
    M = list_to_atom(lists:nth(1, UserInputs)),
    ?FULL_TD_TP([rule_simplify({M,NewF,NewA})],
                [File]).

% in client_files
transform_simplify2({NewF,NewA}, _Args=#args{current_file_name=File,
                 search_paths=SearchPaths,
                 user_inputs=UserInputs}) ->
    M = list_to_atom(lists:nth(1, UserInputs)),
    ?FULL_TD_TP([rule_simplify({M, NewF, NewA})],
                api_refac:client_files(File, SearchPaths)).


transform_exportlist(File, {F, A}) ->
    case collect_exports(File) of 
        [] ->
            % it was exported, but then deleted during transformation, and no other F/A was exported
            {ok,AST} = api_refac:get_ast(File),
            Export=make_export_attr({F,A}),
            NewAST=api_refac:insert_an_attr(AST,Export),
            {ok, [{{File, File}, NewAST}]};
        Exports ->
            Export = lists:last(Exports),
            ?STOP_TD_TP([rule_handle_export_list(Export, {F, A})], [File])
    end.

%---------------------------------------------------------------------
rule_extend({M, F, A}) ->
    ?RULE(?T("fun M@:f@/A@"),
          begin
              % Vars@@ = lists:map(fun(X) -> new_empty_expr(X,variable) end, generate_unique_vars(A)),
              Vars@@ = lists:map(fun wrangler_syntax:variable/1, gen_refac_2_utils:generate_unique_vars(A)),
              ?TO_AST("fun(Vars@@) -> M@:f@(Vars@@) end",
                      % f@(NewVars) would create mult(X,Y,0,0) ->
                      % it'd apply rule3, so we need only to call f with orig args
                      wrangler_syntax:get_pos(_This@))
          end,
          api_refac:fun_define_info(f@) == {M, F, A}).

% beta reduction, substitution - lookup in source code!
% this version only works if the function name and arity was not changed - need fix!
rule_simplify({M,F,A}) ->
    ?RULE(?T("fun (Vars@@) -> M@:f@(Args@@) end"),
          begin
              ArgNames = lists:map(fun decode_value/1, Args@@),
              A@ = wrangler_syntax:integer(length(ArgNames)),
              ?TO_AST("fun M@:f@/A@", wrangler_syntax:get_pos(_This@))
          end,
          lists:map(fun decode_value/1, Args@@) == lists:map(fun decode_value/1, Vars@@) andalso
          api_refac:fun_define_info(f@) == {M, F, A}).

decode_value(Node) ->
    case wrangler_syntax:revert(Node) of
        {Type, _, Val} -> {Type, Val}
    end.


rule_handle_export_list(Export, {NewF, NewA}) ->
    ?RULE(?T("Form@"),
          api_refac:add_to_export(Form@, {NewF, NewA}),
          Form@==Export).

collect_exports(File) ->
    ?STOP_TD_TU([?COLLECT(?T("Form@"),
                          _This@,
                          api_refac:is_attribute(Form@, export))],
                [File]).

format_fa({F,A}) ->
    lists:flatten(io_lib:format("~p/~p", [F,A])).

make_export_attr(FA) ->
    ?TO_AST("-export(["++format_fa(FA)++"]).").

%-----------------------------------------------------------------------
read_orig_functions(FileName) ->
    TempFile = "temp_data.txt",
    Data = api_refac:defined_funs(FileName),
    case file:read_file_info(TempFile) of
       {ok, _FileInfo} ->
           file:write_file(TempFile, lists:flatten(io_lib:format("~p\n", [Data])), [append]),
           Data;
       {error, enoent} ->
           % File doesn't exist
           file:write_file(TempFile, lists:flatten(io_lib:format("~p\n", [Data]))),
           Data
    end.

check_for_difference([FileInfo |  FileInfoList1], FileInfoList2) ->
    FileInfoList = lists:delete(FileInfo, FileInfoList2),
    check_for_difference(FileInfoList1, FileInfoList);
check_for_difference([], _X) ->
    _X.

% was_exported({F,A}, File) ->

create_backup_before_refac(FileName) ->
    % create a copy, which will be refactored
    % run tests on orig & refactored file
    % compare results
    NewName = lists:concat(lists:join("_backupforrefac", string:split(FileName, ".erl"))),
    file:copy(FileName, NewName).

restore_backup_after_error(FileName) ->
    NewName = lists:concat(lists:join("_backupforrefac", string:split(FileName, ".erl"))),
    file:copy(NewName, FileName).

create_backups(FileList) -> 
    ?wrangler_io("- Creating BACKUP files...:\n~p\n", [FileList]),
    lists:map(fun create_backup_before_refac/1, FileList).

restore_backups(FileList) ->
    ?wrangler_io("- Restoring BACKUP files...:\n~p\n", [FileList]),
    lists:map(fun restore_backup_after_error/1, FileList).

clean_backups(FileList) ->
    NewNameList = lists:map(fun (FileName) -> lists:concat(lists:join("_backupforrefac", string:split(FileName, ".erl"))) end, FileList),
    ?wrangler_io("- Deleting BACKUP files...:\n~p\n", [NewNameList]),
    lists:map(fun file:delete/1, NewNameList).
