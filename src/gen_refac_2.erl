%% Copyright (c) 2010, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%
%%
%%@doc This module defines the `gen_refac' behaviour. It provides a
%% high-level abstraction of the logic work-flow of a refactoring
%% process. A user-interactive refactoring
%% process in Emacs generally works in the following way : the user first
%% selects the focus of interest by either pointing the cursor to a
%% particular program entity or by highlighting, then invokes the
%% refactoring command; in case that the refactoring needs certain
%% initial inputs from the user, it will prompt the user to input
%% the values from the mini-buffer. After all these interactions,
%% the refactor engine starts the pre-condition checking to
%% make sure that performing the refactoring initiated by the user
%% will not change the behaviour of the program; the refactorer
%% continues to carry out the program transformation if the pre-condition
%% passes, otherwise aborts the refactoring process and returns the reason
%% for failure.
%%
%% The idea behind this module is that the user module provides functions
%% to handle different parts of the refactoring process that is particular
%% to that refactoring, while `gen_refac' handles the parts that are common
%% to all refactorings.
%%
%% The user module should export:
%% ```input_par_prompts()
%%      ===> [string()]'''
%%  `input_par_prompts' returns the list of prompt strings to be used when
%%   the refactorer asks the user for input. There should be one
%%   prompt string for each input.
%% ```select_focus(Args::#args{})
%%       ===> none|{ok, term()}|{error, Reason}'''
%%  `select_pars' returns the focus of interest selected by the user.
%%   This function should return `none' if no focus selection is needed;
%%   `{error, Reason}' if the user didn't select the kind of entity
%%   expected; or `{ok, term()' when a valid focus of interest has been
%%   selected.
%%  ```check_pre_cond(Args::#args{})
%%       ===> ok | {error, Reason}'''
%%   This function checks whether the pre-conditions of the refactoring
%%   hold, and returns `ok' if the pre-condition checking passes, otherwise
%%   `{error, Reason}'.
%%  ```selective()
%%       ===> true | false'''
%%   This function should returns `true' if the user is allowed to browse
%%   through and select the changes to be made.
%% ```transform(Args::#args())
%%      ===> {ok, [{{filename(),filename()} syntaxTree()}] | {error, Reason}}'''
%%   Function `transform' carries out the transformation part of the
%%   refactorings. If the refactoring succeeds, it returns the list of
%%   file names together with their new AST (only files that have been
%%   changed need to be returned); otherwise `{error, Reason}'.
%%
%% Record `args' defines the data structure that is passed through, and also modified by, the different phases
%% of the refactoring.
%%  ```-record(args,{current_file_name :: filename(),         %% the file name of the current Erlang buffer.
%%                   cursor_pos        :: pos(),              %% the current cursor position.
%%                   highlight_range   :: {pos(), pos()},     %% the start and end location of the highlighted code if there is any.
%%                   user_inputs       :: [string()],         %% the data inputted by the user.
%%                   focus_sel         :: any(),              %% the focus of interest selected by the user.
%%                   selective         :: boolean(),          %% selective refactoring or not.
%%                   search_paths      ::[dir()|filename()],  %% the list of directories or files which specify the scope of the project.
%%                   tabwidth =8        ::integer()           %% the number of white spaces denoted by a tab key.
%%                  }).'''
%%
%% Some example refactorings implemented using the Wrangler API:
%%<li>
%%<a href="file:refac_swap_function_arguments.erl" >Swap arguments of a function;</a>.
%%</li>
%%<li>
%%<a href="file:refac_remove_an_argument.erl" >Remove an argument of a function;</a>.
%%</li>
%%<li>
%%<a href="file:refac_keysearch_to_keyfind.erl">Replace the uses of lists:keysearch/3 with lists:keyfind/3; </a>
%%</li>
%%<li>
%%<a href="file:refac_specialise_a_function.erl">Specialise a function definition; </a>
%%</li>
%%<li>
%%<a href="file:refac_apply_to_remote_call.erl">Apply to remote function call; </a>
%%</li>
%%<li>
%%<a href="file:refac_add_an_import_attribute.erl">Introduce an import attribute; </a>
%%</li>
%%<li>
%%<a href="file:refac_remove_an_import_attribute.erl">Remove an import attribute;</a>
%%</li>
%%<li>
%%<a href="file:refac_batch_rename_fun.erl">Batch renaming of function names from camelCaseto camel_case. </a>
%%</li>
%%<li>
%%<a href="file:inspec_examples.erl">A collection of code inspectors written using the Wrangler API. </a>
%%</li>
%%
%% === How to apply an own-defined refactoring ===
%% To invoke a user's own-defined `gen_refac' refactoring, select menu item Wrangler -> Refactor -> Apply Adhoc Refactoring,
%% Wrangler will then prompt you to input the name of the callback module, and the values for parameters needed by the
%% refactoring.
%%
%% === How to add an own-defined refactoring to the menu ===
%% To add a user-defined `gen_refac' refactoring to the `My gen_refac Refacs' menu, set the file buffer implementing the
%% refactoring as the current buffer, then select Wrangler -> Refactor -> Add to My gen_refac Refacs.
%%</doc>
-module(gen_refac_2).

-export([run_refac/2,
         run_refac/3,
         input_par_prompts/1,
         apply_changes/3
        ]).

-include("wrangler.hrl").


-callback input_par_prompts() -> [string()].
-callback select_focus(Args::#args{}) ->{ok, term()}|{error, term()}.
-callback check_pre_cond(Args::#args{}) -> ok |{error, term()}.
-callback selective() -> true | false.
-callback rule_def(Args::#args{}) ->
     {ok, [{{filename(),filename()},syntaxTree()}]} | {error, term()}.
-callback rule_appl(Args::#args{}) ->
     {ok, [{{filename(),filename()},syntaxTree()}]} | {error, term()}.


-spec(select_focus(Module::module(), Args::args()) ->
             {ok, term()} | {error, term()}).
select_focus(Module, Args) ->
    case apply(Module, select_focus, [Args]) of
        {ok, Term} ->
            {ok, Term};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, "The value returned by callback function select_focus/1 "
             "is different from the return type expected."}
    end.

-spec(check_pre_cond(Module::module()|tuple(), Args::#args{}) ->
             ok | {error, term()}).
check_pre_cond(Module, Args) ->
    case apply(Module,check_pre_cond, [Args]) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason};
        _->
            {error, "The value returned by callback function check_pre_cond/1 "
             "is different from the return type expected."}
    end.

-spec(selective(Module::module()) ->boolean()|{error, term()}).
selective(Module) ->
    case Module:selective() of
        true ->
            true;
        false ->
            false;
        _ ->
            {error, "The value returned by callback function selective/1 "
             "is different from the return type expected."}
    end.


%%@private
-spec(apply_changes(Module::module(), Args::[term()], CandsNotToChange::[term()]) ->
             {ok, [{filename(), filename(), syntaxTree()}]} |
             {error, term()}).
apply_changes(Module, Args, CandsNotToChange) ->
    wrangler_gen_refac_server:set_flag({self(), {false, CandsNotToChange}}),
    case apply(Module, transform_orig_file, [Args]) of
        {ok, Res} ->
            wrangler_gen_refac_server:delete_flag(self()),
            wrangler_write_file:write_refactored_files(
                 Res, 'emacs', Args#args.tabwidth, "");
        {error, Reason} ->
            wrangler_gen_refac_server:delete_flag(self()),
            {error, Reason}
    end.

%%@doc The interface function for invoking a refactoring defined
%% in module `ModName'.
-spec(run_refac(Module::module()|string()|tuple(), Args::[term()])->
             {ok, string()} | {change_set, [{string(), string()}], module(), #args{}}|
             {error, term()}).
run_refac(ModName, Args) ->
    run_refac(ModName, Args, emacs).


%%@doc The interface function for invoking a refactoring defined
%% in module `ModName'.
%%@private
-spec(run_refac(Module::module()|string()|tuple(), Args::[term()], Editor::atom())->
             {ok, string()} | {ok, [{filename(), filename(), string()}]} |
			 {change_set, [{string(), string()}], module(), #args{}}|
             {error, term()}).
run_refac(ModName, Args=[CurFileName, [Line,Col],
                         [[StartLine, StartCol],
                          [EndLn, EndCol]], UserInputs,
                         SearchPaths, TabWidth], Editor) ->
    ?wrangler_io("\nCMD: ~p:run_refac(~p,~p).\n",
		 [?MODULE, ModName, Args]),
    Module = if is_list(ModName) ->
                     list_to_atom(ModName);
                true ->
                    ModName
             end,
    Args0=#args{current_file_name=CurFileName,
                cursor_pos = {Line, Col},
                highlight_range = {{StartLine, StartCol},
                                   {EndLn, EndCol}},
                user_inputs = UserInputs,
                search_paths = SearchPaths,
                tabwidth = TabWidth},
    case select_focus(Module, Args0) of
        {ok, Sel} ->
            Args1 = Args0#args{focus_sel=Sel},
            case check_pre_cond(Module, Args1) of
                ok ->
                    Selective = case Editor of
								   eclipse ->
									   false;
							   	   _ ->
									   selective(Module)
							   end,
                    wrangler_gen_refac_server:set_flag({self(), Selective}),
                    Args2 = Args1#args{selective=Selective},
                    if is_boolean(Selective) ->
                            case Selective of
                                true ->
                                    case apply(gen_refac_2_utils, transform_orig_file, [Module, Args2]) of
                                        {error, Reason} ->
                                            {error, Reason};
                                        _ ->
                                            {ok, ChangeSets}=wrangler_gen_refac_server:get_change_set(self()),
                                            %% wrangler_gen_refac_server:delete_change_set(self()),
                                            case apply(gen_refac_2_utils, transform_client_files, [Args2]) of
                                                {error, Reason2} ->
                                                    {error, Reason2};
                                                _ ->
                                                    {ok, ChangeSets2}=wrangler_gen_refac_server:get_change_set(self()),
                                                    %% wrangler_gen_refac_server:delete_change_set(self()),
                                                    {change_set, ChangeSets++ChangeSets2, Module, Args2}
                                            end
                                    end;
                                false->
                                    try
                                        %ClientFileList = api_refac:client_files(CurFileName, SearchPaths),
                                        gen_refac_2_utils:create_backups([CurFileName] ++ api_refac:client_files(CurFileName, SearchPaths)),
                                        FileData1 = gen_refac_2_utils:read_orig_functions(CurFileName),
                                        ExportedFA = api_refac:exported_funs(CurFileName),
                                        
                                        % transformations of general extension (fun M:F becomes fun(...) -> M:F)
                                        ?wrangler_io("\n- Generalizing fun expressions...:\n", []),
                                        {ok, ResExt1} = apply(gen_refac_2_utils, transform_extend1, [Args2]),
                                        wrangler_write_file:write_refactored_files(ResExt1,Editor,TabWidth,""),
                                        
                                        ?wrangler_io("\n- Refactoring files...:\n", []),
                                        {ok, ResOrig} = apply(gen_refac_2_utils, transform_orig_file, [Module, Args2]),
                                        wrangler_write_file:write_refactored_files(ResOrig,Editor,TabWidth,""),
                                        
                                        FileData2 = gen_refac_2_utils:read_orig_functions(CurFileName),
                                        [OldFormatData] = gen_refac_2_utils:check_for_difference(FileData2, FileData1),
                                        [NewFormatData] = gen_refac_2_utils:check_for_difference(FileData1, FileData2),
                                        % debug prints
                                        ?wrangler_io("- Original functionform (F/A): ~p\n", [OldFormatData]),
                                        ?wrangler_io("- Modified functionform (F/A): ~p\n", [NewFormatData]),

                                        ?wrangler_io("\n- Simplifying fun expressions...:\n", []),
                                        {ok, ResSimpl1} = apply(gen_refac_2_utils, transform_simplify1, [NewFormatData, Args2]),
                                        wrangler_write_file:write_refactored_files(ResSimpl1,Editor,TabWidth,""),

                                        case lists:member(OldFormatData, ExportedFA) of
                                            true ->
                                                ?wrangler_io("\n- Handling files where it is used after exporting...:\n", []),

                                                {ok, ResExt2} = apply(gen_refac_2_utils, transform_extend2, [Args2]),
                                                wrangler_write_file:write_refactored_files(ResExt2,Editor,TabWidth,""),

                                                {ok, ResClient} = apply(gen_refac_2_utils, transform_client_files, [Module, Args2]),
                                                wrangler_write_file:write_refactored_files(ResClient,Editor,TabWidth,""),

                                                {ok, ResSimpl2} = apply(gen_refac_2_utils, transform_simplify2, [NewFormatData, Args2]),    
                                                wrangler_write_file:write_refactored_files(ResSimpl2,Editor,TabWidth,""),

                                                {ok, ResExp} = apply(gen_refac_2_utils, transform_exportlist, [CurFileName, NewFormatData]),
                                                wrangler_write_file:write_refactored_files(ResExp,Editor,TabWidth,"");
                                            false ->
                                                {ok, []}
                                        end
                                    catch
                                        {error, Reason} ->
                                            gen_refac_2_utils:restore_backups([CurFileName] ++ api_refac:client_files(CurFileName, SearchPaths)),
                                            {error, Reason};
                                        X ->
                                            X
                                    after
                                        wrangler_gen_refac_server:delete_flag(self()),
                                        gen_refac_2_utils:clean_backups([CurFileName] ++ api_refac:client_files(CurFileName, SearchPaths))
                                    end
                                end;
                       true->
                            {error, "The value returned by callback function transform/1 "
                             "is different from the return type expected."}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%@private
input_par_prompts(CallBackMod) ->
    Res =input_pars_1(CallBackMod),
    {ok, Res}.

input_pars_1(CallBackMod) when is_atom(CallBackMod)->
    erlang:apply(CallBackMod, input_par_prompts, []);
input_pars_1(CallBackMod) when is_list(CallBackMod)->
    erlang:apply(list_to_atom(CallBackMod), input_par_prompts, []);
input_pars_1(_) ->
    throw:error(badarg).
