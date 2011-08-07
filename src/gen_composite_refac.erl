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
%%@version 0.1 
%%@author  Huiqing Li <H.Li@kent.ac.uk>
%%
%%
-module(gen_composite_refac).

-export([init_composite_refac/2,
         get_next_command/1, 
         input_par_prompts/1]).

-export([behaviour_info/1]).

-compile(export_all).

-include("../include/wrangler.hrl").

-spec behaviour_info(atom()) ->[{atom(), arity()}].
behaviour_info(callbacks) ->
    [{composite_refac,1}, {input_par_prompts, 0}].

input_par_prompts(CallBackMod) ->
    Res =input_pars_1(CallBackMod),
    {ok, Res}.

input_pars_1(CallBackMod) when is_atom(CallBackMod)->
    erlang:apply(CallBackMod, input_par_prompts, []);
input_pars_1(CallBackMod) when is_list(CallBackMod)->
    erlang:apply(list_to_atom(CallBackMod), input_par_prompts, []);
input_pars_1(_) ->
    throw:error(badarg).

init_composite_refac(ModName, Args=[CurFileName, [Line,Col],
                                    [[StartLine, StartCol],
                                     [EndLn, EndCol]], UserInputs,
                                    SearchPaths, TabWidth])->
    ?wrangler_io("\nCMD: ~p:init_composite_refac(~p,~p).\n",
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
    case apply(Module, composite_refac, [Args0]) of
        {error, Reason} ->
            {error, Reason};
        Cmds ->
            Cmds1 = if is_list(Cmds) ->
                            Cmds;
                       true -> [Cmds]
                    end,
            try start_cmd_server(lists:flatten(Cmds1))
            catch
                E1:E2 ->
                    erlang:error({E1,E2})
            end
    end.

start_cmd_server(Cmds) ->
    wrangler_backup_server:reset_backups(),
    wrangler_cmd_server:start_link([Cmds]).

stop_cmd_server() ->
    wrangler_cmd_server:stop().
   
get_next_command(PrevResult) ->
    Cmd=wrangler_cmd_server:get_next_command(PrevResult),
    case Cmd of 
        {ok, none, _ChangedFiles, [error, _Reason]} ->
            stop_cmd_server(),
            wrangler_backup_server:recover_backups(),
            Cmd;
        {ok, none, _ChangedFiles, _Msg} ->
            stop_cmd_server(),
            {ok, PreviewPairs}=wrangler_backup_server:recover_backups(),
            wrangler_preview_server:add_files({PreviewPairs, ""}),
            Cmd;
        _ ->
            Cmd
    end.
    
