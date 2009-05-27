-module(wdistel).

-include_lib("kernel/include/file.hrl").

-export([rpc_entry/3, gl_proxy/1]).

%% ----------------------------------------------------------------------
%% RPC entry point, adapting the group_leader protocol.

rpc_entry(M, F, A) ->
    GL = group_leader(),
    Name = gl_name(GL),
    case whereis(Name) of
        undefined ->
            Pid = spawn(?MODULE, gl_proxy, [GL]),
            register(Name, Pid),
            group_leader(Pid, self());
        Pid ->
            group_leader(Pid, self())
    end,
    apply(M,F,A).

gl_name(Pid) ->
    list_to_atom((lists:flatten(io_lib:format("distel_gl_for_~p", [Pid])))).

gl_proxy(GL) ->
    receive
        {io_request, From, ReplyAs, {put_chars, C}} ->
            GL ! {put_chars, C},
            From ! {io_reply, ReplyAs, ok};
        {io_request, From, ReplyAs, {put_chars, M, F, A}} ->
            GL ! {put_chars, lists:flatten(apply(M, F, A))},
            From ! {io_reply, ReplyAs, ok};
        {io_request, From, ReplyAs, {get_until, _, _, _}} ->
            %% Input not supported, yet
            From ! {io_reply, ReplyAs, eof}
    end,
    gl_proxy(GL).

