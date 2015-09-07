%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% The Initial Developer of the Original Code is Richard Carlsson
%% Portions created by Richard Carlsson are Copyright 2003-2006,
%% Richard Carlsson.
%%
%% Code from edoc extracted and modified in 2015 by Pablo Lamela
%% @copyright 2003-2006 Richard Carlsson
%% @end
%% @hidden
%% @private
%% =====================================================================
-module(edoc_args).

-export([collect_fun_info/1]).

-record(entry, {name, args = [], line = 0, data}).

%% Generic tag information
%% @type tag() = #tag{name = atom(),
%%                    line = integer(),
%%                    origin = comment | code,
%%                    data = term()}
-record(tag, {name, line = 0, origin = comment, data}).

%% @type t_spec() = #t_spec{name = t_name(),
%%                          type = t_type(),
%%                          defs = [t_def()]}

-record(t_spec, {name, type, defs=[]}).		% function specification

%% @type t_fun() = #t_fun{a = list(),
%%                        args = [type()],
%%                        range = type()}

-record(t_fun, {a=[], args, range}).	% function '(t1,...,tN) -> range'

-define(t_ann(X), element(2, X)).

%% This collects the data for the header and the functions of the
%% module. Note that the list of forms is assumed to have been
%% preprocessed first, so that all "invisible" forms are removed, and
%% the only interesting comments are those that are standalone comments
%% in the list.

collect_fun_info(Fs) ->
    functions(collect(wrangler_syntax:form_list_elements(Fs), [], [],
		      [], [], undefined)).

collect([F | Fs], Cs, Ss, Ts, As, Header) ->
    case wrangler_syntax_lib:analyze_form(F) of
	comment ->
	    collect(Fs, [F | Cs], Ss, Ts, As, Header);
	{function, Name} ->
	    L = wrangler_syntax:get_pos(F),
	    Args = parameters(wrangler_syntax:function_clauses(F)),
	    collect(Fs, [], [], [],
                    [#entry{name = Name, args = Args, line = L,
                            data = Ts} | As],
		    Header);
	{attribute, {module, _}} when Header =:= undefined ->
	    L = wrangler_syntax:get_pos(F),
	    collect(Fs, [], [], [], As,
                    #entry{name = module, line = L,
                           data = Ts});
        {attribute, {N, _}} ->
            case tag(N) of
                spec ->
                    collect(Fs, Cs, [F | Ss], Ts, As, Header);
                type ->
                    collect(Fs, Cs, Ss, [F | Ts], As, Header);
                unknown ->
                    %% Drop current seen comments.
                    collect(Fs, [], [], [], As, Header)
            end;
	_ ->
	    %% Drop current seen comments.
	    collect(Fs, [], [], [], As, Header)
    end;
collect([], _Cs, _Ss, _Ts, As, _Header) ->
    lists:reverse(As).

%% @doc Return the kind of the attribute tag.

-type tag_kind() :: 'type' | 'spec' | 'unknown'.
-spec tag(Tag::atom()) -> tag_kind().

tag(opaque) -> type;
tag(spec) -> spec;
tag(type) -> type;
tag(_) -> unknown.

%% Extracting possible parameter names from Erlang clause patterns.  The
%% atom '_' is used when no name can be found. (Better names are made up
%% later, when we also may have typespecs available; see edoc_data.)

parameters(Clauses) ->
    select_names([find_names(Ps) || Ps <- patterns(Clauses)]).

patterns(Cs) ->
    transpose([wrangler_syntax:clause_patterns(C) || C <- Cs]).

%% @private
transpose([]) -> [];
transpose([[] | Xss]) -> transpose(Xss);
transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | _T] <- Xss]]
     | transpose([Xs | [T || [_H | T] <- Xss]])].

find_names(Ps) ->
    find_names(Ps, []).

find_names([P | Ps], Ns) ->
    case wrangler_syntax:type(P) of
	variable ->
	    find_names(Ps, [tidy_name(wrangler_syntax:variable_name(P)) | Ns]);
	match_expr ->
	    %% Right-hand side gets priority over left-hand side!
	    %% Note that the list is reversed afterwards.
	    P1 = wrangler_syntax:match_expr_pattern(P),
	    P2 = wrangler_syntax:match_expr_body(P),
	    find_names([P1, P2 | Ps], Ns);
	list ->
	    P1 = wrangler_syntax:list_tail(P),
	    find_names([P1 | Ps], Ns);
	record_expr ->
	    A = wrangler_syntax:record_expr_type(P),
	    N = list_to_atom(capitalize(wrangler_syntax:atom_name(A))),
	    find_names(Ps, [N | Ns]);
	infix_expr ->
	    %% this can only be a '++' operation
	    P1 = wrangler_syntax:infix_expr_right(P),
	    find_names([P1 | Ps], Ns);
	_ ->
	    find_names(Ps, Ns)
    end;
find_names([], Ns) ->
    lists:reverse(Ns).

select_names(Ls) ->
    select_names(Ls, [], sets:new()).

select_names([Ns | Ls], As, S) ->
    A = select_name(Ns, S),
    select_names(Ls, [A | As], sets:add_element(A, S));
select_names([], As, _) ->
    lists:reverse(As).

select_name([A | Ns], S) ->
    case sets:is_element(A, S) of
	true ->
	    select_name(Ns, S);
	false ->
	    A
    end;
select_name([], _S) ->
    '_'.

%% Strip leading underscore characters from parameter names. If the
%% result does not begin with an uppercase character, we add a single
%% leading underscore. If the result would be empty, the atom '_' is
%% returned.

tidy_name(A) ->
    case atom_to_list(A) of
	[$_ | Cs] ->
	    list_to_atom(tidy_name_1(Cs));
	_ ->
	    A
    end.

tidy_name_1([$_ | Cs]) -> tidy_name_1(Cs);
tidy_name_1([C | _]=Cs) when C >= $A, C =< $Z -> Cs;
tidy_name_1([C | _]=Cs) when C >= $\300, C =< $\336, C =/= $\327-> Cs;
tidy_name_1(Cs) -> [$_ | Cs].

%% Change initial character from lowercase to uppercase.

capitalize([C | Cs]) when C >= $a, C =< $z -> [C - 32 | Cs];
capitalize(Cs) -> Cs.

functions(Es) ->
    [function(N, As, Ts) || #entry{name = {_,_}=N, args = As,
				   data = Ts} <- Es].

function({N, A}, As, Ts) ->
    Args = signature(Ts, As),
    {{atom_to_list(N), A}, Args}.

signature(Ts, As) ->
    case get_tags(spec, Ts) of
	[T] ->
	    Spec = T#tag.data,
	    As0 = arg_names(Spec),
	    As1 = merge_args(As0, As),
	    As1;
	[] ->
	    S = sets:new(),
	    [atom_to_list(A) || A <- fix_argnames(As, S, 1)]
    end.

get_tags(Tag, [#tag{name = Tag} = T | Ts]) -> [T | get_tags(Tag, Ts)];
get_tags(Tag, [_ | Ts]) -> get_tags(Tag, Ts);
get_tags(_, []) -> [].

arg_names(S) ->
    arg_anns(S, fun is_name/1, '_').

is_name(A) when is_atom(A) -> true;
is_name(_) -> false.

arg_anns(#t_spec{type = #t_fun{args = As}}, F, Def) ->
    [find(?t_ann(A), F, Def) || A <- As].

find([A| As], F, Def) ->
    case F(A) of
	true -> A;
	false -> find(As, F, Def)
    end;
find([], _, Def) -> Def.

%% Names are chosen from the first list (the specification) if possible.
merge_args(As, As1) ->
    merge_args(As, As1, [], sets:new(), 1).

merge_args(['_' | As], ['_' | As1], Rs, S, N) ->
    merge_args(As, As1, Rs, S, N, make_name(N, S));
merge_args(['_' | As], [A | As1], Rs, S, N) ->
    merge_args(As, As1, Rs, S, N, A);
merge_args([A | As], [_ | As1], Rs, S, N) ->
    merge_args(As, As1, Rs, S, N, A);
merge_args([], [], Rs, _S, _N) ->
    lists:reverse(Rs).

merge_args(As, As1, Rs, S, N, A) ->
    merge_args(As, As1, [A | Rs],
	       sets:add_element(A, S), N + 1).

fix_argnames(['_' | As], S, N) ->
    A = make_name(N, S),
    [A | fix_argnames(As, sets:add_element(A, S), N + 1)];
fix_argnames([A | As], S, N) ->
    [A | fix_argnames(As, sets:add_element(A, S), N + 1)];
fix_argnames([], _S, _N) ->
    [].

make_name(N, S) ->
    make_name(N, S, "X").

make_name(N, S, Base) ->
    A = list_to_atom(Base ++ integer_to_list(N)),
    case sets:is_element(A, S) of
 	true ->
 	    make_name(N, S, Base ++ "x");
 	false ->
 	    A
    end.
