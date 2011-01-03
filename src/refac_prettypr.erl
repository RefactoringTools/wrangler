%% =====================================================================
%% Pretty printing of abstract Erlang syntax trees
%%
%% Copyright (C) 1997-2002 Richard Carlsson
%%
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
%%
%% $Id: refac_prettypr.erl,v 1.4 2008/04/30 09:28:12 hl Exp $
%%
%% Modified: 17 Jan 2007 by  Huiqing Li <hl@kent.ac.uk>
%% =====================================================================
%%
%% @doc Pretty printing of abstract Erlang syntax trees.
%%
%% <p>This module is a front end to the pretty-printing library module
%% <code>prettypr</code>, for text formatting of abstract syntax trees
%% defined by the module <code>refac_syntax</code>.</p>


-module(refac_prettypr).

-export([format/1,print_ast/2, print_ast/3, 
         print_ast_and_get_changes/3]).

-import(refac_prettypr_0,
	[text/1,nest/2,above/2,beside/2,sep/1,par/1,par/2,
	 floating/3,floating/1,break/1,follow/2,follow/3,
	 empty/0, format/3, best/3]).

-import(refac_parse,
	[preop_prec/1,inop_prec/1,func_prec/0,max_prec/0]).

-define(PADDING, 2).

-define(PAPER, 80).

-define(RIBBON, 56).

-define(NOUSER, undefined).

-define(NOHOOK, none).

-define(TabWidth, 8).

-record(ctxt,
	{prec = 0,sub_indent = 2,break_indent = 4,
	 clause = undefined,hook = ?NOHOOK,paper = ?PAPER,
	 ribbon = ?RIBBON,user = ?NOUSER,
	 tokens = [],
	 tabwidth=?TabWidth,
	 format = unknown}).


%% ====================================================================
%% user-program guided pretty-printing of an abstract syntax tree which
%% must be a form list.

print_ast(FileFmt,AST) ->
    print_ast(FileFmt,AST, 8).

print_ast(FileFmt, AST, TabWidth) ->
    print_ast(FileFmt, AST, [], TabWidth).

print_ast(FileFmt, AST, Options, TabWidth) ->
    element(1, print_ast_and_get_changes(FileFmt, AST, Options, TabWidth)).

print_ast_and_get_changes(FileFmt, AST, TabWidth) ->
    print_ast_and_get_changes(FileFmt, AST, [], TabWidth).

print_ast_and_get_changes(FileFmt, AST, Options, TabWidth) ->
    Fs = refac_syntax:form_list_elements(AST),
    {FmStrs, C} = lists:unzip([print_a_form(F, FileFmt, Options, TabWidth)|| F<-Fs]),
    Content = lists:append(FmStrs),
    {NoFunsChanged, NoToksRemoved, NoToksAdded} =lists:unzip3(C),
    Change={lists:sum(NoFunsChanged), 
            lists:sum(NoToksRemoved),
            lists:sum(NoToksAdded)},
    {Content, Change}.

print_a_form(Form, FileFmt, Options, TabWidth) ->
    case false of %% form_not_changed(Form) of
        true ->
            FormStr=refac_util:concat_toks(refac_util:get_toks(Form)),
            {FormStr, {0, 0, 0}};
        false ->
            %% refac_io:format("Form:\n~p\n", [Form]),
            print_a_form_and_get_changes(Form, FileFmt, Options, TabWidth)
    end.
 
       
print_a_form_and_get_changes(Form, FileFormat, Options, TabWidth) ->
    Ctxt = #ctxt{hook  = proplists:get_value(hook,Options,?NOHOOK),
		 paper = proplists:get_value(paper,Options,?PAPER),
		 ribbon = proplists:get_value(ribbon,Options,?RIBBON),
		 user = proplists:get_value(user,Options),
		 format = FileFormat,
		 tabwidth = TabWidth,
		 tokens = refac_util:get_toks(Form)},
    OrigToks = refac_util:get_toks(Form),
    refac_io:format("Form:\n~p\n", [Form]),
    refac_io:format("OrigToks:\n~p\n", [OrigToks]),
    OrigFormStr=refac_util:concat_toks(OrigToks),
    refac_io:format("OrigFormStr:\n~p\n", [OrigFormStr]),
    NewFormStr0= print_form(Form,reset_prec(Ctxt),fun lay/2),
    refac_io:format("NewFormStr:\n~p\n", [NewFormStr0]),
    NewFormStr=repair_new_form_str(OrigFormStr, NewFormStr0, TabWidth,FileFormat),
    {ok, NewToks, _} = refac_scan:string(NewFormStr),
    Change =get_changes(OrigToks, NewToks),
    {NewFormStr, Change}.
   

get_changes(OrigToks, NewToks) ->
    OrigToks1 = remove_locs_whites_and_comments(OrigToks),
    NewToks1 = remove_locs(NewToks),
    ToksRemoved = OrigToks1 -- NewToks1,
    ToksAdded = NewToks1 -- OrigToks1,
    {1, length(ToksRemoved), length(ToksAdded)}.


print_form(Form,Ctxt,Fun) ->
    Paper = Ctxt#ctxt.paper,
    Ribbon = Ctxt#ctxt.ribbon,
    D=best(Fun(Form,Ctxt),Paper,Ribbon),
    FileFormat = Ctxt#ctxt.format,
    TabWidth = Ctxt#ctxt.tabwidth,
    FStr0=refac_prettypr_0:layout(D,FileFormat,TabWidth),
    FStr=remove_trailing_whitespace(FStr0), 
    Toks = refac_util:get_toks(Form),
    if Toks ==[] ->
            Delimitor = get_delimitor(FileFormat),
            Delimitor++Delimitor++FStr; 
       true ->
            FStr
    end.


get_delimitor(FileFormat) ->        
    case FileFormat of
        dos -> "\r\n";
        mac -> "\r";
        _ -> "\n"
    end.
  
is_special_form(Form) ->
    case refac_syntax:type(Form) of
	error_marker -> 
            true;
	comment -> 
            true; 
	attribute ->
	    AtrName = refac_syntax:attribute_name(Form),
            case refac_syntax:atom_value(AtrName) of
		type -> true;
		spec -> true;
		record -> 
                    [_R, FieldTuple] = refac_syntax:attribute_arguments(Form),
                    Fields = refac_syntax:tuple_elements(FieldTuple),
                    lists:any(fun(F) ->  
                                      refac_syntax:type(F)==typed_record_field
                              end, Fields);
		_ -> false
	    end;
	_ -> false
    end.

form_not_changed(Form) ->
    case is_special_form(Form) of
	true ->
            %% This might change!
            true;  
	false ->
	    Toks = refac_util:get_toks(Form),
	    case Toks of
		[] -> false;
		_ ->
		    form_not_change_1(Form)
	    end
    end.

form_not_change_1(Form) ->
    try
        Toks = refac_util:get_toks(Form),
        Str = refac_util:concat_toks(Toks),
        {ok,Toks1,_} = refac_scan:string(Str,{1,1},?TabWidth,unix),
        OriginalForm = refac_epp_dodger:normal_parser(Toks1,[]),
        NewStr = format(Form,[]),
        {ok,Toks2,_} =
            refac_scan:string(NewStr,{1,1},?TabWidth,unix),
        NewForm = refac_epp_dodger:normal_parser(Toks2,[]),
        best(OriginalForm) == best(NewForm)
    of 
        Res -> Res
    catch 
        _E1:_E2 -> 
            false
    end.


set_prec(Ctxt,Prec) ->
    Ctxt#ctxt{prec = Prec}.   


reset_prec(Ctxt) ->
    set_prec(Ctxt,0).    


%% =====================================================================
%% @spec format(Tree::syntaxTree()) -> string()
%% @equiv format(Tree, [])


format(Node) -> 
    format(Node,[]).
   

%% =====================================================================
%% @spec format(Tree::syntaxTree(), Options::[term()]) -> string()
%%           syntaxTree() = refac_syntax:syntaxTree()
%%
%% @type hook() = (syntaxTree(), context(), Continuation) -> document()
%%	    Continuation = (syntaxTree(), context()) -> document().
%%
%% A call-back function for user-controlled formatting. See <a
%% href="#format-2"><code>format/2</code></a>.
%%
%% @type context(). A representation of the current context of the
%% pretty-printer. Can be accessed in hook functions.
%%
%% @doc Prettyprint-formats an abstract Erlang syntax tree as text.
%%
%% <p>Available options:
%% <dl>
%%   <dt>{hook, none | <a href="#type-hook">hook()</a>}</dt>
%%       <dd>Unless the value is <code>none</code>, the given function
%%       is called for each node whose list of annotations is not empty;
%%       see below for details. The default value is
%%       <code>none</code>.</dd>
%%
%%   <dt>{paper, integer()}</dt>
%%       <dd>Specifies the preferred maximum number of characters on any
%%       line, including indentation. The default value is 80.</dd>
%%
%%   <dt>{ribbon, integer()}</dt>
%%       <dd>Specifies the preferred maximum number of characters on any
%%       line, not counting indentation. The default value is 65.</dd>
%%
%%   <dt>{user, term()}</dt>
%%       <dd>User-specific data for use in hook functions. The default
%%       value is <code>undefined</code>.</dd>
%% </dl></p>
%%
%% <p>A hook function (cf. the <a
%% href="#type-hook"><code>hook()</code></a> type) is passed the current
%% syntax tree node, the context, and a continuation. The context can be
%% examined and manipulated by functions such as
%% <code>get_ctxt_user/1</code> and <code>set_ctxt_user/2</code>. The
%% hook must return a "document" data structure (see
%% <code>layout/2</code> and <code>best/2</code>); this may be
%% constructed in part or in whole by applying the continuation
%% function. For example, the following is a trivial hook:
%% <pre>
%%     fun (Node, Ctxt, Cont) -> Cont(Node, Ctxt) end
%% </pre>
%% which yields the same result as if no hook was given.
%% The following, however:
%% <pre>
%%     fun (Node, Ctxt, Cont) ->
%%         Doc = Cont(Node, Ctxt),
%%         prettypr:beside(prettypr:text("&lt;b>"),
%%                         prettypr:beside(Doc,
%%                                         prettypr:text("&lt;/b>")))
%%     end
%% </pre>
%% will place the text of any annotated node (regardless of the
%% annotation data) between HTML "boldface begin" and "boldface end"
%% tags.</p>
%%
%% @see refac_syntax
%% @see format/1
%% @see layout/2
%% @see best/2
%% @see get_ctxt_user/1
%% @see set_ctxt_user/2


format(Node,Options) ->
    W = proplists:get_value(paper,Options,?PAPER),
    L = proplists:get_value(ribbon,Options,?RIBBON),
    format(layout(Node,Options),W,L).

%% =====================================================================
%% @spec best(Tree::syntaxTree()) -> empty | document()
%% @equiv best(Tree, [])


best(Node) -> best(Node,[]).

%% =====================================================================
%% @spec best(Tree::syntaxTree(), Options::[term()]) ->
%%           empty | document()
%%
%% @doc Creates a fixed "best" abstract layout for a syntax tree. This
%% is similar to the <code>layout/2</code> function, except that here,
%% the final layout has been selected with respect to the given options.
%% The atom <code>empty</code> is returned if no such layout could be
%% produced. For information on the options, see the
%% <code>format/2</code> function.
%%
%% @see best/1
%% @see layout/2
%% @see format/2
%% @see prettypr:best/2


best(Node,Options) ->
    W = proplists:get_value(paper,Options,?PAPER),
    L = proplists:get_value(ribbon,Options,?RIBBON),
    best(layout(Node,Options),W,L).


%% =====================================================================
%% @spec layout(Tree::syntaxTree(), Options::[term()]) -> document()
%%	    document() = prettypr:document()
%%
%% @doc Creates an abstract document layout for a syntax tree. The
%% result represents a set of possible layouts (cf. module
%% <code>prettypr</code>). For information on the options, see
%% <code>format/2</code>; note, however, that the <code>paper</code> and
%% <code>ribbon</code> options are ignored by this function.
%%
%% <p>This function provides a low-level interface to the pretty
%% printer, returning a flexible representation of possible layouts,
%% independent of the paper width eventually to be used for formatting.
%% This can be included as part of another document and/or further
%% processed directly by the functions in the <code>prettypr</code>
%% module, or used in a hook function (see <code>format/2</code> for
%% details).</p>
%%
%% @see prettypr
%% @see format/2
%% @see layout/1


layout(Node,Options) ->
    lay(Node,
	#ctxt{hook = proplists:get_value(hook,Options,?NOHOOK),
	      paper = proplists:get_value(paper,Options,?PAPER),
	      ribbon = proplists:get_value(ribbon,Options,?RIBBON),
	      user = proplists:get_value(user,Options)}).
	    
lay(Node,Ctxt) ->
    case refac_syntax:get_ann(Node) of
        [] ->
            %% Hooks are not called if there are no annotations.
            lay_1(Node,Ctxt);
        _As ->
            case Ctxt#ctxt.hook of
	    ?NOHOOK -> lay_1(Node,Ctxt);
                Hook -> Hook(Node,Ctxt,fun lay_1/2)
            end
    end.

%% This handles attached comments:
lay_1(Node, Ctxt) ->
    case refac_syntax:has_comments(Node) of
	true ->
            D1 = lay_2(Node, Ctxt),
	    PreCs = refac_syntax:get_precomments(Node),
	    PostCs = refac_syntax:get_postcomments(Node),
            {{NodeStartLine, NodeStartCol},{NodeEndLn, _}} = get_start_end_loc(Node),
	    D2 = lay_postcomments_1(PostCs, D1, NodeEndLn),
            lay_precomments(PreCs, D2, {NodeStartLine, NodeStartCol});
	false ->
	    lay_2(Node, Ctxt)
    end.

%% For pre-comments, all padding is ignored.
%% this might change the layout of comments!
lay_precomments([],D, _) -> D;
lay_precomments(Cs,D, {DStartLine, DStartCol}) ->
    D0 =stack_comments(Cs,false),
    LastCom = lists:last(Cs),
    {_, Col} = refac_syntax:get_pos(hd(Cs)),
    {Line, _Col} = refac_syntax:get_pos(LastCom),
    LastComTest = refac_syntax:comment_text(LastCom),
    Offset = erlang:max(Col-DStartCol, 0), 
    case DStartLine-(Line+length(LastComTest)) of
        0 ->
            above(nest(Offset,D0),D);
        N ->
            vertical([nest(Offset, D0), white_lines(N), D])
    end.


lay_postcomments_1([], D, _) -> D;
lay_postcomments_1(Cs, D, DEndLn) ->
    {PostCsLn, _} = refac_syntax:get_pos(hd(Cs)),
    case PostCsLn >= DEndLn + 1 of
        true ->
            lay_postcomments(Cs, above(D, text("")));
        false ->
            lay_postcomments(Cs, D)
    end.
    
%% For postcomments, individual padding is added.
lay_postcomments([],D) -> D;
lay_postcomments(Cs,D) ->
    D0 =floating(break(stack_comments(Cs,true)),1,0),
    beside(D,D0).

%% Format (including padding, if `Pad' is `true', otherwise not)
%% and stack the listed comments above each other,
stack_comments([C| Cs],Pad) ->
    ComText=refac_syntax:comment_text(C),
    D = stack_comment_lines(ComText),
    D1 = case Pad of
             true ->
                 P = case refac_syntax:comment_padding(C) of
                         none -> ?PADDING;
                         P1 -> P1
                     end,
                 beside(text(spaces(P)),D);
	   false -> D
	 end,
    case Cs of
        [] ->
            D1;    % done
        _ -> 
            {L,_} = refac_syntax:get_pos(C),
            {L1, _} = refac_syntax:get_pos(hd(Cs)),
            N = L1 - (L+length(ComText)-1),
            if N>1 ->
                    vertical([D1, white_lines(N-1),stack_comments(Cs, Pad)]);
               true ->
                    above(D1,stack_comments(Cs,Pad))
            end
    end;
stack_comments([],_) -> empty().

%% Stack lines of text above each other and prefix each string in
%% the list with a single `%' character.


stack_comment_lines([S| Ss]) ->
    D = text(add_comment_prefix(S)),
    case Ss of
      [] -> D;
      _ -> above(D,stack_comment_lines(Ss))
    end;
stack_comment_lines([]) -> empty().

add_comment_prefix(S) -> [$%| S].

%% This part ignores annotations and comments:
lay_2(Node, Ctxt) ->
    case refac_syntax:type(Node) of
	%% We list literals and other common cases first.
	variable -> 
            text(refac_syntax:variable_literal(Node));
	atom ->
            Lit=atom_to_list(refac_syntax:atom_value(Node)),
            As= refac_syntax:get_ann(Node),
            case lists:keysearch(qatom, 1, As) of
                {value, _} ->
                    text("'"++Lit++"'");
                false ->
                    text(Lit)
            end;
        integer -> 
	    text(refac_syntax:integer_literal(Node));
	float ->
	    text(tidy_float(refac_syntax:float_literal(Node)));
	text -> 
	    text(refac_syntax:text_string(Node));
	string ->  
	    Str = refac_syntax:string_literal(Node),
	    StrVal = "\"" ++ refac_syntax:string_value(Node) ++ "\"",
	    case lists:keysearch(toks, 1, refac_syntax:get_ann(Node)) of
		{value, {toks, StrToks}} ->
		    Str1 = io_lib:write_string(
			     lists:concat(lists:map
					    (fun ({string, _, S}) 
						 -> S 
					     end, StrToks))),
		    case Str1 == Str of
			true -> lay_string(StrToks);
			_ -> lay_string(StrVal, Ctxt)
		    end;
		_ -> lay_string(StrVal, Ctxt)
	    end;
	nil -> 
	    text("[]");
	tuple -> 
	    %% Done;
	    Es0 = refac_syntax:tuple_elements(Node),
	    Sep = get_separator(Es0, Ctxt, ","),
	    Es = seq(Es0, floating(text(Sep)), reset_prec(Ctxt), fun lay/2),
	    Es1=lay_elems(fun refac_prettypr_0:par/1, Es,refac_syntax:tuple_elements(Node), Ctxt),
	    beside(floating(text("{")),beside(Es1, floating(text("}"))));
	list -> 
	    %% Done;
	    Ctxt1 = reset_prec(Ctxt),
	    Node1 = refac_syntax:compact_list(Node),
	    PrefixElems = refac_syntax:list_prefix(Node1),
	    Sep = get_separator(PrefixElems, Ctxt, ","),
	    D0 = seq(PrefixElems, floating(text(Sep)), Ctxt1, fun lay/2),
	    D1 = lay_elems(fun refac_prettypr_0:par/1, D0, PrefixElems, Ctxt1),
	    case refac_syntax:list_suffix(Node1) of
		none -> 
		    beside(floating(text("[")),beside(D1, floating(text("]"))));
		S ->
		    D2 = lay(S, Ctxt1),
		    {PrefixStart, PrefixEnd} = get_start_end_loc(PrefixElems),
		    {SuffixStart, SuffixEnd} = get_start_end_loc(S),
		    {BarLn, BarCol} = get_keyword_loc_before('|', Ctxt1, SuffixStart),
		    BarD2=append_elems(fun refac_prettypr_0:horizontal/1, 
				       {text("|"), {{BarLn, BarCol}, {BarLn, BarCol}}},
				       {D2, {SuffixStart, SuffixEnd}}),
		    D1BarD2=append_elems(fun refac_prettypr_0:par/1, {D1, {PrefixStart, PrefixEnd}},
					 {BarD2,{{BarLn, BarCol}, SuffixEnd}}),
		    beside(floating(text("[")), beside(D1BarD2,floating(text("]"))))
		end;
	operator ->
	    Op = refac_syntax:operator_literal(Node),
            floating(text(Op));
        infix_expr -> 
	    %% done;
            Left = refac_syntax:infix_expr_left(Node),
	    Operator = refac_syntax:infix_expr_operator(Node),
	    Right = refac_syntax:infix_expr_right(Node),
	    {PrecL, Prec, PrecR} = case refac_syntax:type(Operator) of
				       operator ->
					   inop_prec(refac_syntax:operator_name(Operator));
				       _ -> {0, 0, 0}
				   end,
	    D1 = maybe_parentheses_1(lay(Left, set_prec(Ctxt, PrecL)), Left, Ctxt),
	    D2 = lay(Operator, reset_prec(Ctxt)),
	    D3 = maybe_parentheses_1(lay(Right, set_prec(Ctxt, PrecR)), Right, Ctxt),
	    {{_LeftStartLn, LeftStartCol},{LeftEndLn, _LeftEndCol}} = get_start_end_loc(Left),
	    {OpStartLn, OpStartCol} = get_start_loc_with_comment(Operator),
	    {RightStartLn, RightStartCol} = get_start_loc_with_comment(Right),
	    D12 = case OpStartLn == LeftEndLn andalso OpStartLn =/= 0 of
		      true -> refac_prettypr_0:horizontal([D1, D2]);
		      false when OpStartLn-LeftEndLn==1 ->
                          above(D1, nest(OpStartCol-LeftStartCol-1, D2));
		      _ -> par([D1, D2], Ctxt#ctxt.sub_indent)
		  end,
            D4 = case OpStartLn == RightStartLn andalso OpStartLn =/= 0 of
		     true ->
			 refac_prettypr_0:horizontal([D12, D3]);
		     false when RightStartLn-OpStartLn==1 ->
                         above(D12, nest(RightStartCol-LeftStartCol, D3));
		     _ ->
			 par([D12, D3], Ctxt#ctxt.sub_indent)
		 end,
	    maybe_parentheses(D4, Prec, Ctxt);
	prefix_expr ->  
	    %% done;
	    Operator = refac_syntax:prefix_expr_operator(Node),
	    PrefixArg = refac_syntax:prefix_expr_argument(Node),
	    {{Prec, PrecR}, Name} = case refac_syntax:type(Operator) of
					operator ->
					    N = refac_syntax:operator_name(Operator),
					    {preop_prec(N), N};
					_ -> {{0, 0}, any}
				    end,
	    D1 = lay(Operator, reset_prec(Ctxt)),
	    D2 = maybe_parentheses_1(lay(PrefixArg, set_prec(Ctxt, PrecR)),PrefixArg, Ctxt),
	    {OpEndLn, OpEndCol} = get_end_loc_with_comment(Operator),
	    {ArgStartLn, ArgStartCol} = get_start_loc_with_comment(PrefixArg),
	    D3=case ArgStartLn-OpEndLn==0 andalso ArgStartLn/=0 of 
		   true -> 
		       S=text(empty_str(ArgStartCol-OpEndCol-1)),
		       beside(beside(D1, S), D2);
		   false when  ArgStartLn-OpEndLn==1 ->
		       above(D1, nest(ArgStartCol-OpEndCol, D2));
		   _ ->
		       case Name of
			   '+' -> beside(D1, D2);
			   '-' -> beside(D1, D2);
			   _ ->
			       par([D1, D2], Ctxt#ctxt.sub_indent)
		       end
	       end,
    	    maybe_parentheses(D3, Prec, Ctxt);
	application ->  
	    %% done.
	    {PrecL, Prec} = func_prec(),
	    D = lay(refac_syntax:application_operator(Node), set_prec(Ctxt, PrecL)),
	    Args = refac_syntax:application_arguments(Node),
	    Sep = get_separator(Args, Ctxt, ","),
	    As = seq(Args, floating(text(Sep)), reset_prec(Ctxt), fun lay/2),
            Op = refac_syntax:application_operator(Node),
	    D1 = case Args of
		     [] ->
			 beside(D, beside(text("("), text(")")));
		     [_H| _] ->
			 ArgsD=lay_elems(fun refac_prettypr_0:par/1,As, Args, Ctxt),
                         {OpStartLoc,OpEndLoc}=get_start_end_loc(Op),
			 ArgsD1=make_args(ArgsD, Ctxt, OpEndLoc),
                         LeftBracketLoc=get_keyword_loc_after('(', Ctxt, OpEndLoc),
                         append_elems(fun horizontal/1,
                                      {D, {OpStartLoc, OpEndLoc}}, {ArgsD1, {LeftBracketLoc, LeftBracketLoc}})
                 end,
            maybe_parentheses(D1, Prec, Ctxt);
	match_expr ->    
	    %% Done;
	    {PrecL, Prec, PrecR} = inop_prec('='),
	    Left = refac_syntax:match_expr_pattern(Node),
	    Right = refac_syntax:match_expr_body(Node),
	    D1 = lay(refac_syntax:match_expr_pattern(Node), set_prec(Ctxt, PrecL)),
	    D2 = lay(refac_syntax:match_expr_body(Node), set_prec(Ctxt, PrecR)),
	    {LStart, LEnd} = get_start_end_loc(Left),
	    {RStart, REnd} = get_start_end_loc(Right),
            EqLoc = get_keyword_loc_after('=',Ctxt, LEnd),
            LeftEq=append_elems(fun refac_prettypr_0:horizontal/1, 
				{D1, {LStart, LEnd}}, {text("="), {EqLoc, EqLoc}}),
            D3=append_elems(fun ([Doc1,Doc2]) ->
				    follow(Doc1, Doc2, Ctxt#ctxt.break_indent)
			    end, {LeftEq, {LStart, EqLoc}},
			    {D2,{RStart, REnd}}),
	    maybe_parentheses(D3, Prec, Ctxt);
	underscore ->
	    text("_");
	clause ->  
	    %% Done;
	    %% The style used for a clause depends on its context
	    Ctxt1 = (reset_prec(Ctxt))#ctxt{clause = undefined},
	    Pats = refac_syntax:clause_patterns(Node),
	    Body = refac_syntax:clause_body(Node),
	    Sep = get_separator(Pats, Ctxt, ","),
	    PatDocs = seq(Pats, floating(text(Sep)), Ctxt1, fun lay/2),
	    D1 = lay_elems(fun refac_prettypr_0:par/1, PatDocs, Pats, Ctxt),
	    Guard=refac_syntax:clause_guard(Node),
	    D2 = case Guard of
		     none -> none;
		     G -> lay(G, Ctxt1)
		 end,
	    BodyDocs = seq(Body, floating(text(",")), Ctxt1, fun lay/2),
	    D3 = lay_body_elems(fun vertical/1, BodyDocs, Body, Ctxt1),
	    HeadLastLn = case refac_syntax:clause_guard(Node) of
			     none -> case Pats of
					 [] -> get_start_line_with_comment(Node);
					 _ -> case get_end_line_with_comment(Pats) of
						  0 -> get_start_line_with_comment(Node);
						  L -> L
					      end
				     end;
			     _ -> get_end_line_with_comment(refac_syntax:clause_guard(Node))
			 end,
	    HeadStartLoc={_,HeadStartCol} = get_start_loc(Node),
            HdB=hd(Body),
            {_BodyStartLn, BodyStartCol} = get_start_loc(HdB),
            {BodyStartLn, _BodyStartCol} = get_start_loc_with_comment(HdB),
            BodyStartLoc={BodyStartLn, BodyStartCol},
	    SameLine = {BodyStartLoc, {HeadStartCol, HeadLastLn}},
            case Ctxt#ctxt.clause of
		fun_expr -> 
		    make_fun_clause(D1, D2, D3, Node, Ctxt, SameLine, HeadStartLoc);
		{function, N} -> 
		    make_fun_clause(N, D1, D2, D3, Node,Ctxt, SameLine, HeadStartLoc);
		if_expr -> 
		    make_if_clause(D1, D2, D3, Ctxt, SameLine);
		cond_expr -> 
		    make_if_clause(D1, D2, D3, Ctxt, SameLine);
		case_expr ->
		    make_case_clause(D1, D2, D3, Node,Ctxt, SameLine);
		receive_expr -> 
		    make_case_clause(D1, D2, D3, Node,Ctxt, SameLine);
		try_expr -> 
		    make_case_clause(D1, D2, D3, Node, Ctxt, SameLine);
		{rule, N} ->
		    make_rule_clause(N, D1, D2, D3, Ctxt, SameLine);
		undefined ->
		    %% If a clause is formatted out of context, we
		    %% use a "fun-expression" clause style.
		    make_fun_clause(D1, D2, D3, Guard, Ctxt, SameLine, HeadStartLoc)
	    end;
	function ->  
	    %% Comments on the name itself will be repeated for each
	    %% clause, but that seems to be the best way to handle it.
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(refac_syntax:function_name(Node), Ctxt1),
	    D2 = lay_clauses(refac_syntax:function_clauses(Node), {function, D1}, Ctxt1),
	    beside(D2, floating(text(".")));
	case_expr -> 
	    %% done;
	    Ctxt1 = reset_prec(Ctxt),
	    Arg = refac_syntax:case_expr_argument(Node),
	    D1 = lay(Arg, Ctxt1),
	    Cs = refac_syntax:case_expr_clauses(Node),
	    D2 = lay_clauses(Cs, case_expr, Ctxt1),
            {CsStartLn, CsStartCol} = get_start_loc_with_comment(hd(Cs)),
            {CaseStartLine, CaseStartCol} = get_start_loc(Node),
            {{ArgStartLine, ArgStartCol}, ArgEndLoc={ArgEndLine, _ArgEndCol}} = get_start_end_loc(Arg),
            CaseArgD=case CaseStartLine==ArgStartLine orelse ArgStartLine==0 
	         	 orelse CaseStartLine==0 of 
	         	 true when CaseStartLine==0 ->
	         	     refac_prettypr_0:horizontal([text("case"), D1]);
                         true ->
                             P = ArgStartCol-CaseStartCol-4,
                             horizontal([text("case"), text(spaces(P)), D1]);
	         	 false ->
	         	     above(text("case"),nest(ArgStartCol-CaseStartCol, D1))
	              end,
	    {OfStartLn, OfStartCol} = get_keyword_loc_after('of', Ctxt, ArgEndLoc),
            CaseArgOfD = case OfStartLn -ArgEndLine==1 of 
	         	     true ->
	         		 above(CaseArgD, nest(OfStartCol-CaseStartCol, text("of")));
	         	     false ->
                                 refac_prettypr_0:horizontal([CaseArgD, text("of")])
	         	 end,
            CaseArgOfCsD=case OfStartLn==CsStartLn andalso OfStartLn/=0 of 
                             true ->
                                 P1 = CsStartCol - OfStartCol -2,
                                 horizontal([CaseArgOfD, text(spaces(P1)), D2]);
                             false->
                                 sep([CaseArgOfD, nest(CsStartCol-CaseStartCol, D2)])
                         end,
            CsEndLoc={CsEndLn, _CsEndCol} = get_end_loc_with_comment(lists:last(Cs)),
            {EndStartLn, _EndStartCol} = get_keyword_loc_after("and", Ctxt, CsEndLoc),
            case CsEndLn == EndStartLn andalso EndStartLn/=0 of
                true ->
                    refac_prettypr_0:horizontal(CaseArgOfCsD, text("end"));
                false ->
                    sep([CaseArgOfCsD, text("end")])
            end;
       	if_expr ->  
	    %% Done
	    Ctxt1 = reset_prec(Ctxt),
	    Cs=refac_syntax:if_expr_clauses(Node),
	    D = lay_clauses(Cs, if_expr, Ctxt1),
            append_keywords("if", "end", D, Cs, Ctxt1);
	cond_expr ->  
	    %% Done;
	    Ctxt1 = reset_prec(Ctxt),
	    Cs=refac_syntax:cond_expr_clauses(Node),
	    D = lay_clauses(Cs, cond_expr, Ctxt1),
	    append_keywords("cond", "end", D, Cs, Ctxt1);
	fun_expr ->  
	    %% Done;
	    Ctxt1 = reset_prec(Ctxt),
	    Cs=refac_syntax:fun_expr_clauses(Node),
	    D = lay_clauses(Cs, fun_expr, Ctxt1),
	    append_keywords("fun", "end", D, Cs, Ctxt1);
	module_qualifier -> 
	    %% Done;
	    {PrecL, _Prec, PrecR} = inop_prec(':'),
	    D1 = lay(refac_syntax:module_qualifier_argument(Node), set_prec(Ctxt, PrecL)),
	    D2 = lay(refac_syntax:module_qualifier_body(Node), set_prec(Ctxt, PrecR)),
	    beside(D1, beside(text(":"), D2));
	qualified_name ->  
	    %% Done;
	    Ss = refac_syntax:qualified_name_segments(Node),
	    lay_qualified_name(Ss, Ctxt);
	arity_qualifier ->  
	    %% Done;
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(refac_syntax:arity_qualifier_body(Node), Ctxt1),
	    D2 = lay(refac_syntax:arity_qualifier_argument(Node), Ctxt1),
	    beside(D1, beside(text("/"), D2));
	attribute -> 
	    %% Done;
	    %% The attribute name and arguments are formatted similar to
	    %% a function call, but prefixed with a "-" and followed by
	    %% a period. If the arguments is `none', we only output the
	    %% attribute name, without following parentheses.
	    Ctxt1 = reset_prec(Ctxt),
	    N = refac_syntax:attribute_name(Node),
	    case refac_syntax:attribute_arguments(Node) of
		none -> 
		    D =lay(N, Ctxt1),
		    beside(floating(text("-")), beside(D, floating(text("."))));
		Args ->
		    Sep = get_separator(Args, Ctxt1, ","),
                    As = seq(Args, floating(text(Sep)), Ctxt1, fun lay/2),
                    D = lay_elems(fun refac_prettypr_0:par/1, As, Args, Ctxt),
                    D2=beside(lay(N, Ctxt1), beside(text("("), beside(D, floating(text(")"))))),
		    beside(floating(text("-")), beside(D2, floating(text("."))))
	    end;
	binary ->   
	    %% Done
	    Ctxt1 = reset_prec(Ctxt),
	    Fields = refac_syntax:binary_fields(Node),
	    Sep = get_separator(Fields, Ctxt, ","),
	    Es = seq(Fields, floating(text(Sep)), Ctxt1, fun lay/2),
	    D = lay_elems(fun refac_prettypr_0:par/1, Es, Fields, Ctxt),
	    beside(floating(text("<<")), beside(D, floating(text(">>"))));
	binary_field ->
	    %% To test!
	    Ctxt1 = reset_prec(Ctxt),
	    D1 = lay(refac_syntax:binary_field_body(Node), Ctxt1),
	    D2 = case refac_syntax:binary_field_types(Node) of
		     [] -> 
			 empty();
		     Ts ->
			 beside(floating(text("/")), lay_bit_types(Ts, Ctxt1))
		 end,
	    beside(D1, D2);
	block_expr -> 
	    %% Done;
	    Ctxt1 = reset_prec(Ctxt),
	    Body = refac_syntax:block_expr_body(Node),
	    Es = seq(Body, floating(text(", ")), Ctxt1, fun lay/2),
	    D=lay_body_elems(fun refac_prettypr_0:sep/1, Es, Body, Ctxt1),
	    append_keywords("begin", "end", D, Body, Ctxt1);
	catch_expr ->  
		%% Done;
		{Prec, PrecR} = preop_prec('catch'),
		Body =refac_syntax:catch_expr_body(Node),
		D = lay(Body, set_prec(Ctxt, PrecR)),
		D1 = append_leading_keyword("catch", D, Body, Ctxt),
		maybe_parentheses(D1, Prec, Ctxt);
	class_qualifier -> 
		%% Done;
		Ctxt1 = set_prec(Ctxt, max_prec()),
		D1 = lay(refac_syntax:class_qualifier_argument(Node), Ctxt1),
		D2 = lay(refac_syntax:class_qualifier_body(Node), Ctxt1),
		beside(D1, beside(text(":"), D2));
	comment ->
		D = stack_comment_lines(refac_syntax:comment_text(Node)),
		%% Default padding for standalone comments is empty.
		case refac_syntax:comment_padding(Node) of
		    none -> floating(break(D));
		    P -> floating(break(beside(text(spaces(P)), D)))
		end;
	conjunction -> 
		%% Done;
		Body = refac_syntax:conjunction_body(Node),
		Sep = get_separator(Body, Ctxt, ","),
		Es = seq(Body, floating(text(Sep)), reset_prec(Ctxt), fun lay/2),
		lay_elems(fun refac_prettypr_0:par/1, Es, Body, Ctxt);
	disjunction -> 
		%% Done;
		%% For clarity, we don't paragraph-format
		%% disjunctions; only conjunctions (see above).
		Body = refac_syntax:disjunction_body(Node),
		Es = seq(Body, floating(text(";")), reset_prec(Ctxt), fun lay/2),
		lay_elems(fun refac_prettypr_0:sep/1, Es, Body, Ctxt);
	error_marker -> 
		%% Done;
		E = refac_syntax:error_marker_info(Node),
		beside(text("** "), beside(lay_error_info(E, reset_prec(Ctxt)), text(" **")));
	eof_marker ->
		empty();
	form_list ->
		%% Done.
		Forms = refac_syntax:form_list_elements(Node),
		Es = seq(Forms, none, reset_prec(Ctxt), fun lay/2),
		vertical_sep(text(""), Es);
	generator -> 
		%% Done;
		Pat = refac_syntax:generator_pattern(Node),
		Body = refac_syntax:generator_body(Node),
		lay_generator(Ctxt, Pat, Body);
	binary_generator ->
		%%Done.
		Pat = refac_syntax:binary_generator_pattern(Node),
		Body = efac_syntax:binary_generator_body(Node),
		lay_generator(Ctxt, Pat, Body);
	implicit_fun -> 
		%%Done;
		D = lay(refac_syntax:implicit_fun_name(Node), reset_prec(Ctxt)),
		beside(floating(text("fun ")), D);
	list_comp ->  
		%% Done
		Ctxt1 = reset_prec(Ctxt),
		Temp = refac_syntax:list_comp_template(Node),
		D1 = lay(Temp, Ctxt1),
		Body = refac_syntax:list_comp_body(Node),
		Sep = get_separator(Body, Ctxt, ","),
		Es = seq(Body, floating(text(Sep)), Ctxt1, fun lay/2),
		D2 = lay_elems(fun refac_prettypr_0:par/1, Es, Body, Ctxt),
		{TempStart, TempEnd} = get_start_end_loc(Temp),
		{BodyStart, BodyEnd} = get_start_end_loc(Body),
		{BarLn, BarCol} = get_keyword_loc_before('||', Ctxt1, BodyStart),
		BarD2=append_elems(fun refac_prettypr_0:horizontal/1, 
				   {text("||"), {{BarLn, BarCol}, {BarLn, BarCol+1}}},
				   {D2, {BodyStart, BodyEnd}}),
		D1BarD2=append_elems(fun refac_prettypr_0:par/1, {D1, {TempStart, TempEnd}},
				     {BarD2,{{BarLn, BarCol}, BodyEnd}}),
		beside(floating(text("[")), beside(D1BarD2,floating(text("]"))));
	 binary_comp ->
		%% Done;
		Ctxt1 = reset_prec(Ctxt),
		Temp =refac_syntax:binary_comp_template(Node),
		D1 = lay(Temp, Ctxt1),
		Body = refac_syntax:binary_comp_body(Node),
		Sep = get_separator(Body, Ctxt, ","),
		Es = seq(Body,floating(text(Sep)), Ctxt1, fun lay/2),
		D2 =lay_elems(fun refac_prettypr_0:par/1, Es, Body, Ctxt),
		{TempStart, TempEnd} = get_start_end_loc(Temp),
		{BodyStart, BodyEnd} = get_start_end_loc(Body),
		{BarLn, BarCol} = get_keyword_loc_before('||', Ctxt1, BodyStart),
		BarD2=append_elems(fun refac_prettypr_0:horizontal/1, 
				   {text("||"), {{BarLn, BarCol}, {BarLn, BarCol+1}}},
				   {D2, {BodyStart, BodyEnd}}),
		D1BarD2=append_elems(fun refac_prettypr_0:par/1, {D1, {TempStart, TempEnd}},
				     {BarD2,{{BarLn, BarCol}, BodyEnd}}),
		
		beside(floating(text("<< ")), beside(D1BarD2,floating(text(" >>"))));
	  macro ->  
		%%Done;
		%% This is formatted similar to a normal function call, but
		%% prefixed with a "?".
                Ctxt1 = reset_prec(Ctxt),
		N = refac_syntax:macro_name(Node),
		Args = refac_syntax:macro_arguments(Node),
		Sep = get_separator(Args, Ctxt, ","),
                D = case Args of
                        none ->
                            lay(N, Ctxt1);
                        [] -> beside(lay(N, Ctxt1), text("()"));
                        _->
                            As = seq(Args, floating(text(Sep)), reset_prec(Ctxt), fun lay/2),
			    ArgsD=lay_elems(fun refac_prettypr_0:par/1, As, Args, Ctxt),
			  OpEndLoc = get_end_loc_with_comment(N),
			  ArgsD1=make_args(ArgsD, Ctxt1, OpEndLoc),
			  beside(lay(N, Ctxt1),ArgsD1)
                  end,
	      D1 = beside(floating(text("?")), D),
	      case lists:keysearch(with_bracket, 1, refac_syntax:get_ann(Node)) of
		  {value, {with_bracket, true}} ->
		      lay_parentheses(D1, Ctxt);
		  _ -> D1
	      end;
	parentheses ->
	   %% Done;
	   D = lay(refac_syntax:parentheses_body(Node), reset_prec(Ctxt)),
	   lay_parentheses(D, Ctxt);
	query_expr ->
	   %% Done;
	   Ctxt1 = reset_prec(Ctxt),
	   Body = refac_syntax:query_expr_body(Node),
	   D = lay(Body, Ctxt1),
	   append_keywords("query", "end", D, Body, Ctxt1);
	receive_expr ->
	   %% Done;
	   Ctxt1 = reset_prec(Ctxt),
	   Cs=refac_syntax:receive_expr_clauses(Node),
	   D1 = lay_clauses(Cs, receive_expr, Ctxt1),
	   case refac_syntax:receive_expr_timeout(Node) of
	       none ->
		   append_keywords("receive", "end", D1, Cs, Ctxt1);
	       T ->
		   D3 = lay(T, Ctxt1),
		   A = refac_syntax:receive_expr_action(Node),
		   As=seq(A, floating(text(", ")), Ctxt1, fun lay/2),
		   D4 = lay_elems(fun refac_prettypr_0:sep/1, As, A, Ctxt),
		   AStartLoc=get_start_loc_with_comment(hd(A)),
		   {{_, HeadStartCol}, {HeadLastLn, _}} = get_start_end_loc_with_comment(T),
		   D5 =append_clause_body(D4, D3, Ctxt1, {AStartLoc, {HeadStartCol, HeadLastLn}}),
		   D2= append_leading_keyword("receive", D1, Cs, Ctxt1),
		   D6 = append_keywords("after", "end", D5, [T|A], Ctxt1),
		   sep([D2, D6])
	   end;
	record_access ->
	   {PrecL, Prec, PrecR} = inop_prec('#'),
	   D1 = lay(refac_syntax:record_access_argument(Node), set_prec(Ctxt, PrecL)),
	   D2 = beside(floating(text(".")), lay(refac_syntax:record_access_field(Node), set_prec(Ctxt, PrecR))),
	   D3 = case refac_syntax:record_access_type(Node) of
		    none -> D2;
		    T ->
			beside(beside(floating(text("#")), lay(T, reset_prec(Ctxt))), D2)
		end,
	   maybe_parentheses(beside(D1, D3), Prec, Ctxt);
	record_expr ->
	   %% done;
	   {PrecL, Prec, _} = inop_prec('#'),
	   Ctxt1 = reset_prec(Ctxt),
	   D1 = lay(refac_syntax:record_expr_type(Node), Ctxt1),
	   Fields = refac_syntax:record_expr_fields(Node),
	   Sep = get_separator(Fields, Ctxt, ","),
	   Fs =seq(refac_syntax:record_expr_fields(Node), floating(text(Sep)), Ctxt1, fun lay/2),
	   D2 = lay_elems(fun refac_prettypr_0:par/1, Fs, Fields, Ctxt),
	   D3 = beside(beside(floating(text("#")), D1), beside(text("{"), beside(D2, floating(text("}"))))),
	   D4 = case refac_syntax:record_expr_argument(Node) of
		    none -> D3;
		    A -> beside(lay(A, set_prec(Ctxt, PrecL)), D3)
		end,
	   maybe_parentheses(D4, Prec, Ctxt);
	record_field ->
	   %% done;
	   Ctxt1 = reset_prec(Ctxt),
	   D1 = lay(refac_syntax:record_field_name(Node), Ctxt1),
	   case refac_syntax:record_field_value(Node) of
	       none -> D1;
	       V -> D2 = lay(V, Ctxt1),
                    {LStart, LEnd} = get_start_end_loc(refac_syntax:record_field_name(Node)),
                    {RStart, REnd} = get_start_end_loc(V),
                    EqLoc = get_keyword_loc_after('=', Ctxt, LEnd),
                    LeftEq=append_elems(fun refac_prettypr_0:horizontal/1,
                                        {D1, {LStart, LEnd}}, {text("="), {EqLoc, EqLoc}}),
                    append_elems(fun ([Doc1,Doc2]) ->
                                        follow(Doc1, Doc2, Ctxt#ctxt.break_indent)
                                end, {LeftEq, {LStart, EqLoc}},
                                {D2,{RStart, REnd}})
           end;
	record_index_expr ->
	   %% done
	   {Prec, PrecR} = preop_prec('#'),
	   D1 = lay(refac_syntax:record_index_expr_type(Node), reset_prec(Ctxt)),
	   D2 = lay(refac_syntax:record_index_expr_field(Node), set_prec(Ctxt, PrecR)),
	   D3 = beside(beside(floating(text("#")), D1), beside(floating(text(".")), D2)),
	   maybe_parentheses(D3, Prec, Ctxt);
	rule ->
	   %% done.
	   %% Comments on the name will be repeated; cf.
	   %% `function'.
	   Ctxt1 = reset_prec(Ctxt),
	   D1 = lay(refac_syntax:rule_name(Node), Ctxt1),
	   D2 = lay_clauses(refac_syntax:rule_clauses(Node), {rule, D1}, Ctxt1),
	   beside(D2, floating(text(".")));
	size_qualifier ->
	   %%done;
	   Ctxt1 = set_prec(Ctxt, max_prec()),
	   Body = refac_syntax:size_qualifier_body(Node),
	   D1 = case refac_syntax:type(Body) == variable orelse
		    refac_syntax:type(Body) == underscore orelse
		    refac_syntax:is_literal(Body) == true of
		    true -> lay(Body, Ctxt1);
		    false ->
			case refac_syntax:type(Body) of
			    macro -> case lists:keysearch(with_bracket, 1, refac_syntax:get_ann(Body)) of
					 {value, {with_bracket, true}} ->
					     beside(floating(text("(")), beside(lay(Body, Ctxt1), floating(text(")"))));
					 _ -> lay(Body, Ctxt1)
				     end;
			    _ -> beside(floating(text("(")), beside(lay(Body, Ctxt1), floating(text(")"))))
			end
		end,
	   D2 = lay(refac_syntax:size_qualifier_argument(Node), Ctxt1),
	   beside(D1, beside(text(":"), D2));
	try_expr ->
	   Ctxt1 = reset_prec(Ctxt),
	   Body = refac_syntax:try_expr_body(Node),
	   {BodyStart, BodyEnd} = get_start_end_loc(Body),
	   TryLoc = get_keyword_loc_before('try', Ctxt1, BodyStart),
	   Bs =seq(Body,floating(text(",")),Ctxt1,fun lay/2),
	   D1 = lay_body_elems(fun refac_prettypr_0:sep/1, Bs, Body, Ctxt1),
	   {_NodeStart, NodeEnd} = get_start_end_loc(Node),
	   EndLoc = get_keyword_loc_before('end', Ctxt1, NodeEnd),
	   Es0 = [{text("end"), {EndLoc, NodeEnd}}],
	   Es1 = {append_leading_keyword("try", D1, Body, Ctxt1), {TryLoc, BodyEnd}},
	   Es2 = case refac_syntax:try_expr_after(Node) of
		     [] -> Es0;
		     As ->
			 AsDocs= seq(As,floating(text(",")),Ctxt1,fun lay/2),
			 D2 = lay_elems(fun refac_prettypr_0:sep/1, AsDocs, As, Ctxt),
			 {AsStart, AsEnd} = get_start_end_loc(As),
			 AfterLoc=get_keyword_loc_before('after', Ctxt1, AsStart),
			 [{append_leading_keyword("after", D2, As, Ctxt1), {AfterLoc, AsEnd}}
			  |Es0]
		 end,
	   Es3 = case refac_syntax:try_expr_handlers(Node) of
		     [] -> Es2;
		     Hs ->
			 D3 = lay_clauses(Hs,try_expr,Ctxt1),
			 {HsStart, HsEnd} = get_start_end_loc(Hs),
			 CatchLoc = get_keyword_loc_before('catch', Ctxt1, HsStart),
			 [{append_leading_keyword("catch", D3, Hs, Ctxt1), {CatchLoc, HsEnd}}|Es2]
		 end,
	   case refac_syntax:try_expr_clauses(Node) of
	       [] ->
		   lay_body_elems_1(fun refac_prettypr_0:sep/1,[Es1|Es3],Ctxt1,[],{{1,1},{1,1}});
	       Cs ->
		   D4 = lay_clauses(Cs, try_expr, Ctxt1),
		   {CsStart, CsEnd} = get_start_end_loc(Cs),
		   OfLoc=get_keyword_loc_before('of', Ctxt1, CsStart),
		   Es4 = {append_leading_keyword("of", D4, Cs, Ctxt1),{OfLoc, CsEnd}},
                   lay_body_elems_1(fun refac_prettypr_0:sep/1, [Es1,Es4|Es3], Ctxt1, [], {{1,1},{1,1}})
	   end;
	char ->
	   V = refac_syntax:char_value(Node),
	   case is_integer(V) and (V > 127) of
	       true -> {ok, [Num], _} = io_lib:fread("~u", integer_to_list(V)),
		       [CharStr] = io_lib:fwrite("~.8B", [Num]),
		       text("$\\" ++ CharStr);
	       _ when is_atom(V) ->
		      text(atom_to_list(V));
	       _ -> text(refac_syntax:char_literal(Node))
	   end; %% "
       warning_marker ->
           E = refac_syntax:warning_marker_info(Node),
          beside(text("%% WARNING: "), lay_error_info(E, reset_prec(Ctxt)));
	type -> empty();  %% tempory fix!!
      	typed_record_field -> empty() %% tempory fix!!!
    end.

lay_generator(Ctxt, Pat, Body) ->
    Ctxt1 = reset_prec(Ctxt),
    D1 = lay(Pat,Ctxt1),
    D2 = lay(Body,Ctxt1),
    D1EndLn = get_end_line_with_comment(Pat),
    D2StartLn = get_start_line_with_comment(Body),
    case (D1EndLn==0) or (D2StartLn==0) of
	true ->
	    par([D1,beside(text("<- "),D2)],Ctxt1#ctxt.break_indent);
	_ ->
	    case D2StartLn-D1EndLn of
		0 -> beside(D1,beside(text(" <- "),D2));
		1 -> above(D1,nest(Ctxt#ctxt.break_indent,beside(text("<- "),D2)));
		_ -> par([D1,beside(text("<- "),D2)],Ctxt1#ctxt.break_indent)
	    end
    end.


lay_parentheses(D,_Ctxt) ->
    beside(floating(text("(")),beside(D,floating(text(")")))).

maybe_parentheses(D,Prec,Ctxt) ->
    case Ctxt#ctxt.prec of
      P when P > Prec -> lay_parentheses(D,Ctxt);
        _ -> D 
             %%maybe_parentheses_1(D, Node, Ctxt)
    end.

maybe_parentheses_1(D, Node, Ctxt) ->
    Str=refac_prettypr_0:format(D),
    case Str=="" of 
	true -> 
	    D;
	false ->
	    case hd(Str)==$\( andalso lists:last(Str)==$\) of 
		true ->
		    D;
		false ->
		    case has_parentheses(Node, Ctxt#ctxt.tokens) of 
			true ->		
			    lay_parentheses(D, Ctxt); 
			false ->
			    D
		    end
	    end
    end.
lay_qualified_name([S| Ss1] = Ss,Ctxt) ->
    case refac_syntax:type(S) of
      atom ->
	  case refac_syntax:atom_value(S) of
	    '' -> beside(text("."),lay_qualified_name_1(Ss1,Ctxt));
	    _ -> lay_qualified_name_1(Ss,Ctxt)
	  end;
      _ -> lay_qualified_name_1(Ss,Ctxt)
    end.

lay_qualified_name_1([S],Ctxt) -> lay(S,Ctxt);
lay_qualified_name_1([S| Ss],Ctxt) ->
    beside(lay(S,Ctxt),
	   beside(text("."),lay_qualified_name_1(Ss,Ctxt))).

%% lay_string/1 defined by Huiqing Li;
lay_string([]) -> empty();
lay_string([{string,_,Str}]) ->
    text("\""++Str++"\"");
lay_string([{string,_,Str}| Ts]) ->
    case Ts of
      [] -> text("\""++Str++"\"");
      _ ->
	  above(text("\""++Str++"\""),lay_string(Ts))
    end.

lay_string(S,Ctxt) ->
    %% S includes leading/trailing double-quote characters. The segment
    %% width is 2/3 of the ribbon width - this seems to work well.
    W = Ctxt#ctxt.ribbon * 2 div 3,
    lay_string_1(S,length(S),W).

lay_string_1(S,L,W) when L > W, W > 0 ->
    %% Note that L is the minimum, not the exact, printed length.
    case split_string(S,W - 1,L) of
      {_S1,""} -> text(S);
      {S1,S2} ->
	  above(text(S1 ++ "\""),lay_string_1([$"| S2],L - W +
							 1,W))
    end;
lay_string_1(S,_L,_W) -> text(S).

split_string(Xs,N,L) -> split_string_1(Xs,N,L,[]).

%% We only split strings at whitespace, if possible. We must make sure
%% we do not split an escape sequence.


split_string_1([$\s| Xs],N,L,As) when N =< 0, L >= 5 ->
    {lists:reverse([$\s| As]),Xs};
split_string_1([$\t| Xs],N,L,As) when N =< 0, L >= 5 ->
    {lists:reverse([$t,$\\| As]),Xs};
split_string_1([$\n| Xs],N,L,As) when N =< 0, L >= 5 ->
    {lists:reverse([$n,$\\| As]),Xs};
split_string_1([$\\| Xs],N,L,As) ->
    split_string_2(Xs,N - 1,L - 1,[$\\| As]);
split_string_1(Xs,N,L,As) when N =< -10, L >= 5 ->
    {lists:reverse(As),Xs};
split_string_1([X| Xs],N,L,As) ->
    split_string_1(Xs,N - 1,L - 1,[X| As]);
split_string_1([],_N,_L,As) -> {lists:reverse(As),""}.

split_string_2([$^,X| Xs],N,L,As) ->
    split_string_1(Xs,N - 2,L - 2,[X,$^| As]);
split_string_2([X1,X2,X3| Xs],N,L,As)
    when X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7, X3 >= $0,
	 X3 =< $7 ->
    split_string_1(Xs,N - 3,L - 3,[X3,X2,X1| As]);
split_string_2([X1,X2| Xs],N,L,As)
    when X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7 ->
    split_string_1(Xs,N - 2,L - 2,[X2,X1| As]);
split_string_2([X| Xs],N,L,As) ->
    split_string_1(Xs,N - 1,L - 1,[X| As]).

%% Note that there is nothing in `lay_clauses' that actually requires
%% that the elements have type `clause'; it just sets up the proper
%% context and arranges the elements suitably for clauses.

lay_clauses(Cs,Type,Ctxt) ->   %%done.
    CsDocs = seq(Cs,floating(text(";")),
		 Ctxt#ctxt{clause = Type},fun lay/2),
    lay_body_elems(fun vertical/1, CsDocs, Cs, Ctxt).
    

%% Note that for the clause-making functions, the guard argument
%% can be `none', which has different interpretations in different
%% contexts.


make_fun_clause(P,G,B, CsNode, Ctxt,SameLine, HeadStartLoc) ->
    make_fun_clause(none,P,G,B,CsNode, Ctxt,SameLine, HeadStartLoc).

make_fun_clause(N,P,G,B, CsNode, Ctxt, SameLine, HeadStartLoc={_Ln, _Col}) ->
    D = make_fun_clause_head(N,P,Ctxt, HeadStartLoc),
    make_case_clause(D,G,B,CsNode,Ctxt,SameLine).
    

make_fun_clause_head(N,P,Ctxt,FunNameLoc = {StartLine, StartCol}) ->
    D =make_args(P,Ctxt,FunNameLoc),
    {LeftBracketLine,LeftBracketCol} = get_keyword_loc_after('(',Ctxt,FunNameLoc),
    if N == none -> D;
       true ->
            case LeftBracketLine==StartLine of
                true ->
                    beside(N,D);
                false ->
                    above(N,nest(LeftBracketCol-StartCol,D))
            end
    end.

make_args(P,Ctxt,FunNameLoc) ->
    {LeftBracketLine,LeftBracketCol} = 
	get_keyword_loc_after('(',Ctxt,FunNameLoc),
    {PatStartLine,PatStartCol} = 
	next_token_loc(Ctxt#ctxt.tokens,{LeftBracketLine,LeftBracketCol}),
    {RightBracketLine,RightBracketCol} =
	get_right_bracket_loc(Ctxt#ctxt.tokens,{LeftBracketLine,LeftBracketCol}),
    {PatEndLine,_PatEndCol} = 
	prev_token_loc(Ctxt#ctxt.tokens,{RightBracketLine,RightBracketCol}),
    D0 = case {RightBracketLine,RightBracketCol}=={0,0} orelse 
	     RightBracketLine==PatEndLine
	 of
	     true ->
		 beside(P,floating(text(")")));
	     _ ->
		 above(P,nest(RightBracketCol-PatStartCol,text(")")))
	 end,
    case {LeftBracketLine,LeftBracketCol}=={0,0} orelse 
	LeftBracketLine==PatStartLine of
   	true ->
	    beside(text("("),D0);
	_ ->
	    above(text("("),nest(PatStartCol-LeftBracketCol,D0))
    end.

make_rule_clause(N,P,G,B,Ctxt, SameLine) ->
    D = make_fun_clause_head(N,P,Ctxt,{0,0}),
    append_rule_body(B,append_guard(G,D,Ctxt),Ctxt, SameLine).

make_case_clause(P,G,B,CsNode,Ctxt,SameLine) ->
    append_clause_body(B,append_guard(G,P,CsNode,Ctxt),Ctxt, SameLine).

make_if_clause(_P,G,B,Ctxt, SameLine) ->
    %% We ignore the patterns; they should be empty anyway.
    G1 = if G == none -> text("true");
	    true -> G
	 end,
    append_clause_body(B,G1,Ctxt, SameLine).


append_rule_body(B,D,Ctxt, SameLine={BodyStartLn, HeadLastLn}) ->
    R = case BodyStartLn==HeadLastLn andalso BodyStartLn/=0 of
 	    true -> text(" :- ");
 	    _ -> text(" :-")
 	end,
    append_clause_body(B,D,floating(R),Ctxt, SameLine).

append_clause_body(B,D,Ctxt, SameLine) ->
    append_clause_body(B,D,'->',Ctxt, SameLine).

append_clause_body(B,D, Symbol,Ctxt, _SameLine={{BodyStartLn,BodyStartCol}, 
					       {HeadStartCol, HeadLastLn}}) ->
    S=text(" "++atom_to_list(Symbol)),
    S1=text(" "++atom_to_list(Symbol)++" "),
    case (BodyStartLn==0) orelse (HeadLastLn==0) of 
	true -> %% use default
	    sep([beside(D,S),nest(Ctxt#ctxt.break_indent,B)]);
	false ->
            case BodyStartLn-HeadLastLn  of 
		0 ->
		    beside(beside(D,S1),B);
		N when N>=1->
		    Offset=BodyStartCol-HeadStartCol,
                    {SLn, SCol} = get_keyword_loc_before(Symbol,Ctxt,
							 {BodyStartLn, BodyStartCol}),
		    D1=case SLn==HeadLastLn orelse SLn==0 of
			   true ->
			       beside(D, S);
			   false when N>1->
                               SD=nest(SCol-HeadStartCol, text(atom_to_list(Symbol))),
			       vertical([D, white_lines(N-1), SD]);
                           _ ->
                               SD=nest(SCol-HeadStartCol, text(atom_to_list(Symbol))),
                               vertical([D,SD])
                       end,
                    case SLn==BodyStartLn of 
			true ->
			    refac_prettypr_0:horizontal([D1, B]);
			false when SLn==0->
			    sep([D1, nest(Ctxt#ctxt.break_indent, B)]);
			_ when BodyStartLn-SLn>1 ->
                            vertical([D1, white_lines(BodyStartLn-SLn-1), nest(Offset,B)]);
                        _ ->
                            vertical([D1, nest(Offset, B)])
		    end;
		_  -> %% default
		    sep([beside(D, S), nest(Ctxt#ctxt.break_indent, B)])
	    end
    end.

append_guard(none,D,_) -> D;
append_guard(G,D,Ctxt) ->
    par([D,follow(text("when"),G,Ctxt#ctxt.sub_indent)],
	Ctxt#ctxt.break_indent).


append_guard(none,D,_CsNode,_) -> 
      D;
append_guard(G,D,CsNode,Ctxt) ->
    Guard= refac_syntax:clause_guard(CsNode),
    {{StartLn, StartCol},{_EndLn, _EndCol}}=get_start_end_loc(Guard),
    {WhenLn, WhenCol}=get_prev_keyword_loc(Ctxt#ctxt.tokens, {StartLn, StartCol}, 'when'),
    Pats = refac_syntax:clause_patterns(CsNode),
    {{_PStartLn, _PStartCol}, {PEndLn, _PEndCol}}=get_start_end_loc(Pats),
    {_,CsStartCol} = get_start_loc_with_comment(CsNode),
    D1= case WhenLn-PEndLn==1 of 
	    true ->
		above(D, nest(WhenCol-CsStartCol, text("when")));
	    false when WhenLn/=0 andalso WhenLn==PEndLn->
		refac_prettypr_0:horizontal([D, text("when")]);
	    _ ->
		par([D, text("when")], Ctxt#ctxt.break_indent)
	end,
    case WhenLn/=0 andalso WhenLn==StartLn of
	true ->
	    refac_prettypr_0:horizontal([D1, G]);
	false when StartLn-WhenLn==1 ->
	    above(D1, nest(StartCol-CsStartCol, G));
	_ ->
	    par([D1, G], Ctxt#ctxt.break_indent)
    end.

append_elems(Fun, {D1, {{_D1StartLn, D1StartCol}, {D1EndLn, D1EndCol}}}, 
	     {D2, {{D2StartLn, D2StartCol}, _D2End}}) -> 
    case D1EndLn==0 orelse D2StartLn==0 orelse D2StartLn<D1EndLn of
	true ->
	    Fun([D1,D2]);
	false when D1EndLn==D2StartLn ->
	    Gap = D2StartCol -D1EndCol-1,
	    S = text(empty_str(Gap)),
	    horizontal([D1, S, D2]);
	_ ->
	    Nest=D2StartCol-D1StartCol,
	    above(D1, nest(Nest, D2))
    end.

append_keywords(KeyWord1, KeyWord2, CsD, Node, Ctxt) ->
    {{CsStartLn, CsStartCol}, {CsEndLn, CsEndCol}} = get_start_end_loc(Node),
    {KeyWord1Line, KeyWord1Col} =
	get_keyword_loc_before(list_to_atom(KeyWord1), Ctxt,
			       {CsStartLn, CsStartCol}),
    {KeyWord2Line, KeyWord2Col} =
	get_keyword_loc_after(list_to_atom(KeyWord2), Ctxt,
			      {CsEndLn, CsEndCol}),
    KeyWord1CsD = case KeyWord1Line == 0 of
		      false when KeyWord1Line==CsStartLn ->
			  Gap =CsStartCol-(KeyWord1Col+length(KeyWord1)),
			  Space=text(empty_str(Gap)),
			  [horizontal([text(KeyWord1), Space, CsD])];
		      false when CsStartLn-KeyWord1Line>=1 ->
			  [above(text(KeyWord1), nest(CsStartCol-KeyWord1Col, CsD))];
		      _ ->
			 case lists:member(KeyWord1, ["if", "fun"]) of
			     true ->
				 [follow(text(KeyWord1), CsD, Ctxt#ctxt.break_indent)];
			     false ->
				 [text(KeyWord1), nest(Ctxt#ctxt.sub_indent, CsD)]
			 end
		  end,
    case KeyWord2 == 0 of
	false when KeyWord2Line==CsEndLn ->
	    refac_prettypr_0:horizontal(KeyWord1CsD++[text(KeyWord2)]);
	false when KeyWord2Line-CsEndLn>=1 ->
	    above(sep(KeyWord1CsD), nest(KeyWord2Col-KeyWord1Col, text(KeyWord2)));
	_ ->
	    sep(KeyWord1CsD ++ [text(KeyWord2)])
    end.


append_leading_keyword(KeyWord, CsD, Node, Ctxt) ->
    {CsStartLn, CsStartCol} = case is_list(Node) of
				  true -> get_start_loc(hd(Node));
				  false -> get_start_loc(Node)
			      end,
    {KeyWordLine, KeyWordCol} =
	get_keyword_loc_before(list_to_atom(KeyWord), Ctxt,
			       {CsStartLn, CsStartCol}),
    case KeyWordLine == 0 of
	false when KeyWordLine==CsStartLn ->
	    Gap =case CsStartCol-(KeyWordCol+length(KeyWord)) of
                     G when G>1 ->
                         G;
                     _ -> 1
                 end,           
	    Space=text(empty_str(Gap)),
	    horizontal([text(KeyWord), Space, CsD]);
	false when CsStartLn-KeyWordLine>=1 ->
	    above(text(KeyWord), nest(CsStartCol-KeyWordCol, CsD));
	_ ->
	    sep([text(KeyWord), nest(Ctxt#ctxt.break_indent, CsD)])
    end.
   

lay_bit_types([T],Ctxt) -> lay(T,Ctxt);
lay_bit_types([T| Ts],Ctxt) ->
    beside(lay(T,Ctxt),
	   beside(floating(text("-")),lay_bit_types(Ts,Ctxt))).

lay_error_info({L,M,T} = T0,Ctxt)
    when is_integer(L), is_atom(M) ->
    case catch M:format_error(T) of
      S when is_list(S) ->
	  if L > 0 ->
		 beside(text(io_lib:format("~w: ", [L])),text(S));
	     true -> text(S)
	  end;
      _ -> lay_concrete(T0,Ctxt)
    end;
lay_error_info(T,Ctxt) -> lay_concrete(T,Ctxt).

lay_concrete(T,Ctxt) ->
    lay(refac_syntax:abstract(T),Ctxt).

seq([H|T],Separator,Ctxt,Fun) ->
    case T of
        [] -> [Fun(H,Ctxt)];
	_->
	    [maybe_append(Separator,Fun(H,Ctxt))| seq(T,Separator,
						      Ctxt,Fun)]
    end;
seq([],_,_,_) -> [empty()].

maybe_append(none,D) -> D;
maybe_append(Suffix,D) -> beside(D,Suffix).

vertical([D]) -> D;
vertical([D| Ds]) -> above(D,vertical(Ds));
vertical([]) -> [].

vertical_sep(_Sep,[D]) -> D;
vertical_sep(Sep,[D| Ds]) ->
    above(above(D,Sep),vertical_sep(Sep,Ds));
vertical_sep(_Sep,[]) -> [].

spaces(N) when N > 0 -> [$\s| spaces(N - 1)];
spaces(_) -> [].

tidy_float([$.,C| Cs]) ->
    [$.,C| tidy_float_1(Cs)];  % preserve first decimal digit
tidy_float([$e| _] = Cs) -> tidy_float_2(Cs);
tidy_float([C| Cs]) -> [C| tidy_float(Cs)];
tidy_float([]) -> [].

tidy_float_1([$0,$0,$0| Cs]) ->
    tidy_float_2(Cs);    % cut mantissa at three consecutive zeros.
tidy_float_1([$e| _] = Cs) -> tidy_float_2(Cs);
tidy_float_1([C| Cs]) -> [C| tidy_float_1(Cs)];
tidy_float_1([]) -> [].

tidy_float_2([$e,$+,$0]) -> [];
tidy_float_2([$e,$+,$0| Cs]) ->
    tidy_float_2([$e,$+| Cs]);
tidy_float_2([$e,$+| _] = Cs) -> Cs;
tidy_float_2([$e,$-,$0]) -> [];
tidy_float_2([$e,$-,$0| Cs]) ->
    tidy_float_2([$e,$-| Cs]);
tidy_float_2([$e,$-| _] = Cs) -> Cs;
tidy_float_2([$e| Cs]) -> tidy_float_2([$e,$+| Cs]);
tidy_float_2([_C| Cs]) -> tidy_float_2(Cs);
tidy_float_2([]) -> [].


%% =====================================================================

horizontal([D]) -> D;
horizontal([D| Ds]) -> beside(beside(D, nil()),horizontal(Ds));
horizontal([]) -> [].


lay_elems(_Fun, _ElemDocs,[], _Ctxt) -> null;
lay_elems(Fun, ElemDocs,Elems,Ctxt) ->
    ARanges = [get_start_end_loc(E) || E<-Elems], 
    case lists:all(fun(R) -> R=={{0,0},{0,0}} end, ARanges) of 
	true ->
	    Fun(ElemDocs);
	false ->
	    lay_elems_1(Fun, lists:zip(ElemDocs,ARanges), Ctxt,[],{{1,1},{1,1}}, 1)
    end.

lay_elems_1(_Fun, [], _Ctxt, Acc, _LastLine, _LastOffset) ->
    Docs = lists:map(fun (Ds) -> horizontal(Ds) end, Acc),
    vertical(lists:reverse(Docs));
lay_elems_1(Fun, [{D, SE={{_SLn, SCol}, {_ELn, _ECol}}}|Ts], Ctxt, [], _LastLn, _StartOffset) ->
    lay_elems_1(Fun, Ts, Ctxt, [[D]], SE, SCol);
lay_elems_1(Fun, [{D, SE={{SLn, SCol}, {_ELn, _ECol}}}| Ts], Ctxt, [H| T], 
	     _LastLoc={{_LastSLn, _LastSCol}, {LastELn, LastECol}}, StartOffset) ->
    case SLn == 0 orelse LastELn == 0 orelse SLn =< LastELn of
	true ->
	    lay_elems_1(Fun, Ts, Ctxt, [H ++ [D]| T], SE, StartOffset);
	false  when SLn-LastELn==1->
	    lay_elems_1(Fun, Ts, Ctxt, [[nest(SCol-StartOffset, D)], H|T], SE, StartOffset);
        _ ->
            case SLn-LastELn>1 andalso 
                real_white_line(Ctxt#ctxt.tokens, {LastELn, LastECol}, {SLn, SCol}) of
                true->
                    lay_elems_1(Fun, Ts, Ctxt, [[above(white_lines(SLn-LastELn-1),
                                                       nest(SCol-StartOffset, D))], H|T],
                                SE, StartOffset);
                false ->
                    lay_elems_1(Fun, Ts, Ctxt, [H ++ [D]| T], SE, StartOffset)
            end
    end.

  
lay_body_elems(_Fun, _ElemDocs,[], _Ctxt) -> null;
lay_body_elems(Fun, ElemDocs,Elems, Ctxt) ->
    ARanges=[get_start_end_loc_with_comment(E)||E<-Elems],
    lay_body_elems_1(Fun, lists:zip(ElemDocs,ARanges),Ctxt,[],{{1,1},{1,1}}).


lay_body_elems_1(_Fun, [], _Ctxt, Acc, _LastRange) ->
    Docs = lists:map(fun (Ds) -> refac_prettypr_0:horizontal(Ds) end, Acc),
    vertical(lists:reverse(Docs));
lay_body_elems_1(Fun, [{D, {SLoc={_SLn, _SCol}, ELoc}}| Ts],  Ctxt,[], _LastLoc) ->
    lay_body_elems_1(Fun, Ts,  Ctxt,[[D]], {SLoc, ELoc});
lay_body_elems_1(Fun, [{D, Range={{SLn,SCol}, {_ELn, _ECol}}}| Ts], Ctxt, [H| T], 
		 _LastLoc={{_LastSLn, _LastSCol}, {LastELn, LastECol}}) ->
    case SLn == 0 orelse LastELn == 0 orelse SLn < LastELn of
	true -> 
	    lay_body_elems_1(Fun, Ts,  Ctxt,[[D],H|T], Range);
	false -> 
	    case SLn - LastELn of
		0 ->  %% same line;
                    lay_body_elems_1(Fun, Ts,  Ctxt,[H ++ [D]| T], Range);
		1 ->
                    lay_body_elems_1(Fun, Ts,  Ctxt,[[D], H|T], Range);
                N ->
                    case N>1 andalso 
                        real_white_line(Ctxt#ctxt.tokens, {LastELn, LastECol}, {SLn, SCol}) of
                        true->
                            lay_body_elems_1(Fun, Ts,  Ctxt, [[above(white_lines(N-1),D)], H|T],Range);
                        false ->
                            lay_body_elems_1(Fun, Ts,  Ctxt,[[D],H|T], Range)
                    end
            end
    end.
  

empty_str(N) when N<1 
                  -> "";
empty_str(N) -> 
    lists:append(lists:duplicate(N, " ")). 

nil() -> text("").    
    
white_lines(N) ->
    case N>1 of 
        true ->
            above(text(""), 
                  white_lines(N-1));
        _ ->
            text("")
    end.

real_white_line(Toks, _Loc1, _Loc2) when Toks==[] ->
    false;
real_white_line(Toks, Loc1, Loc2) ->
    Toks1 = lists:dropwhile(fun(T)->
                                    token_loc(T)=<Loc1
                            end, Toks),
    case Toks1 of
        [] ->
            false;
        [_T|Toks2] ->
            Toks3=lists:takewhile(fun(T)->
                                          token_loc(T)<Loc2
                                  end, Toks2),
            lists:all(fun(T) ->
                              is_whitespace_or_comment(T) orelse
                                  size(T)==2
                      end, Toks3)
    end.
    
    

get_prev_keyword_loc(FormToks, StartPos, KeyWord)->
    Ts1 = lists:takewhile(
	    fun(T) ->
		    token_loc(T)=<StartPos
	    end, FormToks),
    Ts2=lists:dropwhile(fun(T)->
				element(1, T)/=KeyWord
			end, lists:reverse(Ts1)),
    case Ts2 of 
	[] ->
	    {0,0};
	[T1|_] ->
	    token_loc(T1)
    end.

get_keyword_loc_after(Keyword, Ctxt,Pos)->
    Toks=Ctxt#ctxt.tokens,
    Toks1 = lists:dropwhile(fun (T) -> token_loc(T) =< Pos end, Toks),
    case Toks1 of
	[] ->
	    {0,0};
	_ ->
	    {Toks2, Toks3} = lists:splitwith(fun (T) -> token_val(T) =/=Keyword end, Toks1),
	    case Toks3 of 
		[] ->
		    {0,0};   
		_ ->
		    case lists:all(fun(T)-> is_whitespace_or_comment(T) orelse 
				   token_val(T)=='(' orelse token_val(T)==')'end, Toks2) of
		      true ->
			  token_loc(hd(Toks3));
		      _ ->
			  {0,0}
		  end
	    end
    end.

get_keyword_loc_before(Keyword, Ctxt,Pos)->
    Toks=Ctxt#ctxt.tokens,
    Toks1 = lists:takewhile(fun (T) -> token_loc(T)< Pos end, Toks),
    case Toks1 of
	[] ->
	    {0,0};
	_ ->
	    {Toks2, Toks3} = lists:splitwith(fun (T) -> token_val(T) =/=Keyword end, lists:reverse(Toks1)),
	    case Toks3 of 
		[] ->
		    {0,0};   
		_ ->
		    case lists:all(fun(T)-> is_whitespace_or_comment(T) orelse 
						token_val(T)=='(' orelse 
						token_val(T)==')'end, Toks2) of
			true ->
			    token_loc(hd(Toks3));
			_ ->
			    {0,0}
		    end
	    end
    end.


next_token_loc(Toks, Loc) ->
    Toks1 = lists:dropwhile(fun (T) -> 
				    token_loc(T) =< Loc orelse
					is_whitespace_or_comment(T) 
			    end, Toks),
    case Toks1 of
	[] ->
	    {0,0};
	_ ->
	    token_loc(hd(Toks1))
    end.

prev_token_loc(Toks, Loc) ->
    Toks1 = lists:takewhile(fun (T) -> 
				    token_loc(T) < Loc end, Toks),
    Toks2=lists:dropwhile(fun(T) ->
				  is_whitespace_or_comment(T) 
			  end, 
			  lists:reverse(Toks1)),
    case Toks2 of
	[] ->
	    {0,0};
	_ ->
	    token_loc(hd(Toks2))
    end.
    
get_right_bracket_loc(Toks, LeftLoc) ->
    Toks1 = lists:dropwhile(fun (T) -> 
				    token_loc(T) =< LeftLoc orelse
					is_whitespace_or_comment(T) end, Toks),
    case Toks1 of
	[] ->
	    {0,0};
	_ ->	
	    get_right_bracket_loc_1(Toks1, 0)
    end.

get_right_bracket_loc_1([], _) ->
    {0,0};
get_right_bracket_loc_1([T|Toks1], UnBalanced) ->
    case token_val(T) of
	'(' ->
	    get_right_bracket_loc_1(Toks1, UnBalanced+1);
	')' ->
	    case UnBalanced of
		0 ->
		    token_loc(T);
		_ ->
		    get_right_bracket_loc_1(Toks1, UnBalanced-1)
	    end;
	_ ->
	    get_right_bracket_loc_1(Toks1, UnBalanced)
    end.

		    
get_separator(_NodeList, [], Default) ->
    Default;
get_separator([],_, Default) ->
    Default;
get_separator(NodeList, Ctxt, Default) when is_list(NodeList) -> 
    Toks = Ctxt#ctxt.tokens,
    NodeToks = get_node_toks(Toks,NodeList),
    NodeListToks = lists:append([get_node_toks(Toks, Elem)||
				    Elem<-NodeList]),
    SepToks = NodeToks -- NodeListToks,
    case SepToks of 
	[] ->
	    Default;
	_ ->
	    {OnlyComma, CommaWithSpace} =get_comma_tokens(SepToks),     
	    case length(OnlyComma) >length(CommaWithSpace) of 
		true ->
		    ",";
		false ->
		    ", "
	    end
    end;
get_separator(_Node, _Ctxt, Default) ->
    Default.

get_node_toks(Toks, Node) ->
    {Start, End} = get_start_end_loc(Node),
    case Start =={0,0} orelse End=={0,0} of
	true -> [];
	false ->
	    Toks1 = lists:dropwhile(
		      fun(T) ->
			      token_loc(T)<Start
		      end, Toks),
	    lists:takewhile(fun(T)->
				    token_loc(T)=<End
			    end, Toks1)
    end.

get_comma_tokens(Toks) ->   
    get_comma_tokens(Toks, {[],[]}).
get_comma_tokens([], {OnlyComma, CommaWithSpace}) ->
    {OnlyComma, CommaWithSpace};
get_comma_tokens([T|Ts], {OnlyComma, CommaWithSpace}) ->
    case T of
	{',', _} ->
	    case Ts  of 
		[T1={whitespace, _, _}|Ts1] ->
		    get_comma_tokens(Ts1, {OnlyComma, [{T, T1}|CommaWithSpace]});
		_ ->
		    get_comma_tokens(Ts, {[T|OnlyComma], CommaWithSpace})
	    end;
	_ ->
	    get_comma_tokens(Ts,  {OnlyComma, CommaWithSpace})
    end.


has_parentheses(Node, Toks)->		  
    {StartLoc,EndLoc} = get_start_end_loc(Node),
    {Toks1, Toks2} = lists:splitwith(
                       fun(T) ->
                               token_loc(T)<StartLoc
                       end, Toks),
    Toks3 = lists:dropwhile(
              fun(T) -> 
                      token_loc(T) =< EndLoc 
              end, Toks2),
    Ts1 = lists:dropwhile(
            fun(B) -> 
                    is_whitespace(B)
            end,
            lists:reverse(Toks1)),
    Ts2 =lists:dropwhile(
           fun(B) -> 
                   is_whitespace(B)
           end,
           lists:reverse(Toks3)),
    Ts1/=[] andalso element(1, hd(Ts1))=='(' andalso
        Ts2/=[] andalso element(1, hd(Ts2))==')'.

    
%%===============================================================
is_whitespace({whitespace, _, _}) ->
    true;
is_whitespace(_) ->
    false.
	
is_whitespace_or_comment({whitespace, _, _}) ->
    true;
is_whitespace_or_comment({comment, _, _}) ->
    true;
is_whitespace_or_comment(_) -> false.


token_loc(T) ->
    case T of
      {_, L, _V} -> L;
      {_, L1} -> L1
    end.

token_val(T) ->
    case T of
      {_, _, V} -> V;
      {V, _} -> V
    end.

remove_trailing_whitespace(Str) ->
    remove_trailing_whitespace(Str, []).
remove_trailing_whitespace([], Acc) ->
    lists:reverse(Acc);
remove_trailing_whitespace([$,,$ ,$\r| S], Acc) ->
    remove_trailing_whitespace(S, [$\r,$,| Acc]);
remove_trailing_whitespace([$,,$ ,$\n| S], Acc) ->
    remove_trailing_whitespace(S, [$\n,$,| Acc]);
remove_trailing_whitespace([C| S], Acc) ->
    remove_trailing_whitespace(S, [C| Acc]).


remove_trailing_whites(Str) ->
    Str1=lists:dropwhile(fun(S)->
                                 lists:member(S, [$\n, $\r, $\t, $\s])
                         end, lists:reverse(Str)),
    lists:reverse(Str1).
    
get_leading_whites(OriginalToks) ->
    Ts1=lists:takewhile(fun(Ts) ->
                                all_whites(Ts)
                        end, OriginalToks),
    refac_util:concat_toks(lists:append(Ts1)).

get_trailing_whites(Str) ->   
    lists:reverse(lists:takewhile(fun(S) ->
                                          lists:member(S, [$\n, $\r, $\t, $\s])
                                  end, lists:reverse(Str))).
    

%% =====================================================================

get_start_loc(Node) ->
    {L, _} = get_start_end_loc(Node),
    L.

get_start_end_loc(Node)->
    refac_util:get_start_end_loc(Node).

get_start_line_with_comment(Node) ->
    {{L, _}, _} = get_start_end_loc_with_comment(Node),
    L.
   
get_end_line_with_comment(Node) ->
    {_, {L, _}} = get_start_end_loc_with_comment(Node),
    L.

get_start_loc_with_comment(Node) ->
    {Start, _}= get_start_end_loc_with_comment(Node),
    Start.

get_end_loc_with_comment(Node) ->
    {_, End}=get_start_end_loc_with_comment(Node),
    End.

get_start_end_loc_with_comment(Node) ->
    {Start, End} = get_start_end_loc(Node),
    PreCs = refac_syntax:get_precomments(Node),
    PostCs = refac_syntax:get_postcomments(Node),
    Start1 = case PreCs of
                 [] ->
                     Start;
                 _ ->
                     refac_syntax:get_pos(hd(PreCs))
             end,
    End1 = case PostCs of
               [] ->
                   End;
               _ ->
                   LastC = refac_syntax:comment_text(lists:last(PostCs)),
                   {L, C}=refac_syntax:get_pos(lists:last(PostCs)),
                   {L, C+length(LastC)-1}
           end,
    {Start1, End1}.

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

repair_new_form_str("", NewFormStr, _TabWidth, _FileFormat) -> NewFormStr;
repair_new_form_str(OldFormStr, NewFormStr, TabWidth, FileFormat)->
    {ok, OldToks0, _} = refac_scan_with_layout:string(OldFormStr,{1,1}, TabWidth, FileFormat),
    OldToksByLine =group_toks_by_line(OldToks0),
    Str1 = get_leading_whites(OldToksByLine),
    Str2 = get_trailing_whites(OldFormStr),
    NewFormStr1=Str1++remove_trailing_whites(NewFormStr)++Str2,
    {ok, NewToks0, _} = refac_scan_with_layout:string(NewFormStr1, {1,1}, TabWidth, FileFormat),
    NewToksByLine = group_toks_by_line(NewToks0),
    DiffByLine = levenshtein_dist(OldToksByLine, NewToksByLine, TabWidth),
    %% refac_io:format("DiffByLine:\n~p\n", [DiffByLine]),
    repair_form_layout(DiffByLine, TabWidth).
    

repair_form_layout(DiffByLine, TabWidth) ->
    repair_form_layout(DiffByLine, none, TabWidth, []).
repair_form_layout([], _, _TabWidth, Acc) ->
    refac_util:concat_toks(lists:append(lists:reverse(Acc)));
repair_form_layout([{'*', LineToks}|Lines], PrevDiff, TabWidth, Acc) ->
    case all_whites(LineToks) of
        true ->
            repair_form_layout(Lines, PrevDiff, TabWidth,[LineToks|Acc]);
        false ->
            repair_form_layout(Lines, '*', TabWidth,[LineToks|Acc])
    end;
repair_form_layout([{'d', _LineToks}|Lines], _PrevDiff, TabWidth, Acc) ->
    repair_form_layout(Lines, 'd', TabWidth, Acc);
repair_form_layout([{'i', LineToks}|Lines], _PrevDiff, TabWidth, Acc) ->
    repair_form_layout(Lines, 'i', TabWidth, [LineToks|Acc]);
repair_form_layout([{'s', OldLineToks, NewLineToks}|Lines], PrevDiff, TabWidth, Acc) ->
    case is_editing_change(OldLineToks, NewLineToks) of
        true ->
            NewLineToks1 = recover_tab_keys(OldLineToks, NewLineToks, TabWidth),
            repair_form_layout(Lines, 's', TabWidth, [NewLineToks1|Acc]);
        false ->
            %% layout change.
            case PrevDiff of 
                '*' ->
                    repair_form_layout(Lines, '*', TabWidth, [OldLineToks|Acc]);
                _ -> %% rather conservertive.
                    case all_whites_or_comments(OldLineToks) of 
                        true ->
                            Lines1 = lists:dropwhile(
                                       fun(L) ->
                                               all_whites_or_comments(element(2,L))
                                       end, Lines),
                            case Lines1 of
                                [{'s', Toks1, Toks2}|_] ->
                                    case has_layout_change(Toks1, Toks2, TabWidth) of 
                                        true ->
                                            NewLineToks1 = recover_tab_keys(OldLineToks, NewLineToks, TabWidth),
                                            repair_form_layout(Lines, 's', TabWidth, [NewLineToks1|Acc]);
                                        false ->
                                            repair_form_layout(Lines, '*', TabWidth, [OldLineToks|Acc])
                                    end;
                                _ ->
                                    repair_form_layout(Lines, '*', TabWidth, [OldLineToks|Acc])
                            end;                                
                        false ->
                            NewLineToks1 = recover_tab_keys(OldLineToks, NewLineToks, TabWidth),
                            repair_form_layout(Lines, 's', TabWidth, [NewLineToks1|Acc])
                    end
            end
    end.

is_editing_change(Toks1, Toks2) ->
    remove_loc_and_whites(Toks1) =/=
        remove_loc_and_whites(Toks2).

has_layout_change(Toks1, Toks2, TabWidth) ->
    Toks11 = expand_tab_keys(Toks1, TabWidth),
    Toks21 = expand_tab_keys(Toks2, TabWidth),
    Whites1 = lists:takewhile(fun(T) -> is_whitespace(T) end, Toks11),
    Whites2 = lists:takewhile(fun(T) -> is_whitespace(T) end, Toks21),
    length(Whites1)/=length(Whites2).
                                      
                                    
    
recover_tab_keys(OldToks, NewToks, TabWidth)->
    TabToks =lists:takewhile(fun(T)-> is_tab_token(T) end, OldToks),
    {WhiteToks, Toks1}=lists:splitwith(fun(T) -> 
                                               case T of
                                                   {whitespace, _L, ' '} ->
                                                       true;
                                                   _ -> false
                                               end
                                       end, NewToks),
    recover_tab_keys(TabToks, WhiteToks, TabWidth, [])++Toks1.

recover_tab_keys([], WhiteSpaces, _TabWidth, Acc) ->
    lists:reverse(Acc)++WhiteSpaces;
recover_tab_keys([T={whitespace, _, '\t'}], WhiteSpaces, TabWidth, Acc) ->
    case length(WhiteSpaces)>=TabWidth of
        true ->
            recover_tab_keys([], lists:nthtail(TabWidth, WhiteSpaces), TabWidth, [T|Acc]);
        false ->
            lists:reverse(Acc)++WhiteSpaces
    end;
recover_tab_keys([T={whitespace, _, '\t'}|Ts], WhiteSpaces, TabWidth, Acc) ->
    case length(WhiteSpaces)>=TabWidth of
        true ->
            recover_tab_keys(Ts, lists:nthtail(TabWidth, WhiteSpaces), TabWidth, [T|Acc]);
        false ->
           lists:reverse(Acc)++WhiteSpaces
    end.

group_toks_by_line(Toks) ->
    Toks1 =[case T  of 
		{K, {L,_C}, V} -> {K, L, V};
		{K, {L,_C}} -> {K, L}
	    end || T <-Toks],
    group_toks_by_line_1(Toks1,[]).

group_toks_by_line_1([],Acc) -> lists:reverse(Acc);
group_toks_by_line_1(Toks = [T| _Ts],Acc) ->
    L = element(2,T),
    {Toks1, Toks2} = 
	lists:partition(fun (T1) ->
				element(2,T1) == L
			end,
			Toks),
    group_toks_by_line_1(Toks2,[Toks1|Acc]).


                        
is_tab_token({whitespace, _, '\t'}) ->
    true;
is_tab_token(_) ->
    false.

                                    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

levenshtein_dist(OldToks, NewToks, TabWidth) ->
    OldLen = length(OldToks),
    NewLen = length(NewToks),
    Is =lists:seq(0, OldLen),
    Js= lists:seq(1, NewLen),
    InitAcc=[{{I,0},I}||I<-Is] ++ [{{0, J}, J}||J<-Js],
    Matrix=levenshtein_dist(OldToks, NewToks, OldLen, NewLen, TabWidth, {1, 1}, InitAcc),
    get_edit_ops(Matrix, OldToks, NewToks, OldLen, NewLen, TabWidth, []). 
    


levenshtein_dist(_OldToks, _NewToks, _OldLen, NewLen, _TabWidth, {_I, J}, Acc)
  when J>NewLen ->  Acc;
levenshtein_dist(OldToks, NewToks, OldLen, NewLen, TabWidth, {I, J}, Acc)
  when I>OldLen ->
    levenshtein_dist(OldToks, NewToks, OldLen, NewLen, TabWidth, {1, J+1}, Acc);
levenshtein_dist(OldToks, NewToks, OldLen, NewLen, TabWidth, {I, J}, Acc) ->
    case same(lists:nth(I, OldToks), lists:nth(J, NewToks), TabWidth) of
        true ->
            {value, {_, V}}=lists:keysearch({I-1, J-1}, 1, Acc),
            levenshtein_dist(OldToks, NewToks, OldLen, NewLen, TabWidth, {I+1, J}, 
                             [{{I,J},V}|Acc]);
        false ->
            {value, {_, Del}} = lists:keysearch({I-1, J}, 1,Acc),
            {value, {_, Ins}} = lists:keysearch({I, J-1}, 1,Acc),
            {value, {_, Sub}} = lists:keysearch({I-1, J-1},1, Acc),
            Min = lists:min([Del, Ins, Sub]),
            levenshtein_dist(OldToks, NewToks, OldLen, NewLen, TabWidth, {I+1, J}, 
                             [{{I,J},Min+1}|Acc])
    end.


get_edit_ops(_Matrix, _OldToks, _NewToks, I, J, _TabWidth, Acc) 
  when I=<0 andalso J=<0 ->
    Acc;
get_edit_ops(Matrix, OldToks, NewToks, I, J, TabWidth,Acc) ->
    Ith = lists:nth(I, OldToks),
    Jth = lists:nth(J, NewToks),
    case same(Ith, Jth, TabWidth) of
        true ->
            get_edit_ops(Matrix, OldToks, NewToks, I-1, J-1, TabWidth, [{'*', Ith}|Acc]);
        false ->
            {value, {_, Del}} = lists:keysearch({I-1, J}, 1,Matrix),
            {value, {_, Ins}} = lists:keysearch({I, J-1}, 1,Matrix),
            {value, {_, Sub}} = lists:keysearch({I-1, J-1},1, Matrix),
            case lists:min([Del, Ins, Sub]) of
                Ins ->
                    get_edit_ops(Matrix, OldToks, NewToks, I, J-1, TabWidth, [{'i', Jth}|Acc]);
                Del ->
                    get_edit_ops(Matrix, OldToks, NewToks, I-1, J, TabWidth, [{'d', Ith}|Acc]);
                Sub ->
                    get_edit_ops(Matrix, OldToks, NewToks, I-1, J-1, TabWidth, [{'s', Ith, Jth}|Acc])
            end
    end.


same(Toks1, Toks2, TabWidth) ->
    case all_whites(Toks1) andalso all_whites(Toks2) of 
	true ->
	    true;
	false ->
	    same_toks_1(Toks1, Toks2, TabWidth)
    end.
same_toks_1(Toks1, Toks2, TabWidth) ->
    Toks11=expand_tab_keys(Toks1, TabWidth),
    Toks21=expand_tab_keys(Toks2, TabWidth),
    Toks12=remove_whites_after_first_non_white_token(remove_locs(Toks11)),
    Toks22=remove_whites_after_first_non_white_token(remove_locs(Toks21)),
    Toks12==Toks22.


all_whites_or_comments(Toks) ->
     lists:all(fun(T) ->
                       is_whitespace_or_comment(T) 
               end, Toks).
    
all_whites(Toks) ->
    lists:all(fun(T) ->
		      is_whitespace(T) 
	      end, Toks).

expand_tab_keys(Toks, TabWidth) ->
    lists:append([case T of 
		      {whitespace, L, '\t'} ->
			  lists:duplicate(TabWidth, {whitespace, L, ' '});
		      _ -> [T]
		  end || T<-Toks]).


remove_whites_after_first_non_white_token(Toks) ->
    {Toks1, Toks2}=lists:splitwith(fun(T) ->
					   is_whitespace_or_comment(T)
				   end, Toks),
    Toks1++[T||T<-Toks2, not is_whitespace(T)].

remove_loc(T) ->
    case T of
        {K, _L, V} -> {K, 0, V};
        {K, _L} -> {K, 0};
	 _ -> T
    end.

remove_locs(Toks) when is_list(Toks) ->
    [remove_loc(T)|| T<-Toks];
remove_locs(Others) ->
    Others.

remove_loc_and_whites(Toks) ->
    [remove_loc(T)||T<-Toks,
         not is_whitespace(T)].


remove_locs_whites_and_comments(Toks) ->
    [remove_loc(T)||T<-Toks,
         not is_whitespace_or_comment(T)].
