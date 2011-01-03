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
%%%-------------------------------------------------------------------
%%% File    : wrangler_ast_server.erl
%%% Author  :  <Huiqing Li>
%%% Description : A gen server manageing the ASTs of the program under refactoring.
%%%
%%% Created : 28 Aug 2008 by  <Huiqing Li>
%%%-------------------------------------------------------------------
-module(wrangler_ast_server).

-behaviour(gen_server).

%% API
-export([parse_annotate_file/2, parse_annotate_file/3,
	 parse_annotate_file/4, parse_annotate_file/5,
	 quick_parse_annotate_file/3]).
%% API
-export([start_ast_server/0, update_ast/2, get_temp_dir/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/wrangler.hrl").

-record(state, {dets_tab=none, asts=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
%%-spec(start_ast_server() ->
%%	     {ok, pid()} | ignore | {error, string()}).
start_ast_server() ->
    gen_server:start_link({local, wrangler_ast_server}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |t
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
%%-spec(init/1::([dir()]) ->{ok, #state{}}).
init(_Args) ->
    process_flag(trap_exit, true),
    case file:get_cwd() of
	{ok, Dir} ->
	    TabDir = filename:join(Dir,"temp"),
	    FileName=filename:join(TabDir, "wrangler_dets"),
	    DetsTab = FileName,  %%  list_to_atom(FileName),
	    file:delete(FileName),
	    case file:make_dir(TabDir) of 
		ok ->
		    case dets:open_file(DetsTab,  [{type, set}, {access, read_write}, {repair, false}]) of 
			{ok, _Name} ->  {ok, #state{dets_tab=DetsTab}};
			_  ->  {ok, #state{dets_tab=none}}
		    end;
		{error, eexist} ->
		    case dets:open_file(DetsTab,  [{type, set}, {access, read_write}, {repair, false}]) of 
			{ok, _Name} ->  {ok, #state{dets_tab=DetsTab}};
			_  ->  {ok, #state{dets_tab=none}}
		    end;
		_Others -> {ok, #state{dets_tab=none}}
	    end;
	{error, _} -> {ok, #state{dets_tab=none}}
    end.
  
%%------------------------------------------------------------------
%%-spec(get_ast/1::({filename(), boolean(), [dir()], integer(), atom()}) ->
%%	     {ok, {syntaxTree(), moduleInfo()}}).
get_ast(Key={_FileName, _ByPassPreP, _SearchPaths, _TabWidth, _FileFormat}) ->
    gen_server:call(wrangler_ast_server, {get,Key}, 500000).


%%-type(modifyTime()::{{integer(), integer(), integer()},{integer(), integer(), integer()}}).
%%-spec(update_ast/2::({filename(),boolean(), [dir()], integer(), atom()}, {syntaxTree(), moduleInfo(), modifyTime()}) -> ok).
update_ast(Key={_FileName, _ByPassPreP, _SearchPaths, _TabWidth, _FileFormat}, {AnnAST, Info, Time}) ->
    gen_server:cast(wrangler_ast_server, {update, {Key, {AnnAST, Info, Time}}}).
 

get_temp_dir() ->
    gen_server:call(wrangler_ast_server, get_temp_dir).
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%%-spec(handle_call/3::({get,{filename(), boolean(), [dir()], integer(), atom()}}, any(), #state{}) -> 
%%			   {reply, {ok, {syntaxTree(), moduleInfo()}}, #state{}}).
handle_call({get, Key}, _From, State) ->
    {Reply, State1} = get_ast(Key, State),
    {reply, Reply, State1};
handle_call(get_temp_dir, _From, State=#state{dets_tab=TabFile}) ->
    TempDir = case TabFile of 
		  none -> none;
		  _ -> filename:dirname(TabFile)
	      end,
    {reply, TempDir, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({update, {Key, {AnnAST, Info, Time}}}, State) ->
    update_ast_1({Key, {AnnAST, Info, Time}}, State),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%%-spec(handle_info/2::(any(), #state{}) -> {noreply, #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
%%-spec(terminate/2::(any(), #state{}) -> ok).
terminate(_Reason, _State=#state{dets_tab=TabFile}) ->
    dets:close(TabFile),
    _Res=file:delete(TabFile),
    TempDir = filename:dirname(TabFile),
    case file:list_dir(TempDir) of 
     	{ok, []} ->
     	    file:del_dir(TempDir);
     	_ -> ok
     end.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
%%-spec(code_change/3::(any(), #state{}, any()) ->
%%	      {ok, #state{}}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------- ---------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%-spec(get_ast/2::({filename(),boolean(), [dir()], integer(), atom()}, #state{}) ->
%%		       {{ok, {syntaxTree(), moduleInfo()}}, #state{}}).      
get_ast({FileName, false, SearchPaths, TabWidth, FileFormat}, State) ->
    %% always re-parse; otherwise need to check the change time of .hrl files.
    wrangler_error_logger:remove_from_logger(FileName),
    {ok, {AnnAST, Info}} = parse_annotate_file(FileName, false, SearchPaths, TabWidth, FileFormat),
    log_errors(FileName, Info),
    {{ok, {AnnAST, Info}}, State};
get_ast(Key = {FileName, ByPassPreP, SearchPaths, TabWidth, FileFormat}, State = #state{dets_tab = TabFile, asts = ASTs}) ->
    case TabFile of
	none ->
	    case lists:keysearch(Key, 1, ASTs) of
		{value, {Key, {AnnAST, Info, Checksum}}} ->
		    NewChecksum = refac_util:filehash(FileName),
		    case Checksum =:= NewChecksum andalso NewChecksum =/= 0 of
			true ->
			    log_errors(FileName, Info),
			    {{ok, {AnnAST, Info}}, State};
			false ->
			    wrangler_error_logger:remove_from_logger(FileName),
			    {ok, {AnnAST1, Info1}} = parse_annotate_file(FileName, ByPassPreP, SearchPaths, TabWidth, FileFormat),
			    log_errors(FileName, Info1),
			    {{ok, {AnnAST1, Info1}}, #state{asts = lists:keyreplace(Key, 1, ASTs, {Key, {AnnAST1, Info1, NewChecksum}})}}
		    end;
		false ->
		    wrangler_error_logger:remove_from_logger(FileName),
		    {ok, {AnnAST, Info}} = parse_annotate_file(FileName, ByPassPreP, SearchPaths, TabWidth, FileFormat),
		    log_errors(FileName, Info),
		    {{ok, {AnnAST, Info}}, #state{asts = [{Key, {AnnAST, Info, refac_util:filehash(FileName)}}| ASTs]}}
	    end;
	_ ->
	    NewChecksum = refac_util:filehash(FileName),
	    case dets:lookup(TabFile, Key) of
		[{Key, {AnnAST, Info, Checksum}}] when Checksum =:= NewChecksum ->
		    {{ok, {AnnAST, Info}}, State};
		_ ->
		    wrangler_error_logger:remove_from_logger(FileName),
		    {ok, {AnnAST1, Info1}} = parse_annotate_file(FileName, ByPassPreP, SearchPaths, TabWidth, FileFormat),
		    dets:insert(TabFile, {Key, {AnnAST1, Info1, NewChecksum}}),
		    log_errors(FileName, Info1),
		    {{ok, {AnnAST1, Info1}}, State}
	    end
    end.

update_ast_1({Key, {AnnAST, Info, _Time}}, _State = #state{dets_tab = TabFile, asts = ASTs}) ->
    {FileName, _ByPassPreP, _SearchPaths, _TabWidth, _FileFormat} = Key,
    Checksum = refac_util:filehash(FileName),
    case TabFile of
	none -> case lists:keysearch(Key, 1, ASTs) of
		    {value, {Key, {_AnnAST1, _Info1, _Time}}} ->
			#state{asts = lists:keyreplace(Key, 1, ASTs, {Key, {AnnAST, Info, Checksum}})};
		    false ->
			#state{asts = [{Key, {AnnAST, Info, Checksum}}| ASTs]}
		end;
	_ ->
	    dets:delete(TabFile, Key),
	    dets:insert(TabFile, [{Key, {AnnAST, Info, Checksum}}])
    end.
    
log_errors(FileName, Info) ->
    case lists:keysearch(errors, 1, Info) of
      {value, {errors, Error}} ->
	  wrangler_error_logger:add_to_logger({FileName, Error});
      false -> ok
    end.


%% =====================================================================
%% @doc Parse an Erlang file, and annotate the abstract syntax tree with static semantic 
%% information. As to the parameters, FName is the name of the file to parse;  ByPassPreP 
%% is a bool value, and 'true' means to use the parse defined in refac_epp_dodger 
%% (which does not expand macros), 'false' means to use the parse defined in refac_epp
%% (which expands macros); SeachPaths is the list of directories to search for related 
%% Erlang files. 
%% The following annotations are added to the AST generated by the parser.
%% <ul>
%%     <li> <code> {env, [Var]}</code>, representing the input enrironment of 
%%     the subtree. </li>
%%
%%     <li> <code> {bound, [Var]} </code>, representing the variables that are 
%%      bound in the subtree. </li>
%%
%%     <li> <code> {free, [Var]}</code>, representing the free variables in the 
%%     subtree </li>
%%   
%%     <li> <code> {range, {Pos, Pos}} </code>, representing the start and end location 
%%     of subtree in the program source. </li>
%%    
%%     <li> <code> {category, atom()} </code>, representing the kind of the syntex phrase 
%%      represented by the subtree. </li>
%%
%%     <li> <code> {def, [Pos]} </code>, representing the defining positions of the variable 
%%     represented by the subtree (only when the subtree does represent a variable). </li>
%%
%%     <li> <code> {fun_def, {Mod, FunName, Arity, Pos, Pos}} </code>, representing the binding 
%%     information of the function represented by the subtree (only when the subtree
%%     represents a function definition, a function application, or an arity qualifier).
%%      </li>
%% </ul>
%%  <code>Var</code>  is a two-element tuple whose first element is an atom representing 
%%   the variable name, second element representing the variable's defining position. 
%%
%% @type syntaxTree(). An abstract syntax tree. The <code>erl_parse</code> "parse tree" 
%%  representation is a subset of the <code>syntaxTree()</code> representation.
%% 
%%  For the data structures used by the AST nodes, please refer to <a href="refac_syntax.html"> refac_syntax </a>.


%%-spec(parse_annotate_file(FName::filename(), ByPassPreP::boolean())
%%                           -> {ok, {syntaxTree(), moduleInfo()}}).
parse_annotate_file(FName, ByPassPreP) ->
    parse_annotate_file(FName, ByPassPreP, [], ?DEFAULT_TABWIDTH).


%%-spec(parse_annotate_file(FName::filename(), ByPassPreP::boolean(), SearchPaths::[dir()])
%%                           -> {ok, {syntaxTree(), moduleInfo()}}).
parse_annotate_file(FName, ByPassPreP, SearchPaths) ->
    parse_annotate_file(FName, ByPassPreP, SearchPaths, ?DEFAULT_TABWIDTH).

%%-spec(parse_annotate_file(FName::filename(), ByPassPreP::boolean(), SearchPaths::[dir()], TabWidth::integer())
%%      -> {ok, {syntaxTree(), moduleInfo()}}).
parse_annotate_file(FName, ByPassPreP, SearchPaths, TabWidth) ->
    FileFormat = refac_util:file_format(FName),
    case whereis(wrangler_ast_server) of
	undefined ->        %% this should not happen with Wrangler + Emacs.
	    ?wrangler_io("wrangler_ast_server is not defined\n", []),
	    parse_annotate_file(FName, ByPassPreP, SearchPaths, TabWidth, FileFormat);
	_ ->
	    get_ast({FName, ByPassPreP, SearchPaths, TabWidth, FileFormat})
    end.

%%-spec(parse_annotate_file(FName::filename(), ByPassPreP::boolean(), SearchPaths::[dir()], integer(), atom())
%%      -> {ok, {syntaxTree(), moduleInfo()}}).
parse_annotate_file(FName, true, SearchPaths, TabWidth, FileFormat) ->
    case refac_epp_dodger:parse_file(FName, [{tab, TabWidth}, {format, FileFormat}]) of
	{ok, Forms} ->
	    Dir = filename:dirname(FName),
	    DefaultIncl2 = [filename:join(Dir, X) || X <- refac_util:default_incls()],
	    Includes = SearchPaths ++ DefaultIncl2,
	    {Info0, Ms} = case refac_epp:parse_file(FName, Includes, [], TabWidth, FileFormat) of
			      {ok, Fs, {MDefs, MUses}} ->
                                  ST = refac_recomment:recomment_forms(Fs, []),
				  Info1 = refac_syntax_lib:analyze_forms(ST),
				  Ms1 = {dict:from_list(MDefs), dict:from_list(MUses)},
				  {Info1, Ms1};
			      _ -> {[], {dict:from_list([]), dict:from_list([])}}
			  end,
	    Comments = refac_comment_scan:file(FName, TabWidth),
            SyntaxTree = refac_recomment:recomment_forms(Forms, Comments),
            Info = refac_syntax_lib:analyze_forms(SyntaxTree),
	    Info2 = merge_module_info(Info0, Info),
	    AnnAST0 = annotate_bindings(FName, SyntaxTree, Info2, Ms, TabWidth),
	    AnnAST = refac_atom_annotation:type_ann_ast(FName, Info2, AnnAST0, SearchPaths, TabWidth),
	    {ok, {AnnAST, Info2}};
	{error, Reason} -> erlang:error(Reason)
    end;
parse_annotate_file(FName, false, SearchPaths, TabWidth, FileFormat) ->
    Dir = filename:dirname(FName),
    DefaultIncl2 = [filename:join(Dir, X) || X <- refac_util:default_incls()],
    Includes = SearchPaths ++ DefaultIncl2,
    case refac_epp:parse_file(FName, Includes, [], TabWidth, FileFormat) of
	{ok, Forms, Ms} -> Forms1 = lists:filter(fun (F) ->
							 case F of
							     {attribute, _, file, _} -> false;
							     {attribute, _, type, {{record, _}, _, _}} -> false;
							     _ -> true
							 end
						 end, Forms),
			   %% I wonder whether the all the following is needed;
			   %% we should never perform a transformation on an AnnAST from resulted from refac_epp;
			   SyntaxTree = refac_recomment:recomment_forms(Forms1, []),
			   Info = refac_syntax_lib:analyze_forms(SyntaxTree),
			   AnnAST0 = annotate_bindings(FName, SyntaxTree, Info, Ms, TabWidth),
			   {ok, {AnnAST0, Info}};
	{error, Reason} -> erlang:error(Reason)
    end.

quick_parse_annotate_file(FName, SearchPaths, TabWidth) ->
    FileFormat = refac_util:file_format(FName),
    case refac_epp_dodger:parse_file(FName, [{tab, TabWidth}, {format, FileFormat}]) of
	{ok, Forms} ->
	    Dir = filename:dirname(FName),
	    DefaultIncl2 = [filename:join(Dir, X) || X <- refac_util:default_incls()],
	    Includes = SearchPaths ++ DefaultIncl2,
	    Ms = case refac_epp:parse_file(FName, Includes, [], TabWidth, FileFormat) of
		     {ok, _, {MDefs, MUses}} ->
			 {dict:from_list(MDefs), dict:from_list(MUses)};
		     _ -> []
		 end,
	    SyntaxTree = refac_recomment:recomment_forms(Forms, []),
	    Info = refac_syntax_lib:analyze_forms(SyntaxTree),
	    AnnAST = annotate_bindings(FName, SyntaxTree, Info, Ms, TabWidth),
	    {ok, {AnnAST, Info}};
	{error, Reason} -> erlang:error(Reason)
    end.


merge_module_info(Info1, Info2) ->
    Info = lists:usort(Info1 ++ Info2),
    F = fun(Attr) ->
		lists:usort(lists:append(
			      [Vs||{Attr1,Vs} <- Info, 
				   Attr1==Attr]))
	end,
    M = case lists:keysearch(module, 1, Info) of
		 {value, R} ->
		     R;
		 _ -> {module, []}
	     end,
    NewInfo=[M, {exports,F(exports)}, {module_imports, F(module_imports)},
	     {imports, F(imports)}, {attributes,F(attributes)},
	     {records, F(records)}, {errors, F(errors)}, {warnings, F(warnings)},
	     {functions, F(functions)}, {rules, F(rules)}],
    [{A,V}||{A, V}<-NewInfo, V=/=[]].

annotate_bindings(FName, AST, Info, Ms, TabWidth) ->
    Toks = refac_util:tokenize(FName, true, TabWidth),
    AnnAST0 = refac_syntax_lib:annotate_bindings(add_token_and_ranges(AST, Toks), ordsets:new(), Ms),
    refac_annotate_ast:add_fun_define_locations(AnnAST0, Info).

  
%% Attach tokens to each form in the AST, and also add 
%% range information to each node in the AST.
%%-spec add_tokens(syntaxTree(), [token()]) -> syntaxTree(). 		 
add_token_and_ranges(SyntaxTree, Toks) ->
    Fs = refac_syntax:form_list_elements(SyntaxTree),
    NewFs = do_add_token_and_ranges(Toks, Fs),
    SyntaxTree1= rewrite(SyntaxTree, refac_syntax:form_list(NewFs)),
    add_range_to_body(SyntaxTree1, NewFs, "", "").

%% do it backwards starting from the last form. 
%% all the white spaces after a form belong to the next form if
%% there is one. 

do_add_token_and_ranges(Toks, Fs) ->
    do_add_token_and_ranges(lists:reverse(Toks), lists:reverse(Fs), []).

do_add_token_and_ranges(_, [], NewFs) ->
    NewFs;
do_add_token_and_ranges(Toks, _Forms=[F| Fs], NewFs) ->
    {FormToks0, RemToks} = get_form_tokens(Toks, F, Fs), 
    FormToks = lists:reverse(FormToks0),
    F1 = refac_syntax:add_ann({toks, FormToks}, F),
    F2 = add_category(add_range(F1, FormToks)),
    do_add_token_and_ranges(RemToks, Fs, [F2| NewFs]).

get_form_tokens(Toks, F, Fs) ->
    case refac_syntax:type(F) of
	comment ->
	    get_comment_form_toks(Toks, F, Fs);
	_ ->
	    get_non_comment_form_toks(Toks, F, Fs) 
    end.

%% stand-alone comments.
get_comment_form_toks(Toks, _F, Fs) when Fs==[] ->
    {Toks,[]};
get_comment_form_toks(Toks, F, _Fs) ->
    StartPos =start_pos(F),
    {Ts1,Ts2} = lists:splitwith(
		  fun(T) ->
			  token_loc(T)>=StartPos andalso 
			   is_whitespace_or_comment(T)
		  end, Toks),
    {Ts21, Ts22} = lists:splitwith(fun(T) ->
					   is_whitespace(T)
				   end, Ts2),
    {Ts1++Ts21, Ts22}.
 
get_non_comment_form_toks(Toks, _F, Fs) when Fs==[] ->
    {Toks, []};
get_non_comment_form_toks(Toks, F, _Fs) ->
    StartPos = start_pos(F),
    {Ts1, Ts2} = lists:splitwith(
		   fun(T) ->
			   token_loc(T)>=StartPos
		   end, Toks),
    {Ts21, Ts22} = lists:splitwith(
		     fun(T) ->
			     element(1, T) /=dot andalso
				 element(1,T)/=comment
		     end, Ts2),
    {Ts1++Ts21, Ts22}.

start_pos(F) ->
    case refac_syntax:type(F) of 
	error_marker ->
	    case F of
		{error, {_, {{Line, Col}, {_Line1, _Col1}}}} ->
		    {Line, Col};
		_ ->
		    refac_syntax:get_pos(F)
	    end;
	_ ->
	    case refac_syntax:get_precomments(F) of
		[] ->
		    refac_syntax:get_pos(F);
		[Com| _Tl] ->
		    refac_syntax:get_pos(Com)
	    end
    end.

%%-spec add_range(syntaxTree(), [token()]) -> syntaxTree(). 
add_range(AST, Toks) ->
    QAtomPs= [Pos||{qatom, Pos, _Atom}<-Toks],
    ast_traverse_api:full_buTP(fun do_add_range/2, AST, {Toks, QAtomPs}).

do_add_range(Node, {Toks, QAtomPs}) ->
    {L, C} = case refac_syntax:get_pos(Node) of
		 {Line, Col} -> {Line, Col};
		 Line -> {Line, 0}
	     end,
    case refac_syntax:type(Node) of
	variable ->
	    Len = length(refac_syntax:variable_literal(Node)),
	    refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node);
	atom ->
            case lists:member({L,C}, QAtomPs) of
                true ->
                    Len = length(atom_to_list(refac_syntax:atom_value(Node))), 
                    Node1 = refac_syntax:add_ann({qatom, true}, Node),
                    refac_syntax:add_ann({range, {{L, C}, {L, C + Len + 1}}}, Node1); 
                false ->
                    Len = length(atom_to_list(refac_syntax:atom_value(Node))), 
                    refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node)
	    end;
        operator ->
	    Len = length(atom_to_list(refac_syntax:atom_value(Node))),
	    refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node);
	char -> refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node);
	integer ->
	    Len = length(refac_syntax:integer_literal(Node)),
	    refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node);
	string ->
	    Toks1 = lists:dropwhile(fun (T) -> token_loc(T) < {L, C} end, Toks),
	    {Toks21, Toks22} = lists:splitwith(fun (T) -> is_string(T) orelse is_whitespace_or_comment(T) end, Toks1),
	    Toks3 = lists:filter(fun (T) -> is_string(T) end, Toks21),
	    case Toks3 of
		[] ->
		    Len = length(refac_syntax:string_literal(Node)),
		    Toks23 = lists:takewhile(fun (T) -> token_loc(T) < {L, C + Len - 1} end, Toks22),
		    Toks31 = lists:filter(fun (T) -> is_string(T) end, Toks23),
		    case Toks31 of
			[] ->
			    refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node);
			_ ->
			    Node1 = refac_syntax:add_ann({range, {{L, C}, {L, C + Len - 1}}}, Node),
			    refac_syntax:add_ann({toks, Toks31}, Node1)
		    end;
		_ -> Toks4 = lists:takewhile(fun (T) -> is_whitespace_or_comment(T) end, lists:reverse(Toks21)),
		     {L3, C3} = case Toks4 of
				    [] -> token_loc(hd(Toks22));
				    _ -> token_loc(lists:last(Toks4))
				end,
		     R = {token_loc(hd(Toks21)), {L3, C3 - 1}},
		     Node1 = refac_syntax:add_ann({range, R}, Node),
		     refac_syntax:add_ann({toks, Toks3}, Node1)
	    end;
	float ->
	    refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node); %% This is problematic.
	underscore -> refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node);
	eof_marker -> refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node);
	nil -> refac_syntax:add_ann({range, {{L, C}, {L, C + 1}}}, Node);
	module_qualifier ->
	    calc_and_add_range_to_node(Node, module_qualifier_argument, module_qualifier_body);
	list ->  %% temporay fix!
	    try
		Es = refac_syntax:list_elements(Node),
		case Es/=[] of
		    true ->
			Last = refac_util:glast("refac_util:do_add_range,list", Es),
			{_, E2} = get_range(Last),
                        E21 = extend_backwards(Toks, E2, ']'),
                        refac_syntax:add_ann({range, {{L,C}, E21}}, Node);
		    false ->
			Node
		end
	    catch
		_:_ ->
		    LP = refac_util:ghead("refac_util:do_add_range,list", refac_syntax:list_prefix(Node)),
		    {{L1, C1}, {L2, C2}} = get_range(LP),
                    case refac_syntax:list_suffix(Node) of
			none ->
			    refac_syntax:add_ann({range, {{L1, C1 - 1}, {L2, C2 + 1}}}, Node);
			Tail ->
			    {_S2, {L3, C3}} = get_range(Tail),
			    refac_syntax:add_ann({range, {{L1, C1 - 1}, {L3, C3}}}, Node)
		    end
	    end;
	application ->
	    O = refac_syntax:application_operator(Node),
	    Args = refac_syntax:application_arguments(Node),
	    {S1, E1} = get_range(O),
	    {S3, E3} = case Args of
			   [] -> {S1, E1};
			   _ -> La = refac_util:glast("refac_util:do_add_range, application", Args),
				{_S2, E2} = get_range(La),
				{S1, E2}
		       end,
	    E31 = extend_backwards(Toks, E3, ')'),
	    refac_syntax:add_ann({range, {S3, E31}}, Node);
	case_expr ->
	    A = refac_syntax:case_expr_argument(Node),
	    Lc = refac_util:glast("refac_util:do_add_range,case_expr", refac_syntax:case_expr_clauses(Node)),
	    calc_and_add_range_to_node_1(Node, Toks, A, Lc, 'case', 'end');
	clause ->
            {S1,_} = case refac_syntax:clause_patterns(Node) of 
                          [] -> case refac_syntax:clause_guard(Node) of 
                                    none ->{{L,C}, {0,0}};
                                    _ ->get_range(refac_syntax:clause_guard(Node))
                                end;
                          Ps -> get_range(hd(Ps))
                      end,         
            Body = refac_util:glast("refac_util:do_add_range, clause", refac_syntax:clause_body(Node)),
	    {_S2, E2} = get_range(Body),
	    refac_syntax:add_ann({range, {min(S1, {L,C}), E2}}, Node);
	catch_expr ->
	    B = refac_syntax:catch_expr_body(Node),
	    {S, E} = get_range(B),
	    S1 = extend_forwards(Toks, S, 'catch'),
	    refac_syntax:add_ann({range, {S1, E}}, Node);
	if_expr ->
	    Cs = refac_syntax:if_expr_clauses(Node),
	    add_range_to_list_node(Node, Toks, Cs, "refac_util:do_add_range, if_expr",
				   "refac_util:do_add_range, if_expr", 'if', 'end');
	cond_expr ->
	    Cs = refac_syntax:cond_expr_clauses(Node),
	    add_range_to_list_node(Node, Toks, Cs, "refac_util:do_add_range, cond_expr",
				   "refac_util:do_add_range, cond_expr", 'cond', 'end');
	infix_expr ->
	    calc_and_add_range_to_node(Node, infix_expr_left, infix_expr_right);
	prefix_expr ->
	    calc_and_add_range_to_node(Node, prefix_expr_operator, prefix_expr_argument);
	conjunction ->
	    B = refac_syntax:conjunction_body(Node),
	    add_range_to_body(Node, B, "refac_util:do_add_range,conjunction",
			      "refac_util:do_add_range,conjunction");
	disjunction ->
	    B = refac_syntax:disjunction_body(Node),
	    add_range_to_body(Node, B, "refac_util:do_add_range, disjunction",
			      "refac_util:do_add_range,disjunction");
	function ->
	    F = refac_syntax:function_name(Node),
	    Cs = refac_syntax:function_clauses(Node),
	    Lc = refac_util:glast("refac_util:do_add_range,function", Cs),
	    {S1, _E1} = get_range(F),
	    {_S2, E2} = get_range(Lc),
	    refac_syntax:add_ann({range, {S1, E2}}, Node);
	fun_expr ->
	    Cs = refac_syntax:fun_expr_clauses(Node),
	    S = refac_syntax:get_pos(Node),
	    Lc = refac_util:glast("refac_util:do_add_range, fun_expr", Cs),
	    {_S1, E1} = get_range(Lc),
	    E11 = extend_backwards(Toks, E1,
				   'end'),   %% S starts from 'fun', so there is no need to extend forwards/
	    refac_syntax:add_ann({range, {S, E11}}, Node);
	arity_qualifier ->
	    calc_and_add_range_to_node(Node, arity_qualifier_body, arity_qualifier_argument);
	implicit_fun ->
	    S = refac_syntax:get_pos(Node),
	    N = refac_syntax:implicit_fun_name(Node),
	    {_S1, E1} = get_range(N),
	    refac_syntax:add_ann({range, {S, E1}}, Node);
	attribute ->
	    Name = refac_syntax:attribute_name(Node),
	    Args = refac_syntax:attribute_arguments(Node),
	    case Args of
		none -> {S1, E1} = get_range(Name),
			S11 = extend_forwards(Toks, S1, '-'),
			refac_syntax:add_ann({range, {S11, E1}}, Node);
		_ -> case length(Args) > 0 of
			 true -> Arg = refac_util:glast("refac_util:do_add_range,attribute", Args),
				 {S1, _E1} = get_range(Name),
				 {_S2, E2} = get_range(Arg),
				 S11 = extend_forwards(Toks, S1, '-'),
				 refac_syntax:add_ann({range, {S11, E2}}, Node);
			 _ -> {S1, E1} = get_range(Name),
			      S11 = extend_forwards(Toks, S1, '-'),
			      refac_syntax:add_ann({range, {S11, E1}}, Node)
		     end
	    end;
	generator ->
	    calc_and_add_range_to_node(Node, generator_pattern, generator_body);
	binary_generator ->
	    calc_and_add_range_to_node(Node, binary_generator_pattern, binary_generator_body);
	tuple ->
	    Es = refac_syntax:tuple_elements(Node),
	    case length(Es) of
		0 -> refac_syntax:add_ann({range, {{L, C}, {L, C + 1}}}, Node);
		_ ->
		    add_range_to_list_node(Node, Toks, Es, "refac_util:do_add_range, tuple",
					   "refac_util:do_add_range, tuple",
					   '{', '}')
	    end;
	list_comp ->
	    %%T = refac_syntax:list_comp_template(Node),
	    B = refac_util:glast("refac_util:do_add_range,list_comp", refac_syntax:list_comp_body(Node)),
	    {_S2, E2} = get_range(B),
	    E21 = extend_backwards(Toks, E2, ']'),
	    refac_syntax:add_ann({range, {{L,C}, E21}}, Node);
	binary_comp ->
	    %%T = refac_syntax:binary_comp_template(Node),
	    B = refac_util:glast("refac_util:do_add_range,binary_comp",
				 refac_syntax:binary_comp_body(Node)),
	    {_S2, E2} = get_range(B),
	    E21 = extend_backwards(Toks, E2, '>>'),
	    refac_syntax:add_ann({range, {{L,C}, E21}}, Node);
	block_expr ->
	    Es = refac_syntax:block_expr_body(Node),
	    add_range_to_list_node(Node, Toks, Es, "refac_util:do_add_range, block_expr",
				   "refac_util:do_add_range, block_expr", 'begin', 'end');
	receive_expr ->
	    case refac_syntax:receive_expr_timeout(Node) of
		none ->
		    Cs = refac_syntax:receive_expr_clauses(Node),
		    case length(Cs) of
			0 -> refac_syntax:add_ann({range, {L, C}, {L, C}}, Node);
			_ ->
			    add_range_to_list_node(Node, Toks, Cs, "refac_util:do_add_range, receive_expr1",
						   "refac_util:do_add_range, receive_expr1", 'receive', 'end')
		    end;
		_E ->
		    Cs = refac_syntax:receive_expr_clauses(Node),
		    A = refac_syntax:receive_expr_action(Node),
		    case length(Cs) of
			0 ->
			    {_S2, E2} = get_range(refac_util:glast("refac_util:do_add_range, receive_expr2", A)),
			    refac_syntax:add_ann({range, {{L, C}, E2}}, Node);
			_ ->
			    Hd = refac_util:ghead("refac_util:do_add_range,receive_expr2", Cs),
			    {S1, _E1} = get_range(Hd),
			    {_S2, E2} = get_range(refac_util:glast("refac_util:do_add_range, receive_expr3", A)),
			    S11 = extend_forwards(Toks, S1, 'receive'),
			    E21 = extend_backwards(Toks, E2, 'end'),
			    refac_syntax:add_ann({range, {S11, E21}}, Node)
		    end
	    end;
	try_expr ->
	    B = refac_syntax:try_expr_body(Node),
	    After = refac_syntax:try_expr_after(Node),
	    {S1, _E1} = get_range(refac_util:ghead("refac_util:do_add_range, try_expr", B)),
	    {_S2, E2} = case After of
			    [] ->
				Handlers = refac_syntax:try_expr_handlers(Node),
				get_range(refac_util:glast("refac_util:do_add_range, try_expr", Handlers));
			    _ ->
				get_range(refac_util:glast("refac_util:do_add_range, try_expr", After))
			end,
	    S11 = extend_forwards(Toks, S1, 'try'),
	    E21 = extend_backwards(Toks, E2, 'end'),
	    refac_syntax:add_ann({range, {S11, E21}}, Node);
	binary ->
	    Fs = refac_syntax:binary_fields(Node),
	    case Fs == [] of
		true -> refac_syntax:add_ann({range, {{L, C}, {L, C + 3}}}, Node);
		_ ->
		    Hd = refac_util:ghead("do_add_range:binary", Fs),
		    Last = refac_util:glast("do_add_range:binary", Fs),
		    calc_and_add_range_to_node_1(Node, Toks, Hd, Last, '<<', '>>')
	    end;
	binary_field ->
	    Body = refac_syntax:binary_field_body(Node),
	    Types = refac_syntax:binary_field_types(Node),
	    {S1, E1} = get_range(Body),
	    {_S2, E2} = if Types == [] -> {S1, E1};
			   true -> get_range(refac_util:glast("refac_util:do_add_range,binary_field", Types))
			end,
	    case E2 > E1  %%Temporal fix; need to change refac_syntax to make the pos info correct.
		of
		true ->
		    refac_syntax:add_ann({range, {S1, E2}}, Node);
		false ->
		    refac_syntax:add_ann({range, {S1, E1}}, Node)
	    end;
	match_expr ->
	    calc_and_add_range_to_node(Node, match_expr_pattern, match_expr_body);
	form_list ->
	    Es = refac_syntax:form_list_elements(Node),
	    
	    add_range_to_body(Node, Es, "refac_util:do_add_range, form_list",
			      "refac_util:do_add_range, form_list");
	parentheses ->
	    B = refac_syntax:parentheses_body(Node),
	    {S, E} = get_range(B),
	    S1 = extend_forwards(Toks, S, '('),
	    E1 = extend_backwards(Toks, E, ')'),
	    refac_syntax:add_ann({range, {S1, E1}}, Node);
	class_qualifier ->
	    calc_and_add_range_to_node(Node, class_qualifier_argument, class_qualifier_body);
	qualified_name ->
	    Es = refac_syntax:qualified_name_segments(Node),
	    
	    add_range_to_body(Node, Es, "refac_util:do_add_range, qualified_name",
			      "refac_util:do_add_range, qualified_name");
	query_expr ->
	    B = refac_syntax:query_expr_body(Node),
	    {S, E} = get_range(B),
	    refac_syntax:add_ann({range, {S, E}}, Node);
	record_field ->
	    Name = refac_syntax:record_field_name(Node),
	    {S1, E1} = get_range(Name),
	    Value = refac_syntax:record_field_value(Node),
	    case Value of
		none -> refac_syntax:add_ann({range, {S1, E1}}, Node);
		_ -> {_S2, E2} = get_range(Value),refac_syntax:add_ann({range, {S1, E2}}, Node)
	    end;
	typed_record_field ->   %% This is not correct; need to be fixed later!
	    Field = refac_syntax:typed_record_field(Node),
	    {S1, _E1} = get_range(Field),
	    Type = refac_syntax:typed_record_type(Node),
	    {_S2, E2} = get_range(Type),
	    refac_syntax:add_ann({range, {S1, E2}}, Node);
	record_expr ->
	    Arg = refac_syntax:record_expr_argument(Node),
	    Type = refac_syntax:record_expr_type(Node),
	    Fields = refac_syntax:record_expr_fields(Node),
	    {S1, E1} = case Arg of
			   none -> get_range(Type);
			   _ -> get_range(Arg)
		       end,
	    case Fields of
		[] -> E11 = extend_backwards(Toks, E1, '}'),
		      refac_syntax:add_ann({range, {S1, E11}}, Node);
		_ ->
		    {_S2, E2} = get_range(refac_util:glast("refac_util:do_add_range,record_expr", Fields)),
		    E21 = extend_backwards(Toks, E2, '}'),
		    refac_syntax:add_ann({range, {S1, E21}}, Node)
	    end;
	record_access ->
	    calc_and_add_range_to_node(Node, record_access_argument, record_access_field);
	record_index_expr ->
	    calc_and_add_range_to_node(Node, record_index_expr_type, record_index_expr_field);
	comment ->
	    T = refac_syntax:comment_text(Node),
	    Lines = length(T),
	    refac_syntax:add_ann({range, {{L, C}, {L + Lines - 1,
						   length(refac_util:glast("refac_util:do_add_range,comment", T))}}},
				 Node);
	macro ->
	    Name = refac_syntax:macro_name(Node),
	    Args = refac_syntax:macro_arguments(Node),
	    {_S1, {L1, C1}} = get_range(Name),
            E1={L1, C1+1},
	    case Args of
		none -> refac_syntax:add_ann({range, {{L, C}, E1}}, Node);
		Ls ->
		    case Ls of
			[] -> E21 = extend_backwards(Toks, E1, ')'),
			      refac_syntax:add_ann({range, {{L, C}, E21}}, Node);
			_ ->
			    La = refac_util:glast("refac_util:do_add_range,macro", Ls),
			    {_S2, E2} = get_range(La),
			    E21 = extend_backwards(Toks, E2, ')'),
			    refac_syntax:add_ann({range, {{L, C}, E21}}, Node)
		    end
	    end;
	size_qualifier ->
	    calc_and_add_range_to_node(Node, size_qualifier_body, size_qualifier_argument);
	error_marker ->
	    refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node);
	type ->   %% This is not correct, and need to be fixed!!
	    refac_syntax:add_ann({range, {{L, C}, {L, C}}}, Node);
	_ ->
	    %% refac_io:format("Node;\n~p\n",[Node]),
	    %% ?wrangler_io("Unhandled syntax category:\n~p\n", [refac_syntax:type(Node)]),
	    Node
    end.

calc_and_add_range_to_node(Node, Fun1, Fun2) ->
    Arg = refac_syntax:Fun1(Node),
    Field = refac_syntax:Fun2(Node),
    {S1,_E1} = get_range(Arg),
    {_S2,E2} = get_range(Field),
    refac_syntax:add_ann({range,{S1,E2}},Node).

calc_and_add_range_to_node_1(Node, Toks, StartNode, EndNode, StartWord, EndWord) ->
    {S1,_E1} = get_range(StartNode),
    {_S2,E2} = get_range(EndNode),
    S11 = extend_forwards(Toks,S1,StartWord),
    E21 = extend_backwards(Toks,E2,EndWord),
    refac_syntax:add_ann({range,{S11,E21}},Node).


get_range(Node) ->
     As = refac_syntax:get_ann(Node),
     case lists:keysearch(range, 1, As) of
       {value, {range, {S, E}}} -> {S, E};
       _ -> {?DEFAULT_LOC,
 	   ?DEFAULT_LOC} 
     end.

add_range_to_list_node(Node, Toks, Es, Str1, Str2, KeyWord1, KeyWord2) ->
    Hd = refac_util:ghead(Str1, Es),
    La = refac_util:glast(Str2, Es),
    calc_and_add_range_to_node_1(Node, Toks, Hd, La, KeyWord1, KeyWord2).

add_range_to_body(Node, B, Str1, Str2) ->
    H = refac_util:ghead(Str1, B),
    La = refac_util:glast(Str2, B),
    {S1, _E1} = get_range(H),
    {_S2, E2} = get_range(La),
    refac_syntax:add_ann({range, {S1, E2}}, Node).
   
extend_forwards(Toks, StartLoc, Val) ->
    Toks1 = lists:takewhile(fun (T) -> token_loc(T) < StartLoc end, Toks),
    Toks2 = lists:dropwhile(fun (T) -> token_val(T) =/= Val end, lists:reverse(Toks1)),
    case Toks2 of
      [] -> StartLoc;
      _ -> token_loc(hd(Toks2))
    end.

extend_backwards(Toks, EndLoc, Val) ->
    Toks1 = lists:dropwhile(fun (T) -> token_loc(T) =< EndLoc end, Toks),
    Toks2 = lists:dropwhile(fun (T) -> token_val(T) =/= Val end, Toks1),
    case Toks2 of
      [] -> EndLoc;
      _ ->
	  {Ln, Col} = token_loc(hd(Toks2)),
	  {Ln, Col + length(atom_to_list(Val)) - 1}
    end.

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

	
is_whitespace({whitespace, _, _}) ->
    true;
is_whitespace(_) ->
    false.

is_whitespace_or_comment({whitespace, _, _}) ->
    true;
is_whitespace_or_comment({comment, _, _}) ->
    true;
is_whitespace_or_comment(_) -> false.
	
    
is_string({string, _, _}) ->
    true;
is_string(_) -> false.


%% =====================================================================
% @doc Attach syntax category information to AST nodes.
%% =====================================================================
%% -type (category():: pattern|expression|guard_expression|record_type|generator
%%                    record_field| {macro_name, none|int(), pattern|expression}
%%                    |operator
%%-spec(add_category(Node::syntaxTree()) -> syntaxTree()).
add_category(Node) ->
    add_category(Node, none).

add_category(Node, C) ->
    {Node1, _} =ast_traverse_api:stop_tdTP(fun do_add_category/2, Node, C),
    Node1.

do_add_category(Node, C) when is_list(Node) ->
    {[add_category(E, C)||E<-Node], true};
do_add_category(Node, C) ->
    case refac_syntax:type(Node) of
	clause ->
	    Body = refac_syntax:clause_body(Node),
	    P = refac_syntax:clause_patterns(Node),
	    G = refac_syntax:clause_guard(Node),
	    Body1 = [add_category(B, expression)||B<-Body],
	    P1 = add_category(P, pattern),
	    G1 = case G of
		     none -> none;
		     _ -> add_category(G, guard_expression)
		 end,
	    Node1 =rewrite(Node, refac_syntax:clause(P1, G1, Body1)),
	    {Node1, true};
	application ->
	    Op = refac_syntax:application_operator(Node),
	    Args = refac_syntax:application_arguments(Node),
	    Op1 = add_category(Op, C),
	    Op2 = refac_util:update_ann(Op1,{category, application_op}),
	    Args1 = add_category(Args, C),
	    Node1 = rewrite(Node, refac_syntax:application(Op2, Args1)),
	    {refac_syntax:add_ann({category, C}, Node1), true};
	match_expr ->
	    P = refac_syntax:match_expr_pattern(Node),
	    B = refac_syntax:match_expr_body(Node),
	    P1 = add_category(P, pattern),
	    B1 = add_category(B, C),
	    Node1=rewrite(Node, refac_syntax:match_expr(P1, B1)),
	    case C/=expression of 
		true ->
		    {refac_syntax:add_ann({category, C}, Node1), true};
		false ->
		    {Node1, true}
	    end;
	generator ->
	    P = refac_syntax:generator_pattern(Node),
	    B = refac_syntax:generator_body(Node),
	    P1 = add_category(P, pattern),
	    B1 = add_category(B, expression),
	    Node1=rewrite(Node, refac_syntax:generator(P1, B1)),
	    {refac_syntax:add_ann({category, generator}, Node1), true};
	binary_generator ->
	    P = refac_syntax:binary_generator_pattern(Node),
	    B = refac_syntax:binary_generator_body(Node),
	    P1 = add_category(P, pattern),
	    B1 = add_category(B, expression),
	    Node1=rewrite(Node, refac_syntax:binary_generator(P1, B1)),
	    {refac_syntax:add_ann({category, generator}, Node1), true};
	macro ->
	    Name = refac_syntax:macro_name(Node),
	    Args = refac_syntax:macro_arguments(Node),
	    NumOfArgs = case Args of
			    none -> none;
			    _ -> length(Args)
			end,
	    Name1 = add_category(Name, {macro_name, NumOfArgs, C}),
	    Args1 = case Args of
			none -> none;
			_ -> add_category(Args, C) 
		    end,
	    Node1 = rewrite(Node, refac_syntax:macro(Name1, Args1)),
	    {refac_syntax:add_ann({category, C}, Node1), true};
	 record_access ->
	    Argument = refac_syntax:record_access_argument(Node),
	    Type = refac_syntax:record_access_type(Node),
	    Field = refac_syntax:record_access_field(Node),
	    Argument1 = add_category(Argument, C),
	    Type1 = case Type of
			none -> none;
			_ -> add_category(Type, record_type)
		    end,
	    Field1 = add_category(Field, record_field),
	    Node1 = rewrite(Node, refac_syntax:record_access(Argument1, Type1, Field1)),
	    {refac_syntax:add_ann({category, C}, Node1), true};
	record_expr ->
	    Argument = refac_syntax:record_expr_argument(Node),
	    Type = refac_syntax:record_expr_type(Node),
	    Fields = refac_syntax:record_expr_fields(Node),
	    Argument1 = case Argument of
			    none -> none;
			    _ -> add_category(Argument, C)
			end,
	    Type1 = add_category(Type, record_type),
	    Fields1 =[refac_syntax:add_ann({category, record_field},rewrite(F, refac_syntax:record_field(
			add_category(refac_syntax:record_field_name(F), record_field),
			case refac_syntax:record_field_value(F) of 
			    none -> 
				none;
			    V -> 
				add_category(V, C)
			end))) || F<-Fields],
	    Node1 = rewrite(Node, refac_syntax:record_expr(Argument1, Type1, Fields1)),
	    {refac_syntax:add_ann({category, C}, Node1), true};
	record_index_expr ->
	    Type = refac_syntax:record_index_expr_type(Node),
	    Field = refac_syntax:record_index_expr_field(Node),
	    Type1 = add_category(Type, record_type),
	    Field1 = add_category(Field, record_field),
	    Node1 = rewrite(Node, refac_syntax:record_index_expr(Type1, Field1)),
	    {refac_syntax:add_ann({category, C}, Node1), true};
	operator ->
	    {refac_syntax:add_ann({category, operator}, Node), true};
	_ -> case C of
		 none ->
		     {Node, false};
		 _ -> 
		     {refac_syntax:add_ann({category,C}, Node),false}
	     end
    end.

rewrite(Tree, Tree1) ->
    refac_syntax:copy_attrs(Tree, Tree1).


           
