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
%%@doc 
%% This module defines the API exposed by Wrangler for users to compose their
%% own refactoring or code inspection functions. A refactoring consist of 
%% two parts: program analysis and program transformation. Both program analysis
%% and program transformation involves various AST traversals and manipulations,
%% and requires deep knowledge of the AST representation details. To make the 
%% processing of writing a refactoring easier, through this refactoring API, we 
%% aim to provide a template and rule based program analysis and transformation 
%% framework to allow users to express their own program transformation in 
%% a very concise way. This is achieved through the following aspects:
%%
%% -- The use of code templates to make the composition/decomposition of 
%%    of AST nodes straightforward.
%%
%% -- The use of template-based transformation rules to allow concise 
%%    representation of transformations.
%%
%% -- Context information annotated to each AST node to make the program 
%%    analysis easier to implement.
%%
%% -- A collect of predefined macros and API functions to capture the 
%%    most frequently used functionalities when writing a refactoring.
%%
%% -- A refactoring behaviour `gen_refac' to provide a high-level abstraction
%%    of the logic work-flow of a refactoring process.
%%
%% Some macros:
%%<ul>
%%<li>
%%?T(TemplateStr).
%%
%%?T(TemplateStr) denotes a template code fragment. A template code fragment 
%% is a string 
%%representing an Erlang expression, function, or a function clause. 
%% Both object variables and meta variables can be 
%% used in a template string. Variable names ending with `@', `@'`@', or `@'`@'`@'
%% are meta-variables; and variable names not ending with `@' are object 
%% variables. A meta variable ending with only one `@' is a place holder 
%% for a single program entity which maps to a single AST tree; whereas 
%% a meta variable ending with `@'`@' represents a list of program entities
%% seperated by comma, which map to a list of AST trees; a meta variable 
%% ending with `@'`@'`@' is only used for writing templates that match 
%% a function with arbitray function clauses or a clause with arbitray 
%% clause bodies. For example `?T("f@(Args@'`@'`@'`) when Guard@'`@'`@'`-> Body@'`@'`@'`.")' can be
%% used to match function definitions with arbitray number of function 
%% clauses.
%% 
%%</li>
%%
%%<li>
%%?RULE(TemplateBefore, TemplateAfter, Cond).
%% 
%% If the AST node represented by `TemplateBefore' pattern matches with the 
%% current AST node, and `Cond' evaluates to `true', then transform the node 
%% according to `TemplateAfter'. When a code template pattern matches to an 
%% AST nodes, each meta variable in the template code is bound to an AST
%% node, and meta variables with the same name should always map to AST nodes
%% that are semantically the same. A special meta variable names `_@This' is 
%% implicitly bound to the whole AST node that matches the code template. 
%% All these meta-variables are visible, and can be used, in `TemplateAfter' and 
%% `Cond'. 
%%
%%</li>
%%<li>
%%?COLLECT(Template, CollectorFun, Cond, Scope).
%%
%% For every AST node in `Scope' that pattern matches the AST represented by `Template', if
%% `Cond' evaluates to `true', then information about this node is collected using
%% the function specified by `CollectorFun'. `Scope' can be an AST or a list of Erlang 
%% files/directories, and in the latter case, each Erlang file included is parsed into
%% an AST first. 
%%</li>
%%<li>
%%?COLLECT_LOC(Template, CollectorFun, Cond, Scope).
%%
%% For every AST node in `Scope' that pattern matches the AST represented by `Template', if
%% `Cond' evaluates to `true', then the location information of this node in the format of 
%% `{filename(), {pos(),pos()}}' is collected. `Scope' should list of Erlang 
%% files/directories.
%%
%%</li>
%%<li>
%%?EQUAL(Tree1, Tree2).
%%
%% Returns `true' if `Tree1' and `Tree2' are syntactically the same up to normalization. 
%% The normalization process includes consistent variable renaming and turning un-qualified 
%% function calls into qualified function calls. 
%%
%%</li>
%%<li>
%%?QUOTE(Str).
%% 
%%Returns the AST representation of the code represented by `Str', which can be an
%%Erlang expression, an Erlang function or a function clause.
%%
%%</li>
%%<li>
%%?SPLICE(Tree).
%%
%%Pretty-prints the AST `Tree', and returns the string representation.
%%
%%</li>
%%<li>
%%?MATCH(Template, Tree).
%%
%%Pattern matches the AST representation of `Template' with the AST `Tree', and returns 
%%`false' if the pattern matching fails, and `{true, Binds}' if succeeds, where `Bind' represents the 
%%binding of meta-variables to AST nodes in the 
%%format: `[{MetaVariableName, syntaxTreee()}]'.
%%
%%</li>
%%<li>
%%?FULL_TD(Rules, Scope).
%%
%% Traverse the AST in a topdown order, and for each node apply the first rule that 
%% succeeds; after a rule has been applied to a node, the subtrees of the node will 
%% continued to be traversed.
%%
%%</li>
%%<li>
%%?STOP_TD(Rules, Scope).
%% Traverse the AST in a topdown order, and for each node apply the first rule that 
%% succeeds; after a rule has been applied to a node, the subtrees of the node will 
%% not to be traversed.
%%</li>
%%</ul>
%% Some example refactorings implemented using the Wrangler API:
%%<ul>
%%<li>
%%<a href="file:refac_swap_args.erl" > Swap arguments of a function;</a>.
%%</li>
%%<li>
%%<a href="file:refac_keysearch_to_keyfind.erl"> replace the use of lists:keysearch/3 with lists:keyfind/3; </a>
%%</li>
%%<li>
%%<a href="file:refac_specialise.erl"> Specialise a function definition; </a>
%%</li>
%%<li>
%%<a href="file:refac_apply_to_remote_call.erl"> Apply to remote function call; </a>
%%</li>
%%<li>
%%<a href="file:refac_intro_import.erl">Introduce an import attribute; </a>
%%</li>
%%<li>
%%<a href="file:refac_remove_import.erl">Remove an import attribute;</a>
%%</li>
%%<li>
%%<a href="file:refac_list.erl"> Various list-related transformations;</a>
%%</li>
%%<li>
%%<a href="file:refac_batch_rename_fun.erl"> Batch renaming of function names from camelCaseto camel_case. </a>
%%</li>
%%</ul>

-module(refac_api).

-export([is_var_name/1, 
         is_fun_name/1,
         make_new_name/2,
         env_vars/1,
         env_var_names/1,
         exported_vars/1,
         exported_var_names/1,
         bound_vars/1,
         bound_var_names/1,
         free_vars/1,
         free_var_names/1,
         start_end_loc/1,
         syntax_context/1,
         syntax_category/1,
         is_expr/1, 
         is_guard_expr/1,
         is_pattern/1, 
         is_exported/2,
         is_attribute/2,
         is_import/2,
         exported_funs/1,
         imported_funs/1,
         imported_funs/2,
         inscope_funs/1, 
         defined_funs/1,
         get_ast/1, 
         get_module_info/1,
         client_files/2,
         module_name/1,
         tokenize/3,
         variable_define_pos/1,
         fun_define_info/1,
         mfa_to_fun_def/2,
         insert_an_attr/2,
         remove_from_import/2,
         add_to_export_after/3,
         splice/1,
         equal/2,
         quote/1]).

-export([parse_annotate_expr/1, 
         parse_annotate_expr/2,
         subst/2, 
         make_rule/3,
         collect/4,
         match/2,
         full_td_search_and_transform/2,
         stop_td_search_and_transform/2]).

-include("../include/wrangler.hrl"). 

%% ====================================================================
%%@doc Returns the start and end locations of an AST node or a sequence 
%%     of AST node. {{0,0},{0,0}} is returned if the AST nodes are not 
%%     annotated with location information.
%%@spec start_end_loc([syntaxTree()]|syntaxTree()) ->{pos(), pos()}
%%@type pos() = {integer(), integer()}

-spec start_end_loc([syntaxTree()]|syntaxTree()) ->{pos(), pos()}.
start_end_loc(Exprs) when is_list(Exprs) ->
    E1 = hd(Exprs),
    En = lists:last(Exprs),
    {S, _E} = get_range(E1),
    {_S, E} = get_range(En),
    {S, E};
start_end_loc(Expr) ->
    get_range(Expr).

get_range(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(range, 1, As) of
	{value, {range, {S, E}}} -> 
            {S, E};
	_ -> 
            {?DEFAULT_LOC,?DEFAULT_LOC} 
    end.

%% ======================================================================
%% @doc Generates a new name by appending "_1" to the end of the 'BaseName'
%%      until the new name is not a member of `UsedNames'.
%%@spec make_new_name(atom(), [atom()]) ->atom()

-spec make_new_name(string(), [atom()]) ->atom().		   
make_new_name(BaseName, UsedNames) ->
    NewName = list_to_atom(atom_to_list(BaseName) ++ "_1"),
    case ordsets:is_element(NewName, UsedNames) of
	true ->
	    make_new_name(NewName, UsedNames);
	_ -> 
	    NewName
    end.

%% =====================================================================
%%@doc Returns `true' if a string is lexically a legal variable name,
%%      otherwise `false'.
%%@spec is_var_name(string())-> boolean()

-spec(is_var_name(Name:: string())-> boolean()).
is_var_name(Name) ->
    case Name of
      [] -> false;
      [H] -> is_upper(H) and (H =/= 95);
      [H| T] -> (is_upper(H) or (H == 95)) and is_var_name_tail(T)
    end.

%%@doc Returns `true' if a string is lexically a legal function name,
%%      otherwise `false'.
%%@spec is_fun_name(string())-> boolean()
is_fun_name(Name) ->
    case Name of
      [H| T] -> is_lower(H) and is_var_name_tail(T);
      [] -> false
    end.

is_var_name_tail(Name) ->
    case Name of
      [H| T] ->
	  (is_upper(H) or is_lower(H) or 
	   is_digit(H) or (H == 64) or (H == 95)) and
	    is_var_name_tail(T);
      [] -> true
    end.

is_upper(L) -> (L >= 65) and (90 >= L).

is_lower(L) -> (L >= 97) and (122 >= L).

is_digit(L) -> (L >= 48) and (57 >= L).


%% =====================================================================
%%@doc Returns all the variables, including both variable name and 
%%     define location, that are visible to `Node'. 
%%@spec env_vars(syntaxTree())-> [{atom(), pos()}]

-spec(env_vars(Node::syntaxTree())-> [{atom(), pos()}]).
env_vars(Node) ->
    Ann = refac_syntax:get_ann(Node),
    case lists:keyfind(env, 1, Ann) of
        {env, Vs} ->
            Vs;
        false ->
            []
    end.

%%@doc Returns all the variable names that are visible to `Node'. 
%%@spec env_var_names(syntaxTree())-> [atom()]

-spec(env_var_names(Node::syntaxTree())-> [atom()]).
env_var_names(Node) ->
    element(1, lists:unzip(env_vars(Node))).

%%=====================================================================
%%@doc Returns all the variables, including both variable name and define
%%      location, that are declared within `Node', and also used by the 
%%      code outside `Node'.
%%@spec exported_vars([syntaxTree()]|syntaxTree())-> [{atom(),pos()}]

-spec(exported_vars(Node::[syntaxTree()]|syntaxTree())-> [{atom(),pos()}]).
exported_vars(Nodes) when is_list(Nodes) ->
    Range = start_end_loc(Nodes),
    lists:flatmap(fun (Node) -> 
                          exported_vars_1(Node, Range)
                  end, Nodes);
exported_vars(Node) ->
    Range = start_end_loc(Node),
    exported_vars_1(Node, Range).

exported_vars_1(Node, {StartLoc, EndLoc}) ->
    Fun = fun (N, Acc) ->
                  Ann = refac_syntax:get_ann(N),
                  case lists:keyfind(use,1,Ann) of 
                      {use, Locs} ->
                          case [L||L<-Locs, L>EndLoc orelse L < StartLoc] of
                              [] -> Acc;
                              _ ->
                                  Name = refac_syntax:variable_name(N),
                                  Pos = refac_syntax:get_pos(N),
                                  ordsets:add_element({Name,Pos}, Acc)
                          end;
                      false ->
                          Acc
                  end
          end,
    ordsets:to_list(ast_traverse_api:fold(Fun,ordsets:new(),Node)).
 
%%@doc Returns all the variable names that are declared within `Node', and 
%%    also used by the code outside `Node'.
%%@spec exported_var_names([syntaxTree()]|syntaxTree())-> [atom()]

-spec(exported_var_names(Node::[syntaxTree()]|syntaxTree())-> [atom()]).
exported_var_names(Node) ->            
    element(1, lists:unzip(exported_vars(Node))).


%%=====================================================================
%%@doc Returns all the variables, including both variable name and define
%%      location, that are declared within `Node'.
%%@spec bound_vars([syntaxTree()]|syntaxTree())-> [{atom(),pos()}]

-spec(bound_vars(Node::[syntaxTree()]|syntaxTree())-> [{atom(),pos()}]).
bound_vars(Nodes) when is_list(Nodes) ->
    lists:usort(lists:flatmap(fun (Node) -> 
                                      bound_vars(Node) 
                              end, Nodes));
bound_vars(Node) ->
    Fun = fun (N, Acc) ->
                  Ann = refac_syntax:get_ann(N),
                  case lists:keyfind(bound,1,Ann) of
                      {bound, Vs} ->
                          Vs ++ Acc;
                      false ->
                          Acc
                  end
          end,
    Vars=ast_traverse_api:fold(Fun, [], Node),
    lists:usort(Vars).

%%@doc Returns all the variable names that are declared within `Node'.
%%@spec bound_var_names([syntaxTree()]|syntaxTree())-> [atom()]

-spec(bound_var_names(Node::[syntaxTree()]|syntaxTree())-> [atom()]).
bound_var_names(Node)->		       
    element(1, lists:unzip(bound_vars(Node))).


%%=====================================================================
%%@doc Returns all the variables, including both variable name and define
%%      location, that are free within `Node'.
%%@spec free_vars([syntaxTree()]|syntaxTree())-> [{atom(),pos()}]

-spec(free_vars(Node::[syntaxTree()]|syntaxTree())-> [{atom(),pos()}]).
free_vars(Nodes) when is_list(Nodes) ->
    {FVs, BVs} = lists:unzip([{free_vars(Node), bound_vars(Node)}
                              ||Node<-Nodes]),
    lists:usort(lists:append(FVs)) -- lists:usort(lists:append(BVs));

free_vars(Node) ->
    Ann = refac_syntax:get_ann(Node),
    case lists:keyfind(free,1,Ann) of 
        {free, Vs} ->
            Vs;
        false ->
            []
    end.

%%@doc Returns all the variable names that are free within `Node'.
%%@spec free_var_names([syntaxTree()]|syntaxTree())-> [atom()]

-spec(free_var_names(Node::[syntaxTree()]|syntaxTree())-> [atom()]).
free_var_names(Node) ->
    element(1, lists:unzip(free_vars(Node))).

%% =============================================================================
%%@doc Returns the syntax context of `Node'.
%%@spec syntax_context(syntaxTree()) -> atom()
-spec(syntax_context(Node::syntaxTree()) ->atom()).
syntax_context(Node) ->
    Ann = refac_syntax:get_ann(Node),
    case lists:keysearch(syntax_path, 1, Ann) of
        {value, {syntax_path, P}} -> P;
        false ->
            throw({error, "Wrangler internal error "
                   "in refac_api:syntax_context/1"})
    end.
   
%% ================================================================================
%%@doc Returns the syntax category of `Node'.
%%@spec syntax_category(syntaxTree()) -> pattern|expression|guard_expression|unknown
-spec(syntax_category(Node::syntaxTree()) -> 
             pattern|expression|guard_expression|operator|
             generator|record_type|record_field|macro_name|
             unknown).
syntax_category(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keyfind(category, 1, As) of 
        {category, C} ->
            C;
        false ->
            unknown
    end.

%% =============================================================================
%%@doc Returns `true' if `Node' represents a guard expression, otherwise `false'.
%%@spec is_guard_expr(Node:: syntaxTree())-> boolean()

-spec(is_guard_expr(Node:: syntaxTree())-> boolean()).
is_guard_expr(Node) ->
    syntax_category(Node) == guard_expression.
   
   
%%========================================================================
%%@doc Returns `true' if `Node' represents an expression (either a general
%%     expression or a guard expression), otherwise `false'.
%%@spec is_expr(syntaxTree())-> boolean()

-spec(is_expr(Node:: syntaxTree())-> boolean()).
is_expr(Node) ->
    C = syntax_category(Node),
    C==guard_expression orelse C==expression.
  
%%=====================================================================
%%@doc Returns `true' if `Node' represents a pattern, otherwise `false'.
%%@spec is_pattern(syntaxTree())-> boolean()

-spec(is_pattern(Node:: syntaxTree())-> boolean()).
is_pattern(Node) ->
    syntax_category(Node) == pattern.
  
%% =====================================================================
%%@doc Returns all the functions that are exported by an Erlang file.
%%@spec exported_funs(filename()) -> [{atom(),integer()}]

-spec(exported_funs/1::(File::filename()) -> [{Function::atom(), Arity::integer()}]).
exported_funs(File) ->
    {ok, {_, Info}} = wrangler_ast_server:parse_annotate_file(File, true),
    case lists:keysearch(exports, 1, Info) of
        {value, {exports, Funs}} ->
            Funs;
        false ->
            []
    end.

%%@doc Returns all the functions that are (auto)imported by an Erlang file.
%%@spec imported_funs(filename()) -> [{modulename(),functionname(),integer()}]
imported_funs(File) ->
    case  refac_api:get_module_info(File) of 
        {error, Reason} ->
              throw({error, Reason});
        {ok, ModInfo} ->
            case lists:keyfind(imports,1,ModInfo) of 
                {imports, MFAs} ->
                    MFAs;
                _ ->
                    []
            end
    end.

%%@doc Returns all the functions that are imported from `ModuleName' by an Erlang file.
%%@spec imported_funs(filename(), modulename()) -> [{functionname(),integer()}]
imported_funs(File, ModuleName) ->    
    case  refac_api:get_module_info(File) of 
        {error, Reason} ->
            throw({error, Reason});
        {ok, ModInfo} ->
            case lists:keyfind(imports,1,ModInfo) of 
                {imports, MFAs} ->
                    case lists:keyfind(list_to_atom(ModuleName), 1, MFAs) of 
                        {_, FAs}->FAs;
                        _ -> []
                    end;
                _ -> []
            end
    end.
  
%% =====================================================================
%% @doc Returns all the functions that are in-scope in the current module.
%%      An in-scope function could be an (auto-)imported function, or a 
%%      function that is defined in the current module.
%%@spec inscope_funs(filename()) -> [{atom(), integer()}]

-spec(inscope_funs/1::(filename()) -> [Function::{atom(), Arity::integer()}]).
inscope_funs(FileOrModInfo) ->
  case filelib:is_regular(FileOrModInfo) of
      true ->
          {ok, {_, Info}} = wrangler_ast_server:parse_annotate_file(FileOrModInfo, true),
          inscope_funs(Info);
      false ->
          inscope_funs_1(FileOrModInfo)
  end.      
inscope_funs_1(ModInfo) ->
    Imps = case lists:keysearch(imports, 1, ModInfo) of
               {value, {imports, I}} ->
                   lists:append(
                     [lists:map(fun ({F, A}) ->
                                        {M1, F, A} 
                                end, Fs) 
                      || {M1, Fs} <- I]);
               _ -> []
           end,
    case lists:keysearch(module, 1, ModInfo) of
        {value, {module, M}} ->
            Funs = case lists:keysearch(functions, 1, ModInfo) of
                       {value, {functions, Fs}} ->
                           lists:map(fun ({F, A}) ->
                                             {M, F, A}
                                     end, Fs);
                       _ -> []
                   end,
            PreDefinedFuns=[{M, module_info, 1}, 
                            {M, module_info, 2}, 
                            {M, record_info, 2}],
            Imps ++ Funs ++ PreDefinedFuns;
        _ -> Imps
    end.
   

%% =====================================================================
%% @doc Returns all the functions that are defined by an Erlang file.
%%@spec defined_funs(filename()) -> [{atom(),integer()}]

-spec(defined_funs/1::(filename()) -> [{Function::atom(), Arity::integer()}]).
defined_funs(File) ->
    {ok, {_, Info}} = wrangler_ast_server:parse_annotate_file(File, true),
    case lists:keysearch(functions, 1, Info) of
        {value, {functions, Funs}} ->
            Funs;
        false ->
            []
    end.

%% =====================================================================
%% @doc Returns the AST representation of an Erlang file.
%%@spec get_ast(filename()) -> syntaxTree()|{error, errorInfo()}
-spec(get_ast(File::filename()) -> syntaxTree()|{error, term()}).
get_ast(File) ->
    case wrangler_ast_server:parse_annotate_file(File, true) of 
        {ok, {AST, _}} ->
            {ok, AST};
        {error, Reason} ->
            {error, Reason}
    end.

%% =====================================================================
%% @doc Returns the module-level information about the Erlang file.
%% ```-record(module_info, 
%%         {module,
%%          exports, 
%%          module_imports,
%%          imports,
%%          attributes,
%%          records,
%%          errors,
%%          warnings,
%%          functions}). '''
%%@spec get_module_info(filename()) -> #module_info{}|{error, errorInfo()}

-record(module_info, 
        {module ::atom(),
         exports ::[{atom(), integer()}],
         module_imports,
         imports,
         attributes,
         records,
         errors,
         warnings,
         functions}).      
       
-spec(get_module_info(File::filename()) -> #module_info{}|{error, term()}).
get_module_info(File) ->
    case wrangler_ast_server:parse_annotate_file(File, true) of 
        {ok, {_AST, ModuleInfo}} ->
            {ok, ModuleInfo};
        {error, Reason} ->
            {error, Reason}
    end.

%%=====================================================================
%%@doc Returns those files, included in `SearchPaths', which use/import
%%     some of the functions defined in `File'.
%%@spec client_files(filename(),[filename()|dir()]) -> [filename()]

-spec(client_files(filename(),[filename()|dir()]) -> [filename()]).
client_files(File, SearchPaths) ->
    wrangler_modulegraph_server:get_client_files(File, SearchPaths).


%% =====================================================================
%%@doc Returns true if `{FunName, Arity}' is exported by the Erlang module
%%     defined in `File'.
%%@spec is_exported({atom(),integer()}, filename()) -> boolean()
-spec (is_exported({FunName::atom(), Arity::integer()}, File::filename())
       -> boolean()).
is_exported({FunName, Arity}, FileOrModInfo) ->
    case filelib:is_regular(FileOrModInfo) of
        true ->
            {ok, {_, ModInfo}} = wrangler_ast_server:parse_annotate_file(FileOrModInfo, true),
            is_exported_1({FunName, Arity}, ModInfo);
        false ->
            is_exported_1({FunName, Arity}, FileOrModInfo)
    end.

is_exported_1({FunName, Arity}, ModInfo) ->
    ImpExport = case lists:keysearch(attributes, 1, ModInfo) of
		    {value, {attributes, Attrs}} -> 
			lists:member({compile, export_all}, Attrs);
		    false -> false
		end,
    ExpExport= 	case lists:keysearch(exports, 1, ModInfo) of
		    {value, {exports, ExportList}} ->
                        lists:member({FunName, Arity}, ExportList);
		    _ -> false
		end,
    ImpExport or ExpExport.

%% =====================================================================
%%@doc Returns `true' if `Node' represents an attribute 
%%     of name `Name'.
%%@spec is_attribute(syntaxTree(), atom()) -> boolean()
-spec(is_attribute(Node::syntaxTree(), Name::atom()) ->
             boolean()).
is_attribute(Node, Name) ->
    case refac_syntax:type(Node) of 
        attribute ->
            AttrName =refac_syntax:attribute_name(Node),
            refac_syntax:atom_value(AttrName)==Name;
        _ ->
            false
    end.

%% =====================================================================
%%@doc Returns `true' if `Node' represents an import attribute  that
%%     imports module `ModName'
%%@spec is_import(syntaxTree(), atom()) -> boolean()
-spec(is_import(Node::syntaxTree(), Name::atom()) ->
             boolean()).
is_import(Node, ModName) ->
    case refac_syntax:type(Node) of 
        attribute ->
            AttrName =refac_syntax:attribute_name(Node),
            refac_syntax:atom_value(AttrName)==import andalso
                element(1, element(4, refac_syntax:revert(Node)))==ModName;
        _ ->
            false
    end.

%% =====================================================================
%%@doc Tokenises an Erlang file, and returns the tokens.
%%@spec tokenize(filename(), boolean(), integer()) -> [token()]|{error, term()}

-spec(tokenize(File::filename(), WithLayout::boolean(), TabWidth::integer()) 
      -> [token()]|{error, term()}).
tokenize(File, WithLayout, TabWidth) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    S = erlang:binary_to_list(Bin),
	    case WithLayout of 
		true -> 
		    {ok, Ts, _} = refac_scan_with_layout:string(
                                    S, {1,1}, TabWidth, 
                                    refac_misc:file_format(File)),
		    Ts;
		_ -> {ok, Ts, _} = refac_scan:string(
                                     S, {1,1}, TabWidth,
                                     refac_misc:file_format(File)),
		     Ts
	    end;
	{error, Reason} ->
            {error, Reason}
    end.


%% =====================================================================
%% @doc Returns the define location of the variable represented by `Node'; 
%% [{0,0}] is returned is the variable is a free variable or `Node' is 
%% not properly annotated.
%%@spec variable_define_pos(syntaxTree()) ->[pos()]

-spec(variable_define_pos(Node::syntaxTree()) ->[pos()]).
variable_define_pos(Node) ->
    case refac_syntax:type(Node) of 
        variable ->
            As = refac_syntax:get_ann(Node),
            case lists:keysearch(def,1,As)  of
                {value, {def, Pos}} ->
                    Pos;
                false->
                    [{0,0}]
            end;
        _->
            erlang:error(bagarg)
    end.

%% ================================================================================
%% @doc Returns the MFA information attached a node that represents a 
%%  function name or a qualified function name. `unknown' is returned is 
%%  no MFA information is annotated to this node or `Node' does not 
%%  represent a function name.
%%@spec fun_define_info(syntaxTree()) ->{modulename(), functionname(), arity()}|unknown

-spec(fun_define_info(Node::syntaxTree()) ->
             {modulename(), functionname(), arity()}|
             unknown).
fun_define_info(Node) ->
    Ann = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def,1, Ann) of
        {value, {fun_def, {M, F, A, _, _}}} ->
            {M,F,A};
        _ ->
            case lists:keysearch(type,1, Ann) of
                {value, {type, {f_atom, [M,F,A]}}} ->
                    {M,F,A};
                _ -> unknown
            end
    end.
    
%% =====================================================================
%% @doc Returns the function form that defines `MFA'; none is returns if no 
%% such function definition found.
%% @spec mfa_to_fun_def(mfa(), filename()|syntaxTree) ->syntaxTree()|none
-spec (mfa_to_fun_def(mfa(), filename()|syntaxTree) ->syntaxTree()|none).
mfa_to_fun_def(MFA,FileOrTree) ->
    case filelib:is_regular(FileOrTree) of 
        true ->
            case wrangler_ast_server:parse_annotate_file(FileOrTree, true) of 
                {ok, {AnnAST, _}} ->
                    mfa_to_fundef_1(AnnAST,MFA);
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            case is_tree(FileOrTree) of 
                true ->
                    mfa_to_fundef_1(FileOrTree, MFA);
                false ->
                    erlang:error(bagarg)
            end
    end.
mfa_to_fundef_1(AnnAST, {M,F,A}) ->
    Forms=refac_syntax:form_list_elements(AnnAST),
    Fun = fun(Form) ->
                  Ann= refac_syntax:get_ann(Form),
                  case lists:keysearch(fun_def, 1, Ann) of
                      {value, {fun_def, {M, F, A, _, _}}} ->
                          false;
                      _ -> true
                  end
          end,
    case lists:dropwhile(Fun, Forms) of
        [Form|_] ->
            Form;
        _ ->
            none
    end.
   
%%=====================================================================
%%@doc Returns the name of the module defined in `File', 
%%@spec module_name(filename()) -> modulename()
-spec (module_name(File::filename()) -> modulename()).
module_name(File) ->
    case wrangler_ast_server:parse_annotate_file(File, true) of 
        {ok, {_AST, ModuleInfo}} ->
            case lists:keysearch(module,1, ModuleInfo) of
                {value, {module, ModName}} ->
                    {ok, ModName};
                false ->
                    {error, "Wrangler failed to the module name"}
            end;
        {error, Reason}->
            {error, Reason}
    end.


%% =====================================================================
%%@doc Inserts an attribute before the first function definition.
%%@spec insert_an_attr(syntaxTree(), attribute()) -> syntaxTree()
insert_an_attr(AST, Attr) ->
    Forms = refac_syntax:form_list_elements(AST),
    {Forms1, Forms2} = lists:splitwith(
                       fun(F) ->
                               refac_syntax:type(F)==attribute orelse
                               refac_syntax:type(F)==comment
                       end, Forms),
    {Forms12, Forms11} = lists:splitwith(fun(F) ->
                                                refac_syntax:type(F)==comment
                                        end, lists:reverse(Forms1)),
    NewForms=lists:reverse(Forms11)++[Attr]++lists:reverse(Forms12)++Forms2,
    refac_syntax:form_list(NewForms).

%% =====================================================================
%%@doc Remove `F/A' from the entity list of the import attribute 
%%     represented by `Node'.
%%@spec remove_from_import(attribute(), {functionname(), arity()}) -> attribute()
remove_from_import(Node, _FA={F,A}) ->
    case is_attribute(Node, import) of 
        true ->
            Name = refac_syntax:attribute_name(Node),
            Args = refac_syntax:attribute_arguments(Node),
            NewArgs=case Args of 
                        [M, L]  ->
                            L0 = refac_syntax:list_elements(L),
                            L1 = [E ||E<-L0, {refac_syntax:atom_value(
                                                refac_syntax:arity_qualifier_body(E)),
                                              refac_syntax:integer_value(
                                                refac_syntax:arity_qualifier_argument(E))} /={F,A}],
                            [M, refac_misc:rewrite(L, refac_syntax:list(L1))];
                        _ -> Args
                    end,
            refac_misc:rewrite(Node, refac_syntax:attribute(Name, NewArgs));
        false ->
            {error, bagarg}
    end.

%% =======================================================================
%%@doc Adds an entity `FAtoAdd' to the export list of an export attribute
%%     right after another entity `FA'; if `FA' is `none' then append 
%%     the new entity to the end of the export list.
%%@spec add_to_export_after(attribute(), fa(), fa()|none) -> attribute()
%% @type fa() = {functionname(), arity()}
add_to_export_after(Node, FAtoAdd, FA) ->
    {F, A} = FAtoAdd,
    case is_attribute(Node, export) of
        true ->
            Name = refac_syntax:attribute_name(Node),
            [L] = refac_syntax:attribute_arguments(Node),
            AQ = refac_syntax:arity_qualifier(refac_syntax:atom(F),
                                              refac_syntax:integer(A)),
            NewL=case FA of
                     none ->
                         refac_misc:rewrite(L, lists:reverse([AQ|L]));
                     {F1,A1} ->
                         L0 = refac_syntax:list_elements(L),
                         L1 = lists:append([begin
                                                FunName = refac_syntax:atom_value(
                                                            refac_syntax:arity_qualifier_body(E)),
                                                Arity = refac_syntax:integer_value(
                                                          refac_syntax:arity_qualifier_argument(E)),
                                                case {FunName, Arity} of
                                                    {F1,A1} ->
                                                        [E, AQ];
                                                    _ -> [E]
                                                end
                                            end || E <- L0]),
                         refac_misc:rewrite(L, refac_syntax:list(L1))
                 end,
            refac_misc:rewrite(Node, refac_syntax:attribute(Name, [NewL]));
        false ->
            erlang:error(badarg)
    end.
    
%%=================================================================
%% @doc Returns `true' if `Tree1' and `Tree2' are syntactically the
%%      same up to normalization. The normalization process includes 
%%      consistent variable renaming and turning un-qualified 
%%      function calls into qualified function calls. 
%%@spec equal(syntaxTree(), syntaxTree()) -> boolean()

-spec (equal(Tree1::syntaxTree(), Tree2::syntaxTree()) -> boolean()).
equal(Tree1, Tree2) ->
    NewTree1=mask_variables(Tree1),
    NewTree2=mask_variables(Tree2),
    {ok, Ts1, _} = erl_scan:string(refac_prettypr:format(NewTree1)),
    {ok, Ts2, _} = erl_scan:string(refac_prettypr:format(NewTree2)),
    case refac_misc:concat_toks(Ts1) == refac_misc:concat_toks(Ts2) of
        true ->
            refac_code_search_utils:var_binding_structure(Tree1) ==
                refac_code_search_utils:var_binding_structure(Tree2);
        false->
            false
    end.

mask_variables(Exp) when is_list(Exp) ->
    [mask_variables(E) || E <- Exp];
mask_variables(Exp) ->
    ast_traverse_api:full_buTP(
      fun (Node, _Others) ->
	      do_mask_variables(Node)
      end, Exp, {}).

do_mask_variables(Node) ->
    case refac_syntax:type(Node) of
        variable ->
            refac_syntax:default_literals_vars(Node, '&');
        _ ->
            Node
    end.

%%=================================================================
%%@private
splice(Expr) when is_list(Expr) ->
    splice_1(Expr);
splice(Expr) ->
    refac_prettypr:format(Expr).

splice_1([E]) ->
    refac_prettypr:format(E);
splice_1([E|Es]) ->  
    refac_prettypr:format(E)++","++splice_1(Es).

%%@private
quote(Str) ->    
    parse_annotate_expr(Str).

%%===================================================================
%%
%%@private
parse_annotate_expr("") ->
    refac_syntax:empty_node();
parse_annotate_expr(ExprStr) ->
    parse_annotate_expr(ExprStr, {1,1}).
%%@private
parse_annotate_expr("", _) ->
    refac_syntax:empty_node();
parse_annotate_expr(ExprStr, StartLoc) when is_integer(StartLoc) ->
    parse_annotate_expr(ExprStr, {StartLoc, 1});
parse_annotate_expr(ExprStr, StartLoc) when is_tuple(StartLoc) ->
    case refac_scan:string(ExprStr, StartLoc) of
        {ok, Toks, _} ->
            [T|Ts] = lists:reverse(Toks),
            Toks1 = case T of 
                        {dot, _} -> Toks;
                        {';',_} -> lists:reverse([{dot, 999}|Ts]);
                        _ -> Toks++[{dot, 999}]
                    end,
            Toks2 = refac_epp_dodger:scan_macros(Toks1,[]),
            case refac_parse:parse_form(Toks2) of 
                {ok, AbsForm} ->
                    case refac_syntax:type(AbsForm) of 
                        function ->
                            Form1 =refac_epp_dodger:fix_pos_in_form(Toks, AbsForm),
                            Form2 =  refac_syntax_lib:annotate_bindings(Form1),
                            Cs = refac_syntax:function_clauses(Form2),
                            case {Cs, T} of 
                                {[C], {';',_L}} ->
                                    Name = refac_syntax:function_name(Form2),
                                    refac_misc:rewrite(C, refac_syntax:function_clause(Name, C));
                                _ ->
                                    Form2
                            end;
                        _ ->
                            refac_epp_dodger:fix_pos_in_form(Toks, AbsForm)
                    end;
                {error, Reason} ->
                    case refac_parse:parse_exprs(Toks2) of
                        {ok, Exprs} ->
                            Exprs1 =refac_epp_dodger:rewrite_list(Exprs),
                            Exprs2 = make_tree({block, StartLoc, Exprs1}),
                            Exprs3=refac_syntax_lib:annotate_bindings(Exprs2),
                            Exprs4 =refac_syntax:block_expr_body(Exprs3),
                            case Exprs4 of 
                                [E] -> E;
                                _ -> Exprs4
                            end;
                        {error, Reason} ->
                            throw({error, Reason})
                    end
            end;
        {error, ErrInfo, ErrLoc} ->
            throw({error, {ErrInfo, ErrLoc}})
    end.

make_tree(Tree) ->
    case refac_syntax:subtrees(Tree) of 
        [] ->
           Tree;
        Gs ->
            Gs1 = [[make_tree(T) || T <- G] || G <- Gs],
            refac_syntax:update_tree(Tree, Gs1)
    end.




%%=================================================================
%%@private
subst(Expr, Subst) when is_list(Expr) ->
    [subst(E, Subst)||E<-Expr];

subst(Expr, Subst) ->
    {Expr1, _} =ast_traverse_api:stop_tdTP(fun do_subst/2, Expr, Subst),
    Expr2=expand_meta_clauses(Expr1),
    remove_fake_begin_end(Expr2).
 
do_subst(Node, Subst) ->
    case refac_syntax:type(Node) of
	variable ->
            VarName = refac_syntax:variable_name(Node),
            case lists:keysearch(VarName, 1, Subst) of
                {value, {VarName, Expr}} ->
                    case is_meta_list_variable(VarName) andalso
                        is_list(Expr) of 
                        true -> 
                            case Expr of 
                                [] -> {refac_syntax:list(Expr), true};
                                [E] ->
                                    %% No longer can guarantee the correctness of annotations.
                                    {reset_pos_and_range(E), true};
                                _ ->
                                    E1=refac_syntax:add_ann(
                                         {fake_block_expr, true},
                                         %%refac_syntax:block_expr(Expr)),
                                         reset_pos_and_range(
                                           refac_syntax:block_expr(Expr))),
                                    {E1, true}
                            end;
                        false ->
                            E1=reset_pos_and_range(Expr),
                            {E1,  true}
                    end;
                _ -> {Node, false}
            end;
        atom ->
            AtomValue = refac_syntax:atom_value(Node),
            case is_meta_atom_name(AtomValue) of
                true ->
                    case lists:keysearch(AtomValue, 1, Subst) of
                        {value, {AtomValue, Expr}} ->
                            {reset_pos_and_range(Expr), true};
                        false ->
                            {Node, false} %% TODO: SHOULD ISSUE AN ERROR MSG HERE!!!
                    end;
                _ ->
                    {Node, false} 
            end;
	_ -> {Node, false}
    end.
                                   
is_meta_list_variable(VarName) ->
    VarName1 = lists:reverse(atom_to_list(VarName)),
    lists:prefix("@@", VarName1) andalso 
        (not lists:prefix("@@@",VarName1)).

reset_pos_and_range(Node) when is_list(Node) ->
    [reset_pos_and_range(N)||N<-Node];
reset_pos_and_range(Node) ->
    case is_tree(Node) of 
        true ->
            refac_syntax:set_pos(
              refac_misc:update_ann(Node, {range, {{0,0},{0,0}}}),
              {0,0});
        false ->
            Node
    end.

%%=================================================================
%%-spec(reverse_function_clause(Tree::syntaxTree()) -> syntaxTree()).   
%%@private            
reverse_function_clause(Tree) ->
    {Tree1, _} = ast_traverse_api:stop_tdTP(
                   fun reverse_function_clause_1/2, Tree, {}),
    Tree1.

reverse_function_clause_1(Node, _OtherInfo) ->
    case refac_syntax:type(Node) of 
        function ->
            Node1=reverse_function_clause_2(Node),
            {Node1, true};
        _ ->
            {Node, false}
    end.
reverse_function_clause_2(FunDef) ->
    FunName = refac_syntax:function_name(FunDef),
    Cs = refac_syntax:function_clauses(FunDef),
    case [C||C<-Cs, refac_syntax:type(C)==function_clause] of 
        [] -> FunDef;
        Cs1->
            Msg ="Wrangler internal error: unconsistent transformation.",
            case length(Cs)==length(Cs1) of 
                true ->
                    NameCs = [{refac_syntax:function_clause_name(C),
                               refac_syntax:function_clause(C)}||C <- Cs1],
                    {Names, Cs2} =lists:unzip(NameCs),
                    NameVals =[refac_syntax:atom_value(Name)||Name<-Names],
                    case lists:usort(NameVals) of 
                        [_] ->
                            NewFunName = refac_misc:rewrite(FunName, hd(Names)),
                            NewFunDef = refac_syntax:function(NewFunName, Cs2),
                            refac_misc:rewrite(FunDef,NewFunDef);
                        _ ->
                            erlang:error(Msg)
                    end;
                false ->
                    erlang:error(Msg)
            end
    end.


%%======================================================================
%%@private 
full_td_search_and_transform(Rules,Input) ->
    search_and_transform(Rules,Input,fun full_tdTP/3).

%%======================================================================
%%@private
stop_td_search_and_transform(Rules,Input) when is_list(Input) ->
    search_and_transform(Rules,Input,fun stop_tdTP/3);
stop_td_search_and_transform(Rules,Input) ->
    search_and_transform(Rules,Input,fun stop_tdTP/3).

%%======================================================================
%% -spec(search_and_transform([rules()], [filename()|dir()]|syntaxTree()) ->
%%              [{{filename(),filename()}, syntaxTree()}]|syntaxTree()).
%%@private
search_and_transform(Rules,Input,TraverseStrategy)
  when is_list(Input) ->
    case lists:all(fun(I) -> 
                           filelib:is_file(I) 
                   end, Input) of 
        true -> 
            search_and_transform_2(Rules, Input,TraverseStrategy);
        false ->
            case lists:all(fun(I) ->
                                   is_tree(I)
                           end, Input) of 
                true ->
                    [search_and_transform(Rules,I,TraverseStrategy)||I <- Input];
                false ->
                    erlang:error(badarg)
            end
    end;
search_and_transform(Rules,Input,TraverseStrategy) ->
    case is_tree(Input) of
        true ->
            {Input1, _} =search_and_transform_4(Rules,Input,TraverseStrategy),
            Input1;
        false->
            erlang:error(bagarg)
    end.

search_and_transform_2(Rules, FileOrDirs,TraverseStrategy)  ->
    Files = refac_misc:expand_files(FileOrDirs, ".erl"),
    Res=lists:append([begin
                          search_and_transform_3(Rules, File,TraverseStrategy)
                      end||File<-Files]),
    {ok, Res}.

search_and_transform_3(Rules, File,TraverseStrategy) ->
    ?wrangler_io("The current file under refactoring is:\n~p\n", [File]),
    {ok, {AST, _}} = wrangler_ast_server:parse_annotate_file(File, true, [], 8),
    {AST1, Changed}=search_and_transform_4(Rules,AST,TraverseStrategy),
    if Changed -> 
            AST2= reverse_function_clause(AST1),
            [{{File, File}, AST2}];
       true ->
            []
    end.

search_and_transform_4(Rules,Tree,TraverseStrategy) ->
    ParsedBeforeAfterConds=[{parse_annotate_expr(R#rule.template_before),
                             R#rule.template_after,
                             R#rule.condition}|| R<-Rules],
    Fun = fun(Node, _) ->
                  Res = try_expr_match(ParsedBeforeAfterConds, Node),
                  case Res of
                      {true, NewExprAfter} ->
                          {NewExprAfter, true};
                      false ->
                          {Node, false}
                  end
          end,
    Tree1 = extend_function_clause(Tree),
    TraverseStrategy(Fun, Tree1, {}).
    
%% pre_order or post_order?
full_tdTP(Fun, Node, Others) ->
    {Node1, C} =full_tdTP_1(Fun, Node, Others),
    {remove_fake_begin_end(Node1),C}.
        
full_tdTP_1(Fun, Node, Others) ->
    {Node1, Changed} =Fun(Node, Others),
    {Node2, Changed1} = if is_list(Node1) -> 
                                {make_fake_block_expr(Node1), true};
                           true ->
                                {Node1, Changed}
                        end,
    Gs = refac_syntax:subtrees(Node2),
    case Gs of 
        [] ->
            {Node2, Changed1};
        _ ->
            Gs1 = [[{full_tdTP_1(Fun, T, Others),C}||T<-G1]||
                      G<-Gs,{G1, C}<-[Fun(G, Others)]],
            Gs2 = [[N || {{N, _B}, _C} <- G] || G <- Gs1],
            G = [[B or C|| {{_N, B}, C} <- G] || G <- Gs1],
            Node3 = refac_syntax:make_tree(refac_syntax:type(Node2), Gs2),
            {refac_misc:rewrite(Node, Node3), 
             Changed1 orelse 
             lists:member(true, lists:flatten(G))}
    end.
   

%% pre_order.
stop_tdTP(Fun, Node, Others) ->
    {Node1, C} =stop_tdTP_1(Fun, Node, Others),
    {remove_fake_begin_end(Node1),C}.

stop_tdTP_1(Fun, Node, Others) ->
    Res= Fun(Node, Others), 
    case Res of 
        {Node1, true} -> 
            if is_list(Node1) ->
                    {make_fake_block_expr(Node1),true};
               true ->
                    {Node1, true}
            end;
        {_, false} ->
            Gs = refac_syntax:subtrees(Node),
            case Gs of 
                [] ->
                    {Node, false};
                _ ->
                    Gs1 = [[{stop_tdTP_1(Fun, T, Others),C}||T<-G1]||
                              G<-Gs,{G1, C}<-[Fun(G, Others)]],
                    Gs2 = [[N || {{N, _B}, _C} <- G] || G <- Gs1],
                    G = [[B or C ||{{_N, B}, C} <- G] || G <- Gs1],
                    Node2 = refac_syntax:make_tree(refac_syntax:type(Node), Gs2),
                    {refac_misc:rewrite(Node, Node2), lists:member(true, lists:flatten(G))}
            end
    end. 
 
remove_fake_begin_end(Node) ->
    case refac_syntax:subtrees(Node) of
        [] -> Node;
        Gs ->
            Gs1 = [[remove_fake_begin_end(T)||
                       T<-remove_fake_begin_end_1(G)] || G <- Gs],
            Node2 = refac_syntax:make_tree(refac_syntax:type(Node), Gs1),
            refac_syntax:copy_attrs(Node, Node2)
    end.


remove_fake_begin_end_1(Node)->
    lists:append([remove_fake_begin_end_2(N)||N<-Node]).

remove_fake_begin_end_2(Node) when is_list(Node) ->
    [Node];
remove_fake_begin_end_2(Node) -> 
    case refac_syntax:type(Node) of
        block_expr ->
            Ann = refac_syntax:get_ann(Node),
            case lists:keysearch(fake_block_expr, 1,Ann) of
                {value, _} ->
                    refac_syntax:block_expr_body(Node);
                false ->
                    [Node]
            end;
        _ ->
            [Node]
    end.
       
   
make_fake_block_expr(Es) ->
    refac_syntax:add_ann({fake_block_expr, true},
                         refac_syntax:block_expr(Es)).

%%=================================================================
%%@private
match(Temp, Node) -> 
    generalised_unification:expr_match(Temp, Node).
          
    
try_expr_match([], _Node) ->false;
try_expr_match([{BeforeExpr, AfterExpr, Cond}|T], Node) 
  when is_list(BeforeExpr) andalso is_list(Node) ->
    try_expr_match_2([{BeforeExpr, AfterExpr, Cond}|T], Node,1);
    
try_expr_match([{BeforeExpr, AfterExpr, Cond}|T], Node) when 
      not is_list(BeforeExpr) andalso not is_list(Node)->
    try_expr_match_1([{BeforeExpr, AfterExpr, Cond}|T], Node);
try_expr_match([_|T], Node) ->
    try_expr_match(T, Node).

try_expr_match_1([{BeforeExpr, AfterExpr, Cond}|T], Node) ->
    case generalised_unification:expr_match(BeforeExpr, Node) of 
        {true, Binds} ->
            Binds1 = convert_meta_atom_to_meta_var(Binds),
            Binds2 = [{'_This@', Node}|Binds1],
            case  Cond(Binds2) of 
                true ->
                    NewExprAfter =AfterExpr(Binds2), 
                    case is_list(NewExprAfter) of
                        true ->
                            {true, NewExprAfter};
                        false ->
                            {true, refac_misc:rewrite(Node, NewExprAfter)}
                    end;
                false ->
                    try_expr_match(T, Node)
            end;
        false ->
            try_expr_match(T, Node)
    end.

try_expr_match_2([{BeforeExpr, AfterExpr, Cond}|T], NodeList, Index) ->
    Len1 = length(BeforeExpr),
    Len2 = length(NodeList),
    case Len1 =< Len2 of
        true ->
            Exprs = lists:sublist(NodeList, Index, Len1),
            case generalised_unification:expr_match(BeforeExpr,Exprs) of 
                {true, Binds} ->
                    Binds1 = convert_meta_atom_to_meta_var(Binds),
                    Binds2 = [{'_This@', Exprs}|Binds1],
                    case Cond(Binds2) of 
                        true ->
                            NewAfterExpr = AfterExpr(Binds),
                            NewAfterExpr1 = case NewAfterExpr of
                                                [E|Es]->
                                                    [refac_misc:rewrite(hd(Exprs), E)|Es];
                                                _ ->
                                                    [refac_misc:rewrite(hd(Exprs),NewAfterExpr)]
                                            end,
                            NewNodeList =  lists:sublist(NodeList, Index-1) ++
                                NewAfterExpr1 ++  lists:nthtail(Index+Len1-1, NodeList),
                            {true, NewNodeList};
                        false ->
                            if Index < Len2 ->
                                    try_expr_match_2([{BeforeExpr, AfterExpr, Cond}|T], NodeList, Index+1);
                               true ->
                                    try_expr_match(T, NodeList)
                            end
                    end;
                _ ->
                    if Index < Len2 ->
                            try_expr_match_2([{BeforeExpr, AfterExpr, Cond}|T], NodeList, Index+1);
                       true ->
                            try_expr_match(T, NodeList)
                    end
            end;
         false->
            try_expr_match(T, NodeList)
    end.
            
    
convert_meta_atom_to_meta_var(Binds) ->
    convert_meta_atom_to_meta_var_1(Binds, []).

convert_meta_atom_to_meta_var_1([], Acc) ->
    lists:reverse(Acc);
convert_meta_atom_to_meta_var_1([{Name, Node}|T], Acc) ->
    case is_meta_atom_name(Name) of
        true ->
            Name1 = list_to_atom(string:to_upper(atom_to_list(Name))), 
            convert_meta_atom_to_meta_var_1(T, [{Name1, Node}, {Name, Node}|Acc]);
        false ->
            convert_meta_atom_to_meta_var_1(T, [{Name, Node}|Acc])
    end.
    
is_meta_atom_name(AtomName) ->
    AtomName1 = atom_to_list(AtomName),
    is_fun_name(AtomName1) andalso
    lists:prefix("@", lists:reverse(AtomName1)).



%%=================================================================
%%doc Create a rule record.
%%@private
make_rule(Before, After, Cond) ->
    #rule{template_before=Before, 
          template_after=After,
          condition=Cond}.
   
%%=================================================================
%%@doc For every AST node in `Scope' that pattern matches the AST 
%% represented by `Template', if `Cond' evaluates to `true', 
%% then information about this node is collected using the function 
%% specified by `CollectorFun'. `Scope' can be an AST or a list of 
%% Erlang files/directories, and in the latter case, each Erlang 
%% file included is parsed into an AST first.
%%@spec collect(string(), Pred, Function, [filename()|dir()|syntaxTree()]) ->
%%          [any()]|{error, Reason}
%%@private
collect(Template, Cond, ReturnFun, Scope) when is_list(Scope) ->
    Files = refac_misc:expand_files(Scope, ".erl"),
    TemplateExpr =parse_annotate_expr(Template),
    Res=[collect_in_one_file(F, {TemplateExpr, Cond, ReturnFun})||F <- Files],
    lists:append(Res);
collect(Template, Cond, ReturnFun, Scope) ->
    case is_tree(Scope) of
        true ->
            TemplateExpr =parse_annotate_expr(Template),
            do_search_matching_code(none, Scope, {TemplateExpr, Cond, ReturnFun});
        false ->
            {error, badarg}
    end.

collect_in_one_file(File, {Template, Cond, ReturnFun}) ->
    ?wrangler_io("Processing file:~p\n", [File]),
    {ok, {AST, _}} = wrangler_ast_server:parse_annotate_file(File, true, [], 8),
    do_search_matching_code(File, AST, {Template, Cond, ReturnFun}).

%% THE FOLLOWING WILL BE REFACTORED!!!.
do_search_matching_code(FileName, AST, {Template, Cond, ReturnFun}) 
  when is_list(Template) ->
    do_search_matching_code_list(FileName, AST, {Template, Cond, ReturnFun});
do_search_matching_code(FileName, AST, {Template, Cond, ReturnFun}) ->
    do_search_matching_code_non_list(FileName, AST, {Template, Cond, ReturnFun}).


do_search_matching_code_list(FileName, AST, {Template, Cond, ReturnFun}) ->
    Fun = fun(Node, Acc) ->
                  Nodes = get_expr_seqs(Node),
                  Res=generalised_unification:expr_match(Template, Nodes),
                  case Res of
                      {true, Binds} ->
                          Binds0 = convert_meta_atom_to_meta_var(Binds),
                          Binds1 = [{'_This@', Nodes}|Binds0],
                          case Cond(Binds1) of
                              true ->
                                  [ReturnFun([{'_File@', FileName}|Binds1])|Acc];
                              false ->
                                  Acc
                          end;
                      false ->
                          Acc
                  end
          end,
    lists:reverse(ast_traverse_api:fold(Fun, [], AST)).
  

get_expr_seqs(T) ->
    case refac_syntax:type(T) of
	clause ->
	    refac_syntax:clause_body(T);
	block_expr ->
	    refac_syntax:block_expr_body(T);
	try_expr ->
	    refac_syntax:try_expr_body(T);
	_ -> []
    end.

do_search_matching_code_non_list(FileName, AST, {Template, Cond, ReturnFun}) ->
    case refac_syntax:type(Template) of
        function_clause ->
            stop_td_collect(FileName, AST, {Template, Cond, ReturnFun}, function_clause);
        function ->
            Template1 = extend_function_clause(Template),
            stop_td_collect(FileName, AST, {Template1, Cond, ReturnFun}, function);
        _ -> 
            full_td_collect(FileName, AST, {Template, Cond, ReturnFun})
    end.

stop_td_collect(FileName, AST, {Template, Cond, ReturnFun}, Type) ->
    Fun= fun(Node) ->
                 Res = generalised_unification:expr_match(Template, Node),
                 case Res of
                     {true, Binds} ->
                         Binds0 = convert_meta_atom_to_meta_var(Binds),
                         Binds1 = [{'_This@', Node}|Binds0],
                         try Cond(Binds1) of
                             true ->
                                 {ReturnFun([{'_File@', FileName}|Binds1]), true};
                             false ->
                                 {[], true}; %% pretend to be sucessful;
                             _ ->
                                 throw({error, "Condition checking returns a non-boolean value."})
                         catch
                             _E1:_E2 ->
                                 throw({error, "Condition checking returns a non-boolean value."})
                         end;
                     false ->
                         {[], false}
                 end
         end,
    AST1=extend_function_clause(AST),
    lists:reverse(stop_tdTU(Fun, [], AST1, Type)).

stop_tdTU(Function, S, Node, Type) ->
    {Res, _} = stop_tdTU_1(Function, S, Node,Type),
    lists:reverse(Res).

stop_tdTU_1(Function, S, Node, Type) ->
    case Function(Node) of
        {R, true} when R==[]-> 
            {S, true};
        {R, true} ->
            {[R|S], true};
        {_R, false} ->
            case refac_syntax:type(Node)==Type of
                true ->
                    {S, true};
                false ->
                    case refac_syntax:subtrees(Node) of
                        [] -> {S, true};
                        Gs ->
                            Flattened_Gs = [T || G <- Gs, T <- G],
                            case Flattened_Gs of
                                [] -> {S, true};
                                [_H | _T1] -> S1 = [[stop_tdTU_1(Function, [], T, Type) || T <- G] || G <- Gs],
                                              S2 = [S12 || G<-S1, {S12, _B} <- G],
                                              {S++lists:append(S2), true}
                            end
                    end
            end
    end.


full_td_collect(FileName, AST, {Template, Cond, ReturnFun}) ->
    Fun= fun(Node, Acc) ->
                 Res = generalised_unification:expr_match(Template, Node),
                 case Res of
                     {true, Binds} ->
                         Binds0 = convert_meta_atom_to_meta_var(Binds),
                         Binds1 = [{'_This@', Node}|Binds0],
                         try Cond(Binds1) of
                             true ->
                                 [ReturnFun([{'_File@', FileName}|Binds1])|Acc];
                             false ->
                                 Acc;
                             _ ->
                                 throw({error, "Condition checking returns a non-boolean value."})
                         catch
                             _E1:_E2 ->
                                 throw({error, "Condition checking returns a non-boolean value."})
                         end;
                     false ->
                         Acc
                 end
         end,
    lists:reverse(ast_traverse_api:fold(Fun, [], AST)).
  
%%================================================================
%%@spec(extend_function_clause(Tree::syntaxTree()) -> syntaxTree()).
%%@private             
extend_function_clause(Tree) ->
    {Tree1, _} = ast_traverse_api:stop_tdTP(
                   fun extend_function_clause_1/2, Tree, {}),
    Tree1.

extend_function_clause_1(Node, _OtherInfo) ->
    case refac_syntax:type(Node) of 
        function ->
            Node1=extend_function_clause_2(Node),
            {Node1, true};
        _ ->
            {Node, false}
    end.

extend_function_clause_2(Node) ->
    Name = refac_syntax:function_name(Node),
    Cs = refac_syntax:function_clauses(Node),
    Cs1= [case refac_syntax:type(C) of 
              clause ->
                  refac_misc:rewrite(C,refac_syntax:function_clause(Name, C));
              _ ->
                  C
          end
          ||C<-Cs],
    refac_misc:rewrite(Node, refac_syntax:function(Name, Cs1)).

is_tree(Node) ->
    refac_syntax:is_tree(Node) orelse refac_syntax:is_wrapper(Node).

expand_meta_clauses(Tree) ->
    {Tree1, _}  = ast_traverse_api:full_tdTP(fun expand_meta_clause_1/2, Tree, {}),
    Tree1.

expand_meta_clause_1(Node, _OtherInfo) ->
    case refac_syntax:type(Node) of 
        function ->
            {Node, false};
        case_expr ->
            Arg = refac_syntax:case_expr_argument(Node),
            Cs = refac_syntax:case_expr_clauses(Node),
            Cs1 = lists:append([expand_meta_clause_2(C)||C<-Cs]),
            case Cs=/=Cs1 of
                 true ->
                     Node1=refac_misc:rewrite(
                             Node,
                             refac_syntax:case_expr(Arg, Cs1)),
                     {Node1, false};
                 false ->
                     {Node, false}
            end;
        receive_expr ->
            {Node, false};
        if_expr ->
            {Node, false};
        try_expr->
            {Node, false};
        fun_expr ->
            {Node, false};
        _ ->
            {Node, false}
    end.

expand_meta_clause_2(Clause) ->
    case is_meta_clause(Clause) of 
        true->
            [Pat] = refac_syntax:clause_patterns(Clause),
            [[Guard]] = revert_clause_guard(refac_syntax:clause_guard(Clause)),
            [Body] = refac_syntax:clause_body(Clause),
            ZippedPGB = lists:zip3(Pat, Guard, Body),
            [refac_syntax:clause(P, G, B)||
                {P, G, B} <-ZippedPGB];
        false ->
            [Clause]
    end.

is_meta_clause(Clause)->
    Pat = refac_syntax:clause_patterns(Clause),
    Guard = refac_syntax:clause_guard(Clause),
    Body = refac_syntax:clause_body(Clause),
    case Pat of 
        [P] ->
            case is_list_of_lists(P) of 
                true ->
                    case revert_clause_guard(Guard) of
                        [[G]] ->
                            case is_list_of_lists(G) of 
                                true ->
                                    case Body of
                                        [B]->
                                            is_list_of_lists(B);
                                        _ ->
                                            false
                                    end;
                                false -> false
                            end;
                        _ -> false 
                    end;
                false ->
                    false
            end;
        _ -> false
    end.
 
is_list_of_lists(L) ->
    is_list(L) andalso 
        lists:all(fun(E) ->
                          is_list(E)
                  end, L).

revert_clause_guard(none) -> [[]];
revert_clause_guard(E)->
    case  refac_syntax:type(E) of
        disjunction -> refac_syntax:revert_clause_disjunction(E);
        conjunction ->
            %% Only the top level expression is
            %% unfolded here; no recursion.
            [refac_syntax:conjunction_body(E)];
        _ ->
            [[E]]       % a single expression
    end.
