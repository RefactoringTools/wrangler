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
%% This module defines the API exposed by Wrangler for users to compose
%% refactorings or code inspectors that meet their own needs. A refactoring
%% consists of two parts: program analysis and program transformation, both 
%% of which involve various AST traversals and manipulations, and require 
%% deep knowledge of the AST representation details. To make the processing 
%% of writing a refactoring easier, we have extended Wrangler with a framework
%% that allows users to define refactorings or code inspectors in an intuitive
%% and concise way. With this framework, user-defined refactorings are no 
%% second-class refactorings. Like the existing refactorings supported by Wrangler,
%% user-defined refactoring can also be invoked from the refactoring menu, and 
%% also benefit the existing features such as preview of refactoring results, 
%% layout preservation, selective refactoring, undo of refactorings, etc, for free.
%%
%% The user-extensibility of Wrangler is achieved by introducing a new layer on top 
%% of the existing low-level API which provides direct access to the details of syntactic
%% and semantic representation of the Erlang language. We call the new layer the 
%% `Wrangler API'. The `Wrangler API' consists of three parts:
%%
%% -- A template and rule based program analysis and transformation framework that 
%% allows users to express their analysis and transformation in an intuitive and 
%% concise way without diving into the details of internal program representation 
%% used by the refactoring tool; 
%%
%% -- A generic behaviour especially for refactoring encapsulates the generic parts 
%% that are common to all refactorings, while the user only has to provide the 
%% parts that are specific to the refactoring under consideration. See <a href="gen_refac.html">gen_refac</a>.
%%
%% -- A collection of API functions, such as API functions for abstract syntax tree 
%% traversal, API functions for retrieving context information from AST nodes, API 
%% functions for mapping the textual selection of program source to its internal 
%% representation (see <a href="interface_api.html">interface_api</a>), etc.
%%
%% Here are some macros that can be used to expression transformation rules and/or
%% information collectors:
%%<ul>
%%<li>
%%?T(TemplateStr).
%%With Wrangler, a template is denoted by a Erlang macro `?T' whose only argument
%%is the string representation of an Erlang code fragment that may contain meta-variables.
%% The template code fragment can be a sequence of  expressions, a function definition, 
%%an attribute, or a single function clause. As a convention, a template representing a 
%%function/attribute should always ends with a full stop; whereas a single function 
%%clause must end with a semicolon, otherwise it will be interpreted as an Erlang 
%%function consisting of a single function clause by default.
%%
%%A meta-variable is a placeholder for a syntax element in the program, or a sequence of
%% syntax elements of the same kind. Templates syntactically are Erlang code, therefore 
%%the use of meta-variables in a template must not violate the syntactic correctness of
%% the code fragment. Templates are matched at AST level, that is, the template's AST is
%% matched to the program's AST. A template consists of only a single meta-variable can
%% match any subtrees within the AST.
%%
%%Syntactically a meta-variable is an Erlang variable ending with the character `@'.
%% Three kinds of meta-variables are supported:
%% -- A meta-variable ending with a single `@' represents a single language element, 
%% and matches a single subtree in the AST. For example, a remote function call which
%% a placeholder for the function name can be represented as:
%%
%%                	`?T("M:F@(1, 2)")'
%%
%%In the template above, variable `M' is an object variable, and only matches an AST node
%% representing a variable of the same name; whereas `F@' is a meta- variable, 
%%and therefore matches any node that represents the function name part of a remote 
%%function call module name is represented by variable `M', and arguments are literals
%%`1' and `2'. 
%%
%% -- A meta-variable ending with`@'`@' represents a `list meta-variable', which matches 
%% an arbitrary sequence of elements of the same sort, e.g. a list of arguments of a 
%% function calls, a sequence of expressions in a clause body, etc. For instance the 
%%template: 
%% 
%%                                    `?T("spawn(Arg@'`@'`)")'
%%
%%matches the application of function `spawn' to an arbitrary number of arguments, and
%%`Arg@'`@' is the place holder for the sequence of argument. 
%%As another example, the template
%%
%%	                `?T("spawn(Arg1@, Arg2@, Args@'`@)")' 
%%
%%  will match the application of function `spawn' to two or more arguments, where 
%% `Arg1@' and `Arg2@' are placeholders for the first and second argument respectively,
%%  and `Args@'`@' is the placeholder for the remaining arguments. If there is no more 
%%  remaining arguments, then `Args@'`@' is an empty list.
%%	 
%% In order to template an arbitrary sequence of clauses, either function clauses or 
%% expression clauses, we introduce a especial meta-variable which ends with `@'`@'`@'. 
%% A meta variable ending with `@'`@'`@' is mapped to a list, each element of which is a 
%% list of subtrees of the same kind. For example, a case expression with an arbitrary 
%% number of clauses can be template as:
%%
%%              `?T("case Expr@ of Pats@'`@'`@' `when Guards'`@'`@'`@'`-> Body'`@'`@'`@")'
%%	
%%in which `Pats@@@' matches the collection of patterns from each clause of the case 
%%expression in the same order;  `Body@@@' matches the collection of body expressions 
%%from each clause; and `Guard@@@' matches the collection of guards from each clause. 
%%In the case that a clause does not have a guard expression, its guard is represented 
%%as an empty list. 
%%
%%`Meta-atoms'. Certain syntax elements in Erlang, such as the function name part of a 
%%function definition, the record name/field in an record expression, etc, can only be 
%%atoms, and cannot be replaced by a variable. In order to represent a placeholder for 
%%this kind of `atom-only' syntax elements, we introduce the notion of 
%%`meta-atom', which acts as a place holder for a single atom. Syntactically, a 
%%meta-atom is an Erlang atom ending with a single `@'.  For example, with the use of
%% meta-atom, an arbitrary function clause can be templated as 
%%
%%           `?T("f@(Args@'`@'`)when Guard@'`@'`-> Body@'`@;")'
%%
%%where `f@' is a placeholder for the function name.
%%</li>
%%
%%<li>
%%?RULE(Template, NewCode, Cond).
%% A conditional transformation rule is denoted by a macro `?RULE'. In 
%% `?RULE(Template, NewCode, Cond)', `Template' is a template representing the kind 
%% of code fragments to search for; `Cond' is an Erlang expression that evaluates to 
%% either `true' or `false'; and `NewCode' is another Erlang expression that returns 
%% the new code fragment. By means of parse transform, all the meta-variables, and also 
%% meta-atoms, from the `Template', are make visible to `NewCode' and `Cond', therefore
%% can be referred by them. 
%%
%% A conditional transformation rule is always used with an AST traversal strategy. An 
%% AST traversal strategy takes one or more conditional transformation rules, and an AST
%% as input. It walks through the AST in a specific order, and for each node it 
%% encounters, the traversal strategy tries to pattern match the AST representation of
%% the `Template' part of the first rule with the current node, if the pattern matching 
%% succeeds, the `Cond' part of the rule is then evaluated to check whether certain 
%% properties are satisfied by the nodes that matches the meta-variables/meta-atoms. 
%% The `NewCode' part is executed only if the evaluation of `Cond' returns `true', and
%% in that case, the current node is replaced with the AST generated by `NewCode', and 
%% the traversal goes on after that. However, it the first rule is not applicable either
%% because the pattern matching fails, or the `Cond' evaluates to `false', the next rule
%% will be tried in the same way until no more rules is available, and the traversal 
%% will continue to other nodes in the AST.
%%
%% As mentioned above, `NewCode' specifies the AST representation of the new code after
%% the transformation. While `NewCode' should evaluate to an AST node, or a sequence of 
%% AST nodes, the user does not have to compose the AST manually, instead the general 
%% way is to create the string representation of the new code fragment, and use the 
%% macro `?QUOTE', which is also part of the Wrangler API, to turn the string 
%% representation  of a code fragment into its AST representation. All the
%% meta-variables/atoms bound in `Template' are visible, and can be used by `NewCode', 
%% and further more, it is also possible for `NewCode' to define its own meta variables 
%% as shown in the example below.
%% ```    rule({M,F,A}, N) ->
%%            ?RULE(?T("F@(Args@@@)"), 
%%                  begin 
%%                     NewArgs@@@=delete(N, Args@@@),
%%                     ?QUOTE("F@(NewArgs@@@)")
%%                   end,
%%                   refac_api:fun_define_info(F@) == {M, F, A}).
%%
%%        delete(N, List) ->
%%            lists:sublist(List, N-1)++ lists:nthtail(N, List). '''
%%
%% </li>
%% <li>
%% ?QUOTE(Str).
%% This macro takes a string representation of a code fragment as input, which 
%% may contain meta-variables, parses the string into the AST representation of the code,
%% and then substitutes the meta-variables and/or meta-atoms with the AST nodes 
%% represented. 
%% </li>
%% <li>
%% ?COLLECT(Template,Collector, Cond).
%%
%% ?COLLECT is a macro used to collect information from code fragements that are 
%% of interest. In `?COLLECT(Template,Collector, Cond)', `Template' is a template 
%% representing the kind of code fragments to search for; `Cond' is an Erlang expression
%% that evaluates to either `true' or `false'; and `Collector' is an Erlang expression
%% which extract the information needed from the current node. Information is collected
%% when the AST representation of the template pattern matches the current AST node, 
%% and `Cond' evaluates to `true'. As an example, the macro application shown below can
%% be used to collect those clause bodies, which an unnecessary match expression at the
%% end. This collector returns the location information of those clause bodies found. 
%% Two special pre-defined meta-variables are used in this macro application. One is 
%% `_File@', whose value is the file name of the source code to which the macro is 
%% applied to, or `none' is no such information is available; and the other one is 
%% `_This@', whose value if the entire subtree that pattern matches the template.
%%
%%         ```?COLLECT(?T("Body@@, V@=Expr@, V@"),
%%                     {_File@, refac_api:start_end_loc(_This@)},
%%                     refac_api:type(V@)==variable)'''
%%
%%</li>
%%<li>
%%?COLLECT_LOC(Template, Cond).
%%
%% A special case of ?COLLECT, which returns the location information of the AST node that
%% matches the template, as shown in the previous example. The code below functions the 
%% same the example above.
%% 
%%         ```?COLLEC_LOC(?T("Body@@, V@=Expr@, V@"),
%%                        refac_api:type(V@)==variable)'''
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
%%?SPLICE(Tree).
%%
%%Pretty-prints the AST `Tree', and returns the string representation.
%%
%%</li>
%%<li>
%%?MATCH(Template, Tree).
%%
%%Pattern matches the AST representation of `Template' with the AST `Tree', and returns 
%%`false' if the pattern matching fails, and 'true' if succeeds.
%%
%%</li>
%%<li>
%%?FULL_TD_TP(Rules, Scope).
%%
%% Traverses the AST in a topdown order, and for each node apply the first rule that 
%% succeeds; after a rule has been applied to a node, the subtrees of the node will 
%% continued to be traversed.
%%
%%</li>
%%<li>
%%?STOP_TD_TP(Rules, Scope).
%%
%% Traverses the AST in a topdown order, and for each node apply the first rule that 
%% succeeds; after a rule has been applied to a node, the subtrees of the node will 
%% not to be traversed.
%%</li>
%%<li>
%% ?STOP_TD_TU(Collectors, Scope).
%%
%% Traverses the AST in a topdown order, and for each node apply the collectors one by
%% one, and collects information returns by each collector.
%%</li>
%%</ul>
%% Some example refactorings implemented using the Wrangler API:
%%<ul>
%%<li>
%%<a href="file:refac_swap_args.erl" > Swap arguments of a function;</a>.
%%</li>
%%<li>
%%<a href="file:refac_remove_arg.erl" > Remove an argument of a function;</a>.
%%</li>
%%<li>
%%<a href="file:refac_keysearch_to_keyfind.erl"> replace the uses of lists:keysearch/3 with lists:keyfind/3; </a>
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
%%<li>
%%<a href="file:code_inspector_examples.erl"> A collection of code inspectors written using the Wrangler API. </a>
%%</li>
%%</ul>

-module(refac_api).

-export([is_var_name/1, 
         is_fun_name/1,
         make_new_name/2,
         make_arity_qualifier/2,
         env_vars/1,
         env_var_names/1,
         exported_vars/1,
         exported_var_names/1,
         bound_vars/1,
         bound_var_names/1,
         free_vars/1,
         free_var_names/1,
         var_refs/1,
         start_end_loc/1,
         syntax_context/1,
         syntax_category/1,
         type/1,
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
         extended_parse_annotate_expr/1,
         extended_parse_annotate_expr/2,
         subst/2, 
         collect/3,
         match/2,
         match/3,
         search_and_transform/3,
         search_and_collect/3]).
        
-include("../include/wrangler_internal.hrl"). 

%% ====================================================================
%%@doc Returns the start and end locations of an AST node or a sequence 
%%     of AST node. {{0,0},{0,0}} is returned if the AST nodes are not 
%%     annotated with location information.
%%@spec start_end_loc([syntaxTree()]|syntaxTree()) ->{pos(), pos()}
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
-spec make_new_name(atom(), [atom()]) ->atom().		   
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
                  case refac_syntax:type(Node) of 
                      variable ->
                          Ann = refac_syntax:get_ann(N),
                          case lists:keyfind(bound, 1, Ann) of 
                              {use, Bound} when Bound/=[] ->
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
                                  end;
                              _ -> Acc
                          end;
                      _ -> Acc        
                  end
          end,
    ordsets:to_list(ast_traverse_api:full_tdTU(Fun,ordsets:new(),Node)).
 
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


%%=====================================================================
%%@doc Returns all the locations where a variable is used, not including
%%     the locations where the variable is bound, when `Node' represents 
%%     a variable, otherwise returns an empty list.
-spec(var_refs(Node::syntaxTree())-> [pos()]).
var_refs(Node) ->
    Ann = refac_syntax:get_ann(Node),
    case lists:keyfind(use,1,Ann) of 
        {use, Vs} ->
            {def, Pos}=lists:keyfind(def,1, Ann),
            Vs--Pos;
        false ->
            []
    end.
  
%%=============================================================================
%%@doc Returns the syntax context of `Node'.
%%@spec syntax_context(syntaxTree()) -> atom()
-spec(syntax_context(Node::syntaxTree()) ->atom()).
syntax_context(Node) ->
    Ann = refac_syntax:get_ann(Node),
    case lists:keysearch(syntax_path, 1, Ann) of
        {value, {syntax_path, P}} -> P;
        false ->
            unknown
            %% Should change to return an error message here!!!
            %% refac_io:format("Node:\n~p\n", [Node]),
            %% throw({error, "Wrangler internal error "
            %%        "in refac_api:syntax_context/1"})
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
    {ok, ModInfo}=refac_api:get_module_info(File), 
    case lists:keyfind(imports,1,ModInfo) of 
        {imports, MFAs} ->
            MFAs;
        _ ->
            []
    end.

%%@doc Returns all the functions that are imported from `ModuleName' by an Erlang file.
%%@spec imported_funs(filename(), modulename()) -> [{functionname(),integer()}]
imported_funs(File, ModuleName) ->    
    {ok, ModInfo}=refac_api:get_module_info(File),
    case lists:keyfind(imports,1,ModInfo) of 
        {imports, MFAs} ->
            case lists:keyfind(list_to_atom(ModuleName), 1, MFAs) of 
                {_, FAs}->FAs;
                _ -> []
            end;
        _ -> []
    end.
  
%% =====================================================================
%% @doc Returns all the functions that are in-scope in the current module.
%%      An in-scope function could be an (auto-)imported function, or a 
%%      function that is defined in the current module.

-spec(inscope_funs/1::(filename()|moduleInfo()) -> [{atom(),atom(),integer()}]).
inscope_funs(FileOrModInfo) ->
  case filelib:is_regular(FileOrModInfo) of
      true ->
          {ok, {_, Info}} = wrangler_ast_server:parse_annotate_file(FileOrModInfo, true),
          inscope_funs_1(Info);
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
-spec(get_module_info(File::filename()) -> {ok, module_info()}).
get_module_info(File) ->
    {ok,{_AST, ModuleInfo}}= wrangler_ast_server:parse_annotate_file(File, true),
    {ok, ModuleInfo}.

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
%%@private
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
-spec (mfa_to_fun_def(mfa(), filename()|syntaxTree()) ->syntaxTree()|none).
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
-spec (module_name(File::filename()) -> {ok, modulename()} |{error,  any()}).
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
%%@spec add_to_export_after(attribute(), {function(), arity()},
%%                        {function(), arity()}|none) -> attribute()
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
                         lists:reverse([AQ|L]);
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
%%@private
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
%%@private
extended_parse_annotate_expr(Str) ->
    extend_function_clause(parse_annotate_expr(Str)).
%%@private
extended_parse_annotate_expr(Str, Pos) ->
    extend_function_clause(parse_annotate_expr(Str, Pos)).
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
                   case T of 
                       {dot, _} ->
                           throw({error, Reason});
                       {';',_} -> 
                           throw({error, Reason});
                       _ ->
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
                               {error, Reason1} ->
                                   throw({error, Reason1})
                           end
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
    Expr2=expand_meta_list(Expr1),
    remove_fake_begin_end(Expr2).
 
do_subst(Node, Subst) ->
    case refac_syntax:type(Node) of
	variable ->
            VarName = refac_syntax:variable_name(Node),
            case is_meta_var_name(VarName) of 
                true ->
                    case lists:keysearch(VarName, 1, Subst) of
                        {value, {VarName, Expr}} ->
                            case is_meta_list_var_name(VarName) andalso
                                is_list(Expr) of 
                                true -> 
                                    case Expr of 
                                        [] -> {refac_syntax:empty_node(), true};
                                        [E] ->
                                            %% No longer can guarantee the correctness of annotations.
                                            {reset_pos_and_range(E), true};
                                        _ ->
                                            E1=refac_syntax:add_ann(
                                                 {fake_block_expr, true},
                                                 reset_pos_and_range(
                                                   refac_syntax:block_expr(Expr))),
                                            {E1, true}
                                    end;
                                false ->
                                    E1=reset_pos_and_range(Expr),
                                    {E1,  true}
                            end;
                        false ->
                            throw({error, lists:flatten(
                                            io_lib:format
                                              ("Meta variable ~p is not bound.", [VarName]))})
                    end;
                false ->
                    {Node, false}
            end;
        atom ->
            AtomValue = refac_syntax:atom_value(Node),
            case is_meta_atom_name(AtomValue) of
                true ->
                    NewAtomValue=list_to_atom("_W_"++atom_to_list(AtomValue)),
                    case lists:keysearch(NewAtomValue, 1, Subst) of
                        {value, {NewAtomValue, Expr}} ->
                            {reset_pos_and_range(Expr), true};
                        false ->
                            throw({error, lists:flatten(
                                            io_lib:format
                                              ("Meta atom ~p is not bound.", [AtomValue]))})
                    end;
                _ ->
                    {Node, false} 
            end;
	_ -> {Node, false}
    end.
                                   
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

copy_pos_and_range(Node1, Node2) ->
    Ann=refac_syntax:get_ann(Node1),
    Range = case lists:keyfind(range,1,Ann) of
                {range, R} ->
                    R;
                false->
                    {0,0}
            end,
    refac_syntax:copy_pos(
      Node1,refac_misc:update_ann(Node2, {range, Range})).

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
-type (rule()::{rule, any(), any()}).
-spec(search_and_transform([rule()], [filename()|dir()]|
                           [{filename(), syntaxTree()}|syntaxTree()],
                           full_td_tp|stop_td_tp) ->
             {ok, [{{filename(),filename()}, syntaxTree()}]}|
             {ok, [{filename(), syntaxTree()}|syntaxTree()]}|
             {ok, syntaxTree()}).
%%@private
search_and_transform(Rules,Input,TraverseStrategy)
  when is_list(Input) ->
    check_rules(Rules),
    case lists:all(fun(I) -> 
                           filelib:is_file(I) 
                   end, Input) of 
        true -> 
            search_and_transform_2(Rules, Input, TraverseStrategy);
        false ->
            case lists:all(fun(I) ->
                                   case I of 
                                       {File, Tree} ->
                                           filelib:is_file(File) andalso is_tree(Tree);
                                       _ ->
                                           is_tree(I)
                                   end
                           end, Input) of 
                true ->
                    Res=[search_and_transform_1(Rules,I,TraverseStrategy)||I <- Input],
                    {ok, Res};
                false ->
                    erlang:error("Transformation rules are applied to an invalid scope.")
            end
    end;
search_and_transform(Rules, Input, TraverseStrategy) ->
    check_rules(Rules),
    case is_tree(Input) of
        true ->
            Res=search_and_transform_1(Rules,Input, TraverseStrategy),
            {ok, Res};
        false ->
            erlang:error("Transformation rules are applied to an invalid scope.")
    end.
 
search_and_transform_1(Rules, {File, Tree}, Fun) ->
    Selective =try wrangler_gen_refac_server:get_flag(self()) of
                  {ok, Val} ->
                      Val
               catch
                   _E1:_E2 ->
                       {false,[]}
               end,
    Res=search_and_transform_4(File, Rules, Tree, Fun, Selective),
    {File, element(1, Res)};
search_and_transform_1(Rules, Tree, Fun) ->
    element(1, search_and_transform_4(none, Rules, Tree, Fun, {false,[]})).
    

search_and_transform_2(Rules, FileOrDirs, Fun) ->
    Selective =try wrangler_gen_refac_server:get_flag(self()) of
                  {ok, Val} ->
                      Val
               catch
                   _E1:_E2 ->
                       {false,[]}
               end,
    Files = refac_misc:expand_files(FileOrDirs, ".erl"),
    Res=lists:append([begin
                          search_and_transform_3(Rules, File, Fun, Selective)
                      end||File<-Files]),
    {ok, Res}.

search_and_transform_3(Rules, File, Fun, Selective) ->
    ?wrangler_io("The current file under refactoring is:\n~p\n", [File]),
    {ok, {AST, _}} = wrangler_ast_server:parse_annotate_file(File, true, [], 8),
    AST0 = extend_function_clause(AST),
    {AST1, Changed}=search_and_transform_4(File, Rules, AST0, Fun, Selective),
    if Changed andalso (not Selective) ->
            AST2= reverse_function_clause(AST1),
            [{{File, File}, AST2}];
       true ->
            if Changed->
                    [{{File, File}, AST1}];
               true ->
                    []
            end
    end.
 
search_and_transform_4(File,Rules,Tree,Fun,Selective) ->
    F = fun(Node, CandsNotToChange) ->
                Res = try_expr_match(Rules, Node),
                case Res of
                    {true, NewExprAfter} ->
                        case Selective of
                            true ->
                                {{SLn, SCol}, {ELn, ECol}}=refac_api:start_end_loc(Node),
                                MD5 = erlang:md5(refac_prettypr:format(Node)),
                                ChangeCand={{File, SLn, SCol, ELn, ECol, MD5},
                                            refac_prettypr:format(NewExprAfter)},
                                wrangler_gen_refac_server:add_change_cand(self(), ChangeCand),
                                {NewExprAfter, true};
                            {false, []} ->
                                {NewExprAfter, true};
                            false ->
                                {NewExprAfter, true};
                            {false, CandsNotToChange} ->
                                {{SLn, SCol}, {ELn, ECol}}=refac_api:start_end_loc(Node),
                                MD5 =erlang:md5(refac_prettypr:format(Node)),
                                Key ={File, SLn, SCol, ELn, ECol, MD5},
                                case lists:keysearch(Key,1,CandsNotToChange) of
                                    false ->
                                        {NewExprAfter, true};
                                    _ ->
                                        {Node, false}
                                end
                        end;
                    false ->
                        {Node, false}
                end
        end,
    CandsNotToChange = case Selective of 
                           true -> [];
                           {false,NotToChange} ->
                               NotToChange
                       end,
    case Fun of
        full_td_tp ->
            extended_full_tdTP(F, Tree, CandsNotToChange);
        stop_td_tp  ->
            extended_stop_tdTP(F, Tree, CandsNotToChange)
    end.

    
%% pre_order
extended_full_tdTP(Fun, Node, Others) ->
    {Node1, C} =extended_full_tdTP_1(Fun, Node, Others),
    {remove_fake_begin_end(Node1),C}.
        
extended_full_tdTP_1(Fun, Node, Others) ->
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
            Gs1 = [[{extended_full_tdTP_1(Fun, T, Others),C}||T <- G1]||
                                 G <- Gs,{G1, C} <- [Fun(G, Others)]],
            Gs2 = [[N || {{N, _B}, _C} <- G] || G <- Gs1],
            G = [[B or C|| {{_N, B}, C} <- G] || G <- Gs1],
            Node3 = refac_syntax:make_tree(refac_syntax:type(Node2), Gs2),
            {refac_misc:rewrite(Node2, Node3), 
             Changed1 orelse 
             lists:member(true, lists:flatten(G))}
    end.
   

%% pre_order.
extended_stop_tdTP(Fun, Node, Others) ->
    {Node1, C} =extend_stop_tdTP(Fun, Node, Others),
    {remove_fake_begin_end(Node1),C}.

extend_stop_tdTP(Fun, Node, Others) ->
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
                    Gs1 = [[{extend_stop_tdTP(Fun, T, Others),C}||T <- G1]||
                                     G <- Gs,{G1, C} <- [Fun(G, Others)]],
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
    lists:append([remove_fake_begin_end_2(N)||N<-Node, not is_empty_node(N)]).
   
                  
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

is_empty_node(Node) ->
    is_tree(Node) andalso refac_syntax:type(Node)==empty_node.
       
   
make_fake_block_expr(Es) ->
     refac_syntax:add_ann({fake_block_expr, true},
                         refac_syntax:block_expr(Es)).

%%=================================================================
%%@private
match(Temp, Node) -> 
    generalised_unification:expr_match(Temp, Node).

%%@private
match(Temp, Node, Cond) -> 
    generalised_unification:expr_match(Temp, Node, Cond).
    

try_expr_match([], _Node) ->false;
try_expr_match([{rule,Fun,BeforeExpr}|T], Node) 
  when is_list(BeforeExpr) andalso is_list(Node) ->
    try_expr_match_2([{rule,Fun, BeforeExpr}|T], Node,1);
    
try_expr_match([{rule,Fun, BeforeExpr}|T], Node) when 
      not is_list(BeforeExpr) andalso not is_list(Node)->
    try_expr_match_1([{rule,Fun, BeforeExpr}|T], Node);
try_expr_match([_|T], Node) ->
    try_expr_match(T, Node).

try_expr_match_1([{rule,Fun, _BeforeExpr}|T], Node) ->
    case Fun(Node) of 
        {NewExpr, true} ->
            case is_list(NewExpr) of
                true ->
                    {true, NewExpr};
                false ->
                    case is_tree(NewExpr) of 
                        true ->
                            {true, copy_pos_and_range(Node, NewExpr)};
                        false ->
                            {true, NewExpr}
                    end
            end;
        {_, false} ->
            try_expr_match(T, Node)
    end.

try_expr_match_2([{rule, Fun, BeforeExpr}|T], NodeList, Index) ->
    Len1 = length(BeforeExpr),
    Len2 = length(NodeList),
    case Len1 =< Len2 of
        true ->
            Exprs = lists:sublist(NodeList, Index, Len1),
            case Fun(Exprs) of 
                {NewExpr, true} ->
                    NewExpr1 = case NewExpr of
                                   [E|Es]->
                                       [copy_pos_and_range(hd(Exprs), E)|Es];
                                   _ ->
                                       [copy_pos_and_range(hd(Exprs),NewExpr)]
                               end,
                    NewNodeList =  lists:sublist(NodeList, Index-1) ++
                        NewExpr1 ++  lists:nthtail(Index+Len1-1, NodeList),
                    {true, NewNodeList};
                {_, false} ->
                    if Index < Len2 ->
                            try_expr_match_2([{rule,Fun, BeforeExpr}|T], NodeList, Index+1);
                       true ->
                            try_expr_match(T, NodeList)
                    end
            end;
        false->
            try_expr_match(T, NodeList)
    end.
            


is_meta_list_var(Var) ->
    case refac_syntax:type(Var) of 
        variable ->
            is_meta_list_var_name(refac_syntax:variable_name(Var));
        _ -> 
            false
    end.
  
is_meta_list_var_name(VarName) ->
    VarName1 = lists:reverse(atom_to_list(VarName)),
    lists:prefix("@@", VarName1) andalso 
        (not lists:prefix("@@@",VarName1)).

is_meta_var_name(VarName) ->
    lists:prefix("@", lists:reverse(atom_to_list(VarName))).
   
is_meta_atom_name(AtomName) ->
    AtomName1 = atom_to_list(AtomName),
    is_fun_name(AtomName1) andalso
    lists:prefix("@", lists:reverse(AtomName1)).



%%=================================================================
%%@doc For every AST node in `Scope' that pattern matches the AST 
%% represented by `Template', if `Cond' evaluates to `true', 
%% then information about this node is collected using the function 
%% specified by `CollectorFun'. `Scope' can be an AST or a list of 
%% Erlang files/directories, and in the latter case, each Erlang 
%% file included is parsed into an AST first.
%%@spec collect(Function, syntaxTree(),[filename()|dir()|syntaxTree()]) ->
%%          [any()]|{error, Reason}
%%@private
collect(Fun,TempAST, Scope) when is_list(Scope) ->
    Files = refac_misc:expand_files(Scope, ".erl"),
    Res=[collect_in_one_file(F, Fun, TempAST)||F <- Files],
    lists:append(Res);
collect(Fun, TempAST, Scope) ->
    case is_tree(Scope) of
        true ->
            do_search_matching_code(none, Scope, Fun, TempAST);
        false ->
            {error, badarg}
    end.
    
collect_in_one_file(File, Fun, TempAST) ->
    %% ?wrangler_io("Processing file:~p\n", [File]),
    {ok, {AST, _}} = wrangler_ast_server:parse_annotate_file(File, true, [], 8),
    do_search_matching_code(File, AST, Fun, TempAST).

do_search_matching_code(FileName, AST, Fun, TempAST) 
  when is_list(TempAST) ->
    do_search_matching_code_list(FileName, AST, Fun, TempAST);
do_search_matching_code(FileName, AST, Fun, TempAST) ->
    do_search_matching_code_non_list(FileName, AST, Fun, TempAST).

do_search_matching_code_list(FileName, AST, Fun, _TempAST) ->
    F = fun(Node, Acc) ->
                Nodes = get_expr_seqs(Node),
                case Fun(FileName,Nodes) of
                      {true, Res} ->
                        [Res|Acc];
                    false -> Acc
                end
        end,
    lists:reverse(ast_traverse_api:full_tdTU(F, [], AST)).


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

do_search_matching_code_non_list(FileName, AST, Fun, TempAST) ->
    case refac_syntax:type(TempAST) of
        function_clause ->
            stop_td_collect(FileName, AST, Fun, function_clause);
        function ->
            stop_td_collect(FileName, AST, Fun, function);
        _ -> 
            full_td_collect(FileName, AST, Fun)
    end.

stop_td_collect(FileName, AST, Fun, Type) ->
    F= fun(Node, Acc) ->
               case Fun(FileName,Node) of 
                     {true, Res} ->
                         {[Res|Acc], true};
                     false -> 
                         case refac_syntax:type(Node)==Type of 
                             true ->
                                 {Acc, true};
                             false ->
                                 {Acc, false}
                         end
                 end
         end,
    AST1=extend_function_clause(AST),
    lists:reverse(ast_traverse_api:stop_tdTU(F, [], AST1)).


full_td_collect(FileName, AST, Fun) ->
    F= fun(Node, Acc) ->
                 case Fun(FileName, Node) of 
                     {true, Res} ->
                         [Res|Acc];
                     false ->
                         Acc
                 end
       end,
    lists:reverse(ast_traverse_api:full_tdTU(F, [], AST)).
  
%%@private
search_and_collect(Collectors, Input, TraverseStrategy) 
  when is_list(Input) ->
    check_collectors(Collectors),
    case lists:all(fun(I) -> 
                           filelib:is_file(I) 
                   end, Input) of 
        true -> 
            search_and_collect_2(Collectors, Input, TraverseStrategy);
        false ->
            case lists:all(fun(I) ->
                                   case I of 
                                       {File, Tree} ->
                                           filelib:is_file(File) andalso is_tree(Tree);
                                       _ ->
                                           is_tree(I)
                                   end
                           end, Input) of 
                true ->
                    Res=[search_and_collect_1(Collectors,I,TraverseStrategy)||I <- Input],
                    lists:append(Res);
                false ->
                    erlang:error("Collectors are applied to an invalid scope.")
            end
    end;
search_and_collect(Collectors, Input, TraverseStrategy) ->
    check_collectors(Collectors),
    case is_tree(Input) of
        true ->
            Input1 = extend_function_clause(Input),
            search_and_collect_1(Collectors,Input1,TraverseStrategy);
        false ->
            erlang:error("Collectors are applied to an invalid scope.")
    end.     

search_and_collect_1(Collectors, {File, Tree},TraverseStrategy) ->
    search_and_collect_4(File, Collectors, Tree, TraverseStrategy);
search_and_collect_1(Collectors, Tree,TraverseStrategy) ->
    search_and_collect_4(none, Collectors, Tree, TraverseStrategy).
   

search_and_collect_2(Collectors, FileOrDirs, TraverseStrategy) ->
    Files = refac_misc:expand_files(FileOrDirs, ".erl"),
    lists:append([begin
                          search_and_collect_3(Collectors, File, TraverseStrategy)
                      end||File<-Files]).
   

search_and_collect_3(Collectors, File, TraverseStrategy) ->
    ?wrangler_io("The current file under checking is:\n~p\n", [File]),
    {ok, {AST, _}} = wrangler_ast_server:parse_annotate_file(File, true, [], 8),
    AST0 = extend_function_clause(AST),
    search_and_collect_4(File, Collectors, AST0,TraverseStrategy).
   

search_and_collect_4(File, Collectors, Tree,TraverseStrategy)->
    F=fun(Node)->  
              try_match_and_collect(Collectors, File, Node)
      end,
    case TraverseStrategy of
        full_td_tu ->
            full_td_tu(F,[], Tree);
        stop_td_tu ->
            stop_td_tu(F,[],Tree)  
    end.


try_match_and_collect([], _File, _Node) ->[];
try_match_and_collect([{collector,Fun,BeforeExpr}|T], File, Node) 
  when is_list(BeforeExpr) andalso is_list(Node) ->
    try_match_and_collect_2([{collector, Fun, BeforeExpr}|T], File, Node);

try_match_and_collect([{collector, Fun, BeforeExpr}|T], File,Node) when 
      not is_list(BeforeExpr) andalso not is_list(Node)->
    try_match_and_collect_1([{collector, Fun, BeforeExpr}|T], File, Node);
try_match_and_collect([_|T], File, Node) ->
    try_match_and_collect(T, File, Node).

try_match_and_collect_1([{collector, Fun, _BeforeExpr}|T], File, Node) ->
    case Fun(File, Node) of 
        {Res, true} ->
            [Res];
        {_, false} ->
            try_match_and_collect(T, File,Node)
    end.
try_match_and_collect_2([{collector, Fun, BeforeExpr}|T], File, NodeList) ->
    Len1 = length([E||E<-BeforeExpr, not is_meta_list_var(E)]),
    Len2 = length(NodeList),
    case Len1 =< Len2 of
        true ->
            Res=[case Fun(File, lists:sublist(NodeList, Index, Len1)) of
                     {R, true} ->
                         [R];
                     {_, false} ->
                         []
                 end
                 ||Index<-lists:seq(1, Len2-Len1+1)],
            lists:append(Res);
        false->
            try_match_and_collect(T, File, NodeList)
    end.

full_td_tu(F, S, Tree) ->
    R = F(Tree),
    case refac_syntax:subtrees(Tree) of
        [] -> 
            R++S;
        Gs -> 
            full_tdTU_1(F, R++S, Gs)
    end.

full_tdTU_1(F, S, [L | Ls]) ->
    S1=case L of
           [L1|_] -> 
               case syntax_context(L1) of
                   body_expr ->
                       F(L)++S;
                   _ -> S
               end;
        _ -> S
       end,
    full_tdTU_1(F, full_tdTU_2(F, S1, L), Ls);
full_tdTU_1(_, S, []) -> S.

full_tdTU_2(F, S, [T | Ts]) -> full_tdTU_2(F, full_td_tu(F, S, T), Ts);
full_tdTU_2(_, S, []) -> S.


stop_td_tu(F, S, Tree) ->
    case F(Tree) of
        [] ->
            case refac_syntax:subtrees(Tree) of
                [] -> 
                    S;
                Gs -> 
                    stop_tdTU_1(F, S, Gs)
            end;
        R ->
            R++S
    end.
    

stop_tdTU_1(F, S, [L | Ls]) ->
    S1=case L of
           [L1|_] -> 
               case syntax_context(L1) of
                   body_expr ->
                       F(L)++S;
                   _ -> S
               end;
        _ -> S
       end,
    stop_tdTU_1(F, stop_tdTU_2(F, S1, L), Ls);
stop_tdTU_1(_, S, []) -> S.

stop_tdTU_2(F, S, [T | Ts]) -> stop_tdTU_2(F, stop_td_tu(F, S, T), Ts);
stop_tdTU_2(_, S, []) -> S.


%%================================================================
%%@spec(extend_function_clause(Tree::syntaxTree()) -> syntaxTree()).
%%@private             
extend_function_clause(Tree) when is_list(Tree) ->
    [extend_function_clause(T)||T<-Tree];
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

expand_meta_list(Tree) ->
    {Tree1, _}  = ast_traverse_api:full_tdTP(fun expand_meta_list_1/2, Tree, {}),
    Tree1.

expand_meta_list_1(Node, _OtherInfo) ->
    case refac_syntax:type(Node) of 
        function ->
            {Node, false};
        case_expr ->
            Arg = refac_syntax:case_expr_argument(Node),
            Cs = refac_syntax:case_expr_clauses(Node),
            Cs1 = lists:append([expand_meta_clause(C)||C <- Cs]),
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
            Cs = refac_syntax:receive_expr_clauses(Node),
            T = refac_syntax:receive_expr_timeout(Node),
            A = refac_syntax:receive_expr_action(Node),
            Cs1 = lists:append([expand_meta_clause(C)||C <- Cs]),
            case Cs=/=Cs1 of
                true ->
                    Node1=refac_misc:rewrite(
                            Node,
                            refac_syntax:receive_expr(Cs1, T, A)),
                    {Node1, false};
                false ->
                    {Node, false}
            end;
        if_expr ->
            Cs = refac_syntax:if_expr_clauses(Node),
            Cs1 = lists:append([expand_meta_clause(C)||C <- Cs]),
            case Cs=/=Cs1 of
                true ->
                    Node1=refac_misc:rewrite(
                            Node,refac_syntax:if_expr(Cs1)),
                    {Node1, false};
                false ->
                    {Node, false}
            end;
        try_expr->
            B = refac_syntax:try_expr_body(Node),
            Cs= refac_syntax:try_expr_clauses(Node),
            H = refac_syntax:try_expr_handlers(Node),
            A = refac_syntax:try_expr_after(Node),
            Cs1 = lists:append([expand_meta_clause(C)||C <- Cs]),
            case Cs=/=Cs1 of
                true ->
                    Node1=refac_misc:rewrite(
                            Node,refac_syntax:try_expr(B,Cs1, H, A)),
                    {Node1, false};
                false ->
                    {Node, false}
            end;
        fun_expr ->
            Cs = refac_syntax:fun_expr_clauses(Node),
            Cs1 = lists:append([expand_meta_clause(C)||C <- Cs]),
            case Cs=/=Cs1 of
                true ->
                    Node1=refac_misc:rewrite(
                            Node,refac_syntax:fun_expr(Cs1)),
                    {Node1, false};
                false ->
                    {Node, false}
            end;
        arity_qualifier ->
            Body = refac_syntax:arity_qualifier_body(Node),
            Arity = refac_syntax:arity_qualifier_argument(Node),
            Body1 = case refac_syntax:type(Body) of
                        block_expr ->
                            refac_syntax:block_expr_body(Body);
                        _ -> Body
                    end,
            Arity1 =case refac_syntax:type(Arity) of
                       block_expr ->
                            refac_syntax:block_expr_body(Arity);
                        _ -> Arity
                    end,
            case is_list(Body1) andalso is_list(Arity1) andalso
                length(Body1)==length(Arity1) of 
                true ->
                    AQs=[refac_syntax:arity_qualifier(A, Q)||
                            {A,Q}<-lists:zip(Body1, Arity1)],
                    Node1 = 
                        refac_syntax:add_ann(
                          {fake_block_expr, true},
                          reset_pos_and_range(
                            refac_syntax:block_expr(AQs))),
                    {Node1, true};
                false ->
                    case refac_syntax:type(Body1) of
                        nil ->
                            {refac_syntax:empty_node(), true};
                        _ -> 
                            {Node, false}
                    end
            end;
        _ ->
            {Node, false}
    end.

expand_meta_clause(Clause) ->
    case is_meta_clause(Clause) of 
        true->
            [Pat] = refac_syntax:clause_patterns(Clause),
            case revert_clause_guard(refac_syntax:clause_guard(Clause)) of
                [[]] ->
                    [Body] = refac_syntax:clause_body(Clause),
                    ZippedPB = lists:zip(Pat, Body),
                    [refac_syntax:clause(P, none, B)||
                        {P, B} <-ZippedPB];
                [[Guard]] ->
                    [[Guard]] = revert_clause_guard(refac_syntax:clause_guard(Clause)),
                    [Body] = refac_syntax:clause_body(Clause),
                    ZippedPGB = lists:zip3(Pat, Guard, Body),
                    [case G of 
                         [] -> refac_syntax:clause(P, none, B);
                         _ ->
                             refac_syntax:clause(P, G, B)
                     end
                     ||{P, G, B} <-ZippedPGB]
            end;
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
                        [[]] ->
                            case Body of
                                [B]->
                                    is_list_of_lists(B);
                                _ ->
                                    false
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

%% =====================================================================
%% @spec type(Node::syntaxTree()) -> atom()
%%
%% @doc The function is the same as erl_syntax:type/1. It returns the 
%% type tag of <code>Node</code>. If <code>Node</code>
%% does not represent a syntax tree, evaluation fails with reason
%% <code>badarg</code>. Node types currently defined are:
%% <p><center><table border="1">
%%  <tr>
%%   <td>application</td>
%%   <td>arity_qualifier</td>
%%   <td>atom</td>
%%   <td>attribute</td>
%%  </tr><tr>
%%   <td>binary</td>
%%   <td>binary_field</td>
%%   <td>block_expr</td>
%%   <td>case_expr</td>
%%  </tr><tr>
%%   <td>catch_expr</td>
%%   <td>char</td>
%%   <td>class_qualifier</td>
%%   <td>clause</td>
%%  </tr><tr>
%%   <td>comment</td>
%%   <td>cond_expr</td>
%%   <td>conjunction</td>
%%   <td>disjunction</td>
%%  </tr><tr>
%%   <td>eof_marker</td>
%%   <td>error_marker</td>
%%   <td>float</td>
%%   <td>form_list</td>
%%  </tr><tr>
%%   <td>fun_expr</td>
%%   <td>function</td>
%%   <td>generator</td>
%%   <td>if_expr</td>
%%  </tr><tr>
%%   <td>implicit_fun</td>
%%   <td>infix_expr</td>
%%   <td>integer</td>
%%   <td>list</td>
%%  </tr><tr>
%%   <td>list_comp</td>
%%   <td>macro</td>
%%   <td>match_expr</td>
%%   <td>module_qualifier</td>
%%  </tr><tr>
%%   <td>nil</td>
%%   <td>operator</td>
%%   <td>parentheses</td>
%%   <td>prefix_expr</td>
%%  </tr><tr>
%%   <td>qualified_name</td>
%%   <td>query_expr</td>
%%   <td>receive_expr</td>
%%   <td>record_access</td>
%%  </tr><tr>
%%   <td>record_expr</td>
%%   <td>record_field</td>
%%   <td>record_index_expr</td>
%%   <td>rule</td>
%%  </tr><tr>
%%   <td>size_qualifier</td>
%%   <td>string</td>
%%   <td>text</td>
%%   <td>try_expr</td>
%%  </tr><tr>
%%   <td>tuple</td>
%%   <td>underscore</td>
%%   <td>variable</td>
%%   <td>warning_marker</td>
%%  </tr>
%% </table></center></p>
type(Node) ->
    refac_syntax:type(Node).

%%@private
-spec(make_arity_qualifier(atom(), integer()) ->
             syntaxTree()).
make_arity_qualifier(FunName, Arity) when 
      is_atom(FunName) andalso is_integer(Arity) ->
    refac_syntax:arity_qualifier(refac_syntax:atom(FunName),
                                  refac_syntax:integer(Arity));
make_arity_qualifier(_FunName, _Arity) ->
    erlang:error("badarg to function refac_api:make_arity_qualifier/2.").


   
check_rules(Rules) when is_list(Rules)->
    AllRules=lists:all(fun(R) ->
                               case R of
                                   {rule, _, _} ->
                                       true;
                                   _ ->
                                       false
                           end
                       end, Rules),
    case AllRules of 
        true ->
            ok;
        false ->
            throw({error, "The first argument of a TP traverse "
                   "strategy can only be a list of rules."})
    end;
check_rules(_Rules) ->
    throw({error, "The first argument of a TP traverse "
            "strategy can only be a list of rules."}).
       
  
check_collectors(Collectors) when is_list(Collectors)->
    AllCollectors=lists:all(fun(R) ->
                                    case R of
                                        {collector, _, _} ->
                                            true;
                                        _ ->
                                            false
                                    end
                            end, Collectors),
    case AllCollectors of
        true ->
            ok;
        false ->
            throw({error, "The first argument of a TU traverse "
                   "strategy can only be a list of collectors."})
    end;
check_collectors(_Collectors) ->
    throw({error, "The first argument of a TU traverse "
           "strategy can only be a list of collectors."}).
        
