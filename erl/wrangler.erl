%% =====================================================================
%% Reactoring Interface Functions.
%%
%% Copyright (C) 2006-2008  Huiqing Li, Simon Thompson

%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.

%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%%
%% =====================================================================

%% @copyright 2006-2008 Huiqing Li, Simon Thompson
%%
%% @author Huiqing Li <hl@kent.ac.uk>
%%   [http://www.cs.kent.ac.uk/projects/forse]

%% @version  0.2
%% @end

%%
%% @doc The collection of refactorings supported by Wrangler.
%%
%% @end
%% ============================================


-module(wrangler).

-export([rename_var/5, 
	 rename_fun/5, 
	 rename_mod/3, 
	 rename_mod_batch/3,
	 generalise/4,
	 move_fun/6,
	 duplicated_code/3,
	 expression_search/3]).

%% ====================================================================================================
%% @doc Rename a variable name with a user-supplied new name.
%% <p> To apply this refactoring, point the cursor to  any occurrence of this variable, then select
%% <em> Rename Variable Name </em> from the <em> Refactor </em> menu, after that the refactorer will prompt
%% to enter the new parameter name in the mini-buffer. 
%% </p>
%% <p> This refactoring has a local effect, i.e., it only affects the function in which the refactoring is initialised. 
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring: 
%% <li> The new variable name should not conflict with any of the declared variable names in the same scope;</li>
%% <li> The new name should not shadow any of the existing variables in the outer scopes, or be shadowed by any of 
%% of existing variables in the inner scopes, i.e., renaming to the new name should not change the semantics of the 
%% program.</li>
%% </p>
%% @end

%% =====================================================================
%% @spec rename_var(FileName::filename(), Line::integer(), Col::integer(), NewName::string(),SearchPaths::[string()])
%% -> term()
%%    
rename_var(FileName, Line, Col, NewName, SearchPaths) ->
    refac_rename_var:rename_var(FileName, Line, Col, NewName, SearchPaths).



%%=========================================================================================
%% @doc Rename a function name with a user-supplied new name.
%% <p> To apply this refactoring, point the cursor to any occurrence of this 
%% function name, then select <em> Rename Function Name </em> from the <em> Refactor </em> menu, 
%% after that the refactorer will prompt to enter  the new function name in the mini-buffer.
%% </p>
%% <p>
%% When renaming an exported function name, this refactoring has a global effect, i.e.,
%% it affects all those modules in which this function is imported/used.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new function name should not cause confliction with any of the functions which are in scope in the 
%% current module;</li>
%% <li> In the case that the function to be renamed is imported by another module, the new function name (with the same 
%% arity) should not be already in scope (either defined or imported) in that module. </li>
%% </p>
%% @end

%% =====================================================================
%% @spec rename_fun(FileName::filename(), Line::integer(), Col::integer(), NewName::string(), SearchPaths::[string()])
%% -> term()
%% @end
%%======================================================================
rename_fun(FileName, Line, Col, NewName, SearchPaths) ->
    refac_rename_fun:rename_fun(FileName, Line, Col, NewName, SearchPaths).


%%======================================================================================
%% @doc Rename a module name with a user-supplied new name.
%% <p> To apply this refactoring, point the cursor to anywhere in the module to be renamed, then select 
%% <em> Rename Module Name </em> from the <em> Refactor </em> menu, after that, the refactorer will prompt to enter 
%% the new module name in the mini-buffer.
%% </p>
%% <p> This refactoring has a global effect, i.e., it affects all those modules in which the module to be renamed is 
%% imported, or used as a module qualifier.
%% </p>
%% <p>
%% The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new module name should be a fresh name. </li>
%% <li> This refactoring assume that the file basename is always the same as the module name, therefore this 
%% refactoring changes the filename as well. </li>
%% </p>
%% @end
%% =====================================================================
%% @spec rename_mod(FileName::filename(), NewName::string(), SearchPaths::[string()])-> term()
%%   
rename_mod(FileName, NewName, SearchPaths) ->
    refac_rename_mod:rename_mod(FileName, NewName, SearchPaths).


%% =====================================================================
%% @doc Rename a collect of module names in batch mode. 
%% <p> This refactoring is supposed to be run from the Erlang shell. For example, 
%% to rename all those module names which match the regular expression "foo_*" to 
%% "foo_*_1_0" in the directory <code> c:/wrangler/test </code>, just type the following command:
%% <code> wrangler:rename_mod_batch("foo_*, "foo_*_1_0", ["c:/wrangler/test"]) </code>.
%% </p>
%% <p> This refactoring has a global effect. </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> The new module names should not conflict with each other, or any existing module names 
%% in the same scope which will not be renamed. </li>
%% <li> This refactorings assumes that the file basename is always the same as the module name. </li>
%% </p>
%% @end
%% =====================================================================
%% @spec rename_mod_batch(OldNamePattern::string(), NewNamePattern::string(), 
%%                        SearchPaths::[string()])-> ok | {error, string()}
%%   
rename_mod_batch(OldNamePattern, NewNamePattern, SearchPaths) ->
    refac_batch_rename_mod:batch_rename_mod(OldNamePattern, NewNamePattern, SearchPaths).


%% ==========================================================================================
%% @doc Generalise a function definition by selecting a sub-expression of its right-hand 
%% side and making this the value of a new argument added to the definition of the function. 
%% The sub-expression becomes the actual parameter at the call sites. 
%%
%% <p> To apply this refactoring, highlight the expression first, then  select 
%% <em> Generalise Function Definition </em> from the <em>Refactor</em> menu, after 
%% that the refactorer will prompt to enter the parameter name in the mini-buffer. </p>
%% 
%% <p> Here is an example of generalisation, in which the function <code> add_one </code> defined 
%% on the left-hand side is generalised on the expression <code> 1 </code>, and the result is 
%% shown on the right-hand side. 
%%
%%        ```    -module (test).                          -module (test). 
%%               -export([f/1]).                          -export([f/1]).
%%        
%%               add_one ([H|T]) ->                       add_one (N, [H|T]) ->
%%                  [H+1 | add_one(T)];                      [H+N | add_one(N,T)];
%%               add_one ([]) -> [].                      add_one (N, []) -> [].
%%
%%               f(X) -> add_one(X).                      f(X) -> add_one(1,X)
%%        ''' 
%%  </p>
%%
%% <p> In the case that the selected expression has a side-effect, the refactorer will wrap this expression 
%% in an function expression before passing it at the actual parameter to the call-sites. This is illustrated 
%% in the following example, in which function <code>repeat/1</code> is generalised on the expression 
%% <code>io:format("Hello\n")</code>.
%% 
%%         ```   -module (test).                          -module (test).                          
%%               -export([f/0]).                          -export([f/0]).
%%
%%               repeat(0) -> ok;                         repeat(A, 0) -> ok;
%%               repeat(N) ->                             repeat(A, N) ->
%%                 io:format("Hello\n"),                    A( ),
%%                 repeat(N-1).                             repeat(A,N-1).
%%
%%               f() -> repeat(5).                        f( ) -> 
%%                                                           repeat (fun( )->io:format ("Hello\n") end, 5).
%%          '''
%% </p>
%%
%% <p> This refactoring <em>only </em> affects the module in which the refactoring is initialised. In the case that 
%% the generalised function is exported by the module, an auxiliary function will be created 
%% to wrap the generalised function up, so that the module's interface is not changed.
%% </p>
%% <p> The following <em> side-conditions </em> apply to this refactoring:
%% <li> Suppose the function to be generalised is <code>foo/n </code>, then <code>foo/n+1</code> should not  
%% be in scope before the generalisation;</li>
%% <li> The selected expression should not contain any locally declared variable(s), unless the selected expression 
%% has side effect, in which case the locally declared variables will become the parameters of the function expression.
%% </li>
%% <li> The user-provided parameter name should not conflict with the existing parameters or
%% change the semantics of the function to be generalised. </li>
%% </p>
%% @end

%% ==============================================================================
%% @spec generalise(FileName::filename(), Start::Pos, End::Pos, ParName::string())-> term()
%%         Pos = {integer(), integer()}

generalise(FileName, Start, End, ParName) ->
    refac_gen:generalise(FileName, Start, End, ParName).

%% ================================================================================
%% @doc Move a function definition from its current module to another module.
%% <p> To apply this refactoring, point the cursor at the function definition, then 
%% select <em> Move Definition to Another Module</em> from the <em> Refactor </em> menu, 
%% after that the refactorer will prompt to enter the target module name in the mini-buffer. 
%% </p>
%% <p> This refactoring has a global effect, i.e., it affects all the modules in which 
%%     the function is imported/used.
%% </p>
%% <p> This refactoring assumes that an erlang module name always matches it file name.
%% </p>
%% <p> Suppose we move functin <em> foo/n </em> from its current module <em> M </em> 
%%     to module <em> N </em>, then the following <em> side-conditions </em> apply to 
%%     this refactoring: 
%% <li> If <em> foo/n </em> is already in scope in module <em> N </em>, then its defining 
%%      module should be  <em> M </em>.
%% </li>
%% <li> Function <em> foo/n </em> should not contain any uses of <em> implicit fun expressions </em> (Note: move a 
%% collection of modules together to another module will be supported by another refactoring).
%% </li>
%% </p>
%% @end

%% ===================================================================================
%% @spec move_fun(FileName::filename(),Line::integer(),Col::integer(),ModName::string(), 
%%                CreateNewFile::boolean(),SearchPaths::[string()])-> term()
%%         
move_fun(FileName, Line, Col, TargetModName, CreateNewFile, SearchPaths) ->
    refac_move_fun:move_fun(FileName, Line, Col, TargetModName, CreateNewFile, SearchPaths).



%% ==================================================================================
%% @doc Find duplicated code in a Erlang source file.
%% <p> This function only reports the duplicated code fragments found in the current source file. It does 
%% not remove those duplicated code. Two parameters can be provided by the user to specify the minimum code clones 
%% to report, and they are:  \emph{the  minimum number of lines of a code clone} and \emph{the minimum number of 
%% duplicated times}, the default values are 5 and 2 respectively.
%% </p>
%% =====================================================================================
%% @spec duplicated_code(FileName::filename(),MinLines::integer(),MinClones::integer()) -> term().
%%                
duplicated_code(FileName, MinLines, MinClones) -> 
    refac_duplicated_code:duplicated_code(FileName, MinLines, MinClones).
    


%% @doc Search a user-selected expression or a sequence of expressions from an Erlang source file.
%%
%% <p> This functionality allows the user to search an selected expression or a sequence of expressions
%% from the current Erlang buffer. The searching ignores variables names and literals, but it takes
%% the binding structure of variables into account. Therefore the found expressions are the same to the 
%% highlighted expression up to variable renaming and literal substitution. Layout and comments are ignored 
%% by the searching.
%% </p>
%% <p> When the selected code contains multiple, but non-continuous sequence of, expressions, the first
%% continuous sequence of expressions is taken as the user-selected expression. A continuous sequence of
%% expressions is a sequence of expressions separated by ','.
%% <p>
%% =====================================================================================
%% @spec expr_search(FileName::filename(),Start::Pos, End::Pos) -> term().
%%   
expression_search(FileName, Start, End) ->
    refac_expr_search:expr_search(FileName, Start, End).
