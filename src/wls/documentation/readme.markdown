# Wrangler Language Server

The Wrangler Language Server is an extension to the [Erlang Language Server](https://erlang-ls.github.io/). It provides Erlang refactorings while implementing the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) through Erlang LS.

You can read more about Wrangler in the [Wrangler documentation](https://refactoringtools.github.io/wrangler/).

# Installation

1. Follow the [installation instructions](https://github.com/RefactoringTools/wrangler/blob/master/INSTALL) of Wrangler.

2. [Install the Erlang Language Server](https://erlang-ls.github.io/getting-started/overview/) in your chosen text editor.

3. Modify the [erlang_ls.config](https://erlang-ls.github.io/configuration/) file:

```
wrangler:
  enabled: true
  path: "/path/to/wrangler/ebin" 
  tab_with: 8
  enabled_refactorings:
    - "comment-out-spec"
    - "fold-expression"
    - "generalise-fun"
    - "move-fun"
    - "new-fun"
    - "new-macro"
    - "new-var"
    - "rename-fun2"
    - "rename-var"

```
# Usage

## General case: 
1. Highlight/set the cursor to the place where you want to apply the refactoring.\
![Screenshot](1_highlight.png)
2. Select one of the refactorings from the appearing menu (code actions).\
![Screenshot](2_select.png)
3. Provide additionial information if needed (eg. new variable name).\
![Screenshot](3_input.png)
4. Save the changes.\
![Screenshot](4_save.png)

### Wrangler forms:
In some cases, refactorings can be done in multiple places. Wrangler highlights the possible refactor candidates that you can choose from in a way we call Wrangler forms.

Select `Refactor this instance` in all the places you want the execute the refactoring.

Until you choose `exit` or select all the possible refactorings, please do not change the file manually.

For example, while folding ```fun_to_fold```:\
![Screenshot](5_form.png)

## Supported refactorings

### Comment out spec - `comment-out-spec`
Comment out all the lines starting with -spec in a file. 
This can be initiated by a code lens at the beginning of the file:\
![Screenshot](6_comment_out_spec.png)

### Fold expression - `fold-expression`
With Wrangler form, replace instances of the function body by the corresponding function definition. 

### Generalise function - `generalise-fun`
Refactor the highlighted expression as the function's new argument. 

### Introduce new variable - `new-var`
Refactor the highlighted expression as a new variable.

### Move function - `move-fun`
Move a function definition from its current module to another module. It changes all function calls in the working directory.

### Extract function - `new-fun`
Introduce a new function definition to represent a selected expression sequence inside an existing function. That selected sequence of expressions is replaced by a call to the new function.

### New macro - `new-macro`
Define a macro to represent a selected sequence of expressions.

### Rename function - `rename-fun2`
Rename a function.

### Rename variable - `rename-var`
Rename a variable.

## For Wrangler delevopers

Wrangler-ls extends Erlang-ls with additional codeActions, codeLens etc. The `wls_utils` module is used to convert between Erlang-ls/LSP and Wrangler type representations (eg. positions) and to provide some LSP request/notification constructors (eg. textEdit, showMessage). 

In the Erlang-ls side, first, Erlang-ls loads the Wrangler modules and starts Wrangler. Then, `wrangler_handler` module adds the corresponding language features for all possible requests. 

In the Wrangler side, you can find similar files and erlang behaviours to ELS' `els_code_actions`, `els_code_lens`, `els_execute_command_provider` modules. (These files are prefixed with `wls_`).

Used [LSP features](https://microsoft.github.io/language-server-protocol/specifications/specification-current/):
- `codeAction` - to offer refactorings for the user
- `executeCommand` - to execute the refactorings
- `applyEdit` - to apply the code changes on the code editor side
- `showMessage` - to inform the user about success/error
- `semantic tokens` - used in Wrangler forms to colorize the code`s foreground
- `documentHighlight` - used in Wrangler forms to colorize the code`s background
- `codeLens` - used in Wrangler forms to act as buttons

### Providing user input

User inputs (eg. asking for a new variable name) are handled by a middleware in the extension since these functions are not yet supported by LSP.

To request an input from the user, add the following attribute to the command arguments (`text` field  is not required):

```user_input => #{'type' => variable|atom|file, 'text' => <<"Placeholder text">>}``` 

On successful inputs, a `value` field will be added to the `user_input` with the given value.

User inputs are yet only supported in the VSCode`s extension. In other editors, default names will be used (eg. "NewVar")

### Adding LSP support for a refactoring
To make a refactoring available through LSP, create a new Code Action module prefixed with `wls_code_action_` implementing `wls_code_actions` behaviour:

```
%% Title which is shown to the user.
-callback title() -> binary().

%% Identifier of the action and the commands. 
%% Same as the file's name without the wls_code_action_ prefix.
-callback id() -> action_id().

%% Initialize the action. 
%% Optional callback.
-callback init(els_core:uri(), els_core:range()) -> state().

%% Whether the action is offered based on the highlighted range. 
%% Optional callback, defaults to true.
-callback precondition(els_core:uri(), els_core:range()) -> boolean().

%% The command`s arguments.
-callback command_args(els_core:uri(), els_core:range(), state()) -> map().

%% Execute the command with the given arguments. 
%% The first element of the argument list is the one returned by command_args. 
-callback execute_command([any()]) -> [map()].
```

Register the code action in `wls_code_actions:available_actions/0`

To understand how ELS Code Lenses works, see this article: [How to: Code Lenses](https://erlang-ls.github.io/articles/tutorial-code-lenses/). Code Actions has a similar behaviour.


### Wrangler forms

For complex refactorings (eg. fold), Wrangler forms are introduced.

The form's state are handled by `wls_server`.