
                    Wrangler -- An Erlang Refactorer
                           21/10/2013
              http://www.cs.kent.ac.uk/projects/wrangler/

---------------------------------------------------------------------------
Refactoring is the process of improving the design of a program without 
changing its external behaviour. While refactoring by hand is possible, tool
support for refactoring is invaluable as it is more reliable, and allows 
refactoring to be done (or undone) easily. Wrangler is such a tool that 
supports interactive refactoring of Erlang programs. Wrangler is integrated 
into both (X)Emacs and Eclipse.

Wrangler's refactorings cover structural changes such as function, variable 
and module renaming, function extraction and generalisation. Wrangler 
recognises macros in code, and can be used on a single file or across a 
whole project.

Wrangler can also be used to locate and remove code clones, and to improve 
the module structure of projects.

Wrangler is extensible, with an API for writing new refactorings and a DSL 
for scripting complex refactoring combinations.

Wrangler also supports testing in EUnit, QuickCheck and Common Test, so your 
tests are refactored automatically when you refactor your code.

A new functionality added recently to Wrangler is the support for API 
migration. With Wrangler, API migration is achieved in this way: whenever 
an API function's interface is changed, the author of this API function only 
needs to implement an adapter function in Erlang, which defines the old API 
functions in terms of the new. From this definition we automatically generate 
rules to transform the client code to make use of the new API rather than 
the old. 
 
--------------------------------- Documentation -------------------------------

Wrangler online documentation is available from http://refactoringtools.github.com/wrangler

-------------------------------- Wrangler Installation ------------------------
         
		 See INSTALL for instructions.
 
--------------------- how to use the refactorer -------------------------------
 1.  Open an Erlang source file in the Emacs editor. You should now have a
     menu called <em>Erlang</em>, if you have configured Emacs properly to
     use the Erlang-mode (see the file INSTALL otherwise).
	 
 2.  To start Wrangler, type: `M-x erlang-wrangler-on', or
     alternatively `Ctrl-c Ctrl-r'. After that, new menu <em>Wrangler</em>   
	 should appear in the menu bar (see INSTALL otherwise).
	 
 3.  To stop Wrangler, type `M-x erlang-wrangler-off', or
     alternatively use `Ctrl-c Ctrl-r' again. The Refactor and Inspector
     menus will disappear from the menu bar.
	 
 4.  You can use `Ctrl-c Ctrl-r' to toggle Wrangler on or off.
 
 5.  For most refactorings, the editor interface will pass the current
     filename (should be the module name, as well), the refactoring command,
     and the current cursor position. For some refactorings, you'll also
     need to highlight an expression, or enter a new name.
	 
 6.  The 'Customize Wrangler' menu in the Wrangler submenu allows you
     to specify the boundary of the system by giving the list of directories
     to search for Erlang source files/header files that could be affected
     by a refactoring.
	 
 7.  For most refactorings, the editor interface will pass the current 
     filename (should be the module name, as well), the refactoring command,
     and the current cursor position. For some refactorings, you'll also
     need to highlight an expression, or enter a new name. More details are 
     available from the documenation.
	 
