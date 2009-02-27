
                    Wrangler, the Erlang Refactorer
                a snapshot of our current prototype
                            15/01/2009 
              http://www.cs.kent.ac.uk/projects/forse/

--------------------------------------------------------------------------------

Wrangler is an Erlang refactoring tool that supports interactive
refactoring of Erlang programs.  The current snapshot of Wrangler 
supports a number of basic Erlang refactorings, including 
renaming variable/function/module names, generalisation of a 
function definition, function extraction, folding against a function 
definition, duplicated code detection, etc.

Wrangler is embedded in the Emacs editing environment, and built on
top of Distel, an Emacs-based user interface toolkit for Erlang, to
manage to communication between the refactoring tool and
Emacs. We are now also in the process of integrating Wrangler with 
the Eclipse environment.

When Emacs is used, Wrangler is supposed to be installed as part of
Distel, therefore this snapshot includes both Distel and the current 
snapshot of Wrangler (Wrangler 0.6.1). The Distel included in this snapshot 
is Distel-4.03, which is downloadable from distel.googlecode.com.


--------------------- how to build  ----------------------------

  See INSTALL or INSTALL.WIN32

--------------------- the directory structure -------------------

We try keep Distel's original directory structure.

  AUTHORS             <-  AUTHORS of Distel and Wrangler
  ChangeLog           <-  changeLog of Distel 
  INSTALL             <-  installation instructions of distel-wrangler
  INSTALL.WIN32       <-  installation instrunctions for Windows
  Makefile.in 
  NEWS                <-  News about Distel
  README-distel       <-  README file of Distel
  README-wrangler     <-  README file of Wrangler (you're reading this)
  README.ie-session   <-  README file of Distel's 'interactive Erlang'
  configure          
  doc                 <-  documentation from Distel
  ebin                <-  directory for Erlang beam files
  elisp               <-  elisp source code from Distel
  src                 <-  Erlang source code from Distel
  wrangler            <-  All the source code from Wrangler

--------------------- how to use the refactorer ----------------

  1. Open an Erlang source file in the Emacs editor, you should have a
     submenu called 'Distel' in the Erlang menu. 

  2. To start the Erlang refactorer:

      M-x erlang-refactor-on  

     In the mini-buffer, you might be asked to input the Erlang node
     to connect to if such a connection has not been established.
     So make sure that an Erlang node has been started before starting 
     the Erlang refactorer.

     After that, a submenu called 'Refactor' should appear in the
     Erlang menu.

  3. To stop the Erlang refactorer:
     
      M-x erlang-refactor-off 

      The 'Refactor' submenu will disappear from the Erlang menu.

  4. You could also use "Ctrl-c Ctrl-r" to toggle the refactorer on/off.
 
  5. For most refactorings, the editor interface will pass the
     current filename (should be the module name, as well), the
     refactoring command, and the current cursor position. For
     some refactorings, you'll also need to highlight an 
     expression, or enter a new name. Here's the current list:

     Rename variable/function   : place cursor at the identifier to
                                  be renamed, you'll be prompted for a
                                  new name.

     Rename module              : place cursor at anywhere within the
                                  module, you'll be prompted for a new 
				  name.

     Generalise definition       : highlight the expression on which
                                  the function is going to be
                                  generalised, you'll be prompted for
                                  a new parameter name.
     Move a function definition
     to another module          : place cursor at anywhere within the
                                  function definition, you'll be
                                  prompted for the target module name.

     Function extraction        : highlight the expression/expression
	                          sequence that you wish to extract,
	                          you'll be prompted for a new function
                                  name.
     Fold expression against 
     function                   : place cursor at anywhere at the function
                                  clause. Wrangler will guide you through 
	                          the possible candidates one by one, and 
                                  ask whether you want to fold it or not.

     Introduce new macro        : highlight the expression/pattern that you
                                  wish to replace with macro application, and
                                  you'll be prompted from a new macro name.

     Fold against macro def     : place cursor at anywhere at the function
                                  definition. Wrangler will direct you through
                                  the possible candidates one by one, and for 
                                  each candidate ask whether you want to fold
                                  it or not. 


     Tuple function arguments   : place cursor at the beginning of the
                                  parameter that is to be the first
                                  element of the tuple, and you will
                                  be prompted for the number of     
			          parameters put into a tuple.
  

     Expression Search          : highlight the expression/expression sequence
	                          you are interested, Wrangler will show you 
	                          the found expression/expression sequences one 
                                  by one. Press the 'enter' key to go the next
                                  one, and the 'Esc' key to quit.

     Detect duplicated code  
     in current buffer          : select the refactoring command from the 
                                  menu, you'll be prompted for the minimum 
	                          number of tokens a duplicated code fragment 
                                  should have, and the number of times that a 
                                  code fragment is duplicated.
                                  
     Detect duplicated code 
     in Dirs                    : select the refactoring command from the menu,
	                          and you will be prompted for the minimum 
                                  number of tokens a duplicated code fragment 
                                  should have, and the minimum number of times  
                                  that a code fragment is duplicated. Wrangler 
                                  will search duplicated code fragments from the 
                                 directories specified by the search paths (see 
                                 Customize Wrangler).
	                        
     Rename a process           :place the cursor at the process name
                                 to be renamed, and you will be
                                 prompted for the new name.

     Register a process         :highlight the match expression whose
                                 right-hand side is the spawn
                                 expression that creates the process,
                                 and left-hand side is the process
                                 identifier, and you will be prompted
                                 for the new process name.

     From fun to process        :place the cursor at the function name
                                 of the function definition to be
                                 refactored, and you will be prompted
                                 for the new process name.

    Add tag to messages         :place the cursor at the function
                                 whose body contains the receive
                                 expression of the server process, and
                                 you will be prompted for the tag name.
     

 6. The 'Customize Wrangler' menu in the 'Refactor' submenu allows you specify 
 the boundary of the system by giving the list of directories to
 search for Erlang source files/header files that could be affected by a
 refactoring.

---------------------------Changes from Wrangler-0.6-------------
-- 'Move a function to another module' now allows moving a function even 
    if the same function (with same syntax and semantics) is already 
    defined in the target module.
-- Bugfix with pretty-printing prefix expressions.
-- Bugfix with duplicated code detection.
-- Automatic inference of include directories when parsing an Erlang file.

---------------------------Changes from Wrangler-0.5--------------

-- new refactorings:
   ++ Introduce a macro
   ++ Fold against macro definition 
-- Faster duplicated code detection.
-- Improved layout preservation.
-- Added a number of code inspection functionalities.
-- A number of bug fixes.
     
---------------------------Changes from Wrangler-0.4---------------

-- new refactorings:
    ++ Rename a process
    ++ From function to process
    ++ Register a process
    ++ Add a tag to the messages received by a server process
-- Infrastructure change to make use of OTP behaviours.
-- Improved layout preservation.
-- Support for refactoring incomplete code.
-- A number of bug fixes

----------------------------Changes from Wrangler-0.3---------------

-- A number of bugs reported by Dialyzer have been fixed.

---------------------------Changes from Wrangler-0.2------------------

-- new refactorings:
   ++ Tuple function arguments.

-- A number of bugs have been fixed.


---------------------------Changes from wrangler-0.1------------------
-- new refactorings:
   ++ Move a function definition to another module.

-- bugfix   
  -- operators are now distinguished from expressions. 
  -- for renaming variable/function names, error messages are now
     disabled when the new name is the same as the old name. 
  -- More checking has been added when appy/spawn/spawn_link is used.
  -- bug fix within side-effect calculation. 
  -- further checking has been added to detect whether undicidablity really    
     occurs when 'apply' is used. 
  -- generalise a function definition: the new parameter name captured later  
     introduced variables with the same name.

---------------------------Changes from wrangler-0.2-------------------
-- new refactorings:
  ++ Function extraction.
  ++ Folding against a function definition.

-- Duplicated code detection.
  ++ Expression search.
  ++ Duplicated code search across multiple modules. 

-- bugfix
   -- With 'generalisation', Wrangler now asks the user in the case that it 
   cannot decide whether the selected expression has side effect or not.
   -- fixed the path problem with finding the side effect table.
   -- Wrangler now keeps two side effect tables, one for the libraries and one 
      for the user's own code. 
   -- rewrote the module graph calculation algorithm.
    
   
