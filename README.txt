
                    Wrangler, the Erlang Refactorer
                a snapshot of our current prototype
                             21/09/2008
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
Emacs. 

Wrangler is supposed to be installed as part of Distel, therefore this
snapshot includes both Distel and the current snapshot of Wrangler
(Wrangler-0.5). 

---------------------*** Please NOTE ***---------------------------
The Distel included in this snapshot is  Distel-4.0, which is
downloadable from distel.googlecode.com


--------------- how to build----------------

1) If you do not have Distel-4.0 (or cvs version from distel.googlecode.com)
  installed, then download the snapshot, follow the installation
  instructions in the INSTALL file to build the system.

2) If you already have Distel-4.0 (or cvs version from
   distel.googlecode.com) installed, then download the snapshot,
   copy the 'wrangler' directory and the Makefile.in file to your
   own distel directory, rebuild and install the software in the usual
   way:  
	 ./configure && make && [sudo] make install
    
--------------- how to use the refactorer ----------------

  1. Open an Erlang source file in the Emacs editor, you should have a
     submenu called 'Distel' in the Erlang menu. 

  2. To start the Erlang refactorer:

      M-x erlang-refactor-on  

     In the mini-buffer, you might be asked to input the Erlang node
     to connect to if such a connection has not been established. 

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
                                  ask whether you want to fold it.

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
                                  should have, and the minmum number of times  
                                  that a code fragment is duplicated. Wrangler 
                                  will search duplicated code fragments from the 
                                 directories specified by the search paths (see 
                                 Customize Wrangler).
	                        
     Rename a process           :place the cursor at the process name
                                 to be renamed, and you will be
                                 prompted for the new name.

     Register a process         :highlight the match expression whose
                                 right-hand side is the spawn
                                 expresssion that creates the process,
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


---------------------------Changes from Wrangler-0.4---------------

-- new refactorings:
    ++ Rename a process
    ++ From function to process
    ++ Register a process
    ++ Add a tag to the messages received by a server process
-- Infrasture change to make use of OTP behavioures.
-- Improved layout preservation.
-- Support for refactoring incomplete code.
-- A number of bug fixes

----------------------------Changes from Wrangler-0.3---------------

-- A number of bugs reported by dialyzer has been fixed.

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
  -- generalise a function definition: the new parameter name captured laterly  
     introduce variables with the same name.

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
    
   
