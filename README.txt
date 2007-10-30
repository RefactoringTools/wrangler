
                    Wrangler, the Erlang Refactorer
                a snapshot of our current prototype
                             04/10/2007
              http://www.cs.kent.ac.uk/projects/forse/

--------------------------------------------------------------------------------

Wrangler is an Erlang refactoring tool that supports interactive
refactoring of Erlang programs.  The current snapshot of Wrangler 
supports a small number of basic Erlang refactorings, including 
renaming variable/function/module names and generalisation of a 
function definition.

Wrangler is embedded in the Emacs editing environment, and built on
top of Distel, an Emacs-based user interface toolkit for Erlang, to
manage to communication between the refactoring tool and
Emacs. 

Wrangler is supposed to be installed as part of Distel, therefore this
snapshot includes both and the current snapshot of Wrangler
(Wrangler-0.2). **** Please NOTE: the Distel included in this snapshot refers
to Distel-3.3, NOT the cvs version from distel.googlecode.com ****

Wrangler 0.2 is still a prototype, made available so you can play with 
basic refactoring support for Erlang, and give us feedback or bug 
reports.

WE DO NOT RECOMMEND TO USE THIS PROTOTYPE  ON YOUR PRODUCTION SOURCE
JUST YET!


--------------- how to build----------------

1) If you do not have Distel-3.3 installed, then download the snapshot,
   follow the installation instructions in the INSTALL file to build 
   the system.

2) If you already have Distel installed, then download the snapshot,
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
 
  4. for most refactorings, the editor interface will pass the
     current filename (should be the module name, as well), the
     refactoring command, and the current cursor position. For
     some refactorings, you'll also need to highlight an 
     expression, or enter a new name. Here's the current list:

     rename variable/function   : place cursor at the identifier to
                                  be renamed, you'll be prompted for a
                                  new name
     rename module              : place cursor at anywhere within the
                                  module, you'll be prompted for a new 
				  name
     Generalise definition       : highlight the expression on which
                                  the function is going to be
                                  generalised, you'll be prompted for
                                  a new parameter name.
     Move a function definition
     to another module          : place cursor at anywhere within the
                                  function definition, you'll be
                                  prompted for the target module name.

 5. The 'Customize' menu in the 'Refactor' submenu allows you specify 
 the boundary of the system by giving the list of directories to
 search for Erlang source files/header files that could be affected by a
 refactoring.

---------------------------Changes from wrangler-0.1------------------
-- new refactorings:
   ++ Move a function definition to another module.

-- bugfix   
  -- operators are now distinguished from expressions. 
  -- for renaming variable/function names, error messages are now
     disabled when the new name is the same as the old name. 
  -- More checkings have been added when appy/spawn/spawn_link is used.
  -- bug fix within side-effect calculation. 
  -- further checkings has been added to detect whether undicidablity really    
     occurs when 'apply' is used. 
  -- generalise a function definition: the new parameter name captured laterly  
     introduce variables with the same name.
