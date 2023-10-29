# Wrangler

<img align="right"  width="200px" style="vertical-align:middle;margin:0px 20px" src="wrangler.png">

Wrangler is an interactive refactoring tool for Erlang, integrated into Visual Studio Code and other LSP-compliant editors as the 
[Wrangler language server](wls/wrangler_language_server), as well as emacs and Eclipse.

The refactorings in Wrangler cover structural changes such as function, variable and module renaming, function extraction and generalisation. Wrangler recognises macros in code, and can be used on a single file or across a whole project.

Wrangler can also be used to locate and remove code clones, and to improve the module structure of projects. Wrangler is extensible, with an API for writing new refactorings and a DSL for scripting complex refactoring combinations.

Wrangler also supports testing in EUnit, QuickCheck and Common Test, so your tests are refactored automatically when you refactor your code.

We have written a paper on [Refactoring tools for functional languages](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/refactoring-tools-for-functional-languages/F78282D0AE831BD11AD5531826892139), which covers a lot of the background to the development of Wrangler and refactoring for Erlang. Our other publications about Wrangler are listed here.

### Acknowledgements

The development of Wrangler has been supported by UK Research and Innovation, the 7th Framework Programme of the European Union, and the Erlang Ecosystem Foundation.

## Getting Started with Wrangler

Wrangler for unix (Mac OS X and linux), which uses emacs as a front end, is available from [github](https://github.com/RefactoringTools/Wrangler). 

Wrangler [installer for windows](https://github.com/RefactoringTools/wrangler/releases/download/wrangler1.2/Wrangler_Setup.exe).

Wrangler can be used within Eclipse alongside [Erlide](https://erlide.org/index.html), the Erlang plugin.

Details about installing and getting started with Wrangler are [here](getting-started.html).

## Documentation

[Online Documentation](doc/index.html).

## Videos

- [Getting started with Wrangler](http://www.youtube.com/watch?v=TsiZR9I22VY) 
- [Basic refactorings in Wrangler](http://www.youtube.com/watch?v=3GAN69shGLk)
- [Structural refactorings in Wrangler](http://www.youtube.com/watch?v=NURUuTQ9NoA)
- [Refactoring and Clone Detection in Wrangler](http://www.youtube.com/watch?v=RMYwv2daTVg)
- [InfoQ interview with Huiqing and Simon on refactoring](http://www.infoq.com/interviews/thompson-li-refactoring) 
- [DIY Refactoring with Wrangler](http://www.cs.kent.ac.uk/people/staff/sjt/presentations/TutorialNov11.ppt): a presentation at the 2011 Erlang User Conference (EUC).
- [Let's make refactoring tools user-extensible!](http://www.cs.kent.ac.uk/people/staff/sjt/Release/FunInTheAfternoon.pdf) Fun in the Afternoon, Brighton, Nov 2012. 
- Evolving your projects with Wrangler: EUC 2014, [slides](http://www.cs.kent.ac.uk/people/staff/sjt/presentations/EvolvingYourProjectsWithWrangler.pdf) [video](http://vimeo.com/100523695).
- [Evolving projects to concurrency with Wrangler](http://www.infoq.com/presentations/wrangler), Erlang Factory 2015.

## Tutorial from EUC 2010

- Slides: [PPT](http://www.cs.kent.ac.uk/projects/wrangler/Misc/TutorialNov10.ppt) [PDF](http://www.cs.kent.ac.uk/projects/wrangler/Misc/TutorialNov10.pdf)
- Exercises: [DOC](http://www.cs.kent.ac.uk/projects/wrangler/Misc/WranglerExercise.doc) [PDF](http://www.cs.kent.ac.uk/projects/wrangler/Misc/WranglerExercise.pdf)
- Example code: [gzip](http://www.cs.kent.ac.uk/projects/wrangler/Misc/wrangler_ex.tar.gz)

## Links

- [Publications](publications.html)
- [The RELEASE project](http://www.release-project.eu/) 
- [HaRe: the Haskell refactorer](https://github.com/RefactoringTools/HaRe)
- [Simon Thompson](http://www.cs.kent.ac.uk/people/staff/sjt/) 



