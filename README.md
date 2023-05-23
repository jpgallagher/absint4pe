# Generating linear clauses from big-step semantics

## General requirements - Ciao Prolog with Ciao bundle logen and chclibs. 

*  ciao get github.com/jfmc/logen
*  ciao get github.com/bishoksan/chclibs

## Compile the following files with ciaoc.
=======
absint.pl.ann is the version of absint.pl annotated for specialisation with Logen.

*  renamePreds.pl, clp2logen.pl

## Files for semantics based translation

*  linearSolve.pl - linear interpreter for Horn clauses 
*  linearSolve.pl.ann - Logen-annotated version of interpreter
*  renamePreds.pl - applies the renaming to the Logen output
*  big2small.sh - script takes file of input clauses and entry goal. Generates linear clauses file 

Arguments can be eliminated from the output of big2small.sh using the elimargs.sh script (see bigstep)
=======
## Dependencies

1. [Ciao bindings](https://github.com/jfmc/ciao_yices) for
   [Yices SMT solver](https://yices.csl.sri.com/)
   (`ciao get github.com/jfmc/ciao_yices`)
2. [CHCLibs](https://github.com/bishoksan/chclibs)
   (`ciao get github.com/bishoksan/chclibs`)

## Specialisation

The interpreter is designed to be specialised with [Logen](https://github.com/jfmc/logen).
