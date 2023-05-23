# Generating linear clauses from big-step semantics

## General requirements - Ciao Prolog with Ciao bundle logen and chclibs. 

*  ciao get github.com/jfmc/logen
*  ciao get github.com/bishoksan/chclibs

## Compile the following files with ciaoc.

*  renamePreds.pl, clp2logen.pl

## Files for semantics based translation

*  linearSolve.pl - linear interpreter for Horn clauses 
*  linearSolve.pl.ann - Logen-annotated version of interpreter
*  renamePreds.pl - applies the renaming to the Logen output
*  big2small.sh - script takes file of input clauses and entry goal. Generates linear clauses file 

Arguments can be eliminated from the output of big2small.sh using the elimargs.sh script (see bigstep)
