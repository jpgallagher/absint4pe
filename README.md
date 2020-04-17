# absint4pe
Abstract interpreters designed to be specialised

## Files

The interpreter for Horn clauses absint.pl is written in Ciao Prolog.
It implements an abstract interpreter for a property-based abstraction.

absint.pl.ann is the version of absint.pl annotated for specialisation with Logen.

absint.sh is a shell script for running the specialiser with respect to a program and a set of properties.

## Requirements

[Ciao](https://github.com/ciao-lang/ciao) 1.16 or newer (installed
from git repository with `./ciao-boot.sh local-install`)

## Dependencies

1. [Ciao bindings](https://github.com/jfmc/ciao_yices) for
   [Yices SMT solver](https://yices.csl.sri.com/)
   (`ciao get github.com/jfmc/ciao_yices`)
2. [CHCLibs](https://github.com/bishoksan/chclibs)
   (`ciao get github.com/bishoksan/chclibs`)

## Specialisation

The interpreter is designed to be specialises with [Logen](https://github.com/jfmc/logen).
