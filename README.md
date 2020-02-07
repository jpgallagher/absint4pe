# absint4pe
Abstract interpreters designed to be specialised

The interpreter absint.pl is written in Ciao Prolog.
It implements an abstract interpreter for a property-based abstraction.

## Requirements

[Ciao](https://github.com/ciao-lang/ciao) 1.16 or newer (installed
from git repository with `./ciao-boot.sh local-install`)

## Dependencies

1. [Ciao bindings](https://github.com/jfmc/ciao_yices) for
   [Yices SMT solver](https://yices.csl.sri.com/)
   (`ciao get github.com/jfmc/ciao_yices`)
2. [CHCLibs](https://github.com/bishoksan/chclibs)
   (`ciao get github.com/bishoksan/chclibs`)
