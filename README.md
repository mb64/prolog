# A simple Prolog

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A simple Prolog implementation in Rust.

Example interaction:

```prolog
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.03s
     Running `target/debug/prolog`
> edge(x, y).
> edge(y, z).
> edge(y, w).
> 
> path(A, A).
> path(A, B) :- edge(A, X), path(X, B).
> 
> path(x, Place) ?

Solution:
   Place = x()
? ;

Solution:
   Place = y()
? ;

Solution:
   Place = z()
? ;

Solution:
   Place = w()
? 

Yes.
> 

Bye!
```

## TODO

More datatypes:
 - Numbers (only integers? or also floats?)
 - Strings
 - Atoms? what is an atom

More builtins:
 - Arithmetic
 - `is`
 - Negation: `not`
 - `cpu_time`, to run benchmarks
 - load clauses from file
 - Cuts: `!` (will probably require some restructuring)
 - standard library
 - IO

More usability:
 - Refactor core engine into library
 - Load file in REPL
 - Reset REPL
 - Command-line args to load files, run queries, optional REPL
 - Nicer lexer: allow for unicode, base-n literals, etc
 - Errors? Would be nice to have debug info on each clause, and be able to give
   pretty stacktraces on fatal errors
 - Debugging facilities?
 - Investigate alternatives to rustyline

More fancy datastructures:
 - Probably not
 - For `,` operator, figure out something better than CPS
 - Rewrite `scoped_map` to give a different interface: single map, mutable
   operations to push and pop scope
 - Build it on a disjoint-stack style typed arena, with unsafe reset operations
 - Bytecode VM? Almost certainly not
