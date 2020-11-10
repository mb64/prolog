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
   Place = x
? ;

Solution:
   Place = y
? ;

Solution:
   Place = z
? ;

Solution:
   Place = w
? 

Yes.
> 

Bye!
```

## TODO

More datatypes:
 - ~~Integers~~ Done!
 - ~~Strings~~ Done-ish
 - Maybe floats, and maybe integers with constraints? (Would be difficult)

More builtins:
 - ~~Parse arithmetic operators~~ (but somehow keep negated literals as literals)
 - ~~`is`~~
 - `call`
 - ~~Negation: `not`, `\=`, `\+`~~
 - ~~`cpu_time`, to run benchmarks~~
 - load clauses from file
 - Cuts: `!` (will probably require a new `Command` variant)
 - `->` and `*->` (soft cut)
 - standard library
 - IO

More usability:
 - Refactor core engine into library
 - Load file in REPL
 - Reset REPL
 - Command-line args to load files, run queries, optional REPL
 - Nicer lexer: allow unicode, base-n literals, etc
 - ~~Allow multiple goals in a query~~
 - ~~Print atoms as `x`, not `x()`~~
 - ~~Allow `_Variables`, and~~ generate warnings for singleton variables
    * Question -- should it report the found values of `_Variables`? SWI Prolog
      does but GNU Prolog doesn't
 - ~~Errors? Would be nice to have debug info on each clause, and be able to
   give pretty stacktraces on fatal errors~~ Done!
 - Maybe don't abuse `codespan_reporting` for stack traces
 - Debugging facilities?
 - Investigate alternatives to rustyline

More fancy datastructures:
 - Probably not
 - ~~Allocate functor arguments in an arena -- switch `Box<[VarId]>` to `&'arena [VarId]`~~ Done!
 - Rewrite `scoped_map` to give a different interface: single map, mutable
   operations to push and pop scope
 - For `,` operator, figure out something better than CPS
 - Build it on a disjoint-stack style typed arena, with unsafe reset operations
 - Bytecode VM? Almost certainly not

