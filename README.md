# A simple Prolog

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A simple Prolog implementation in Rust.

Example interaction:

```prolog
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.03s
     Running `target/debug/prolog`
> connected(x, y).
> connected(y, z).
> connected(y, w).
> 
> path(A, B) :- connected(A, B).
> path(A, B) :- connected(A, X), path(X, B).
> 
> path(x, Place) ?

Solution:
   Place = y()
? ;

Solution:
   Place = z()
? ;

Solution:
   Place = w()
? ;

No.
> 

Bye!
```
