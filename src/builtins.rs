//! Built-in Prolog operations

use lasso::{Rodeo, Spur};
use std::collections::HashMap;
use std::convert::TryInto;

use crate::context::*;
use crate::runner::*;
use crate::unify::State;
use crate::vars::*;

/// Part of the `Context`, it stores the `Spur`s associated with a bunch of built-in functors
pub struct Builtins {
    pub cons: Spur,
    pub nil: Spur,
    pub add: Spur,
    pub sub: Spur,
    pub mul: Spur,
    pub div: Spur,
}

impl Builtins {
    pub fn new(rodeo: &mut Rodeo) -> Self {
        Self {
            cons: rodeo.get_or_intern_static("."),
            nil: rodeo.get_or_intern_static("[]"),
            add: rodeo.get_or_intern_static("+"),
            sub: rodeo.get_or_intern_static("-"),
            mul: rodeo.get_or_intern_static("*"),
            div: rodeo.get_or_intern_static("/"),
        }
    }
}

type Builtin = fn(&Context, &mut VarTable<'_>, &[VarId], &mut dyn Runner) -> SolverResult;

/// `fail` builtin -- immediately backtracks
fn fail(
    _ctx: &Context,
    _vars: &mut VarTable<'_>,
    _args: &[VarId],
    _runner: &mut dyn Runner,
) -> SolverResult {
    Ok(Command::KeepGoing)
}

/// `print` builtin -- prints its argument
fn print(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    match *args {
        [x] => print!("{}", vars.show(x, ctx)),
        _ => panic!("Wrong number of arguments"),
    }
    runner.solution(ctx, vars)
}

/// `println` builtin -- prints its argument, followed by a newline
fn println(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    match *args {
        [x] => println!("{}", vars.show(x, ctx)),
        _ => panic!("Wrong number of arguments"),
    }
    runner.solution(ctx, vars)
}

/// `nl` builtin -- prints a newline
fn nl(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    match *args {
        [] => println!(""),
        _ => panic!("Wrong number of arguments"),
    }
    runner.solution(ctx, vars)
}

/// `=` builtin -- unifies its arguments
fn unify(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    let (a, b) = match *args {
        [a, b] => (a, b),
        _ => panic!("Wrong number of arguments"),
    };
    State { ctx, vars, runner }.unify(a, b)
}

/// `not`/1 builtin, aka `\+` -- fails if its goal can be met
fn not(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    let arg = match *args {
        [arg] => arg,
        _ => panic!("Wrong number of arguments"),
    };

    log::trace!("trying goal {}", vars.dbg(arg, ctx));
    let mut new_vars = vars.backtrackable();
    let mut state = State {
        ctx,
        vars: &mut new_vars,
        runner: &mut OneSoln,
    };
    match state.solve(arg)? {
        // Stop requested, it must have reached a solution
        // Fail
        Command::Stop => {
            log::trace!("subgoal succeeded, so not(subgoal) fails");
            Ok(Command::KeepGoing)
        }
        // Did not reach a solution -- successfully unsolvable
        Command::KeepGoing => {
            drop(new_vars);
            log::trace!("subgoal failed, so not(subgoal) succeeds");
            runner.solution(ctx, vars)
        }
    }
}

/// `'\='/2` -- fails if its args can be unified
fn not_unify(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    let (a, b) = match *args {
        [a, b] => (a, b),
        _ => panic!("Wrong number of arguments"),
    };

    log::trace!(
        "trying to unify {} and {}",
        vars.dbg(a, ctx),
        vars.dbg(b, ctx)
    );
    let mut new_vars = vars.backtrackable();
    let mut state = State {
        ctx,
        vars: &mut new_vars,
        runner: &mut OneSoln,
    };
    match state.unify(a, b)? {
        // Stop requested, it must have reached a solution
        // Fail
        Command::Stop => {
            log::trace!("subgoal succeeded, so not(subgoal) fails");
            Ok(Command::KeepGoing)
        }
        // Did not reach a solution -- successfully unsolvable
        Command::KeepGoing => {
            drop(new_vars);
            log::trace!("subgoal failed, so not(subgoal) succeeds");
            runner.solution(ctx, vars)
        }
    }
}

/// A helper for `is`
fn compute(ctx: &Context, vars: &mut VarTable<'_>, var: VarId) -> SolverResult<i64> {
    match vars.lookup(var) {
        Item::Unresolved => Err("Can't compute: contains uninstantiated variable(s)".into()),
        Item::Var(_) => panic!("lookup {} returned var", var),
        Item::Number(x) => Ok(x),
        Item::Functor { name, args } if name == ctx.builtins.add => {
            let mut sum = 0;
            for &arg in args {
                sum += compute(ctx, vars, arg)?;
            }
            Ok(sum)
        }
        Item::Functor { name, args } if name == ctx.builtins.sub => match *args {
            [x] => Ok(-compute(ctx, vars, x)?),
            [x, y] => Ok(compute(ctx, vars, x)? - compute(ctx, vars, y)?),
            _ => Err(format!(
                "Can't compute: wrong number of arguments for '-' ({})",
                args.len()
            )
            .into()),
        },
        Item::Functor { name, args } if name == ctx.builtins.mul => {
            let mut prod = 0;
            for &arg in args {
                prod *= compute(ctx, vars, arg)?;
            }
            Ok(prod)
        }
        Item::Functor { name, args } if name == ctx.builtins.div => match *args {
            [x, y] => Ok(compute(ctx, vars, x)? / compute(ctx, vars, y)?),
            _ => Err(format!(
                "Can't compute: wrong number of arguments for '/' ({})",
                args.len()
            )
            .into()),
        },
        Item::Functor { name, args } => Err(format!(
            "Can't compute: unknown operator {}/{}",
            ctx.rodeo.resolve(&name),
            args.len()
        )
        .into()),
    }
}

/// `is/2` builtin: `A is B` performs computation on B, then unifies it with A
fn is(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    let (var, eqn) = match *args {
        [var, eqn] => (var, eqn),
        _ => panic!("Wrong number of arguments"),
    };

    let result = compute(ctx, vars, eqn)?;

    State { ctx, vars, runner }.unify_with_known(var, Item::Number(result))
}

/// `<`/2 -- `x < y` succeeds if `x` is less than `y`
fn less_than(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    let (lhs, rhs) = match *args {
        [lhs, rhs] => (lhs, rhs),
        _ => panic!("Wrong number of arguments"),
    };

    if let (Item::Number(x), Item::Number(y)) = (vars.lookup(lhs), vars.lookup(rhs)) {
        if x < y {
            // Success!
            runner.solution(ctx, vars)
        } else {
            // Nope
            Ok(Command::KeepGoing)
        }
    } else {
        Err("</2: args are not numbers".into()) // TODO better error message
    }
}

/// `>`/2 -- `x > y` succeeds if `x` is greater than `y`
fn greater_than(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    let (lhs, rhs) = match *args {
        [lhs, rhs] => (lhs, rhs),
        _ => panic!("Wrong number of arguments"),
    };

    if let (Item::Number(x), Item::Number(y)) = (vars.lookup(lhs), vars.lookup(rhs)) {
        if x > y {
            // Success!
            runner.solution(ctx, vars)
        } else {
            // Nope
            Ok(Command::KeepGoing)
        }
    } else {
        Err(">/2: args are not numbers".into()) // TODO better error message
    }
}

/// `cpu_time/1` builtin: `cpu_time(X)` unifies `X` with the CPU time since the start of the
/// program, in milliseconds
fn cpu_time(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    let arg = match *args {
        [arg] => arg,
        _ => panic!("Wrong number of arguments"),
    };

    let result = cpu_time::ThreadTime::now()
        .as_duration()
        .as_millis()
        .try_into()
        .unwrap();

    State { ctx, vars, runner }.unify_with_known(arg, Item::Number(result))
}

/// `call/n` -- add some extra args to a functor, then call it as the goal
fn call(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    if args.len() == 0 {
        panic!("Wrong number of arguments");
    }

    let extra = &args[1..];
    match vars.lookup(args[0]) {
        Item::Functor {
            name,
            args: ref orig,
        } => {
            let new =
                vars.new_var_of_functor(name, orig.iter().copied().chain(extra.iter().copied()));

            State { ctx, vars, runner }.solve(new)
        }
        _ => Err("call: type error: not a functor".into()),
    }
}

/// `functor/3` -- give the name and arity of a functor
fn functor(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: &[VarId],
    runner: &mut dyn Runner,
) -> SolverResult {
    let (x, f, n) = match *args {
        [x, f, n] => (x, f, n),
        _ => panic!("Wrong number of arguments"),
    };

    match vars.lookup_with_varid(x) {
        (_, Item::Var(_)) => panic!("got var from lookup"),
        (_, Item::Number(_)) => {
            log::trace!("functor/3: a number, not a functor");
            Ok(Command::KeepGoing)
        }
        (_, Item::Functor { name, args }) => {
            // Unify f with name, and n with args.len()

            // Runner to unify n with args.len()
            struct Temp<'a> {
                n: VarId,
                len: usize,
                base: &'a mut dyn Runner,
            }
            impl Runner for Temp<'_> {
                fn solution(&mut self, ctx: &Context, vars: &mut VarTable) -> SolverResult {
                    State {
                        ctx,
                        vars,
                        runner: self.base,
                    }
                    .unify_with_known(self.n, Item::Number(self.len as i64))
                }
            }

            State {
                ctx,
                vars,
                runner: &mut Temp {
                    n,
                    len: args.len(),
                    base: runner,
                },
            }
            .unify_with_known(f, Item::Functor { name, args: &[] })
        }
        (vx, Item::Unresolved) => {
            // Make sure f and n are cool, then allocate new vars for args and resolve vx to
            // Functor { f, args }
            match (vars.lookup(f), vars.lookup(n)) {
                (Item::Functor { name, args: &[] }, Item::Number(arity)) if arity >= 0 => {
                    vars.update_to_functor(vx, name, arity as usize);
                    runner.solution(ctx, vars)
                }
                _ => {
                    // TODO: Different error depending on what it is
                    // (instantiation error, type error)
                    Err("functor/3: bad arguments".into())
                }
            }
        }
    }
}

pub fn builtins(rodeo: &mut Rodeo) -> HashMap<RelId, Relation> {
    [
        ("=", 2, unify as Builtin),
        ("\\=", 2, not_unify as Builtin),
        ("fail", 0, fail as Builtin),
        ("not", 1, not as Builtin),
        ("\\+", 1, not as Builtin),
        ("is", 2, is as Builtin),
        ("print", 1, print as Builtin),
        ("write", 1, print as Builtin),
        ("println", 1, println as Builtin),
        ("nl", 0, nl as Builtin),
        ("<", 2, less_than as Builtin),
        (">", 2, greater_than as Builtin),
        ("cpu_time", 1, cpu_time as Builtin),
        ("functor", 3, functor as Builtin),
        // call takes any number of arguments, but unfortunately there's no great way to express
        // that rn
        ("call", 2, call as Builtin),
        ("call", 3, call as Builtin),
        ("call", 4, call as Builtin),
        ("call", 5, call as Builtin),
    ]
    .iter()
    .map(|&(name, arity, action)| {
        (
            RelId {
                name: rodeo.get_or_intern_static(name),
                arity,
            },
            Relation::Builtin(action),
        )
    })
    .collect()
}
