//! Built-in Prolog operations

use lasso::Rodeo;
use std::collections::HashMap;

use crate::runner::*;
use crate::state::*;
use crate::unify::State;

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
        [x] => print!("{}", vars.show(x, &ctx.rodeo)),
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
        [x] => println!("{}", vars.show(x, &ctx.rodeo)),
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

    log::trace!("trying goal {}", vars.dbg(arg, &ctx.rodeo));
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
        vars.dbg(a, &ctx.rodeo),
        vars.dbg(b, &ctx.rodeo)
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

pub fn builtins(rodeo: &mut Rodeo) -> HashMap<RelId, Relation> {
    [
        ("'='", 2, unify as Builtin),
        ("'\\='", 2, not_unify as Builtin),
        ("fail", 0, fail as Builtin),
        ("not", 1, not as Builtin),
        ("'\\+'", 1, not as Builtin),
        ("print", 1, print as Builtin),
        ("write", 1, print as Builtin),
        ("println", 1, println as Builtin),
        ("nl", 0, nl as Builtin),
        // TODO more
    ]
    .iter()
    .map(|&(name, arity, action)| {
        (
            RelId {
                name: rodeo.get_or_intern(name),
                arity,
            },
            Relation::Builtin(action),
        )
    })
    .collect()
}
