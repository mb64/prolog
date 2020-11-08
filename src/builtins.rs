//! Built-in Prolog operations

use lasso::Rodeo;

use crate::runner::*;
use crate::state::*;
use crate::unify::State;

type Builtin = fn(&Context, &mut VarTable<'_>, Box<[VarId]>, &mut dyn Runner) -> Result<Command>;

/// `fail` builtin -- immediately backtracks
fn fail(
    _ctx: &Context,
    _vars: &mut VarTable<'_>,
    _args: Box<[VarId]>,
    _runner: &mut dyn Runner,
) -> Result<Command> {
    Ok(Command::KeepGoing)
}

/// `print` builtin -- prints its argument
fn print(
    ctx: &Context,
    vars: &mut VarTable<'_>,
    args: Box<[VarId]>,
    runner: &mut dyn Runner,
) -> Result<Command> {
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
    args: Box<[VarId]>,
    runner: &mut dyn Runner,
) -> Result<Command> {
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
    args: Box<[VarId]>,
    runner: &mut dyn Runner,
) -> Result<Command> {
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
    args: Box<[VarId]>,
    runner: &mut dyn Runner,
) -> Result<Command> {
    let (a, b) = match *args {
        [a, b] => (a, b),
        _ => panic!("Wrong number of arguments"),
    };
    State { ctx, vars, runner }.unify(a, b)
}

pub fn builtins(mut rodeo: Rodeo) -> Context {
    let rels = [
        ("=", 2, unify as Builtin),
        ("fail", 0, fail as Builtin),
        // print and write typically have different behavior, but this is non-standard anyways
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
    .collect();
    Context { rels, rodeo }
}
