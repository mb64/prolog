//! Runner: the logic to control the unification engine

use crate::context::*;
use crate::error::*;
use crate::parser::{Expr, Span};
use crate::unify::State;
use crate::vars::*;
use lasso::Spur;
use rustyline::Editor;
use std::collections::HashMap;

/// What to do next
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Command {
    KeepGoing,
    Stop,
}

/// `SolverResult` is returned by basically every function
pub type SolverResult<T = Command> = Result<T, Box<SolveError>>;

pub trait Runner {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> SolverResult;
}

pub struct Printing<'r> {
    base: &'r mut dyn Runner,
}

impl Runner for Printing<'_> {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> SolverResult {
        if ctx.var_names.len() == 0 {
            // No use in continuing -- no other solutions to be found
            return Ok(Command::Stop);
        }
        println!("\nSolution:");
        for (&name, &var) in &ctx.var_names {
            println!("   {} = {}", ctx.rodeo.resolve(&name), vars.show(var, ctx));
        }
        self.base.solution(ctx, vars)
    }
}

impl Runner for Editor<()> {
    fn solution(&mut self, _ctx: &Context, _vars: &mut VarTable<'_>) -> SolverResult {
        // TODO: prompt user in a better way
        let response = match self.readline("? ") {
            Ok(r) => r,
            Err(_) => return Ok(Command::Stop),
        };
        if response.trim() == ";" {
            Ok(Command::KeepGoing)
        } else {
            Ok(Command::Stop)
        }
    }
}

/// A Runner which goes through all solutions.
pub struct OneSoln;
/// A Runner which only asks for the first solution.
pub struct AllSolns;

impl Runner for AllSolns {
    fn solution(&mut self, _ctx: &Context, _vars: &mut VarTable<'_>) -> SolverResult {
        Ok(Command::KeepGoing)
    }
}
impl Runner for OneSoln {
    fn solution(&mut self, _ctx: &Context, _vars: &mut VarTable<'_>) -> SolverResult {
        Ok(Command::Stop)
    }
}

// Aaaa this is really shitty
// CPS without tail calls trashing the stack
// whatever, it's doesn't need to be fancy

pub struct All<'a> {
    pub items: &'a [(Span, VarId)],
    pub base: &'a mut dyn Runner,
}

impl<'a> Runner for All<'a> {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> SolverResult {
        match *self.items {
            [] => self.base.solution(ctx, vars),
            [(span, head)] => {
                log::trace!("solving last clause");
                let mut state = State {
                    ctx,
                    vars,
                    runner: self.base,
                };
                state.solve(head).map_err(|e| e.add_trace(span))
            }
            [(span, head), ref tail @ ..] => {
                log::trace!("solving next clause");
                let mut state = State {
                    ctx,
                    vars,
                    runner: &mut All {
                        items: tail,
                        base: self.base,
                    },
                };
                state.solve(head).map_err(|e| e.add_trace(span))
            }
        }
    }
}

fn reify_ast<'a>(ast: &Expr, vars: &mut VarTable<'a>, my_vars: &mut HashMap<Spur, VarId>) -> VarId {
    match *ast {
        Expr::Wildcard { .. } => vars.new_var(),
        Expr::Var { name, .. } => *my_vars.entry(name).or_insert_with(|| vars.new_var()),
        Expr::Number { value, .. } => vars.new_var_of(Item::Number(value)),
        Expr::Functor { name, ref args, .. } => {
            let args = args
                .iter()
                .map(|a| reify_ast(a, vars, my_vars))
                .collect::<Vec<_>>();
            vars.new_var_of_functor(name, args.into_iter())
        }
    }
}

pub fn do_query<'e, 'v, R: Runner>(
    q: &[Expr],
    r: &'e mut R,
    ctx: &mut Context,
    vars: &mut VarTable<'v>,
) -> SolverResult {
    let mut var_names = HashMap::new();
    let items = q
        .iter()
        .map(|e| (e.span(), reify_ast(e, vars, &mut var_names)))
        .collect::<Vec<_>>();

    ctx.var_names = var_names;

    let mut base = Printing { base: r };
    log::debug!("{}", ctx.dbg_var_names());

    All {
        items: &items[..],
        base: &mut base,
    }
    .solution(ctx, vars)
}
