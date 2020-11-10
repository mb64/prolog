//! Runner: the logic to control the unification engine

use crate::parser::{Expr, Span};
use crate::state::*;
use crate::unify::State;
use itertools::Itertools;
use lasso::{Rodeo, Spur};
use rustyline::Editor;
use std::collections::HashMap;

pub trait Runner {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> SolverResult;
}

pub struct Printing<'r, R> {
    interesting_vars: HashMap<Spur, VarId>,
    base: &'r mut R,
}

impl<R> Printing<'_, R> {
    pub fn dbg(&self, rodeo: &Rodeo) -> String {
        format!(
            "Top-level unification vars:\n   {}",
            self.interesting_vars
                .iter()
                .map(|(n, v)| format!("{} = {}", rodeo.resolve(n), v))
                .format("\n   ")
        )
    }
}

impl<R: Runner> Runner for Printing<'_, R> {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> SolverResult {
        if self.interesting_vars.len() == 0 {
            // No use in continuing -- no other solutions to be found
            return Ok(Command::Stop);
        }
        println!("\nSolution:");
        for (&name, &var) in &self.interesting_vars {
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
        Expr::String { ref value, .. } => {
            // SAFETY: we leak the string
            // FIXME: actually store strings somewhere
            let s = value.clone();
            let s_ref = unsafe { std::mem::transmute::<&str, &'static str>(&s) };
            std::mem::forget(s);
            vars.new_var_of(Item::String(s_ref))
        }
        Expr::Functor { name, ref args, .. } => {
            let args = args
                .iter()
                .map(|a| reify_ast(a, vars, my_vars))
                .collect::<Vec<_>>();
            vars.new_var_of_functor(name, args.into_iter())
        }
    }
}

///// Returns `(var, runner)`.
/////
///// Next, run `unify::State {..}.solve(var)`
//pub fn from_question<'e, 'v, R>(
//    q: &Expr,
//    r: &'e mut R,
//    vars: &mut VarTable<'v>,
//) -> (VarId, Printing<'e, R>) {
//    let mut interesting = HashMap::new();
//    let res = reify_ast(q, vars, &mut interesting);
//    (
//        res,
//        Printing {
//            interesting_vars: interesting,
//            base: r,
//        },
//    )
//}

pub fn do_query<'e, 'v, R: Runner>(
    q: &[Expr],
    r: &'e mut R,
    ctx: &Context,
    vars: &mut VarTable<'v>,
) -> SolverResult {
    let mut interesting = HashMap::new();
    let items = q
        .iter()
        .map(|e| (e.span(), reify_ast(e, vars, &mut interesting)))
        .collect::<Vec<_>>();

    let mut base = Printing {
        interesting_vars: interesting,
        base: r,
    };
    log::debug!("{}", base.dbg(&ctx.rodeo));

    All {
        items: &items[..],
        base: &mut base,
    }
    .solution(ctx, vars)
}
