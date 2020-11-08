//! Runner: the logic to control the unification engine

use crate::parser::Expr;
use crate::state::*;
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
        println!("\nSolution:");
        for (&name, &var) in &self.interesting_vars {
            println!(
                "   {} = {}",
                ctx.rodeo.resolve(&name),
                vars.show(var, &ctx.rodeo)
            );
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

fn reify_ast<'a>(ast: &Expr, vars: &mut VarTable<'a>, my_vars: &mut HashMap<Spur, VarId>) -> VarId {
    match *ast {
        Expr::Wildcard { .. } => vars.new_var(),
        Expr::Var { name, .. } => *my_vars.entry(name).or_insert_with(|| vars.new_var()),
        Expr::Functor { name, ref args, .. } => {
            let args = args
                .iter()
                .map(|a| reify_ast(a, vars, my_vars))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            vars.new_var_of(Item::Functor { name, args })
        }
    }
}

/// Returns `(var, runner)`.
///
/// Next, run `unify::State {..}.solve(var)`
pub fn from_question<'e, 'v, R>(
    q: &Expr,
    r: &'e mut R,
    vars: &mut VarTable<'v>,
) -> (VarId, Printing<'e, R>) {
    let mut interesting = HashMap::new();
    let res = reify_ast(q, vars, &mut interesting);
    (
        res,
        Printing {
            interesting_vars: interesting,
            base: r,
        },
    )
}
