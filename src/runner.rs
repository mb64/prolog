//! Runner: the logic to control the unification engine

use crate::parser::Expr;
use crate::state::*;
use lasso::Spur;
use std::collections::HashMap;

pub trait Runner {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> Result<Command>;
}

pub struct Repl {
    interesting_vars: HashMap<Spur, VarId>,
}

impl Runner for Repl {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> Result<Command> {
        println!("\nSolution:");
        for (&name, &var) in &self.interesting_vars {
            println!(
                "   {} = {}",
                ctx.rodeo.resolve(&name),
                vars.show(var, &ctx.rodeo)
            );
        }
        // TODO: prompt user
        Ok(Command::Stop)
    }
}

fn reify_ast<'a>(ast: &Expr, vars: &mut VarTable<'a>, my_vars: &mut HashMap<Spur, VarId>) -> VarId {
    match *ast {
        Expr::Var(name) => *my_vars.entry(name).or_insert_with(|| vars.new_var()),
        Expr::Functor { name, ref args } => {
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
pub fn from_question<'a>(q: &Expr, vars: &mut VarTable<'a>) -> (VarId, Repl) {
    let mut interesting = HashMap::new();
    let res = reify_ast(q, vars, &mut interesting);
    (
        res,
        Repl {
            interesting_vars: interesting,
        },
    )
}
