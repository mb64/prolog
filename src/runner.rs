//! Runner: the logic to control the unification engine

use crate::state::*;
use lasso::Spur;

pub trait Runner {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> Result<Command>;
}

pub struct Repl {
    pub interesting_vars: Vec<(Spur, VarId)>,
}

impl Runner for Repl {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> Result<Command> {
        println!("Solution:");
        for &(name, var) in &self.interesting_vars {
            println!(
                "   {} = {}",
                ctx.rodeo.resolve(&name),
                vars.show(var, &ctx.rodeo)
            );
        }
        // TODO: prompt user
        Ok(Command::KeepGoing)
    }
}
