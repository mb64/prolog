//! Runner: the logic to control the unification engine

use crate::state::*;
use lasso::{Rodeo, Spur};

pub trait Runner {
    fn solution(&mut self, rels: &Relations, vars: &mut VarTable<'_>) -> Result<Command>;
}

pub struct Repl {
    pub rodeo: Rodeo,
    pub interesting_vars: Vec<(Spur, VarId)>,
}

impl Runner for Repl {
    fn solution(&mut self, _rels: &Relations, vars: &mut VarTable<'_>) -> Result<Command> {
        println!("Solution:");
        for &(name, var) in &self.interesting_vars {
            println!(
                "   {} = {}",
                self.rodeo.resolve(&name),
                vars.show(var, &self.rodeo)
            );
        }
        // TODO: prompt user
        Ok(Command::KeepGoing)
    }
}
