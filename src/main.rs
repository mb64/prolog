//! WIP

#![allow(mutable_borrow_reservation_conflict)]

use lasso::Rodeo;
use scoped_map::ScopedMapBase;
use std::io::{self, prelude::*};

pub mod builtins;
pub mod parser;
pub mod runner;
pub mod state;
pub mod unify;

use parser::ReplItem;
use state::{Command, VarTable};
use unify::State;

fn main() {
    let mut ctx = builtins::builtins(Rodeo::default());
    let base_map = ScopedMapBase::new();

    for line in io::stdin().lock().lines() {
        match parser::parse_repl(&line.unwrap(), &mut ctx.rodeo) {
            Some(ReplItem::Clause(ast)) => {
                if let Err(e) = ctx.add_ast_clause(ast) {
                    println!("Error: {}", e);
                }
            }
            Some(ReplItem::Question(ast)) => {
                let mut vars = VarTable::new(&base_map);

                let (query, mut runner) = runner::from_question(&ast, &mut vars);

                let mut state = State {
                    ctx: &ctx,
                    vars: &mut vars,
                    runner: &mut runner,
                };
                match state.solve(query) {
                    Ok(Command::KeepGoing) => println!("No."),
                    Ok(Command::Stop) => println!("Yes."),
                    Err(e) => println!("Error: {:?}", e),
                }
            }
            None => (),
        }
    }
}
