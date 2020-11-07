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

fn main() {
    let mut rodeo = Rodeo::default();

    for line in io::stdin().lock().lines() {
        match parser::parse_repl(&line.unwrap(), &mut rodeo) {
            Some(parser::ReplItem::Clause(ast)) => {
                let clause = state::Clause::from_ast(&ast, &mut rodeo);
                println!("{:#?}", clause);
            }
            Some(parser::ReplItem::Question(ast)) => {
                println!("{:#?}", ast);
                todo!()
            }
            None => (),
        }
    }
    // let base = ScopedMapBase::new();

    // let initial_state = State::new
}
