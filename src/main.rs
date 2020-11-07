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
        for ast in parser::parse("stdin", &line.unwrap(), &mut rodeo)
            .into_iter()
            .flatten()
        {
            let clause = state::Clause::from_ast(&ast, &mut rodeo);
            println!("{:#?}", clause);
        }
    }
    // let base = ScopedMapBase::new();

    // let initial_state = State::new
}
