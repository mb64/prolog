//! WIP

#![allow(mutable_borrow_reservation_conflict)]

use lasso::Rodeo;
use scoped_map::ScopedMapBase;
use std::io::{self, prelude::*};

pub mod parser;
pub mod runner;
pub mod state;
pub mod unify;

fn main() {
    let mut rodeo = Rodeo::default();

    for line in io::stdin().lock().lines() {
        println!("{:#?}", parser::parse("stdin", &line.unwrap(), &mut rodeo));
    }
    // let base = ScopedMapBase::new();

    // let initial_state = State::new
}
