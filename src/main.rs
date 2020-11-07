//! WIP

#![allow(mutable_borrow_reservation_conflict)]

use lasso::Rodeo;
use rustyline::Editor;
use scoped_map::ScopedMapBase;

pub mod builtins;
pub mod parser;
pub mod runner;
pub mod state;
pub mod unify;

use parser::ReplItem;
use state::{Command, VarTable};
use unify::State;

fn main() {
    pretty_env_logger::init();
    let mut rl = Editor::<()>::new();

    let mut ctx = builtins::builtins(Rodeo::default());
    let base_map = ScopedMapBase::new();

    while let Ok(line) = rl.readline("> ") {
        rl.add_history_entry(&line);
        match parser::parse_repl(&line, &mut ctx.rodeo) {
            Some(ReplItem::Clauses(asts)) => {
                for ast in asts {
                    if let Err(e) = ctx.add_ast_clause(ast) {
                        println!("Error: {}", e);
                    }
                }
            }
            Some(ReplItem::Question(ast)) => {
                let mut vars = VarTable::new(&base_map);

                let (query, mut runner) = runner::from_question(&ast, &mut rl, &mut vars);
                log::debug!("{}", runner.dbg(&ctx.rodeo));

                let mut state = State {
                    ctx: &ctx,
                    vars: &mut vars,
                    runner: &mut runner,
                };
                match state.solve(query) {
                    Ok(Command::KeepGoing) => println!("\nNo."),
                    Ok(Command::Stop) => println!("\nYes."),
                    Err(e) => println!("Error: {:?}", e),
                }
            }
            None => (),
        }
    }

    println!("\nBye!");
}
