//! WIP

#![allow(mutable_borrow_reservation_conflict)]

use lasso::Rodeo;
use rustyline::Editor;

pub mod builtins;
pub mod parser;
pub mod runner;
pub mod state;
pub mod unify;

use parser::ReplItem;
use runner::Runner;
use state::{Command, Context, VarTable, VarTableBase};
use unify::State;

fn process_query<R: Runner>(
    ast: parser::Expr,
    ctx: &Context,
    base_map: &VarTableBase,
    runner: &mut R,
) {
    let mut vars = VarTable::new(&base_map);

    let (query, mut runner) = runner::from_question(&ast, runner, &mut vars);
    log::debug!("{}", runner.dbg(&ctx.rodeo));

    let mut state = State {
        ctx,
        vars: &mut vars,
        runner: &mut runner,
    };

    match state.solve(query) {
        Ok(Command::KeepGoing) => println!("\nNo."),
        Ok(Command::Stop) => println!("\nYes."),
        Err(e) => println!("Error: {:?}", e),
    }
}

fn repl(ctx: &mut Context) {
    let mut rl = Editor::<()>::new();

    let base_map = VarTableBase::new();

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
                process_query(ast, &ctx, &base_map, &mut rl);
            }
            None => (),
        }
    }

    println!("\nBye!");
}

fn load_file(filename: &str, ctx: &mut Context) {
    let contents = std::fs::read_to_string(filename).unwrap();
    if let Some(ast) = parser::parse(filename, &contents, &mut ctx.rodeo) {
        for ast_clause in ast {
            if let Err(e) = ctx.add_ast_clause(ast_clause) {
                println!("Error: {}", e);
            }
        }
    }
}

fn main() {
    pretty_env_logger::init();

    let mut ctx = builtins::builtins(Rodeo::default());

    repl(&mut ctx);
}
