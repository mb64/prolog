//! WIP

#![allow(mutable_borrow_reservation_conflict)]

use codespan_reporting::term::DisplayStyle;
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

fn process_query<R: Runner>(
    ast: Vec<parser::Expr>,
    ctx: &Context,
    vars_base: &VarTableBase,
    runner: &mut R,
) {
    let mut vars = VarTable::new(&vars_base);

    let result = runner::do_query(&ast[..], runner, ctx, &mut vars);

    // TODO: configurable error display style
    let style = DisplayStyle::Short;

    match result {
        Ok(Command::KeepGoing) => println!("\nNo."),
        Ok(Command::Stop) => println!("\nYes."),
        Err(e) => e.report(ctx, style),
    }
}

fn repl(ctx: &mut Context) {
    let mut rl = Editor::<()>::new();

    let base_map = VarTableBase::default();

    while let Ok(line) = rl.readline("> ") {
        rl.add_history_entry(&line);
        match parser::parse_repl(ctx, line) {
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
    if let Some(ast) = parser::parse(ctx, filename.to_owned(), contents) {
        for ast_clause in ast {
            if let Err(e) = ctx.add_ast_clause(ast_clause) {
                println!("Error: {}", e);
            }
        }
    }

    println!("Successfully loaded {}", filename);
}

fn main() {
    pretty_env_logger::init();

    let mut rodeo = Rodeo::default();
    let rels = builtins::builtins(&mut rodeo);
    let files = codespan_reporting::files::SimpleFiles::new();
    let builtins = builtins::Builtins::new(&mut rodeo);

    let mut ctx = Context {
        rodeo,
        rels,
        files,
        builtins,
    };

    // load_file("sudoku.pl", &mut ctx);

    repl(&mut ctx);
}
