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
    vars_base: &VarTableBase,
    runner: &mut R,
) {
    let mut vars = VarTable::new(&vars_base);

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
        // TODO: configurable error display style
        Err(e) => e.report(ctx, codespan_reporting::term::DisplayStyle::Short),
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

fn load_file(filename: String, ctx: &mut Context) {
    let contents = std::fs::read_to_string(&filename).unwrap();
    if let Some(ast) = parser::parse(ctx, filename, contents) {
        for ast_clause in ast {
            if let Err(e) = ctx.add_ast_clause(ast_clause) {
                println!("Error: {}", e);
            }
        }
    }
}

fn main() {
    pretty_env_logger::init();

    let mut rodeo = Rodeo::default();
    let rels = builtins::builtins(&mut rodeo);
    let files = codespan_reporting::files::SimpleFiles::new();

    let mut ctx = Context { rodeo, rels, files };

    // load_file("test.pl".to_string(), &mut ctx);

    repl(&mut ctx);
}
