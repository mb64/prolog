//! Lexer and parser

// TODO: convert AST to Context, parse queries, wildcards, better error handling

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use itertools::Itertools;
use lalrpop_util::{lalrpop_mod, ParseError};
use lasso::{Rodeo, Spur};
use logos::Logos;
use std::convert::TryInto;
use std::iter::Iterator;
use std::ops::Range;

use crate::state::Context;

lalrpop_mod!(parser);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Logos)]
pub enum Tok<'a> {
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("|")]
    Bar,

    #[token("=")]
    Equals,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token("?")]
    Question,

    #[token(":-")]
    Turnstile,

    #[token("_")]
    Wildcard,

    #[regex(r"[a-z][a-zA-Z0-9_]*")]
    Functor(&'a str),
    #[regex(r"[A-Z_][a-zA-Z0-9_]*")]
    Variable(&'a str),
}

struct Lexer<'input> {
    lexer: logos::Lexer<'input, Tok<'input>>,
}

impl<'input> Iterator for Lexer<'input> {
    type Item = (usize, Tok<'input>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.lexer.next()?;
        let Range { start, end } = self.lexer.span();
        Some((start, tok, end))
    }
}

/// A range of input
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    /// A handle for use with `codespan_reporting`
    pub file_id: u32,
    pub start: usize,
    pub len: u32,
}

impl Span {
    pub fn new(file_id: u32, start: usize, end: usize) -> Self {
        Self {
            file_id,
            start,
            len: (end - start).try_into().unwrap(),
        }
    }
}

/// A single clause for some functor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Clause {
    pub span: Span,
    pub functor: Spur,
    pub args: Vec<Expr>,
    pub conditions: Vec<Expr>,
}

/// Either a functor or a variable
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// Wildcard variable: `_`
    Wildcard { span: Span },
    /// Actual variable: `A`
    Var { span: Span, name: Spur },
    /// Functor: `f(x, A)`
    Functor {
        span: Span,
        name: Spur,
        args: Vec<Expr>,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match *self {
            Expr::Wildcard { span } => span,
            Expr::Var { span, .. } => span,
            Expr::Functor { span, .. } => span,
        }
    }
}

/// An input from the REPL
/// Unlike a typical Prolog REPL, more like Makam's
pub enum ReplItem {
    Clauses(Vec<Clause>),
    Question(Expr),
}

impl Expr {
    /// The empty list
    pub fn nil(span: Span, rodeo: &mut Rodeo) -> Self {
        Self::Functor {
            span,
            name: rodeo.get_or_intern("$nil"),
            args: vec![],
        }
    }

    /// List `cons`
    pub fn cons(head: Self, tail: Self, span: Span, rodeo: &mut Rodeo) -> Self {
        Self::Functor {
            name: rodeo.get_or_intern("$cons"),
            span,
            args: vec![head, tail],
        }
    }
}

pub fn parse(ctx: &mut Context, file_name: String, input: String) -> Option<Vec<Clause>> {
    let file_id = ctx.files.add(file_name, input);
    let source = ctx.files.get(file_id).unwrap().source();
    let lexer = Lexer {
        lexer: Tok::lexer(source),
    };
    parser::ProgramParser::new()
        .parse(source, &mut ctx.rodeo, file_id.try_into().unwrap(), lexer)
        .map_err(|e| print_err(&ctx.files, file_id, e))
        .ok()
}

pub fn parse_repl(ctx: &mut Context, input: String) -> Option<ReplItem> {
    let file_id = ctx.files.add("stdin".to_owned(), input);
    let source = ctx.files.get(file_id).unwrap().source();
    let lexer = Lexer {
        lexer: Tok::lexer(source),
    };
    parser::ReplItemParser::new()
        .parse(source, &mut ctx.rodeo, file_id.try_into().unwrap(), lexer)
        .map_err(|e| print_err(&ctx.files, file_id, e))
        .ok()
}

fn print_err(files: &SimpleFiles<String, String>, file_id: usize, err: ParseError<usize, Tok, ()>) {
    use ParseError::*;
    let (label, notes) = match err {
        InvalidToken { location } => (
            Label::primary(file_id, location..location).with_message("Invalid Token"),
            vec![],
        ),
        UnrecognizedEOF { location, expected } => (
            Label::primary(file_id, location..location).with_message("Unexpeced EOF"),
            vec![format!("Expected one of {}", expected.iter().format(", "))],
        ),
        UnrecognizedToken {
            token: (l, _, r),
            expected,
        } => (
            Label::primary(file_id, l..r).with_message("Unrecognized Token"),
            vec![format!("Expected one of {}", expected.iter().format(", "))],
        ),
        ExtraToken { token: (l, _, r) } => (
            Label::primary(file_id, l..r).with_message("Extra Token"),
            vec![],
        ),
        User { .. } => panic!(),
    };
    let diagnostic = Diagnostic::error()
        .with_message("Parse error")
        .with_labels(vec![label])
        .with_notes(notes);

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    codespan_reporting::term::emit(&mut writer.lock(), &config, files, &diagnostic).unwrap();
}
