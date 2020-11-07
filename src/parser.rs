//! Lexer and parser

// TODO: convert AST to Relations, parse queries, wildcards, better error handling

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use itertools::Itertools;
use lalrpop_util::{lalrpop_mod, ParseError};
use lasso::{Rodeo, Spur};
use logos::Logos;
use std::iter::Iterator;
use std::ops::Range;

lalrpop_mod!(parser);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Logos)]
pub enum Tok<'a> {
    #[error]
    #[regex(r"[ \t\f]+", logos::skip)]
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

    #[token(":-")]
    Turnstile,

    #[token("_")]
    Wildcard,

    #[regex(r"[a-z][a-zA-Z_]*")]
    Functor(&'a str),
    #[regex(r"[A-Z][a-zA-Z_]*")]
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

/// A single clause for some functor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Clause {
    functor: Spur,
    args: Vec<Expr>,
    conditions: Vec<Expr>,
}

/// Either a functor or a variable
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(Spur),
    Functor { name: Spur, args: Vec<Expr> },
}

impl Expr {
    /// The empty list
    pub fn nil(rodeo: &mut Rodeo) -> Self {
        Self::Functor {
            name: rodeo.get_or_intern("$nil"),
            args: vec![],
        }
    }

    /// List `cons`
    pub fn cons(head: Self, tail: Self, rodeo: &mut Rodeo) -> Self {
        Self::Functor {
            name: rodeo.get_or_intern("$cons"),
            args: vec![head, tail],
        }
    }
}

pub fn parse(file_name: &str, input: &str, rodeo: &mut Rodeo) -> Option<Vec<Clause>> {
    use ParseError::*;
    let lexer = Lexer {
        lexer: Tok::lexer(input),
    };
    let err = match parser::ProgramParser::new().parse(input, rodeo, lexer) {
        Ok(r) => return Some(r),
        Err(e) => e,
    };
    // Print error
    let (label, notes) = match err {
        InvalidToken { location } => (
            Label::primary((), location..location).with_message("Invalid Token"),
            vec![],
        ),
        UnrecognizedEOF { location, expected } => (
            Label::primary((), location..location).with_message("Unexpeced EOF"),
            vec![format!("Expected one of {}", expected.iter().format(", "))],
        ),
        UnrecognizedToken {
            token: (l, _, r),
            expected,
        } => (
            Label::primary((), l..r).with_message("Unrecognized Token"),
            vec![format!("Expected one of {}", expected.iter().format(", "))],
        ),
        ExtraToken { token: (l, _, r) } => {
            (Label::primary((), l..r).with_message("Extra Token"), vec![])
        }
        User { .. } => panic!(),
    };
    let files = SimpleFile::new(file_name, input);
    let diagnostic = Diagnostic::error()
        .with_message("Parse error")
        .with_labels(vec![label])
        .with_notes(notes);

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();

    None
}
