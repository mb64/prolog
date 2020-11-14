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

use crate::context::Context;

lalrpop_mod!(parser);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Logos)]
pub enum Tok<'a> {
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"/\*[^*]*(\*[^/][^*]*)*\*/", logos::skip)]
    // The final \n is not included in the regex so that it can terminate at EOF too
    #[regex(r"%[^\n]*", logos::skip)]
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

    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token("?")]
    Question,
    #[token(":-")]
    Turnstile,

    // These are vaguely in order of precedence
    #[token("\\+")]
    WeirdNot,
    #[token("is")]
    Is,
    #[token("=")]
    Equals,
    #[token("\\=")]
    NotEquals,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("/")]
    Divide,

    #[token("_")]
    Wildcard,

    // TODO: figure out a nice way to parse literally everything without ambiguity
    // Ideally, should also have =(a, b) and is(a, b) work
    #[regex(r"[a-z][a-zA-Z0-9_]*")]
    #[regex(r"'[=*/+-]'")]
    Functor(&'a str),
    #[regex(r"[A-Z_][a-zA-Z0-9_]*")]
    Variable(&'a str),

    // TODO: figure out a smarter way to have signed literals
    // Might need whitespace sensitivity
    //
    // Item | SWI Prolog | GNU prolog
    // -----+------------+-----------
    // -(5) | '-'(5)     | '-'(5)
    // - 5  | '-'(5)     | -5        (Different!)
    //  -5  | -5         | -5
    // 0-5  | '-'(0, 5)  | '-'(0, 5)
    #[regex(r"[0-9][0-9_]*", |lex| lex.slice().parse())]
    Number(i64),

    /// No escape sequences escaped
    // TODO: change to Box<str>, and give a legit lexing function that can return an error
    // then plum the errors thru the parser so they can be reported nicely
    #[regex(r"'[^']*(\\'[^']*)*'", |lex| &lex.slice()[1..lex.slice().len()-1])]
    String(&'a str),
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
    /// Number
    Number { span: Span, value: i64 },
    /// String
    String { span: Span, value: String },
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
            Expr::Number { span, .. } => span,
            Expr::String { span, .. } => span,
            Expr::Functor { span, .. } => span,
        }
    }

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

/// An input from the REPL
/// Unlike a typical Prolog REPL, more like Makam's
/// TODO: change it to be more prolog-y?
pub enum ReplItem {
    Clauses(Vec<Clause>),
    Question(Vec<Expr>),
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
