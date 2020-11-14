//! The main datastructures

use codespan_reporting::files::SimpleFiles;
use itertools::Itertools;
use lasso::{Rodeo, Spur};
use std::collections::{hash_map::Entry, HashMap};
use unicode_segmentation::UnicodeSegmentation;

use crate::builtins::Builtins;
use crate::parser::{self, Span};
use crate::runner::{Runner, SolverResult};
use crate::vars::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelId {
    pub name: Spur,
    pub arity: u32,
}

pub struct Context {
    pub rels: HashMap<RelId, Relation>,
    pub rodeo: Rodeo,
    pub files: SimpleFiles<String, String>,
    pub builtins: Builtins,
    pub var_names: HashMap<Spur, VarId>,
}

impl Context {
    pub fn add_clause(&mut self, rel_id: RelId, clause: Clause) -> Result<(), &'static str> {
        let entry = self
            .rels
            .entry(rel_id)
            .or_insert(Relation::User(Vec::new()));

        match *entry {
            Relation::User(ref mut cs) => {
                cs.push(clause);
                Ok(())
            }
            // TODO: report as a full error, with its span
            Relation::Builtin(_) => Err("Cannot extend builtin relation"),
        }
    }

    pub fn add_ast_clause(&mut self, ast_clause: parser::Clause) -> Result<(), &'static str> {
        let rel_id = RelId {
            name: ast_clause.functor,
            arity: ast_clause.args.len() as u32,
        };
        let clause = Clause::from_ast(&ast_clause, &mut self.rodeo);
        self.add_clause(rel_id, clause)
    }

    pub fn dbg_var_names(&self) -> String {
        format!(
            "Top-level unification vars:\n   {}",
            self.var_names
                .iter()
                .map(|(n, v)| format!("{} = {}", self.rodeo.resolve(n), v))
                .format("\n   ")
        )
    }
}

#[derive(Clone)]
pub enum Relation {
    Builtin(fn(&Context, &mut VarTable<'_>, &[VarId], &mut dyn Runner) -> SolverResult),
    User(Vec<Clause>),
}

/// A local variable
// Negative number is arguments, positive number is local var
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local(pub(crate) i32);

impl Local {
    fn arg(n: i32) -> Self {
        assert!(n >= 0);
        Self(-n - 1)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClauseItem {
    Var(Local),
    Number(i64),
    Functor { name: Spur, args: Box<[ClauseItem]> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Clause {
    /// The number of local variables used in the clause.
    /// The first `n` locals are the parameters.
    pub(crate) locals: u32,
    /// The requirements
    pub reqs: Vec<(Span, ClauseItem)>,
}

impl Clause {
    pub fn from_ast(ast: &parser::Clause, rodeo: &mut Rodeo) -> Clause {
        AstTranslator {
            next_local: 0,
            locals: HashMap::new(),
            reqs: vec![],
            rodeo,
        }
        .translate_clause(ast)
    }
}

/// A helper struct for translating AST clauses into ClauseItems
struct AstTranslator<'a> {
    next_local: i32,
    locals: HashMap<Spur, Local>,
    reqs: Vec<(Span, ClauseItem)>,
    rodeo: &'a mut Rodeo,
}

impl AstTranslator<'_> {
    fn nil(&mut self) -> ClauseItem {
        ClauseItem::Functor {
            name: self.rodeo.get_or_intern("[]"),
            args: vec![].into_boxed_slice(),
        }
    }

    fn cons(&mut self, head: ClauseItem, tail: ClauseItem) -> ClauseItem {
        ClauseItem::Functor {
            name: self.rodeo.get_or_intern("."),
            args: vec![head, tail].into_boxed_slice(),
        }
    }

    fn list(&mut self, items: &[parser::Expr], tail: Option<&parser::Expr>) -> ClauseItem {
        let mut result = if let Some(tail) = tail {
            self.translate_expr(tail)
        } else {
            self.nil()
        };

        for e in items.iter().rev() {
            let i = self.translate_expr(e);
            result = self.cons(i, result);
        }

        result
    }

    fn string(&mut self, s: &str) -> ClauseItem {
        // Aaaa really wish there was TRMC
        // If there was TRMC, iterating backwards wouldn't be necessary

        let mut result = self.nil();

        for ch in s.graphemes(/* extended: */ true).rev() {
            let item = ClauseItem::Functor {
                name: self.rodeo.get_or_intern(ch),
                args: vec![].into_boxed_slice(),
            };
            result = self.cons(item, result);
        }

        result
    }

    fn translate_expr(&mut self, ast: &parser::Expr) -> ClauseItem {
        use parser::Expr;
        match *ast {
            Expr::Paren { ref inner, .. } => self.translate_expr(inner),
            Expr::Wildcard { .. } => {
                let l = Local(self.next_local);
                self.next_local += 1;
                ClauseItem::Var(l)
            }
            Expr::Var { name, .. } => {
                let next_local = &mut self.next_local;
                let l = self.locals.entry(name).or_insert_with(|| {
                    let l = Local(*next_local);
                    *next_local += 1;
                    l
                });
                ClauseItem::Var(*l)
            }
            Expr::Number { value, .. } => ClauseItem::Number(value),
            Expr::Functor { name, ref args, .. } => ClauseItem::Functor {
                name,
                args: args
                    .iter()
                    .map(|arg| self.translate_expr(arg))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            Expr::String { ref value, .. } => self.string(&value),
            Expr::List {
                ref items,
                ref tail,
                ..
            } => self.list(&items[..], tail.as_ref().map(|x| &**x)),
        }
    }

    fn unify_arg(&mut self, arg: i32, item: ClauseItem) -> ClauseItem {
        ClauseItem::Functor {
            name: self.rodeo.get_or_intern("="),
            args: Box::new([ClauseItem::Var(Local::arg(arg)), item]),
        }
    }

    fn handle_arg(&mut self, i: i32, arg: &parser::Expr) {
        use parser::Expr;
        match *arg {
            Expr::Paren { ref inner, .. } => self.handle_arg(i, inner),
            Expr::Wildcard { .. } => (),
            Expr::Var { span, name } => match self.locals.entry(name) {
                Entry::Occupied(entry) => {
                    let l = *entry.get();
                    let req = self.unify_arg(i, ClauseItem::Var(l));
                    self.reqs.push((span, req));
                }
                Entry::Vacant(entry) => {
                    entry.insert(Local::arg(i as i32));
                }
            },
            Expr::Number { span, value } => {
                let req = self.unify_arg(i, ClauseItem::Number(value));
                self.reqs.push((span, req));
            }
            Expr::Functor { span, .. } => {
                let e = self.translate_expr(arg);
                let req = self.unify_arg(i, e);
                self.reqs.push((span, req));
            }
            Expr::String { span, ref value } => {
                let e = self.string(value);
                let req = self.unify_arg(i, e);
                self.reqs.push((span, req));
            }
            Expr::List {
                span,
                ref items,
                ref tail,
            } => {
                let e = self.list(items, tail.as_ref().map(|x| &**x));
                let req = self.unify_arg(i, e);
                self.reqs.push((span, req));
            }
        }
    }

    fn translate_subgoals(&mut self, subgoals: &[parser::Expr]) {
        for goal in subgoals {
            let req = self.translate_expr(goal);
            self.reqs.push((goal.span(), req));
        }
    }

    fn translate_clause(mut self, ast: &parser::Clause) -> Clause {
        for (i, arg) in ast.args.iter().enumerate() {
            self.handle_arg(i as i32, arg);
        }

        self.translate_subgoals(&ast.subgoals[..]);

        Clause {
            locals: self.next_local as u32,
            reqs: self.reqs,
        }
    }
}

/// Returns a tuple `(locals, subgoals)`, allocates initial variables, and stores their names in
/// `ctx.var_names`.
///
/// Next, run unify::State{..}.solve_clause_items(locals, )
pub fn translate_query(
    query: &[parser::Expr],
    ctx: &mut Context,
    vars: &mut VarTable,
) -> (LocalVars<'static>, Vec<(Span, ClauseItem)>) {
    let mut translator = AstTranslator {
        next_local: 0,
        locals: HashMap::new(),
        reqs: vec![],
        rodeo: &mut ctx.rodeo,
    };
    translator.translate_subgoals(query);

    let locals = vars.allocate_locals(translator.next_local as u32, &[]);

    ctx.var_names = HashMap::new();
    for (name, l) in translator.locals {
        ctx.var_names.insert(name, locals.get(l));
    }

    (locals, translator.reqs)
}
