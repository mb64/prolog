//! The main datastructures

use codespan_reporting::files::SimpleFiles;
use itertools::Itertools;
use lasso::{Rodeo, Spur};
use std::collections::{hash_map::Entry, HashMap};

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
    String(Box<str>),
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
        use parser::Expr;

        let mut next_local = 0;
        let mut locals = HashMap::<Spur, Local>::new();
        let mut reqs = vec![];

        fn translate_expr(
            ast: &Expr,
            next_local: &mut i32,
            locals: &mut HashMap<Spur, Local>,
        ) -> ClauseItem {
            match *ast {
                Expr::Wildcard { .. } => {
                    let l = Local(*next_local);
                    *next_local += 1;
                    ClauseItem::Var(l)
                }
                Expr::Var { name, .. } => {
                    let l = locals.entry(name).or_insert_with(|| {
                        *next_local += 1;
                        Local(*next_local - 1)
                    });
                    ClauseItem::Var(*l)
                }
                Expr::Number { value, .. } => ClauseItem::Number(value),
                Expr::String { ref value, .. } => ClauseItem::String(value.clone().into()),
                Expr::Functor { name, ref args, .. } => ClauseItem::Functor {
                    name,
                    args: args
                        .iter()
                        .map(|arg| translate_expr(arg, next_local, locals))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                },
            }
        }

        let mut unify_arg = move |arg: usize, item: ClauseItem| -> ClauseItem {
            ClauseItem::Functor {
                name: rodeo.get_or_intern("'='"),
                args: Box::new([ClauseItem::Var(Local::arg(arg as i32)), item]),
            }
        };

        for (i, arg) in ast.args.iter().enumerate() {
            match *arg {
                Expr::Wildcard { .. } => (),
                Expr::Var { span, name } => match locals.entry(name) {
                    Entry::Occupied(entry) => {
                        let l = *entry.get();
                        reqs.push((span, unify_arg(i, ClauseItem::Var(l))));
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(Local::arg(i as i32));
                    }
                },
                Expr::Number { span, value } => {
                    reqs.push((span, unify_arg(i, ClauseItem::Number(value))));
                }
                Expr::String { span, ref value } => {
                    reqs.push((span, unify_arg(i, ClauseItem::String(value.clone().into()))));
                }
                Expr::Functor { span, .. } => {
                    let e = translate_expr(arg, &mut next_local, &mut locals);
                    reqs.push((span, unify_arg(i, e)));
                }
            }
        }

        for cond in &ast.conditions {
            reqs.push((
                cond.span(),
                translate_expr(cond, &mut next_local, &mut locals),
            ));
        }

        Clause {
            locals: next_local as u32,
            reqs,
        }
    }
}
