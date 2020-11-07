//! The main datastructures

use lasso::{Rodeo, Spur};
use scoped_map::{ScopedMap, ScopedMapBase};
use std::collections::{hash_map::Entry, HashMap};
use std::fmt::{self, Write};

use crate::parser;
use crate::runner::Runner;

/// An unrecoverable error
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct SolveError(pub &'static str);

pub type Result<T> = std::result::Result<T, SolveError>;

/// What to do next
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Command {
    KeepGoing,
    Stop,
}
pub use Command::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(u64);

impl fmt::Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{{}}}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelId {
    pub name: Spur,
    pub arity: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Item {
    /// Look up what it is in the current State
    Var(VarId),
    /// A functor is like f(Args...)
    Functor { name: Spur, args: Box<[VarId]> },
}

pub struct VarTable<'a> {
    map: ScopedMap<'a, VarId, Item>,
    next_var: u64,
}

impl<'a> VarTable<'a> {
    pub fn new(base: &'a ScopedMapBase<VarId, Item>) -> Self {
        Self {
            map: base.make_map(),
            next_var: 0,
        }
    }

    pub fn new_var(&mut self) -> VarId {
        let new_id = VarId(self.next_var);
        self.next_var += 1;
        self.map.insert(new_id, Item::Var(new_id));
        new_id
    }

    pub fn new_var_of(&mut self, item: Item) -> VarId {
        let new_id = VarId(self.next_var);
        self.next_var += 1;
        self.map.insert(new_id, item);
        new_id
    }

    /// Note: Only do this on variables that currently refer to themselves
    pub fn update(&mut self, var: VarId, to: Item) {
        self.map.insert(var, to);
    }

    pub fn backtrackable(&self) -> VarTable<'_> {
        VarTable {
            map: self.map.new_scope(),
            next_var: self.next_var,
        }
    }

    pub fn lookup_helper(&mut self, var: VarId) -> VarId {
        let i = self.map.lookup(&var).unwrap();
        match i {
            &Item::Var(v) if v == var => var,
            &Item::Var(v) => {
                let res = self.lookup_helper(v);
                // Collapse indirection
                self.map.insert(var, Item::Var(res));
                res
            }
            Item::Functor { .. } => return var,
        }
    }

    pub fn lookup(&mut self, var: VarId) -> &Item {
        let i = self.map.lookup(&var).unwrap();
        match i {
            &Item::Var(v) if v == var => i,
            &Item::Var(v) => {
                let w = self.lookup_helper(v);
                self.map.insert(var, Item::Var(w));
                self.map.lookup(&w).unwrap()
            }
            Item::Functor { .. } => i,
        }
    }

    pub fn lookup_imm(&self, var: VarId) -> &Item {
        self.map.lookup(&var).unwrap()
    }
}

impl<'a> VarTable<'a> {
    pub fn show(&self, var: VarId, rodeo: &Rodeo) -> String {
        let mut s = String::new();
        self.fmt_helper(&mut s, var, rodeo, false);
        s
    }

    pub fn dbg(&self, var: VarId, rodeo: &Rodeo) -> String {
        let mut s = String::new();
        self.fmt_helper(&mut s, var, rodeo, true);
        s
    }

    fn fmt_helper(&self, f: &mut impl Write, var: VarId, rodeo: &Rodeo, dbg: bool) {
        if dbg {
            write!(f, "{}", var).unwrap();
        }
        let item = self.map.lookup(&var).unwrap();
        match *item {
            Item::Var(v) if v == var => write!(f, "{}", v).unwrap(),
            Item::Var(v) => self.fmt_helper(f, v, rodeo, dbg),
            Item::Functor { name, ref args } => {
                write!(f, "{}(", rodeo.resolve(&name)).unwrap();
                for (i, arg) in args.iter().copied().enumerate() {
                    self.fmt_helper(f, arg, rodeo, dbg);
                    if i != args.len() - 1 {
                        write!(f, ", ").unwrap();
                    }
                }
                write!(f, ")").unwrap();
            }
        }
    }
}

pub struct Context {
    pub rels: HashMap<RelId, Relation>,
    pub rodeo: Rodeo,
}

impl Context {
    pub fn add_clause(
        &mut self,
        rel_id: RelId,
        clause: Clause,
    ) -> std::result::Result<(), &'static str> {
        let entry = self
            .rels
            .entry(rel_id)
            .or_insert(Relation::User(Vec::new()));

        match *entry {
            Relation::User(ref mut cs) => {
                cs.push(clause);
                Ok(())
            }
            Relation::Builtin(_) => Err("Cannot extend builtin relation"),
        }
    }

    pub fn add_ast_clause(
        &mut self,
        ast_clause: parser::Clause,
    ) -> std::result::Result<(), &'static str> {
        let rel_id = RelId {
            name: ast_clause.functor,
            arity: ast_clause.args.len() as u32,
        };
        let clause = Clause::from_ast(&ast_clause, &mut self.rodeo);
        self.add_clause(rel_id, clause)
    }
}

#[derive(Clone)]
pub enum Relation {
    Builtin(fn(&Context, &mut VarTable<'_>, Box<[VarId]>, &mut dyn Runner) -> Result<Command>),
    User(Vec<Clause>),
}

/// A local variable
// Negative number is arguments, positive number is local var
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local(i32);

impl Local {
    fn arg(n: i32) -> Self {
        assert!(n >= 0);
        Self(-n - 1)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClauseItem {
    Var(Local),
    Functor { name: Spur, args: Box<[ClauseItem]> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Clause {
    /// The number of local variables used in the clause.
    /// The first `n` locals are the parameters.
    locals: u32,
    /// The requirements
    pub reqs: Vec<ClauseItem>,
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
                Expr::Var(n) => {
                    let v = locals.entry(n).or_insert_with(|| {
                        *next_local += 1;
                        Local(*next_local - 1)
                    });
                    ClauseItem::Var(*v)
                }
                Expr::Functor { name, ref args } => ClauseItem::Functor {
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
                name: rodeo.get_or_intern("="),
                args: Box::new([ClauseItem::Var(Local::arg(arg as i32)), item]),
            }
        };

        for (i, arg) in ast.args.iter().enumerate() {
            match *arg {
                Expr::Var(n) => match locals.entry(n) {
                    Entry::Occupied(entry) => {
                        let l = *entry.get();
                        reqs.push(unify_arg(i, ClauseItem::Var(l)));
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(Local::arg(i as i32));
                    }
                },
                Expr::Functor { .. } => {
                    let e = translate_expr(arg, &mut next_local, &mut locals);
                    reqs.push(unify_arg(i, e));
                }
            }
        }

        for cond in &ast.conditions {
            reqs.push(translate_expr(cond, &mut next_local, &mut locals));
        }

        Clause {
            locals: next_local as u32,
            reqs,
        }
    }
}

/// A collection holding the local variables and parameters
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LocalVars {
    args: Box<[VarId]>,
    start: u64,
    count: u32,
}

impl VarTable<'_> {
    pub fn allocate_locals<'a>(&mut self, clause: &Clause, args: Box<[VarId]>) -> LocalVars {
        let start = self.next_var;
        let res = LocalVars {
            args,
            start,
            count: clause.locals,
        };
        self.next_var += clause.locals as u64;
        for v in start..self.next_var {
            self.map.insert(VarId(v), Item::Var(VarId(v)));
        }
        res
    }
}

impl LocalVars {
    pub fn get(&self, local: Local) -> VarId {
        if local.0 < 0 {
            self.args[(-local.0 - 1) as usize]
        } else {
            assert!(local.0 < self.count as i32);
            VarId(self.start + local.0 as u64)
        }
    }
}
