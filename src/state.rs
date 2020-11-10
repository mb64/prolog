//! The main datastructures

use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::DisplayStyle;
use lasso::{Rodeo, Spur};
use scoped_map::{ScopedMap, ScopedMapBase};
use std::collections::{hash_map::Entry, HashMap};
use std::fmt::{self, Write};
use typed_arena::{Arena, SubArena};

use crate::parser::{self, Span};
use crate::runner::Runner;

/// A fatal, unrecoverable error
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SolveError {
    pub message: String,
    /// A trace of every clause item on the way to encountering this error
    pub trace: Vec<Span>,
}

impl<T: Into<String>> From<T> for Box<SolveError> {
    fn from(message: T) -> Self {
        Box::new(SolveError {
            message: message.into(),
            trace: vec![],
        })
    }
}
impl SolveError {
    pub fn add_trace(mut self: Box<Self>, span: Span) -> Box<Self> {
        self.trace.push(span);
        self
    }

    /// Print this error
    /// The given display style is used for all notes
    pub fn report(&self, ctx: &Context, display_style: DisplayStyle) {
        use codespan_reporting::diagnostic::Severity::*;
        use codespan_reporting::diagnostic::{Diagnostic, Label};
        use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

        let writer = StandardStream::stderr(ColorChoice::Always);
        let mut config = codespan_reporting::term::Config::default();

        for (i, span) in self.trace.iter().copied().enumerate() {
            let (severity, message) = if i == 0 {
                (Error, self.message.as_str())
            } else {
                (Note, "Called from here")
            };
            let diagnostic = Diagnostic::new(severity)
                .with_message(message)
                .with_labels(vec![Label::primary(
                    span.file_id as usize,
                    span.start..span.start + span.len as usize,
                )]);

            codespan_reporting::term::emit(&mut writer.lock(), &config, &ctx.files, &diagnostic)
                .unwrap();

            config.display_style = display_style.clone();
        }
    }
}

/// What to do next
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Command {
    KeepGoing,
    Stop,
}

/// `SolverResult` is returned by basically every function
pub type SolverResult = Result<Command, Box<SolveError>>;

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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Item<'a> {
    /// Could be anything -- not resolved/unified yet
    Unresolved,
    /// Look up what it is in the current State
    Var(VarId),
    /// An integer -- a simple representation since there's no fancy constraints
    // TODO: should it also have floats?
    // Also, look into adding constraints, and maybe a solver for finite domains
    Number(i64),
    /// A functor is like f(Args...)
    Functor { name: Spur, args: &'a [VarId] },
}

/// Construct a `VarTableBase` with `<VarTableBase as Default>::default()`
/// Then, make a `VarTable` with `VarTable::new(&base)`
#[derive(Default)]
pub struct VarTableBase {
    map: ScopedMapBase<VarId, Item<'static>>,
    arena: Arena<VarId>,
}

// This is actually self-borrowing!
// Really, it should be `Item<'self.arena>`, but `Item<'v>` is a good-enough approximation
/// The VarTable stores all unification state
/// Make one by going through `VarTableBase`
pub struct VarTable<'v> {
    map: ScopedMap<'v, VarId, Item<'v>>,
    next_var: u64,
    arena: SubArena<'v, VarId>,
}

impl<'v> VarTable<'v> {
    pub fn new(base: &'v VarTableBase) -> Self {
        Self {
            map: base.map.make_map(),
            next_var: 0,
            arena: SubArena::new(&base.arena),
        }
    }

    pub fn new_var(&mut self) -> VarId {
        let new_id = VarId(self.next_var);
        self.next_var += 1;
        self.map.insert(new_id, Item::Unresolved);
        new_id
    }

    pub fn new_var_of(&mut self, item: Item<'v>) -> VarId {
        let new_id = VarId(self.next_var);
        self.next_var += 1;
        self.map.insert(new_id, item);
        new_id
    }

    pub fn new_var_of_functor(&mut self, name: Spur, args: impl Iterator<Item = VarId>) -> VarId {
        // SAFETY: only safe bc we're careful everywhere else
        // As noted, `VarTable` is self-borrowing, and `'v` is only an approximation of the
        // lifetime `&'self.arena` of things borrowed from the arena
        unsafe {
            let args_wrong_lifetime = self.arena.alloc_extend(args);
            let args = std::mem::transmute::<&[VarId], &'v [VarId]>(args_wrong_lifetime);
            self.new_var_of(Item::Functor { name, args })
        }
    }

    /// Note: Only do this on variables that currently refer to themselves
    pub fn update(&mut self, var: VarId, to: Item<'v>) {
        debug_assert_eq!(self.map.lookup(&var).copied(), Some(Item::Unresolved));
        self.map.insert(var, to);
    }

    pub fn backtrackable(&self) -> VarTable<'_> {
        VarTable {
            map: self.map.new_scope(),
            next_var: self.next_var,
            arena: SubArena::new(&*self.arena),
        }
    }

    fn lookup_helper(&mut self, var: VarId) -> VarId {
        log::trace!("Looking up {}", var);
        let i = self.map.lookup(&var).unwrap();
        if let Item::Var(v) = *i {
            let res = self.lookup_helper(v);
            // Collapse indirection
            if v != res {
                self.map.insert(var, Item::Var(res));
            }
            res
        } else {
            var
        }
    }

    /// Like `.lookup()`, but it also returns the main varid for that variable
    pub fn lookup_with_varid(&mut self, var: VarId) -> (VarId, Item<'v>) {
        log::trace!("Looking up {}", var);
        let i = *self.map.lookup(&var).unwrap();
        if let Item::Var(v) = i {
            let w = self.lookup_helper(v);
            self.map.insert(var, Item::Var(w));
            (w, *self.map.lookup(&w).unwrap())
        } else {
            (var, i)
        }
    }

    /// Lookup never returns `Item::Var`
    pub fn lookup(&mut self, var: VarId) -> Item<'v> {
        self.lookup_with_varid(var).1
    }

    // pub fn lookup_imm(&self, var: VarId) -> &Item {
    //     self.map.lookup(&var).unwrap()
    // }
}

/// Functions to display variables
impl VarTable<'_> {
    pub fn show(&self, var: VarId, rodeo: &Rodeo) -> String {
        let mut s = String::new();
        self.fmt_helper(&mut s, var, rodeo, false).unwrap();
        s
    }

    pub fn dbg(&self, var: VarId, rodeo: &Rodeo) -> String {
        let mut s = String::new();
        self.fmt_helper(&mut s, var, rodeo, true).unwrap();
        s
    }

    fn fmt_helper(&self, f: &mut impl Write, var: VarId, rodeo: &Rodeo, dbg: bool) -> fmt::Result {
        if dbg {
            write!(f, "{}", var)?;
        }
        let item = self.map.lookup(&var).unwrap();
        match *item {
            Item::Unresolved => write!(f, "{}", var),
            Item::Var(v) => self.fmt_helper(f, v, rodeo, dbg),
            Item::Number(x) => write!(f, "{}", x),
            Item::Functor { name, ref args } => {
                write!(f, "{}", rodeo.resolve(&name))?;
                match **args {
                    [] => Ok(()),
                    [first, ref rest @ ..] => {
                        write!(f, "({}", first)?;
                        for arg in rest {
                            write!(f, ", {}", arg)?;
                        }
                        write!(f, ")")
                    }
                }
            }
        }
    }
}

pub struct Context {
    pub rels: HashMap<RelId, Relation>,
    pub rodeo: Rodeo,
    pub files: SimpleFiles<String, String>,
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
}

#[derive(Clone)]
pub enum Relation {
    Builtin(fn(&Context, &mut VarTable<'_>, &[VarId], &mut dyn Runner) -> SolverResult),
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
                name: rodeo.get_or_intern("="),
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

/// A collection holding the local variables and parameters
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct LocalVars<'a> {
    args: &'a [VarId],
    start: u64,
    count: u32,
}

impl VarTable<'_> {
    pub fn allocate_locals<'a>(&mut self, clause: &Clause, args: &'a [VarId]) -> LocalVars<'a> {
        let start = self.next_var;
        let res = LocalVars {
            args,
            start,
            count: clause.locals,
        };
        self.next_var += clause.locals as u64;
        for v in start..self.next_var {
            self.map.insert(VarId(v), Item::Unresolved);
        }
        res
    }
}

impl LocalVars<'_> {
    pub fn get(&self, local: Local) -> VarId {
        if local.0 < 0 {
            self.args[(-local.0 - 1) as usize]
        } else {
            assert!(local.0 < self.count as i32);
            VarId(self.start + local.0 as u64)
        }
    }
}
