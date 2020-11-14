//! The datastructures to keep track of variables

use lasso::Spur;
use scoped_map::{ScopedMap, ScopedMapBase};
use std::fmt::{self, Write};
use typed_arena::{Arena, SubArena};

use crate::context::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(u64);

impl fmt::Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{{}}}", self.0)
    }
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

    unsafe fn alloc_functor_args(
        arena: &SubArena<'v, VarId>,
        args: impl Iterator<Item = VarId>,
    ) -> &'v [VarId] {
        let args_wrong_lifetime = arena.alloc_extend(args);
        std::mem::transmute::<&[VarId], &'v [VarId]>(args_wrong_lifetime)
    }

    pub fn new_var_of_functor(&mut self, name: Spur, args: impl Iterator<Item = VarId>) -> VarId {
        // SAFETY: only safe bc we're careful everywhere else
        // As noted, `VarTable` is self-borrowing, and `'v` is only an approximation of the
        // lifetime `&'self.arena` of things borrowed from the arena
        unsafe {
            let args = Self::alloc_functor_args(&self.arena, args);
            self.new_var_of(Item::Functor { name, args })
        }
    }

    /// Note: Only do this on variables that are currently unresolved
    /// (checked by debug_assertion)
    pub fn update(&mut self, var: VarId, to: Item<'v>) {
        debug_assert_eq!(self.map.lookup(&var), Some(&Item::Unresolved));
        self.map.insert(var, to);
    }

    /// Resolves the variable to a new functor, with the given name and arity
    /// Used by functor/3
    pub fn update_to_functor(&mut self, var: VarId, name: Spur, arity: usize) {
        let next_var = &mut self.next_var;
        let map = &mut self.map;
        // SAFETY: see `new_var_of_functor`
        unsafe {
            let args = Self::alloc_functor_args(
                &self.arena,
                (0..arity).map(|_| {
                    let new_var = VarId(*next_var);
                    map.insert(new_var, Item::Unresolved);
                    *next_var += 1;
                    new_var
                }),
            );
            let item = Item::Functor { name, args };
            self.update(var, item)
        }
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
            // Don't need to check if var != w -- it's guaranteed to be true
            debug_assert_ne!(var, w);
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
}

/// Intrusive linked list of variables that have been visited so far
/// Used to avoid infinitely traversing cylic structures
struct Seen<'a> {
    var: VarId,
    next: Option<&'a Seen<'a>>,
}

/// Whether the linked list contains that var
fn has_seen(var: VarId, seen: Option<&Seen<'_>>) -> bool {
    let mut next = seen;
    while let Some(node) = next {
        if node.var == var {
            return true;
        }
        next = node.next;
    }
    false
}

/// Functions to display variables
impl VarTable<'_> {
    pub fn show(&self, var: VarId, ctx: &Context) -> String {
        let mut s = String::new();
        self.fmt_helper(&mut s, var, ctx, None, false).unwrap();
        s
    }

    pub fn dbg(&self, var: VarId, ctx: &Context) -> String {
        let mut s = String::new();
        self.fmt_helper(&mut s, var, ctx, None, true).unwrap();
        s
    }

    fn fmt_helper(
        &self,
        f: &mut impl Write,
        var: VarId,
        ctx: &Context,
        seen: Option<&Seen<'_>>,
        dbg: bool,
    ) -> fmt::Result {
        if has_seen(var, seen) {
            for (name, &v) in &ctx.var_names {
                if var == v {
                    return write!(f, "{}", ctx.rodeo.resolve(name));
                }
            }
            return write!(f, "{}", var);
        }
        let new_seen_node = Seen { var, next: seen };
        let seen = Some(&new_seen_node);

        if dbg {
            write!(f, "{}", var)?;
        }
        match *self.map.lookup(&var).unwrap() {
            Item::Unresolved => write!(f, "{}", var),
            Item::Var(v) => self.fmt_helper(f, v, ctx, seen, dbg),
            Item::Number(x) => write!(f, "{}", x),

            // Lists
            Item::Functor {
                name,
                args: &[head, tail],
            } if !dbg && name == ctx.builtins.cons => {
                write!(f, "[")?;
                self.fmt_helper(f, head, ctx, seen, dbg)?;
                self.fmt_list(f, tail, ctx, seen)
            }

            // Regular functors
            Item::Functor { name, args } => {
                write!(f, "{}", ctx.rodeo.resolve(&name))?;
                match *args {
                    [] => Ok(()),
                    [first, ref rest @ ..] => {
                        write!(f, "(")?;
                        self.fmt_helper(f, first, ctx, seen, dbg)?;
                        for &arg in rest {
                            write!(f, ", ")?;
                            self.fmt_helper(f, arg, ctx, seen, dbg)?;
                        }
                        write!(f, ")")
                    }
                }
            }
        }
    }

    fn fmt_list(
        &self,
        f: &mut impl Write,
        var: VarId,
        ctx: &Context,
        seen: Option<&Seen<'_>>,
    ) -> fmt::Result {
        if has_seen(var, seen) {
            for (name, &v) in &ctx.var_names {
                if var == v {
                    return write!(f, " | {}]", ctx.rodeo.resolve(name));
                }
            }
            return write!(f, " | {}]", var);
        }
        let new_seen_node = Seen { var, next: seen };
        let seen = Some(&new_seen_node);

        let item = self.map.lookup(&var).unwrap();
        match *item {
            Item::Var(v) => self.fmt_list(f, v, ctx, seen),

            // Nil
            Item::Functor { name, args: &[] } if name == ctx.builtins.nil => write!(f, "]"),
            // Cons
            Item::Functor {
                name,
                args: &[head, tail],
            } if name == ctx.builtins.cons => {
                write!(f, ", ")?;
                self.fmt_helper(f, head, ctx, seen, false)?;
                self.fmt_list(f, tail, ctx, seen)
            }

            // Not a list
            _ => {
                write!(f, " | ")?;
                self.fmt_helper(f, var, ctx, seen, false)?;
                write!(f, "]")
            }
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
    pub fn allocate_locals<'a>(&mut self, count: u32, args: &'a [VarId]) -> LocalVars<'a> {
        let start = self.next_var;
        let res = LocalVars { args, start, count };
        self.next_var += count as u64;
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
