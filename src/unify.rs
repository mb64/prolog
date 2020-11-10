//! Unification

use crate::parser::Span;
use crate::runner::*;
use crate::state::*;

pub struct State<'a, 'v> {
    pub ctx: &'a Context,
    pub vars: &'a mut VarTable<'v>,
    pub runner: &'a mut dyn Runner,
}

impl ClauseItem {
    pub fn reify<'v>(&self, vars: &mut VarTable<'v>, locals: &LocalVars) -> VarId {
        use ClauseItem::*;
        match *self {
            Var(l) => locals.get(l),
            Functor { name, ref args } => {
                let new_args = args
                    .iter()
                    .map(|ci| ci.reify(vars, locals))
                    .collect::<Vec<_>>();
                vars.new_var_of_functor(name, new_args.into_iter())
            }
        }
    }
}

impl<'a, 'v> State<'a, 'v> {
    pub fn solve(&mut self, v: VarId) -> SolverResult {
        log::debug!("Solving {}", self.vars.dbg(v, &self.ctx.rodeo));
        match self.vars.lookup(v) {
            Item::Unresolved => Err("can't solve ambiguous metavariable".into()),
            Item::Var(var) => panic!("Internal error: lookup {} returned var {}", v, var),
            Item::Number(n) => Err(format!("Type error: {} is not a functor", n).into()),
            Item::Functor { name, ref args } => match self.ctx.rels.get(&RelId {
                name,
                arity: args.len() as u32,
            }) {
                None => {
                    let message = format!(
                        "Unknown functor {}/{}",
                        self.ctx.rodeo.resolve(&name),
                        args.len()
                    );
                    log::debug!("{}", message);
                    Err(message.into())
                }
                Some(&Relation::Builtin(func)) => {
                    let args = args.clone();
                    func(self.ctx, self.vars, args, self.runner)
                }
                Some(Relation::User(clauses)) => {
                    let args = args.clone();
                    // FIXME: don't need the last clause to be backtrackable
                    // Really wish Rust had tail recursion
                    for clause in clauses {
                        // Would be good not to clone
                        let args = args.clone();
                        let mut tmp_state = State {
                            ctx: self.ctx,
                            vars: &mut self.vars.backtrackable(),
                            runner: self.runner,
                        };
                        match tmp_state.solve_clause(clause, args)? {
                            Command::Stop => return Ok(Command::Stop),
                            Command::KeepGoing => continue,
                        }
                    }
                    Ok(Command::KeepGoing)
                }
            },
        }
    }
}

// Aaaa this is really shitty
// CPS without tail calls trashing the stack
// whatever, it's doesn't need to be fancy

struct All<'a> {
    items: &'a [(Span, VarId)],
    base: &'a mut dyn Runner,
}

impl<'a> Runner for All<'a> {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> SolverResult {
        match *self.items {
            [] => self.base.solution(ctx, vars),
            [(span, head)] => {
                let mut state = State {
                    ctx,
                    vars,
                    runner: self.base,
                };
                state.solve(head).map_err(|e| e.add_trace(span))
            }
            [(span, head), ref tail @ ..] => {
                let mut state = State {
                    ctx,
                    vars,
                    runner: &mut All {
                        items: tail,
                        base: self.base,
                    },
                };
                state.solve(head).map_err(|e| e.add_trace(span))
            }
        }
    }
}

impl<'a, 'v> State<'a, 'v> {
    fn solve_clause(&mut self, clause: &Clause, args: &'v [VarId]) -> SolverResult {
        let locals = self.vars.allocate_locals(clause, args);
        // loop thru and solve each clause item
        // (in an ultra-cursed CPS no tail calls way)
        let mut items = clause
            .reqs
            .iter()
            .map(|(span, req)| (*span, req.reify(self.vars, &locals)));
        if let Some((span, first)) = items.next() {
            let rest = items.collect::<Vec<_>>();
            if rest.len() == 0 {
                // Oh well, no tail call :'(
                self.solve(first).map_err(|e| e.add_trace(span))
            } else {
                let mut state = State {
                    ctx: self.ctx,
                    vars: self.vars,
                    runner: &mut All {
                        items: &rest[..],
                        base: self.runner,
                    },
                };
                state.solve(first).map_err(|e| e.add_trace(span))
            }
        } else {
            drop(locals);
            self.runner.solution(self.ctx, self.vars)
        }
    }
}

struct UnifyAll<'a> {
    // Hate how there's a whole extra `usize` in there storing literally nothing
    // There's no way to specify that the two arrays have the same length, so two of the same value
    // need to be stored and then checked for equality
    lhs: &'a [VarId],
    rhs: &'a [VarId],
    base: &'a mut dyn Runner,
}

impl<'a> Runner for UnifyAll<'a> {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable) -> SolverResult {
        match (self.lhs, self.rhs) {
            ([], []) => self.base.solution(ctx, vars),
            (&[x], &[y]) => State {
                ctx,
                vars,
                runner: self.base,
            }
            .unify(x, y),
            (&[x, ref xs @ ..], &[y, ref ys @ ..]) => State {
                ctx,
                vars,
                runner: &mut UnifyAll {
                    lhs: xs,
                    rhs: ys,
                    base: self.base,
                },
            }
            .unify(x, y),
            _ => panic!("Internal error: mismatches lengths"),
        }
    }
}

// Finally
impl<'a, 'v> State<'a, 'v> {
    pub fn unify(&mut self, a: VarId, b: VarId) -> SolverResult {
        log::debug!(
            "Unifying {} and {}",
            self.vars.dbg(a, &self.ctx.rodeo),
            self.vars.dbg(b, &self.ctx.rodeo)
        );
        match (
            self.vars.lookup_with_varid(a),
            self.vars.lookup_with_varid(b),
        ) {
            ((va, Item::Unresolved), (vb, _)) => {
                log::trace!("updating {} to {}", va, vb);
                self.vars.update(va, Item::Var(vb));
                self.runner.solution(self.ctx, self.vars)
            }
            ((va, _), (vb, Item::Unresolved)) => {
                log::trace!("updating {} to {}", vb, va);
                self.vars.update(vb, Item::Var(va));
                self.runner.solution(self.ctx, self.vars)
            }
            (
                (
                    _,
                    Item::Functor {
                        name: name_a,
                        args: args_a,
                    },
                ),
                (
                    _,
                    Item::Functor {
                        name: name_b,
                        args: args_b,
                    },
                ),
            ) if name_a == name_b && args_a.len() == args_b.len() => {
                // Unify all arguments
                let len = args_a.len();
                if len == 0 {
                    log::trace!("Nothing left to unify! Solved.");
                    self.runner.solution(self.ctx, self.vars)
                } else if len == 1 {
                    log::trace!("Unify the arguments");
                    self.unify(args_a[0], args_b[0])
                } else {
                    log::trace!("Unify the arguments");
                    let first_a = args_a[0];
                    let first_b = args_b[0];
                    let lhs = args_a[1..].to_owned();
                    let rhs = args_b[1..].to_owned();
                    State {
                        ctx: self.ctx,
                        vars: self.vars,
                        runner: &mut UnifyAll {
                            lhs: &lhs,
                            rhs: &rhs,
                            base: self.runner,
                        },
                    }
                    .unify(first_a, first_b)
                }
            }
            (
                (
                    _,
                    Item::Functor {
                        name: name_a,
                        args: args_a,
                    },
                ),
                (
                    _,
                    Item::Functor {
                        name: name_b,
                        args: args_b,
                    },
                ),
            ) => {
                log::trace!(
                    "Could not unify {}/{} and {}/{} -- backtrack",
                    self.ctx.rodeo.resolve(&name_a),
                    args_a.len(),
                    self.ctx.rodeo.resolve(&name_b),
                    args_b.len()
                );
                Ok(Command::KeepGoing)
            }
        }
    }
}
