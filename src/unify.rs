//! Unification

use crate::context::*;
use crate::parser::Span;
use crate::runner::*;
use crate::vars::*;

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
            Number(x) => vars.new_var_of(Item::Number(x)),
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
        log::debug!("Solving {}", self.vars.dbg(v, &self.ctx));
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
                Some(Relation::User(clauses)) => match &**clauses {
                    [] => Ok(Command::KeepGoing),
                    [clause] => self.solve_clause(clause, args),
                    [first @ .., last] => {
                        for clause in first {
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
                        self.solve_clause(last, args)
                    }
                },
            },
        }
    }
}

impl<'a, 'v> State<'a, 'v> {
    fn solve_clause(&mut self, clause: &Clause, args: &[VarId]) -> SolverResult {
        let locals = self.vars.allocate_locals(clause.locals, args);
        self.solve_clause_items(locals, &clause.reqs)
    }

    pub fn solve_clause_items(
        &mut self,
        locals: LocalVars,
        reqs: &[(Span, ClauseItem)],
    ) -> SolverResult {
        // loop thru and solve each clause item
        // (in an ultra-cursed CPS no tail calls way)
        let mut items = reqs
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
            _ => panic!("Internal error: mismatched lengths"),
        }
    }
}

// Finally
impl<'a, 'v> State<'a, 'v> {
    pub fn unify(&mut self, a: VarId, b: VarId) -> SolverResult {
        log::debug!(
            "Unifying {} and {}",
            self.vars.dbg(a, &self.ctx),
            self.vars.dbg(b, &self.ctx)
        );
        match (
            self.vars.lookup_with_varid(a),
            self.vars.lookup_with_varid(b),
        ) {
            // lookup should never return vars
            ((_, Item::Var(v)), _) => panic!("lookup {} returned var {}", a, v),
            (_, (_, Item::Var(v))) => panic!("lookup {} returned var {}", b, v),

            // Avoid creating cyclic things
            ((va, _), (vb, _)) if va == vb => {
                log::trace!("already unified: {} and {}", va, vb);
                self.runner.solution(self.ctx, self.vars)
            }

            // if either is unresolved, unify
            // TODO: here would be the right place to add occurs checks in the future
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

            // unify two identical numbers
            ((_, Item::Number(x)), (_, Item::Number(y))) if x == y => {
                log::trace!("Numbers {} and {} are equal! Solved.", x, y);
                self.runner.solution(self.ctx, self.vars)
            }

            // unify two functors recursively
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
                    log::trace!("Both are {}! Solved.", self.ctx.rodeo.resolve(&name_a));
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

            // Different things
            ((va, _), (vb, _)) => {
                log::trace!(
                    "Could not unify {} and {} -- backtrack",
                    self.vars.dbg(va, &self.ctx),
                    self.vars.dbg(vb, &self.ctx)
                );
                Ok(Command::KeepGoing)
            }
        }
    }
}
