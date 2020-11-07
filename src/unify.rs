//! Unification

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
                    .collect::<Vec<VarId>>()
                    .into_boxed_slice();
                vars.new_var_of(Item::Functor {
                    name,
                    args: new_args,
                })
            }
        }
    }
}

impl<'a, 'v> State<'a, 'v> {
    pub fn solve(&mut self, v: VarId) -> Result<Command> {
        match *self.vars.lookup(v) {
            Item::Var(_) => Err(SolveError("can't solve ambiguous metavariable")),
            Item::Functor { name, ref args } => match self.ctx.rels.get(&RelId {
                name,
                arity: args.len() as u32,
            }) {
                None => Err(SolveError("Unknown functor")), // TODO: better message
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
                            Stop => return Ok(Stop),
                            KeepGoing => continue,
                        }
                    }
                    Ok(KeepGoing)
                }
            },
        }
    }
}

// Aaaa this is really shitty
// CPS without tail calls trashing the stack
// whatever, it's doesn't need to be fancy

struct All<'a> {
    items: &'a [VarId],
    base: &'a mut dyn Runner,
}

impl<'a> Runner for All<'a> {
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable<'_>) -> Result<Command> {
        match *self.items {
            [] => self.base.solution(ctx, vars),
            [head] => {
                let mut state = State {
                    ctx,
                    vars,
                    runner: self.base,
                };
                state.solve(head)
            }
            [head, ref tail @ ..] => {
                let mut state = State {
                    ctx,
                    vars,
                    runner: &mut All {
                        items: tail,
                        base: self.base,
                    },
                };
                state.solve(head)
            }
        }
    }
}

impl<'a, 'v> State<'a, 'v> {
    fn solve_clause(&mut self, clause: &Clause, args: Box<[VarId]>) -> Result<Command> {
        let locals = self.vars.allocate_locals(clause, args);
        // loop thru and solve each clause item
        // (in an ultra-cursed CPS no tail calls way)
        let mut items = clause.reqs.iter().map(|req| req.reify(self.vars, &locals));
        if let Some(first) = items.next() {
            let rest = items.collect::<Vec<VarId>>();
            if rest.len() == 0 {
                // Hope for a tail call :'(
                drop(locals);
                drop(rest);
                self.solve(first)
            } else {
                let mut state = State {
                    ctx: self.ctx,
                    vars: self.vars,
                    runner: &mut All {
                        items: &rest[..],
                        base: self.runner,
                    },
                };
                state.solve(first)
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
    fn solution(&mut self, ctx: &Context, vars: &mut VarTable) -> Result<Command> {
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
    pub fn unify(&mut self, a: VarId, b: VarId) -> Result<Command> {
        let a = self.vars.lookup_helper(a);
        let b = self.vars.lookup_helper(b);
        match *self.vars.lookup_imm(a) {
            Item::Var(v) => {
                self.vars.update(v, Item::Var(b));
                self.runner.solution(self.ctx, self.vars)
            }
            Item::Functor { name, ref args } => match *self.vars.lookup_imm(b) {
                Item::Var(v) => {
                    self.vars.update(v, Item::Var(a));
                    self.runner.solution(self.ctx, self.vars)
                }
                Item::Functor {
                    name: name_b,
                    args: ref args_b,
                } if name == name_b && args.len() == args_b.len() => {
                    // Unify all arguments
                    if args.len() == 0 {
                        // Nothing left to unify!
                        // Solved.
                        self.runner.solution(self.ctx, self.vars)
                    } else if args.len() == 1 {
                        self.unify(args[0], args_b[0])
                    } else {
                        let first_a = args[0];
                        let first_b = args_b[0];
                        let lhs = args[1..].to_owned();
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
                // Could not unify -- backtrack
                Item::Functor { .. } => Ok(KeepGoing),
            },
        }
    }
}
