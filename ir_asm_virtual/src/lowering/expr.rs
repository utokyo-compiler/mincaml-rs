use data_structure::index::vec::{Idx, IndexVec};

use crate::{
    builder, AbsCallingConv, ArgIndex, BasicBlock, BasicBlockData, Closure, Context,
    DisambiguatedIdent, ExprKind, FnName, Ident, LitKind, Local, Place, ProjectionKind, StmtIndex,
    StmtKind, TerminatorKind, Ty, Typed,
};

use super::Lowering;

#[derive(Default)]
/// The state of the lowering process.
pub(crate) struct State<'ctx> {
    binders: Vec<Binder<'ctx>>,
}

impl<'ctx> State<'ctx> {
    fn with_one_binder(binder: Binder<'ctx>) -> Self {
        Self {
            binders: vec![binder],
        }
    }

    fn pop_innermost_binder(&mut self) -> Option<Binder<'ctx>> {
        self.binders.pop()
    }

    fn push_binder(&mut self, binder: Binder<'ctx>) {
        self.binders.push(binder);
    }
}

enum Binder<'ctx> {
    LetBinding(ir_closure::Pattern<'ctx>),
    FnReturn,
    Branch {
        /// The target basic block.
        target: BasicBlock,
    },
}

enum Bindee<'ctx> {
    Expr {
        expr: ExprKind<'ctx>,
        ty: Ty<'ctx>,
    },
    Call {
        calling_conv: AbsCallingConv,
        fn_name: FnName<'ctx>,
        args: IndexVec<ArgIndex, Local>,
    },
}

impl<'ctx> Binder<'ctx> {
    /// Bind the given expression to the current binding.
    ///
    /// ### Returns
    ///
    /// If the binding is a `FnReturn` or `Branch`, the function returns
    /// the created basic block. Otherwise, it returns `None`.
    fn bind(
        self,
        ctx: &'ctx Context<'ctx>,
        builder: &mut builder::FunctionBuilder<'ctx>,
        bindee: Bindee<'ctx>,
    ) -> Option<BasicBlock> {
        match self {
            Binder::LetBinding(pattern) => {
                match pattern {
                    ir_closure::Pattern::Unit => {
                        builder.push_stmt(StmtKind::Assign {
                            place: Place::Discard,
                            value: ctx.new_expr(Typed::new(expr, ty)),
                        });
                    }
                    ir_closure::Pattern::Var(var) => {
                        let local = builder.get_local(var);
                        builder.push_stmt(StmtKind::Assign {
                            place: Place::Local(local),
                            value: ctx.new_expr(Typed::new(expr, ty)),
                        });
                    }
                    ir_closure::Pattern::Tuple(vars) => {
                        let tuple_assign_rhs =
                            evaluated_local(ctx, builder, "tuple_assign_rhs", expr, ty);
                        for (tuple_index, var) in vars.into_iter_enumerated() {
                            let local = builder.get_local(var);
                            builder.push_stmt(StmtKind::Assign {
                                place: Place::Local(local),
                                value: ctx.new_expr(Typed::new(
                                    ExprKind::Read(Place::Projection {
                                        base: tuple_assign_rhs,
                                        projection_kind: ProjectionKind::TupleIndex(tuple_index),
                                    }),
                                    ty,
                                )),
                            });
                        }
                    }
                };
                None
            }
            Binder::FnReturn => {
                builder.push_stmt(StmtKind::Assign {
                    place: Place::Local(Local::RETURN_LOCAL),
                    value: ctx.new_expr(Typed::new(expr, ty)),
                });
                Some(builder.finish_block(TerminatorKind::Return))
            }
            Binder::Branch { target } => {
                let branch_arg = evaluated_local(ctx, builder, "branch_arg", expr, ty);
                let mut args = IndexVec::new();
                args.push(branch_arg);
                Some(builder.finish_block(TerminatorKind::Branch { target, args }))
            }
        }
    }
}

fn new_local<'ctx>(
    ctx: &'ctx Context<'ctx>,
    builder: &mut builder::FunctionBuilder<'ctx>,
    name: &'static str,
    ty: Ty<'ctx>,
) -> Local {
    static COMPILER_GENERATED_COUNTER: std::sync::atomic::AtomicUsize =
        std::sync::atomic::AtomicUsize::new(0);
    let ident = ctx.new_ident_unchecked(Typed::new(
        DisambiguatedIdent::new_compiler_unchecked(
            name,
            COMPILER_GENERATED_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        ),
        ty,
    ));
    builder.get_local(ident)
}

fn evaluated_local<'ctx>(
    ctx: &'ctx Context<'ctx>,
    builder: &mut builder::FunctionBuilder<'ctx>,
    name: &'static str,
    expr: ExprKind<'ctx>,
    ty: Ty<'ctx>,
) -> Local {
    match expr {
        ExprKind::Read(Place::Local(ident)) => {
            // if the expression is a variable, we can reuse the variable name
            ident
        }
        expr => {
            let local = new_local(ctx, builder, name, ty);
            builder.push_stmt(StmtKind::Assign {
                place: Place::Local(local),
                value: ctx.new_expr(Typed::new(expr, ty)),
            });
            local
        }
    }
}

impl<'ctx> Lowering<'ctx> for ir_closure::Expr<'ctx> {
    type Output = Option<BasicBlock>;
    type State = State<'ctx>;

    fn lower(
        &self,
        ctx: &'ctx Context<'ctx>,
        builder: &mut builder::FunctionBuilder<'ctx>,
        state: &mut Self::State,
    ) -> Self::Output {
        let ty = self.ty;
        let expr = match self.kind() {
            // trivial cases
            ir_closure::ExprKind::Const(lit_kind) => ExprKind::Const(*lit_kind),
            ir_closure::ExprKind::Unary(un_op, e1) => {
                let e1 = builder.get_local(*e1);
                ExprKind::Unary(*un_op, e1)
            }
            ir_closure::ExprKind::Binary(bin_op, e1, e2) => {
                let e1 = builder.get_local(*e1);
                let e2 = builder.get_local(*e2);
                ExprKind::Binary(*bin_op, e1, e2)
            }
            ir_closure::ExprKind::ClosureMake(closure) => ExprKind::ClosureMake(Closure {
                fn_name: closure.fn_name,
                captured_args: closure
                    .captured_args
                    .iter()
                    .map(|arg| builder.get_local(*arg))
                    .collect(),
            }),
            ir_closure::ExprKind::Tuple(vec) => ExprKind::Tuple(
                vec.iter()
                    .map(|var| builder.get_local(*var))
                    .collect::<IndexVec<_, _>>(),
            ),
            ir_closure::ExprKind::ArrayMake(e1, e2) => {
                let e1 = builder.get_local(*e1);
                let e2 = builder.get_local(*e2);
                ExprKind::ArrayMake(e1, e2)
            }
            ir_closure::ExprKind::Var(e) => {
                let e = builder.get_local(*e);
                ExprKind::Read(Place::Local(e))
            }
            ir_closure::ExprKind::Get(e1, e2) => {
                let e1 = builder.get_local(*e1);
                let e2 = builder.get_local(*e2);
                ExprKind::Read(Place::Projection {
                    base: e1,
                    projection_kind: ProjectionKind::ArrayElem(e2),
                })
            }

            // stmt creation
            ir_closure::ExprKind::Let(ir_closure::LetBinding { pattern, value }, follows) => {
                state.push_binder(Binder::LetBinding(pattern.clone()));
                value.lower(ctx, builder, state);
                // At this point, the value has been bound to the pattern.

                // Continue lowering the following expression.
                // Nested `Let` structures are fully handled by this recursive call.
                return follows.lower(ctx, builder, state);
            }
            ir_closure::ExprKind::Set(base, displacement, value) => {
                let ty = value.ty;
                let base = builder.get_local(*base);
                let displacement = builder.get_local(*displacement);
                let value = builder.get_local(*value);
                builder.push_stmt(StmtKind::Assign {
                    place: Place::Projection {
                        base,
                        projection_kind: ProjectionKind::ArrayElem(displacement),
                    },
                    value: ctx.new_expr(Typed::new(ExprKind::Read(Place::Local(value)), ty)),
                });
                // Result of `Set` is not a meaningful value, but
                // it is allowed to be bound to a variable or returned.
                ExprKind::Const(LitKind::Unit)
            }

            // basic-block creation
            ir_closure::ExprKind::If(e1, e2, e3) => {
                let bb_dummy = BasicBlock::ENTRY_BLOCK;
                let condition = builder.get_local(*e1);
                let bb_before_if = builder.finish_block(TerminatorKind::ConditionalBranch {
                    condition,
                    targets: [bb_dummy, bb_dummy],
                });
                let innermost_binder = state.pop_innermost_binder().unwrap();
                let bb_true_block = e2
                    .lower(
                        ctx,
                        builder,
                        &mut State::with_one_binder(Binder::Branch { target: bb_dummy }),
                    )
                    .unwrap();
                let bb_false_block = e3
                    .lower(
                        ctx,
                        builder,
                        &mut State::with_one_binder(Binder::Branch { target: bb_dummy }),
                    )
                    .unwrap();
                // Remove the dummy.
                *builder.basic_blocks_mut()[bb_before_if]
                    .terminator
                    .as_mut_conditional_branch_targets()
                    .unwrap() = [bb_true_block, bb_false_block];
                let bb_after_if = builder.next_basic_block();
                *builder.basic_blocks_mut()[bb_true_block]
                    .terminator
                    .as_mut_branch_target()
                    .unwrap() = bb_after_if;
                *builder.basic_blocks_mut()[bb_false_block]
                    .terminator
                    .as_mut_branch_target()
                    .unwrap() = bb_after_if;
                return None;
            }
            ir_closure::ExprKind::App(apply_kind, fn_name, args) => {
                let args = args
                    .iter()
                    .map(|arg| builder.get_local(*arg))
                    .collect::<IndexVec<_, _>>();
                let innermost_binder = state.pop_innermost_binder().unwrap();
                return innermost_binder.bind(
                    ctx,
                    builder,
                    Bindee::Call {
                        calling_conv: *apply_kind,
                        fn_name: *fn_name,
                        args,
                    },
                );
            }
        };
        // We have reached the block tail expression.
        // Simply bind it to the current binding.
        let innermost_binder = state.pop_innermost_binder().unwrap();
        innermost_binder.bind(ctx, builder, Bindee::Expr { expr, ty })
    }
}
