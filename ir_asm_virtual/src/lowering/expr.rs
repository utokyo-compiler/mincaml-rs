use core::range::Range;

use data_structure::index::vec::IndexVec;

use crate::{
    builder, AbsCallingConv, ArgIndex, BasicBlock, Branch, Closure, Context, DisambiguatedIdent,
    ExprKind, FnName, LitKind, Local, MutVisitor, Place, ProjectionKind, StmtKind, TerminatorKind,
    Ty, Typed,
};

#[derive(Default)]
/// The state of the lowering process.
pub(crate) struct State {
    binders: Vec<Binder>,
}

impl State {
    pub fn with_return() -> Self {
        Self::with_one_binder(Binder::FnReturn)
    }

    fn with_one_binder(binder: Binder) -> Self {
        Self {
            binders: vec![binder],
        }
    }

    fn pop_innermost_binder(&mut self) -> Binder {
        self.binders.pop().expect("found an orphan bindee")
    }

    fn push_binder(&mut self, binder: Binder) {
        self.binders.push(binder);
    }
}

#[derive(Clone, Copy)]
enum BindingPlace {
    Discard,
    Local(Local),
}

impl BindingPlace {
    fn into_place(self) -> Place {
        match self {
            Self::Discard => Place::Discard,
            Self::Local(local) => Place::Local(local),
        }
    }
}

#[derive(Clone, Copy)]
/// Generalized binding agent.
enum Binder {
    LetBinding {
        /// The place to bind the expression to.
        place: BindingPlace,
    },

    /// Bind the expression to the return value of the function.
    FnReturn,

    /// Branch to the target basic block.
    ///
    /// The target block has only one argument at this point.
    Branch {
        /// The target basic block.
        target: BasicBlock,
    },
}

impl Binder {
    fn to_if_arms_binder(self, bb_after_if: BasicBlock) -> Self {
        match self {
            Self::LetBinding { .. } => Self::Branch {
                target: bb_after_if,
            },
            Self::FnReturn => Self::FnReturn,
            Self::Branch { target } => {
                // `If` expression in another `If` expression.
                // In this case, we do not need to create a new block
                // which confluences the inner two branches.
                Self::Branch { target }
            }
        }
    }
}

/// Generalized expression to be bound.
enum Bindee<'ctx> {
    Expr {
        expr: ExprKind<'ctx>,
        ty: Ty<'ctx>,
    },
    Call {
        calling_conv: AbsCallingConv,
        fn_name: FnName<'ctx>,
        args: IndexVec<ArgIndex, Local>,

        /// return type of the function.
        ty: Ty<'ctx>,
    },
}

impl Binder {
    /// Bind the given expression to the current binding.
    fn bind<'ctx>(
        self,
        ctx: &'ctx Context<'ctx>,
        builder: &mut builder::FunctionBuilder<'ctx>,
        bindee: Bindee<'ctx>,
    ) {
        match self {
            Binder::LetBinding { place } => {
                match bindee {
                    Bindee::Expr { expr, ty } => {
                        builder.push_stmt(StmtKind::Assign {
                            place: place.into_place(),
                            value: ctx.new_expr(Typed::new(expr, ty)),
                        });
                    }
                    Bindee::Call {
                        calling_conv,
                        fn_name,
                        args,
                        ..
                    } => {
                        let target = builder.next_basic_block();
                        builder.finish_block(TerminatorKind::Call {
                            calling_conv,
                            fn_name,
                            args,
                            branch: Branch {
                                target,
                                args: match place {
                                    BindingPlace::Discard => IndexVec::new(),
                                    BindingPlace::Local(local) => {
                                        IndexVec::from_raw_vec(vec![local])
                                    }
                                },
                            },
                        });
                    }
                };
            }
            Binder::FnReturn => {
                let return_value = match bindee {
                    Bindee::Expr { expr, ty } => ctx.new_expr(Typed::new(expr, ty)),
                    Bindee::Call {
                        calling_conv,
                        fn_name,
                        args,
                        ty,
                    } => {
                        let call_result = new_local(ctx, builder, "call_result", ty);
                        let target = builder.next_basic_block();
                        builder.finish_block(TerminatorKind::Call {
                            calling_conv,
                            fn_name,
                            args,
                            branch: Branch::no_args(target),
                        });
                        builder.set_args(IndexVec::from_raw_vec(vec![call_result]));
                        ctx.new_expr(Typed::new(ExprKind::Read(Place::Local(call_result)), ty))
                    }
                };
                builder.push_stmt(StmtKind::Assign {
                    place: Place::Local(Local::RETURN_LOCAL),
                    value: return_value,
                });
                builder.finish_block(TerminatorKind::Return);
            }
            Binder::Branch { target } => match bindee {
                Bindee::Expr { expr, ty } => {
                    let branch_arg = evaluated_local(ctx, builder, "branch_arg", expr, ty);
                    let mut args = IndexVec::new();
                    args.push(branch_arg);
                    builder.finish_block(TerminatorKind::Branch(Branch { target, args }));
                }
                Bindee::Call {
                    calling_conv,
                    fn_name,
                    args,
                    ..
                } => {
                    builder.finish_block(TerminatorKind::Call {
                        calling_conv,
                        fn_name,
                        args,
                        branch: Branch::no_args(target),
                    });
                }
            },
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

/// Lower the given expression.
///
/// The expression is lowered and bound to the current binding.
pub fn lower_expr<'ctx>(
    expr: &ir_closure::Expr<'ctx>,
    ctx: &'ctx Context<'ctx>,
    builder: &mut builder::FunctionBuilder<'ctx>,
    state: &mut State,
) {
    let ty = expr.ty;
    let bindee = 'bindee: {
        let expr = match expr.kind() {
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
                match pattern {
                    ir_closure::Pattern::Unit => {
                        state.push_binder(Binder::LetBinding {
                            place: BindingPlace::Discard,
                        });
                        lower_expr(value, ctx, builder, state);
                    }
                    ir_closure::Pattern::Var(var) => {
                        let local = builder.get_local(*var);
                        state.push_binder(Binder::LetBinding {
                            place: BindingPlace::Local(local),
                        });
                        lower_expr(value, ctx, builder, state);
                    }
                    ir_closure::Pattern::Tuple(vars) => {
                        let tuple_assign_rhs = new_local(ctx, builder, "tuple_assign_rhs", ty);
                        state.push_binder(Binder::LetBinding {
                            place: BindingPlace::Local(tuple_assign_rhs),
                        });
                        lower_expr(value, ctx, builder, state);
                        for (tuple_index, var) in vars.iter_enumerated() {
                            let local = builder.get_local(*var);
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
                }
                // At this point, the value has been bound to the pattern.

                // Continue lowering the following expression.
                // Nested `Let` structures are fully handled by this recursive call.
                lower_expr(follows, ctx, builder, state);
                return;
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
            ir_closure::ExprKind::App(apply_kind, fn_name, args) => {
                let args = args
                    .iter()
                    .map(|arg| builder.get_local(*arg))
                    .collect::<IndexVec<_, _>>();
                break 'bindee Bindee::Call {
                    calling_conv: *apply_kind,
                    fn_name: *fn_name,
                    args,
                    ty,
                };
            }
            ir_closure::ExprKind::If(e1, e2, e3) => {
                // In general, it's not a good idea to type invalid
                // values, but it makes it easier to see the flow here.
                let bb_dummy = BasicBlock::ENTRY_BLOCK;

                let condition = builder.get_local(*e1);
                let bb_before_if = builder.finish_block(TerminatorKind::ConditionalBranch {
                    condition,
                    targets: [bb_dummy, bb_dummy],
                });
                let binder_of_if = state.pop_innermost_binder();
                let new_binder = binder_of_if.to_if_arms_binder(bb_dummy);

                let bb_true_block = builder.next_basic_block();
                lower_expr(e2, ctx, builder, &mut State::with_one_binder(new_binder));

                let bb_false_block = builder.next_basic_block();
                lower_expr(e3, ctx, builder, &mut State::with_one_binder(new_binder));

                let bb_after_if = builder.next_basic_block();
                if let Binder::LetBinding {
                    place: BindingPlace::Local(local),
                } = binder_of_if
                {
                    builder.set_args(IndexVec::from_raw_vec(vec![local]));
                }

                // Remove the dummy.
                *builder.basic_blocks_mut()[bb_before_if]
                    .terminator
                    .as_mut_conditional_branch_targets()
                    .unwrap() = [bb_true_block, bb_false_block];
                struct RemoveDummy {
                    bb_dummy: BasicBlock,
                    target: BasicBlock,
                }
                impl MutVisitor<'_> for RemoveDummy {
                    fn visit_basic_block(&mut self, basic_block: &mut BasicBlock) {
                        if basic_block == &self.bb_dummy {
                            *basic_block = self.target;
                        }
                    }
                }
                RemoveDummy {
                    bb_dummy,
                    target: bb_after_if,
                }
                .super_ranged_block_data(
                    builder.basic_blocks_mut(),
                    Range::from(bb_true_block..bb_after_if),
                );
                return;
            }
        };
        Bindee::Expr { expr, ty }
    };
    // We have reached the block tail expression or a call.
    // Simply bind it to the current binding.
    let innermost_binder = state.pop_innermost_binder();
    innermost_binder.bind(ctx, builder, bindee)
}
