mod label;

use data_structure::index::vec::IndexVec;
use label::{Label, LabelResolution, ResolveHandler, TerminatorCtor};

use crate::{
    builder, AbsCallingConv, ArgIndex, BasicBlock, Branch, Closure, Context, DisambiguatedIdent,
    ExprKind, FnName, LitKind, Local, Place, ProjectionKind, StmtKind, TerminatorKind, Ty, Typed,
};

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
/// Generalized binding agent. You can think of it as a defunctionalized
/// version of the following trait:
///
/// ```no_run
/// trait PlaceBinder<'ctx> {
///     fn bind(
///         self,
///         ctx: &'ctx Context<'ctx>,
///         builder: &mut builder::FunctionBuilder<'ctx>,
///         bindee: impl PlaceBindee<'ctx>,
///     );
/// }
/// ```
enum PlaceBinder {
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
        target: Label,
    },
}

impl PlaceBinder {
    fn to_if_arms_binder(self, label_after_if: Label) -> Self {
        match self {
            // `Binder::Branch` is created here first.
            Self::LetBinding { .. } => Self::Branch {
                target: label_after_if,
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
enum PlaceBindee<'ctx> {
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

impl PlaceBinder {
    /// Bind the given expression to the current binding.
    fn bind<'ctx>(
        self,
        ctx: &'ctx Context<'ctx>,
        state: &mut State<'_, 'ctx>,
        bindee: PlaceBindee<'ctx>,
    ) {
        match self {
            PlaceBinder::LetBinding { place } => {
                match bindee {
                    PlaceBindee::Expr { expr, ty } => {
                        state.builder.push_stmt_to_current(StmtKind::Assign {
                            place: place.into_place(),
                            value: ctx.new_expr(Typed::new(expr, ty)),
                        });
                    }
                    PlaceBindee::Call {
                        calling_conv,
                        fn_name,
                        args,
                        ..
                    } => {
                        let target = state.builder.next_basic_block();
                        state.builder.terminate_block(TerminatorKind::Call {
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
            PlaceBinder::FnReturn => {
                let return_value = match bindee {
                    PlaceBindee::Expr { expr, ty } => ctx.new_expr(Typed::new(expr, ty)),
                    PlaceBindee::Call {
                        calling_conv,
                        fn_name,
                        args,
                        ty,
                    } => {
                        let call_result = new_local(ctx, state, "call_result", ty);
                        let target = state.builder.next_basic_block();
                        state.builder.terminate_block(TerminatorKind::Call {
                            calling_conv,
                            fn_name,
                            args,
                            branch: Branch::no_args(target),
                        });
                        state
                            .builder
                            .set_args_to_current(IndexVec::from_raw_vec(vec![call_result]));
                        ctx.new_expr(Typed::new(ExprKind::Read(Place::Local(call_result)), ty))
                    }
                };
                state.builder.push_stmt_to_current(StmtKind::Assign {
                    place: Place::Local(Local::RETURN_LOCAL),
                    value: return_value,
                });
                state.builder.terminate_block(TerminatorKind::Return);
            }
            PlaceBinder::Branch { target } => match bindee {
                PlaceBindee::Expr { expr, ty } => {
                    let branch_arg = evaluated_local(ctx, state, "branch_arg", expr, ty);
                    state.defer_terminate_block(
                        TerminatorCtor::Branch {
                            target,
                            args: IndexVec::from_raw_vec(vec![branch_arg]),
                        },
                        target,
                    );
                }
                PlaceBindee::Call {
                    calling_conv,
                    fn_name,
                    args,
                    ty,
                } => {
                    let call_result = new_local(ctx, state, "call_result", ty);
                    state.defer_terminate_block(
                        TerminatorCtor::Call {
                            calling_conv,
                            fn_name,
                            args,
                            branch_target: target,
                            branch_args: IndexVec::from_raw_vec(vec![call_result]),
                        },
                        target,
                    );
                }
            },
        }
    }
}

fn new_local<'ctx>(
    ctx: &'ctx Context<'ctx>,
    state: &mut State<'_, 'ctx>,
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
    state.builder.get_local(ident)
}

fn evaluated_local<'ctx>(
    ctx: &'ctx Context<'ctx>,
    state: &mut State<'_, 'ctx>,
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
            let local = new_local(ctx, state, name, ty);
            state.builder.push_stmt_to_current(StmtKind::Assign {
                place: Place::Local(local),
                value: ctx.new_expr(Typed::new(expr, ty)),
            });
            local
        }
    }
}

/// The state of the lowering process.
pub struct State<'builder, 'ctx> {
    binders: Vec<PlaceBinder>,
    builder: &'builder mut builder::FunctionBuilder<'ctx>,
    label_resolution: LabelResolution<'builder, 'ctx>,
}

impl<'builder, 'ctx> State<'builder, 'ctx> {
    /// The only way to create a new `State` outside of this module.
    /// This means that lowering expressions should be capsuled in this module.
    pub fn new(builder: &'builder mut builder::FunctionBuilder<'ctx>) -> Self {
        Self {
            binders: vec![PlaceBinder::FnReturn],
            builder,
            label_resolution: LabelResolution::default(),
        }
    }

    /// Pop the innermost binder. This is a safe operation because
    /// all expressions must be bound to a binder.
    fn pop_innermost_binder(&mut self) -> PlaceBinder {
        self.binders.pop().expect("found an orphan bindee")
    }

    /// Add a new binder to the current binding.
    ///
    /// Nested `Let` structures are fully handled by this stack structure.
    fn push_binder(&mut self, binder: PlaceBinder) {
        self.binders.push(binder);
    }

    /// Resolve the label and run the handlers if any.
    fn resolve_label(&mut self, label: Label, basic_block: BasicBlock) {
        if let Some(handlers) = self.label_resolution.insert(label, basic_block) {
            for handler in handlers {
                handler.run(self);
            }
        }
    }

    /// Defer the creation of a terminator until the label is resolved.
    ///
    /// This call suspends the current block creation and moves to the next block.
    ///
    /// # Arguments
    ///
    /// * `ctor` - The constructor of the terminator.
    /// * `until_resolve` - The label of the latest created basic block.
    fn defer_terminate_block(&mut self, ctor: TerminatorCtor<'ctx>, until_resolve: Label) {
        let deferred = self.builder.defer_terminate_block();
        self.label_resolution
            .register(until_resolve, ResolveHandler::new(deferred, ctor));
    }
}

/// Lower the given expression.
///
/// The expression is lowered and bound to the current binding. The
/// binder is then popped.
pub fn lower_expr<'ctx>(
    expr: &ir_closure::Expr<'ctx>,
    ctx: &'ctx Context<'ctx>,
    state: &mut State<'_, 'ctx>,
) {
    let ty = expr.ty;
    let bindee = 'bindee: {
        let expr = match expr.kind() {
            // trivial cases
            ir_closure::ExprKind::Const(lit_kind) => ExprKind::Const(*lit_kind),
            ir_closure::ExprKind::Unary(un_op, e1) => {
                let e1 = state.builder.get_local(*e1);
                ExprKind::Unary(*un_op, e1)
            }
            ir_closure::ExprKind::Binary(bin_op, e1, e2) => {
                let e1 = state.builder.get_local(*e1);
                let e2 = state.builder.get_local(*e2);
                ExprKind::Binary(*bin_op, e1, e2)
            }
            ir_closure::ExprKind::ClosureMake(closure) => ExprKind::ClosureMake(Closure {
                fn_name: closure.fn_name,
                captured_args: closure
                    .captured_args
                    .iter()
                    .map(|arg| state.builder.get_local(*arg))
                    .collect(),
            }),
            ir_closure::ExprKind::Tuple(vec) => ExprKind::Tuple(
                vec.iter()
                    .map(|var| state.builder.get_local(*var))
                    .collect::<IndexVec<_, _>>(),
            ),
            ir_closure::ExprKind::ArrayMake(e1, e2) => {
                let e1 = state.builder.get_local(*e1);
                let e2 = state.builder.get_local(*e2);
                ExprKind::ArrayMake(e1, e2)
            }
            ir_closure::ExprKind::Var(e) => {
                let e = state.builder.get_local(*e);
                ExprKind::Read(Place::Local(e))
            }
            ir_closure::ExprKind::Get(e1, e2) => {
                let e1 = state.builder.get_local(*e1);
                let e2 = state.builder.get_local(*e2);
                ExprKind::Read(Place::Projection {
                    base: e1,
                    projection_kind: ProjectionKind::ArrayElem(e2),
                })
            }

            // stmt creation
            ir_closure::ExprKind::Let(ir_closure::LetBinding { pattern, value }, follows) => {
                match pattern {
                    ir_closure::Pattern::Unit => {
                        state.push_binder(PlaceBinder::LetBinding {
                            place: BindingPlace::Discard,
                        });
                        lower_expr(value, ctx, state);
                    }
                    ir_closure::Pattern::Var(var) => {
                        let local = state.builder.get_local(*var);
                        state.push_binder(PlaceBinder::LetBinding {
                            place: BindingPlace::Local(local),
                        });
                        lower_expr(value, ctx, state);
                    }
                    ir_closure::Pattern::Tuple(vars) => {
                        let tuple_assign_rhs = new_local(ctx, state, "tuple_assign_rhs", ty);
                        state.push_binder(PlaceBinder::LetBinding {
                            place: BindingPlace::Local(tuple_assign_rhs),
                        });
                        lower_expr(value, ctx, state);
                        for (tuple_index, var) in vars.iter_enumerated() {
                            let local = state.builder.get_local(*var);
                            state.builder.push_stmt_to_current(StmtKind::Assign {
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
                lower_expr(follows, ctx, state);
                return;
            }
            ir_closure::ExprKind::Set(base, displacement, value) => {
                let ty = value.ty;
                let base = state.builder.get_local(*base);
                let displacement = state.builder.get_local(*displacement);
                let value = state.builder.get_local(*value);
                state.builder.push_stmt_to_current(StmtKind::Assign {
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
                    .map(|arg| state.builder.get_local(*arg))
                    .collect::<IndexVec<_, _>>();
                break 'bindee PlaceBindee::Call {
                    calling_conv: *apply_kind,
                    fn_name: *fn_name,
                    args,
                    ty,
                };
            }
            ir_closure::ExprKind::If(e1, e2, e3) => {
                let condition = state.builder.get_local(*e1);

                let label_true_block = Label::new();
                let label_false_block = Label::new();
                let label_after_if = Label::new();

                // This basic block has a terminator which depends on the
                // basic blocks created by the branches. We need to defer
                // its creation.
                state.defer_terminate_block(
                    TerminatorCtor::ConditionalBranch {
                        condition,
                        targets: [label_true_block, label_false_block],
                    },
                    label_false_block,
                );

                let binder_of_if = state.pop_innermost_binder();
                let new_binder = binder_of_if.to_if_arms_binder(label_after_if);

                let bb_true_block = state.builder.next_basic_block();
                state.resolve_label(label_true_block, bb_true_block);

                state.push_binder(new_binder);
                lower_expr(e2, ctx, state);

                let bb_false_block = state.builder.next_basic_block();
                // This call will fire the deferred terminator creation.
                state.resolve_label(label_false_block, bb_false_block);

                state.push_binder(new_binder);
                lower_expr(e3, ctx, state);

                let bb_after_if = state.builder.next_basic_block();
                // This call will also fire the deferred terminator creation.
                state.resolve_label(label_after_if, bb_after_if);

                if let PlaceBinder::LetBinding {
                    place: BindingPlace::Local(local),
                } = binder_of_if
                {
                    state
                        .builder
                        .set_args_to_current(IndexVec::from_raw_vec(vec![local]));
                }

                return;
            }
        };
        PlaceBindee::Expr { expr, ty }
    };
    // We have reached the block tail expression or a call.
    // Simply bind it to the current binding.
    let innermost_binder = state.pop_innermost_binder();
    innermost_binder.bind(ctx, state, bindee)
}
