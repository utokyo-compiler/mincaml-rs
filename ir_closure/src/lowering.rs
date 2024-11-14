use data_structure::{index::vec::IndexVec, FxHashMap, FxHashSet, SetLikeVec};

use crate::{
    context::Context, ApplyKind, ArgIndex, Closure, Expr, ExprKind, FnIndex, FnName, FunctionDef,
    FunctionInstance, Ident, LetBinding, Pattern, Program, Typed,
};

/// The main entrypoint of closure conversion.
///
/// The reader should understand the following:
///
/// 1. Whether the implementation used here is "やや賢い" or "もっと賢い"
/// 2. The distinction between "やや賢い" and "もっと賢い"
/// 3. 2. is a problem that should be handled by an optimization pass
pub fn lowering<'ctx>(ctx: &'ctx Context<'ctx>, knorm_expr: ir_knorm::Expr<'ctx>) -> Program<'ctx> {
    let mut state = LoweringState::default();
    let main = FunctionDef::new(
        FnName::MAIN_FN_NAME,
        IndexVec::new(),
        IndexVec::new(),
        false,
    );
    let main_index = state.push_function(main);
    let main_body = lower_expr(ctx, &mut state, &knorm_expr);
    state.functions[main_index].set_body(main_body);
    Program {
        functions: state.functions,
    }
}

#[derive(Default)]
struct LoweringState<'ctx> {
    knowledge: DirectCallKnowledge<'ctx>,
    functions: IndexVec<FnIndex, FunctionDef<'ctx>>,
    function_resolutions: FxHashMap<Ident<'ctx>, FnIndex>,
}

impl<'ctx> LoweringState<'ctx> {
    fn push_function(&mut self, func: FunctionDef<'ctx>) -> FnIndex {
        let name = func.name;
        let idx = self.functions.push(func);
        if let Some(name) = name.get_inner() {
            self.function_resolutions.insert(name, idx);
        }
        idx
    }

    fn resolve_function(&self, fn_name: FnName<'ctx>) -> FunctionInstance<'ctx> {
        let Some(fn_name) = fn_name.get_inner() else {
            unreachable!("main function cannot be called")
        };
        match self.function_resolutions.get(&fn_name) {
            Some(resolution) => FunctionInstance::Defined(*resolution),
            None => FunctionInstance::Imported(fn_name.as_intrinsic().unwrap()),
        }
    }
}

#[derive(Default)]
struct DirectCallKnowledge<'ctx> {
    decided_to_call_directly: FxHashSet<FnName<'ctx>>,
}

impl<'ctx> DirectCallKnowledge<'ctx> {
    /// Returns `true` if the function is going to be called directly.
    fn call_directly(&self, fn_name: &FnName<'ctx>) -> bool {
        if let Some(ident) = fn_name.get_inner() {
            if ident.as_intrinsic().is_some() {
                return true;
            }
        }
        self.decided_to_call_directly.contains(fn_name)
    }

    fn ack_decide_to_call_directly(&mut self, fn_name: FnName<'ctx>) {
        self.decided_to_call_directly.insert(fn_name);
    }
}

fn lower_expr<'ctx>(
    ctx: &'ctx Context<'ctx>,
    state: &mut LoweringState<'ctx>,
    knorm_expr: &ir_knorm::Expr<'ctx>,
) -> Expr<'ctx> {
    let expr_kind = match knorm_expr.kind() {
        ir_knorm::ExprKind::Const(lit_kind) => ExprKind::Const(*lit_kind),
        ir_knorm::ExprKind::Unary(un_op, e) => ExprKind::Unary(*un_op, *e),
        ir_knorm::ExprKind::Binary(bin_op, e1, e2) => ExprKind::Binary(*bin_op, *e1, *e2),
        ir_knorm::ExprKind::If(interned, e1, e2) => ExprKind::If(
            *interned,
            lower_expr(ctx, state, e1),
            lower_expr(ctx, state, e2),
        ),
        ir_knorm::ExprKind::Let(
            binding @ ir_knorm::LetBinding {
                pattern,
                args,
                value,
            },
            follows,
        ) => {
            let pattern = pattern.clone();
            let (value, follows) = if args.is_empty() {
                // Let or LetTuple
                let value = lower_expr(ctx, state, value);
                let follows = lower_expr(ctx, state, follows);
                (value, follows)
            } else {
                // LetRec
                let fn_name = FnName::new(pattern.as_var().unwrap());
                let LetRecAnalysisResult {
                    did_fn_used_as_value,
                    fv_set,
                } = analyze_let_rec(&state.knowledge, binding, follows);

                let decide_to_call_directly = !did_fn_used_as_value && fv_set.is_empty();

                if decide_to_call_directly {
                    // DO NOT call `lower_expr` on `body`
                    // before `ack_decide_to_make_closure`.
                    state.knowledge.ack_decide_to_call_directly(fn_name);
                }
                let fv_set: IndexVec<_, _> = fv_set.into_iter().collect();
                let func = FunctionDef::new(
                    fn_name,
                    args.clone(),
                    fv_set.clone(),
                    !state.knowledge.call_directly(&fn_name),
                );
                let index = state.push_function(func);
                let body = lower_expr(ctx, state, value);
                state.functions[index].set_body(body);

                let follows = lower_expr(ctx, state, follows);

                if state.knowledge.call_directly(&fn_name) {
                    // The function is not used as a value and does not capture any variables

                    // Remove the binding. Calling this function is allowed
                    // only if the `App` has `ApplyKind::Direct`, so we do not
                    // need to keep the binding.
                    return follows;
                } else {
                    let value = ctx.new_expr(Typed::new(
                        ExprKind::ClosureMake(Closure {
                            function: state.resolve_function(fn_name),
                            captured_args: fv_set,
                        }),
                        value.ty,
                    ));
                    (value, follows)
                }
            };
            ExprKind::Let(LetBinding { pattern, value }, follows)
        }
        ir_knorm::ExprKind::Var(var) => ExprKind::Var(*var),
        ir_knorm::ExprKind::App(f, args) => {
            let fn_name = FnName::new(*f);
            ExprKind::App(
                if state.knowledge.call_directly(&fn_name) {
                    ApplyKind::Direct {
                        function: state.resolve_function(fn_name),
                    }
                } else {
                    ApplyKind::Closure { ident: *f }
                },
                args.clone(),
            )
        }
        ir_knorm::ExprKind::Tuple(es) => ExprKind::Tuple(es.clone()),
        ir_knorm::ExprKind::ArrayMake(len, init) => ExprKind::ArrayMake(*len, *init),
        ir_knorm::ExprKind::Get(base, index) => ExprKind::Get(*base, *index),
        ir_knorm::ExprKind::Set(base, index, value) => ExprKind::Set(*base, *index, *value),
        ir_knorm::ExprKind::Invalid => unreachable!("invalid expression"),
    };
    ctx.new_expr(Typed::new(expr_kind, knorm_expr.ty))
}

struct LetRecAnalysisResult<'ctx> {
    did_fn_used_as_value: bool,
    /// Free variables of the binding body.
    ///
    /// The function name and the arguments are not included
    /// because they are bound by the binding.
    ///
    /// N.B. `!did_fn_used_as_value && fv_set.is_empty()` is equivalent
    /// to `fv_set.is_empty()` if we do not remove the function name
    /// `fn_name` from `fv_set` at the end of `analyze_let_rec`,
    /// but we remove it to make the process easier to understand.
    ///
    /// This is not a free variable set of the `LetRec`
    /// expression of `KNormal.t` in terms of the original reference.
    ///
    /// This variable corresponds to `zts` defined in `Closure.g`.
    fv_set: SetLikeVec<Ident<'ctx>>,
}

/// Analyzes `LetRec` to decide whether to make a closure.
///
/// Original implementation tries closure conversion and reverts
/// their mutable state if it fails (it is a bad idea in general
/// to have side effects in computations that can be backtracked).
/// In this implementation, we know whether closure conversion
/// will fail by analyzing in advance.
fn analyze_let_rec<'ctx>(
    frozen_knowledge: &DirectCallKnowledge<'ctx>,
    binding: &ir_knorm::LetBinding<'ctx>,
    follows: &ir_knorm::Expr<'ctx>,
) -> LetRecAnalysisResult<'ctx> {
    let fn_name = binding.pattern.as_var().unwrap();
    let mut fv_set = SetLikeVec::default();
    let mut collect_fv = ir_knorm::FvVisitorHelper::new({
        struct CollectFvVisitor<'a, 'ctx> {
            fv_set: &'a mut SetLikeVec<Ident<'ctx>>,
        }

        impl<'a, 'ctx> ir_knorm::FvVisitor<'ctx> for CollectFvVisitor<'a, 'ctx> {
            fn visit_fv(&mut self, ident: Ident<'ctx>) {
                self.fv_set.insert(ident);
            }
        }

        CollectFvVisitor {
            fv_set: &mut fv_set,
        }
    });
    {
        /// Collect free variables of the binding body.
        struct AnalyzeBindingVisitor<'ctx, 'k, 'helper, F: ir_knorm::FvVisitor<'ctx>> {
            frozen_knowledge: &'k DirectCallKnowledge<'ctx>,
            collect_fv: &'helper mut ir_knorm::FvVisitorHelper<'ctx, F>,
            fn_name: Ident<'ctx>,
        }
        use ir_knorm::Visitor;
        let mut binding_visitor = AnalyzeBindingVisitor {
            frozen_knowledge,
            collect_fv: &mut collect_fv,
            fn_name,
        };
        binding_visitor.visit_binding(binding);

        impl<'ctx, F: ir_knorm::FvVisitor<'ctx>> Visitor<'ctx> for AnalyzeBindingVisitor<'ctx, '_, '_, F> {
            fn visit_app(&mut self, e: &Ident<'ctx>, es: &IndexVec<ArgIndex, Ident<'ctx>>) {
                // If the function `e` is the function we are analyzing or
                // if the function `e` is going to be called directly,
                // we do not need to collect it as a free variable.
                //
                // This condition cannot be merged, because:
                // - we won't know whether `fn_name` can be called directly
                // - a function other than `fn_name` can be called directly
                if *e == self.fn_name || self.frozen_knowledge.call_directly(&FnName::new(*e)) {
                    // skip
                } else {
                    self.visit_ident(e);
                }
                for e in es {
                    self.visit_ident(e);
                }
            }
            fn visit_ident(&mut self, ident: &Ident<'ctx>) {
                self.collect_fv.super_ident(ident);
            }
            fn visit_binding(&mut self, binding: &ir_knorm::LetBinding<'ctx>) {
                self.visit_pattern(&binding.pattern);
                self.collect_fv.super_binding_args(&binding.args);
                self.visit_expr(&binding.value);
            }
            fn visit_pattern(&mut self, pattern: &Pattern<'ctx>) {
                // initial binding
                if let Some(var) = pattern.as_var() {
                    if var == self.fn_name {
                        return;
                    }
                }
                self.collect_fv.super_pattern(pattern);
            }
        }
    }
    {
        /// Check whether the function is used as a value outside of the binding.
        struct AnalyzeFollowingVisitor<'ctx, 'helper, F: ir_knorm::FvVisitor<'ctx>> {
            collect_fv: &'helper mut ir_knorm::FvVisitorHelper<'ctx, F>,
            fn_name: Ident<'ctx>,
        }
        use ir_knorm::Visitor;
        let mut following_visitor = AnalyzeFollowingVisitor {
            collect_fv: &mut collect_fv,
            fn_name,
        };
        following_visitor.visit_expr(follows);

        impl<'ctx, F: ir_knorm::FvVisitor<'ctx>> Visitor<'ctx> for AnalyzeFollowingVisitor<'ctx, '_, F> {
            fn visit_app(&mut self, _e: &Ident<'ctx>, args: &IndexVec<ArgIndex, Ident<'ctx>>) {
                for arg in args {
                    self.visit_ident(arg);
                }
            }
            fn visit_ident(&mut self, ident: &Ident<'ctx>) {
                if *ident == self.fn_name {
                    self.collect_fv.super_ident(ident);
                }
            }
            fn visit_binding(&mut self, binding: &ir_knorm::LetBinding<'ctx>) {
                // optimization: no need to visit the binder
                self.visit_expr(&binding.value);
            }
        }
    }

    LetRecAnalysisResult {
        did_fn_used_as_value: fv_set.remove(&fn_name).is_some(),
        fv_set,
    }
}
