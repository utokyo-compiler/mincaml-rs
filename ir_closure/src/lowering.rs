use data_structure::{index::vec::IndexVec, FxHashMap, FxHashSet, FxIndexSet};

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
    knowledge: FunctionKnowledge<'ctx>,
    functions: IndexVec<FnIndex, FunctionDef<'ctx>>,
    function_resolutions: FxHashMap<FnName<'ctx>, FnIndex>,
    letrec_binders: FxHashSet<FnName<'ctx>>,
}

impl<'ctx> LoweringState<'ctx> {
    fn push_function(&mut self, func: FunctionDef<'ctx>) -> FnIndex {
        let name = func.name;
        let idx = self.functions.push(func);
        self.function_resolutions.insert(name, idx);
        idx
    }

    fn resolve_function(&self, fn_name: &FnName<'ctx>) -> FunctionInstance<'ctx> {
        match self.function_resolutions.get(fn_name) {
            Some(resolution) => FunctionInstance::Defined(*resolution),
            None => FunctionInstance::Imported(
                fn_name
                    .get_inner()
                    .expect("unbound function")
                    .as_intrinsic()
                    .expect("unbound non-intrinsic function"),
            ),
        }
    }

    /// Returns the function instance if the function is going to be called directly at the call site.
    fn call_directly(&self, fn_name: &FnName<'ctx>) -> Option<FunctionInstance<'ctx>> {
        if match self.knowledge.of(fn_name) {
            FunctionKnowledgeKind::CallDirectly => true,
            FunctionKnowledgeKind::MitigatedClosure {
                mitigation: Mitigation::InsideOfSelf,
            } => self.letrec_binders.contains(fn_name),
            FunctionKnowledgeKind::CallViaClosure => false,
        } {
            Some(self.resolve_function(fn_name))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionKnowledgeKind {
    /// The function is going to be called directly and no closure is needed.
    CallDirectly,

    /// The function needs a closure but can be called directly in certain cases.
    ///
    /// Why this variant needed? : This is an optimization - There is a case where a function has no free variables
    /// but still needs a closure, e.g., the function used as a value after the function is created.
    /// In that case, we can call *the* inner function directly where we know which function is called.
    MitigatedClosure { mitigation: Mitigation },

    /// The variable is a function and is going to be called via a closure, or the variable is a closure.
    CallViaClosure,
}

impl FunctionKnowledgeKind {
    fn is_closure(self) -> bool {
        matches!(self, Self::MitigatedClosure { .. } | Self::CallViaClosure)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// See [`FunctionKnowledgeKind::MitigatedClosure`].
enum Mitigation {
    /// Inside of the function itself.
    InsideOfSelf,
}

#[derive(Default)]
struct FunctionKnowledge<'ctx> {
    map: FxHashMap<FnName<'ctx>, FunctionKnowledgeKind>,
}

impl<'ctx> FunctionKnowledge<'ctx> {
    /// Returns the knowledge of the function.
    fn of(&self, fn_name: &FnName<'ctx>) -> FunctionKnowledgeKind {
        if let Some(ident) = fn_name.get_inner() {
            if ident.as_intrinsic().is_some() {
                // Intrinsic functions are always called directly.
                return FunctionKnowledgeKind::CallDirectly;
            }
        }
        self.map
            .get(fn_name)
            .copied()
            .unwrap_or(FunctionKnowledgeKind::CallViaClosure)
    }

    fn ack_decision(&mut self, fn_name: FnName<'ctx>, kind: FunctionKnowledgeKind) {
        self.map.insert(fn_name, kind);
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
                let LetRecAnalysisResult { fv_set } = analyze_let_rec(state, binding, follows);

                let fv_set: IndexVec<_, _> = fv_set.into_iter().collect();
                let func = FunctionDef::new(
                    fn_name,
                    args.clone(),
                    fv_set.clone(),
                    state.knowledge.of(&fn_name).is_closure(),
                );
                let index = state.push_function(func);
                state.letrec_binders.insert(fn_name);
                let body = lower_expr(ctx, state, value);
                state.letrec_binders.remove(&fn_name);
                state.functions[index].set_body(body);

                if !state.knowledge.of(&fn_name).is_closure() {
                    // The function is not used as a value and does not capture any variables.

                    // Remove the binding. Calling this function is allowed
                    // iff the `App` has `ApplyKind::Direct`, so we do not
                    // need to keep the binding.

                    // optimization: tail call
                    return lower_expr(ctx, state, follows);
                } else {
                    let follows = lower_expr(ctx, state, follows);
                    let value = ctx.new_expr(Typed::new(
                        ExprKind::ClosureMake(Closure {
                            function: FunctionInstance::Defined(index),
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
                if let Some(function) = state.call_directly(&fn_name) {
                    ApplyKind::Direct { function }
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
    fv_set: FxIndexSet<Ident<'ctx>>,
}

/// Analyzes `LetRec` to decide whether to make a closure.
///
/// Original implementation tries closure conversion and reverts
/// their mutable state if it fails (it is a bad idea in general
/// to have side effects in computations that can be backtracked).
/// In this implementation, we know whether closure conversion
/// will fail by analyzing in advance.
fn analyze_let_rec<'ctx>(
    state: &mut LoweringState<'ctx>,
    binding: &ir_knorm::LetBinding<'ctx>,
    follows: &ir_knorm::Expr<'ctx>,
) -> LetRecAnalysisResult<'ctx> {
    let fn_name = binding.pattern.as_var().unwrap();
    let mut fv_set = FxIndexSet::default();
    let mut collect_fv = ir_knorm::FvVisitorHelper::new({
        struct CollectFvVisitor<'a, 'ctx> {
            fv_set: &'a mut FxIndexSet<Ident<'ctx>>,
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
        struct AnalyzeBindingVisitor<'ctx, 'state, 'helper, F: ir_knorm::FvVisitor<'ctx>> {
            frozen_state: &'state LoweringState<'ctx>,
            collect_fv: &'helper mut ir_knorm::FvVisitorHelper<'ctx, F>,
            fn_name: Ident<'ctx>,
        }
        use ir_knorm::Visitor;
        let mut binding_visitor = AnalyzeBindingVisitor {
            frozen_state: state,
            collect_fv: &mut collect_fv,
            fn_name,
        };
        binding_visitor.visit_binding(binding);

        impl<'ctx, F: ir_knorm::FvVisitor<'ctx>> Visitor<'ctx> for AnalyzeBindingVisitor<'ctx, '_, '_, F> {
            fn visit_app(&mut self, e: &Ident<'ctx>, es: &IndexVec<ArgIndex, Ident<'ctx>>) {
                // If `e` is the function we are analyzing or `e` is going to be called directly,
                // we do not need to count it as a free occurrence.
                //
                // This condition cannot be merged, because:
                // - we won't know whether `fn_name` can be called directly
                // - a function other than `fn_name` can be called directly
                if *e == self.fn_name || self.frozen_state.call_directly(&FnName::new(*e)).is_some()
                {
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

    let fn_used_as_value = fv_set.shift_remove(&fn_name);
    let fn_name = FnName::new(fn_name);

    // Make a decision.
    let decision = if fv_set.is_empty() {
        if !fn_used_as_value {
            FunctionKnowledgeKind::CallDirectly
        } else {
            // The function is used as a value but does not capture any variables.
            // In this case, we can call the function directly inside of itself.
            FunctionKnowledgeKind::MitigatedClosure {
                mitigation: Mitigation::InsideOfSelf,
            }
        }
    } else {
        FunctionKnowledgeKind::CallViaClosure
    };

    state.knowledge.ack_decision(fn_name, decision);

    LetRecAnalysisResult { fv_set }
}
