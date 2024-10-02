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
    let main = FunctionDef {
        name: FnName::MAIN_FN_NAME,
        args: IndexVec::new(),
        args_via_closure: IndexVec::new(),
        body: lower_expr(ctx, &mut state, &knorm_expr),
    };
    state.push_function(main);
    Program {
        functions: state.functions,
    }
}

#[derive(Default)]
struct LoweringState<'ctx> {
    decided_to_make_closure: FxHashSet<FnName<'ctx>>,
    functions: IndexVec<FnIndex, FunctionDef<'ctx>>,
    function_resolutions: FxHashMap<FnName<'ctx>, FnIndex>,
}

impl<'ctx> LoweringState<'ctx> {
    fn push_function(&mut self, func: FunctionDef<'ctx>) {
        let name = func.name;
        let idx = self.functions.push(func);
        self.function_resolutions.insert(name, idx);
    }

    fn resolve_function(&self, fn_name: FnName<'ctx>) -> FunctionInstance<'ctx> {
        match self.function_resolutions.get(&fn_name) {
            Some(resolution) => FunctionInstance::Defined(*resolution),
            None => FunctionInstance::Imported(fn_name),
        }
    }

    fn decided_to_make_closure(&self, fn_name: &FnName<'ctx>) -> bool {
        self.decided_to_make_closure.contains(fn_name)
    }

    fn ack_decide_to_make_closure(&mut self, fn_name: FnName<'ctx>) {
        self.decided_to_make_closure.insert(fn_name);
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
                } = analyze_let_rec(binding);

                let decide_to_make_closure = did_fn_used_as_value || !fv_set.is_empty();

                if decide_to_make_closure {
                    // DO NOT call `lower_expr` on `body`
                    // before `ack_decide_to_make_closure`.
                    state.ack_decide_to_make_closure(fn_name);
                }
                let fv_set: IndexVec<_, _> = fv_set.into_iter().collect();
                let func = FunctionDef {
                    name: fn_name,
                    args: args.clone(),
                    args_via_closure: fv_set.clone(),
                    body: lower_expr(ctx, state, value),
                };
                state.push_function(func);

                let follows = lower_expr(ctx, state, follows);

                if state.decided_to_make_closure(&fn_name) {
                    let value = ctx.new_expr(Typed::new(
                        ExprKind::ClosureMake(Closure {
                            function: state.resolve_function(fn_name),
                            captured_args: fv_set,
                        }),
                        value.ty,
                    ));
                    (value, follows)
                } else {
                    // the function is not used as a value and does not capture any variables

                    // remove the binding. Calling this function is allowed
                    // only if the `App` has `ApplyKind::Direct`, so we do not
                    // need to keep the binding.
                    return follows;
                }
            };
            ExprKind::Let(LetBinding { pattern, value }, follows)
        }
        ir_knorm::ExprKind::Var(var) => ExprKind::Var(*var),
        ir_knorm::ExprKind::App(f, args) => {
            let fn_name = FnName::new(*f);
            ExprKind::App(
                if state.decided_to_make_closure(&fn_name) {
                    ApplyKind::Closure { ident: *f }
                } else {
                    ApplyKind::Direct {
                        function: state.resolve_function(fn_name),
                    }
                },
                args.clone(),
            )
        }
        ir_knorm::ExprKind::Tuple(es) => ExprKind::Tuple(es.clone()),
        ir_knorm::ExprKind::ArrayMake(_, _) => todo!(),
        ir_knorm::ExprKind::Get(_, _) => todo!(),
        ir_knorm::ExprKind::Set(_, _, _) => todo!(),
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

/// Analyzes LetRec binding to decide whether to make a closure.
///
/// Original implementation tries closure conversion and reverts
/// their mutable state if it fails (it is a bad idea in general
/// to have side effects in computations that can be backtracked).
/// In this implementation, we know whether closure conversion
/// will fail by analyzing in advance.
fn analyze_let_rec<'ctx>(binding: &ir_knorm::LetBinding<'ctx>) -> LetRecAnalysisResult<'ctx> {
    let fn_name = binding.pattern.as_var().unwrap();
    let mut fv_set = SetLikeVec::default();
    let visitor_helper = ir_knorm::FvVisitorHelper::new({
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
        struct AnalysisVisitor<'ctx, F: ir_knorm::FvVisitor<'ctx>> {
            visitor_helper: ir_knorm::FvVisitorHelper<'ctx, F>,
            fn_name: Ident<'ctx>,
        }
        use ir_knorm::Visitor;
        let mut visitor = AnalysisVisitor {
            visitor_helper,
            fn_name,
        };
        visitor.visit_binding(binding);

        impl<'ctx, F: ir_knorm::FvVisitor<'ctx>> ir_knorm::Visitor<'ctx> for AnalysisVisitor<'ctx, F> {
            fn visit_app(&mut self, e: &Ident<'ctx>, es: &IndexVec<ArgIndex, Ident<'ctx>>) {
                if *e != self.fn_name {
                    self.visit_ident(e);
                }
                for e in es {
                    self.visit_ident(e);
                }
            }
            fn visit_ident(&mut self, ident: &Ident<'ctx>) {
                self.visitor_helper.super_ident(ident);
            }
            fn visit_binding(&mut self, binding: &ir_knorm::LetBinding<'ctx>) {
                self.visit_pattern(&binding.pattern);
                self.visitor_helper.super_binding_args(&binding.args);
                self.visit_expr(&binding.value);
            }
            fn visit_pattern(&mut self, pattern: &Pattern<'ctx>) {
                if let Some(var) = pattern.as_var() {
                    if var == self.fn_name {
                        return;
                    }
                }
                self.visitor_helper.super_pattern(pattern);
            }
        }
    }

    LetRecAnalysisResult {
        did_fn_used_as_value: fv_set.remove(&fn_name).is_some(),
        fv_set,
    }
}
