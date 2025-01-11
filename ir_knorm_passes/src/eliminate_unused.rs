use ir_knorm::{Context, Expr, ExprKind, Ident, MutVisitor, Visitor};

use crate::KnormPass;

/// Check if the expression has side effects.
fn is_pure(expr: &Expr<'_>) -> bool {
    struct SideEffectVisitor {
        pub has_side_effect: bool,
    }

    impl<'ctx> Visitor<'ctx> for SideEffectVisitor {
        fn visit_expr_kind(&mut self, expr: &ExprKind<'ctx>) {
            if let ExprKind::Set(..) | ExprKind::App(..) = expr {
                self.has_side_effect = true;
            } else {
                self.super_expr_kind(expr);
            }
        }
    }

    let mut visitor = SideEffectVisitor {
        has_side_effect: false,
    };
    visitor.visit_expr(expr);

    !visitor.has_side_effect
}

/// Check if the variable is used in the expression.
fn is_every_var_free<'ctx>(vars: &[Ident<'ctx>], expr: &Expr<'ctx>) -> bool {
    struct AppearanceVisitor<'ctx, 'a> {
        vars: &'a [Ident<'ctx>],
        appeared: bool,
    }

    impl<'ctx, 'a> Visitor<'ctx> for AppearanceVisitor<'ctx, 'a> {
        fn visit_ident(&mut self, ident: &Ident<'ctx>) {
            if self.vars.iter().any(|x| x == ident) {
                self.appeared = true;
            }
        }
    }

    let mut visitor = AppearanceVisitor {
        vars,
        appeared: false,
    };
    visitor.visit_expr(expr);

    !visitor.appeared
}

pub struct EliminateUnused;

impl<'ctx> KnormPass<'ctx> for EliminateUnused {
    fn run_pass(&mut self, ctx: &'ctx middleware::GlobalContext<'ctx>, expr: &mut Expr<'ctx>) {
        struct EliminationVisitor<'ctx> {
            ctx: &'ctx Context<'ctx>,
        }

        impl<'ctx> MutVisitor<'ctx> for EliminationVisitor<'ctx> {
            fn visit_expr(&mut self, expr: &mut Expr<'ctx>) {
                match &mut expr.value {
                    ExprKind::Let(let_expr) if is_pure(let_expr.binding.bindee()) => {
                        let should_eliminate = match &let_expr.binding.pattern {
                            ir_knorm::Pattern::Var(x) => is_every_var_free(&[*x], let_expr.body()),
                            ir_knorm::Pattern::Tuple(xs) => is_every_var_free(xs, let_expr.body()),
                            ir_knorm::Pattern::Unit => true,
                        };

                        if should_eliminate {
                            // Skip the binding and directly visit the continuation.
                            *expr = self.ctx.new_expr(let_expr.body_mut().take());

                            self.visit_expr(expr);
                        }
                    }
                    _ => self.super_expr(expr),
                }
            }
        }

        EliminationVisitor {
            ctx: ctx.knorm_context(),
        }
        .visit_expr(expr);
    }
}
