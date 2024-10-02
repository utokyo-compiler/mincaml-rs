use ir_knorm::{Context, Expr, ExprKind, Ident, MutVisitor, Visitor};

use crate::KnormPass;

fn is_pure<'ctx>(expr: &Expr<'ctx>) -> bool {
    struct SideEffectVisitor {
        pub has_side_effect: bool,
    }

    impl<'ctx> Visitor<'ctx> for SideEffectVisitor {
        fn visit_expr_kind(&mut self, expr: &ExprKind<'ctx>) {
            match expr {
                ExprKind::Set(_, _, _) | ExprKind::App(_, _) => self.has_side_effect = true,
                _ => self.super_expr_kind(expr),
            }
        }
    }

    let mut visitor = SideEffectVisitor { has_side_effect: false };
    visitor.visit_expr(expr);

    !visitor.has_side_effect
}

// check if the variable is used in the expression
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

    let mut visitor = AppearanceVisitor { vars, appeared: false };
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
                    ExprKind::Let(binding, continuation) if is_pure(&binding.value) => {
                        let should_eliminate: bool = match &binding.pattern {
                            ir_knorm::Pattern::Var(x) => is_every_var_free(&[*x], continuation),
                            ir_knorm::Pattern::Tuple(xs) => is_every_var_free(xs, continuation),
                            ir_knorm::Pattern::Unit => true
                        };

                        if should_eliminate {
                            let mut dummy = self.ctx.new_expr(ir_knorm::Typed::new(ExprKind::Const(ir_knorm::LitKind::Int(0)), binding.value.ty.clone()));
                            std::mem::swap(&mut dummy, continuation);
                            *expr = dummy;
                        }
                    }
                    _ => self.super_expr(expr),
                }
            }
        }

        EliminationVisitor {
            ctx: ctx.knorm_context()
        }.visit_expr(expr);
    }
}