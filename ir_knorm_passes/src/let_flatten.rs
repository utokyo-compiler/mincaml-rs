use ir_knorm::{Expr, ExprKind, MutVisitor};

use crate::KnormPass;

pub struct LetFlatten;

impl<'ctx> KnormPass<'ctx> for LetFlatten {
    fn run_pass(&mut self, ctx: &'ctx middleware::GlobalContext<'ctx>, expr: &mut Expr<'ctx>) {
        /// Extracts `let` binders from an [`Expr`]ession and returns a tail [`Expr`]ession.
        fn extract_binders<'ctx>(
            binders: &mut Vec<Expr<'ctx>>,
            mut expr: Expr<'ctx>,
        ) -> Expr<'ctx> {
            if let ExprKind::Let(let_expr) = &mut expr.value
                && !let_expr.binding.is_function()
            {
                let body = let_expr.take_body().unwrap();
                let new_bindee = extract_binders(binders, let_expr.binding.take_bindee().unwrap());
                let_expr.binding.set_bindee(new_bindee);
                binders.push(expr);
                extract_binders(binders, body)
            } else {
                expr
            }
        }

        /// Folds binders to reconstruct `let` chains.
        fn fold_binders<'ctx>(
            binders: impl DoubleEndedIterator<Item = Expr<'ctx>>,
            bottom: Expr<'ctx>,
        ) -> Expr<'ctx> {
            binders.rfold(bottom, |acc, mut binder| {
                let ExprKind::Let(let_expr) = &mut binder.value else {
                    unreachable!()
                };
                let_expr.set_body(acc);
                binder
            })
        }

        struct LookupVisitor<'ctx> {
            ctx: &'ctx ir_knorm::Context<'ctx>,
        }

        impl<'ctx> MutVisitor<'ctx> for LookupVisitor<'ctx> {
            fn visit_expr(&mut self, expr: &mut Expr<'ctx>) {
                if let ExprKind::Let(let_expr) = &mut expr.value {
                    if let_expr.binding.is_function() {
                        self.visit_expr(let_expr.binding.bindee_mut());
                        self.visit_expr(let_expr.body_mut());
                    } else if !let_expr.binding.bindee().kind().is_let() {
                        self.visit_expr(let_expr.body_mut());
                    } else {
                        let body = let_expr.take_body().unwrap();
                        let mut binders = Vec::new();
                        let bindee =
                            extract_binders(&mut binders, let_expr.binding.take_bindee().unwrap());
                        let_expr.binding.set_bindee(bindee);
                        binders.push(self.ctx.new_expr(expr.take()));
                        let new_expr = fold_binders(binders.into_iter(), body);
                        *expr = new_expr;
                        self.visit_expr(expr);
                    }
                } else {
                    self.super_expr(expr);
                }
            }
        }

        LookupVisitor {
            ctx: ctx.knorm_context(),
        }
        .visit_expr(expr);
    }
}
