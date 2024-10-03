use ir_knorm::{Expr, ExprKind, MutVisitor};

use crate::KnormPass;

pub struct LetFlatten;

impl<'ctx> KnormPass<'ctx> for LetFlatten {
    fn run_pass(&mut self, ctx: &'ctx middleware::GlobalContext<'ctx>, expr: &mut Expr<'ctx>) {
        struct LookupVisitor<'ctx> {
            ctx: &'ctx ir_knorm::Context<'ctx>,
            outer_lets: Vec<Expr<'ctx>>,
            last_result: Option<Expr<'ctx>>,
        }

        impl<'ctx> MutVisitor<'ctx> for LookupVisitor<'ctx> {
            fn visit_expr(&mut self, expr: &mut Expr<'ctx>) {
                match &mut expr.value {
                    ExprKind::Let(binding, continuation) => {
                        self.visit_expr(continuation);

                        let mut new_inner = self.ctx.new_expr(binding.value.take());
                        let new_outer_let = self.ctx.new_expr(expr.take());

                        self.outer_lets.push(new_outer_let);
                        self.visit_expr(&mut new_inner);

                        *expr = self.last_result.take().unwrap();
                    }
                    _ => {
                        // Not a let binding
                        if let Some(mut outer) = self.outer_lets.pop() {
                            match &mut outer.value {
                                ExprKind::Let(b, _) => {
                                    std::mem::swap(&mut b.value, expr);
                                }
                                _ => {
                                    unreachable!()
                                }
                            }
                            self.last_result = Some(outer);
                        }
                    }
                }
            }
        }

        LookupVisitor {
            ctx: ctx.knorm_context(),
            outer_lets: vec![],
            last_result: None,
        }
        .visit_expr(expr);
    }
}
