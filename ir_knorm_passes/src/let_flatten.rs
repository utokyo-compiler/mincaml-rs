use ir_knorm::{Expr, MutVisitor, ExprKind};

use crate::KnormPass;

pub struct LetFlatten;

impl<'ctx> KnormPass<'ctx> for LetFlatten {
    fn run_pass(&mut self, ctx: &'ctx middleware::GlobalContext<'ctx>, expr: &mut Expr<'ctx>) {
        struct LookupVisitor<'ctx> {
            ctx: &'ctx ir_knorm::Context<'ctx>,
            outer_let: Vec<Expr<'ctx>>,
            last_result: Option<Expr<'ctx>>
        }

        impl<'ctx> MutVisitor<'ctx> for LookupVisitor<'ctx> {
            fn visit_expr(&mut self, expr: &mut Expr<'ctx>) {
                let t = expr.ty.clone();
                match &mut expr.value {
                    ExprKind::Let(binding, continuation) => {
                        self.visit_expr(continuation);

                        // Dummeis for swapping with the body of the let binding.
                        // Values do not reflect the actual type of the binding,
                        // but they will be reset or discarded later.
                        let mut inner_dummy = self.ctx.new_expr(ir_knorm::Typed::new(ExprKind::Const(ir_knorm::LitKind::Int(0)), t.clone()));
                        let mut expr_dummy = self.ctx.new_expr(ir_knorm::Typed::new(ExprKind::Const(ir_knorm::LitKind::Int(0)), t));

                        std::mem::swap(&mut inner_dummy, &mut binding.value);
                        std::mem::swap(&mut expr_dummy, expr);

                        self.outer_let.push(expr_dummy);
                        self.visit_expr(&mut inner_dummy);

                        *expr = self.last_result.take().unwrap();
                    }
                    _ => { // Not a let binding
                        if let Some(mut outer) = self.outer_let.pop() {
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
            outer_let: vec![],
            last_result: None
        }
        .visit_expr(expr);
    }
}
