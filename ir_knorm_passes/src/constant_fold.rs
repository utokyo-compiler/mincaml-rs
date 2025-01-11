use ir_knorm::{BinOp, Context, Expr, ExprKind, Ident, LetBinding, LitKind, MutVisitor, UnOp};
use middleware::{FxHashMap, GlobalContext};

use crate::KnormPass;

pub struct ConstantFold;

fn bin_op_fold(bin_op: BinOp, x: LitKind, y: LitKind) -> Option<LitKind> {
    use ir_knorm::{FloatBinOpKind as FOp, IntBinOpKind as IOp, RelationBinOpKind as ROp};
    use LitKind::{Bool, Float, Int};

    match (bin_op, x, y) {
        (BinOp::Relation(ROp::Eq), Bool(x), Bool(y)) => Some(Bool(x == y)),
        (BinOp::Relation(ROp::Ne), Bool(x), Bool(y)) => Some(Bool(x != y)),
        (BinOp::Relation(ROp::Eq), Int(x), Int(y)) => Some(Bool(x == y)),
        (BinOp::Relation(ROp::Ne), Int(x), Int(y)) => Some(Bool(x != y)),
        (BinOp::Relation(ROp::Eq), Float(x), Float(y)) => Some(Bool(x == y)),
        (BinOp::Relation(ROp::Ne), Float(x), Float(y)) => Some(Bool(x != y)),
        (BinOp::Relation(ROp::Ge), Int(x), Int(y)) => Some(Bool(x >= y)),
        (BinOp::Relation(ROp::Le), Int(x), Int(y)) => Some(Bool(x <= y)),
        (BinOp::Relation(ROp::Gt), Int(x), Int(y)) => Some(Bool(x > y)),
        (BinOp::Relation(ROp::Lt), Int(x), Int(y)) => Some(Bool(x < y)),
        (BinOp::Relation(ROp::Ge), Float(x), Float(y)) => Some(Bool(x >= y)),
        (BinOp::Relation(ROp::Le), Float(x), Float(y)) => Some(Bool(x <= y)),
        (BinOp::Relation(ROp::Gt), Float(x), Float(y)) => Some(Bool(x > y)),
        (BinOp::Relation(ROp::Lt), Float(x), Float(y)) => Some(Bool(x < y)),
        (BinOp::Int(IOp::Add), Int(x), Int(y)) => Some(Int(x + y)),
        (BinOp::Int(IOp::Sub), Int(x), Int(y)) => Some(Int(x - y)),
        (BinOp::Int(IOp::Mul), Int(x), Int(y)) => Some(Int(x * y)),
        (BinOp::Int(IOp::Div), Int(x), Int(y)) => Some(Int(x / y)),
        (BinOp::Float(FOp::FAdd), Float(x), Float(y)) => {
            Some(Float(f32::to_bits(f32::from_bits(x) + f32::from_bits(y))))
        }
        (BinOp::Float(FOp::FSub), Float(x), Float(y)) => {
            Some(Float(f32::to_bits(f32::from_bits(x) - f32::from_bits(y))))
        }
        (BinOp::Float(FOp::FMul), Float(x), Float(y)) => {
            Some(Float(f32::to_bits(f32::from_bits(x) * f32::from_bits(y))))
        }
        (BinOp::Float(FOp::FDiv), Float(x), Float(y)) => {
            Some(Float(f32::to_bits(f32::from_bits(x) / f32::from_bits(y))))
        }
        _ => None,
    }
}

impl<'ctx> KnormPass<'ctx> for ConstantFold {
    fn run_pass(&mut self, ctx: &'ctx GlobalContext<'ctx>, expr: &mut Expr<'ctx>) {
        struct ConstantFoldVisitor<'ctx> {
            ctx: &'ctx Context<'ctx>,
            env: FxHashMap<Ident<'ctx>, ir_knorm::LitKind>,
        }

        impl<'ctx> MutVisitor<'ctx> for ConstantFoldVisitor<'ctx> {
            fn visit_binding(&mut self, binding: &mut LetBinding<'ctx>) {
                if let Some(x) = binding.pattern.as_var() {
                    if let ExprKind::Const(lit) = binding.bindee().kind() {
                        self.env.insert(x, *lit);
                    }
                }

                self.super_binding(binding);
            }

            fn visit_expr(&mut self, expr: &mut Expr<'ctx>) {
                match &expr.value {
                    ExprKind::Unary(un_op, x) => match (un_op, self.env.get(x)) {
                        (UnOp::Ineg, Some(LitKind::Int(i))) => {
                            *expr = self.ctx.new_expr(ir_knorm::Typed::new(
                                ExprKind::Const(LitKind::Int(-i)),
                                expr.ty,
                            ));
                        }
                        (UnOp::Not, Some(LitKind::Bool(b))) => {
                            *expr = self.ctx.new_expr(ir_knorm::Typed::new(
                                ExprKind::Const(LitKind::Bool(!b)),
                                expr.ty,
                            ));
                        }
                        (UnOp::Fneg, Some(LitKind::Float(f))) => {
                            *expr = self.ctx.new_expr(ir_knorm::Typed::new(
                                ExprKind::Const(LitKind::Float(f32::to_bits(-f32::from_bits(*f)))),
                                expr.ty,
                            ));
                        }
                        _ => {}
                    },
                    ExprKind::Binary(bin_op, x, y) => {
                        if let (Some(x), Some(y)) = (self.env.get(x), self.env.get(y)) {
                            if let Some(folded) = bin_op_fold(*bin_op, *x, *y) {
                                *expr = self.ctx.new_expr(ir_knorm::Typed::new(
                                    ExprKind::Const(folded),
                                    expr.ty,
                                ));
                            }
                        }
                    }
                    _ => {}
                }
                self.super_expr(expr);
            }
        }

        ConstantFoldVisitor {
            ctx: ctx.knorm_context(),
            env: FxHashMap::default(),
        }
        .visit_expr(expr);
    }
}
