use ir_knorm::{Context, Expr, ExprKind, Ident, MutVisitor, UnOp, BinOp, LetBinding, LitKind };
use middleware::{FxHashMap, GlobalContext};

use crate::KnormPass;

pub struct ConstantFold;

fn bin_op_fold(bin_op: BinOp, x: LitKind, y: LitKind) -> Option<LitKind> {
    use ir_knorm:: {BooleanBinOpKind as BOp, FloatBinOpKind as FOp, IntBinOpKind as IOp};

    match (bin_op, x, y) {
        (BinOp::Boolean(BOp::Eq), LitKind::Bool(x), LitKind::Bool(y)) => Some(LitKind::Bool(x == y)),
        (BinOp::Boolean(BOp::Ne), LitKind::Bool(x), LitKind::Bool(y)) => Some(LitKind::Bool(x != y)),
        (BinOp::Boolean(BOp::Eq), LitKind::Int(x), LitKind::Int(y)) => Some(LitKind::Bool(x == y)),
        (BinOp::Boolean(BOp::Ne), LitKind::Int(x), LitKind::Int(y)) => Some(LitKind::Bool(x != y)),
        (BinOp::Boolean(BOp::Eq), LitKind::Float(x), LitKind::Float(y)) => Some(LitKind::Bool(x == y)),
        (BinOp::Boolean(BOp::Ne), LitKind::Float(x), LitKind::Float(y)) => Some(LitKind::Bool(x != y)),
        (BinOp::Boolean(BOp::Ge), LitKind::Int(x), LitKind::Int(y)) => Some(LitKind::Bool(x >= y)),
        (BinOp::Boolean(BOp::Le), LitKind::Int(x), LitKind::Int(y)) => Some(LitKind::Bool(x <= y)),
        (BinOp::Boolean(BOp::Gt), LitKind::Int(x), LitKind::Int(y)) => Some(LitKind::Bool(x > y)),
        (BinOp::Boolean(BOp::Lt), LitKind::Int(x), LitKind::Int(y)) => Some(LitKind::Bool(x < y)),
        (BinOp::Boolean(BOp::Ge), LitKind::Float(x), LitKind::Float(y)) => Some(LitKind::Bool(x >= y)),
        (BinOp::Boolean(BOp::Le), LitKind::Float(x), LitKind::Float(y)) => Some(LitKind::Bool(x <= y)),
        (BinOp::Boolean(BOp::Gt), LitKind::Float(x), LitKind::Float(y)) => Some(LitKind::Bool(x > y)),
        (BinOp::Boolean(BOp::Lt), LitKind::Float(x), LitKind::Float(y)) => Some(LitKind::Bool(x < y)),
        (BinOp::Int(IOp::Add), LitKind::Int(x), LitKind::Int(y)) => Some(LitKind::Int(x + y)),
        (BinOp::Int(IOp::Sub), LitKind::Int(x), LitKind::Int(y)) => Some(LitKind::Int(x - y)),
        (BinOp::Int(IOp::Mul), LitKind::Int(x), LitKind::Int(y)) => Some(LitKind::Int(x * y)),
        (BinOp::Int(IOp::Div), LitKind::Int(x), LitKind::Int(y)) => Some(LitKind::Int(x / y)),
        (BinOp::Float(FOp::FAdd), LitKind::Float(x), LitKind::Float(y)) => Some(LitKind::Float(f32::to_bits(f32::from_bits(x) + f32::from_bits(y)))),
        (BinOp::Float(FOp::FSub), LitKind::Float(x), LitKind::Float(y)) => Some(LitKind::Float(f32::to_bits(f32::from_bits(x) - f32::from_bits(y)))),
        (BinOp::Float(FOp::FMul), LitKind::Float(x), LitKind::Float(y)) => Some(LitKind::Float(f32::to_bits(f32::from_bits(x) * f32::from_bits(y)))),
        (BinOp::Float(FOp::FDiv), LitKind::Float(x), LitKind::Float(y)) => Some(LitKind::Float(f32::to_bits(f32::from_bits(x) / f32::from_bits(y)))),
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
                    match &binding.value.value {
                        ExprKind::Const(lit) => {
                            self.env.insert(x, lit.clone());
                        }
                        _ => {}
                    }
                }

                self.super_binding(binding);
            }

            fn visit_expr(&mut self, expr: &mut Expr<'ctx>) {
                match &expr.value {
                    ExprKind::Unary(un_op, x) => {
                        match (un_op, self.env.get(x)) {
                            (UnOp::Neg, Some(LitKind::Int(i))) => {
                                *expr = self.ctx.new_expr(ir_knorm::Typed::new(ExprKind::Const(LitKind::Int(-i)), expr.ty.clone()));
                            },
                            (UnOp::Not, Some(LitKind::Bool(b))) => {
                                *expr = self.ctx.new_expr(ir_knorm::Typed::new(ExprKind::Const(LitKind::Bool(!b)), expr.ty.clone()));
                            },
                            (UnOp::FNeg, Some(LitKind::Float(f))) => {
                                *expr = self.ctx.new_expr(ir_knorm::Typed::new(ExprKind::Const(LitKind::Float(f32::to_bits(-f32::from_bits(*f)))), expr.ty.clone()));
                            },
                            _ => {}
                        }
                    },
                    ExprKind::Binary(bin_op, x, y) => {
                        if let (Some(x), Some(y)) = (self.env.get(x), self.env.get(y)) {
                            if let Some(folded) = bin_op_fold(*bin_op, x.clone(), y.clone()) {
                                *expr = self.ctx.new_expr(ir_knorm::Typed::new(ExprKind::Const(folded), expr.ty.clone()));
                            }
                        }
                    },
                    _ => {}
                }
                self.super_expr(expr);
            }
        }

        ConstantFoldVisitor {
            ctx: ctx.knorm_context(),
            env: FxHashMap::default(),
        }.visit_expr(expr);
    }
}