use data_structure::arena::TypedArena;

use crate::{Expr, TypedExprKind};

pub struct Context<'ctx> {
    expr_arena: &'ctx TypedArena<TypedExprKind<'ctx>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new(expr_arena: &'ctx TypedArena<TypedExprKind<'ctx>>) -> Self {
        Self { expr_arena }
    }

    pub fn new_expr(&self, expr: TypedExprKind<'ctx>) -> Expr<'ctx> {
        self.expr_arena.alloc_boxed(expr)
    }
}
