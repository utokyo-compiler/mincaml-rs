use data_structure::{arena::TypedArena, interning::Interned};

use crate::{Expr, Ident, TypedExprKind, TypedIdent};

pub struct Context<'ctx> {
    ident_arena: &'ctx TypedArena<TypedIdent<'ctx>>,
    expr_arena: &'ctx TypedArena<TypedExprKind<'ctx>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new(
        ident_arena: &'ctx TypedArena<TypedIdent<'ctx>>,
        expr_arena: &'ctx TypedArena<TypedExprKind<'ctx>>,
    ) -> Self {
        Self {
            ident_arena,
            expr_arena,
        }
    }

    pub fn new_expr(&self, expr: TypedExprKind<'ctx>) -> Expr<'ctx> {
        self.expr_arena.alloc_boxed(expr)
    }

    pub fn new_ident(&self, ident: TypedIdent<'ctx>) -> Ident<'ctx> {
        Ident::new(Interned::new_unchecked(self.ident_arena.alloc(ident)))
    }
}
