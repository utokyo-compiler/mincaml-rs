use data_structure::{arena::TypedArena, interning::Interned};

use crate::{DisambiguatedIdent, Expr, Ident, Typed, TypedExprKind};

pub struct Context<'ctx> {
    ident_arena: &'ctx TypedArena<Typed<'ctx, DisambiguatedIdent<'ctx>>>,
    expr_arena: &'ctx TypedArena<TypedExprKind<'ctx>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new(
        ident_arena: &'ctx TypedArena<Typed<'ctx, DisambiguatedIdent<'ctx>>>,
        expr_arena: &'ctx TypedArena<TypedExprKind<'ctx>>,
    ) -> Self {
        Self {
            ident_arena,
            expr_arena,
        }
    }

    pub fn new_ident_unchecked(&self, ident: Typed<'ctx, DisambiguatedIdent<'ctx>>) -> Ident<'ctx> {
        Ident::new(Interned::new_unchecked(self.ident_arena.alloc(ident)))
    }

    pub fn new_expr(&self, expr: TypedExprKind<'ctx>) -> Expr<'ctx> {
        self.expr_arena.alloc_boxed(expr)
    }
}
