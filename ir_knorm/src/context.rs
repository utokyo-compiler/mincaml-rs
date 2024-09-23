use data_structure::{
    arena::TypedArena,
    interning::{HashSetInterner, Interned},
};

use crate::{DisambiguatedIdent, Expr, ExprKind, Ident, Typed};

pub struct Context<'ctx> {
    ident_arena: &'ctx TypedArena<Typed<'ctx, DisambiguatedIdent<'ctx>>>,
    ident_interner: HashSetInterner<&'ctx Typed<'ctx, DisambiguatedIdent<'ctx>>>,
    expr_arena: &'ctx TypedArena<Typed<'ctx, ExprKind<'ctx>>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new(
        ident_arena: &'ctx TypedArena<Typed<'ctx, DisambiguatedIdent<'ctx>>>,
        expr_arena: &'ctx TypedArena<Typed<'ctx, ExprKind<'ctx>>>,
    ) -> Self {
        Self {
            ident_arena,
            ident_interner: Default::default(),
            expr_arena,
        }
    }

    pub fn intern_resolved_ident(
        &self,
        ident: Typed<'ctx, DisambiguatedIdent<'ctx>>,
    ) -> Ident<'ctx> {
        Ident::new(Interned::new_unchecked(
            self.ident_interner
                .intern(ident, |ident| self.ident_arena.alloc(ident)),
        ))
    }

    pub fn new_expr(&self, expr: Typed<'ctx, ExprKind<'ctx>>) -> Expr<'ctx> {
        self.expr_arena.alloc_boxed(expr)
    }
}
