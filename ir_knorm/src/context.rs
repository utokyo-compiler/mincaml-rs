use data_structure::{
    arena::TypedArena,
    interning::{HashSetInterner, Interned},
};
use ir_typed_ast::{DisambiguatedIdent, Typed};

use crate::{Expr, ExprKind};

pub struct KnormContext<'ctx> {
    ident_arena: &'ctx TypedArena<Typed<'ctx, DisambiguatedIdent<'ctx>>>,
    ident_interner: HashSetInterner<&'ctx Typed<'ctx, DisambiguatedIdent<'ctx>>>,
    knorm_expr_arena: &'ctx TypedArena<Typed<'ctx, ExprKind<'ctx>>>,
}

impl<'ctx> KnormContext<'ctx> {
    pub fn new(
        ident_arena: &'ctx TypedArena<Typed<'ctx, DisambiguatedIdent<'ctx>>>,
        knorm_expr_arena: &'ctx TypedArena<Typed<'ctx, ExprKind<'ctx>>>,
    ) -> Self {
        Self {
            ident_arena,
            ident_interner: Default::default(),
            knorm_expr_arena,
        }
    }

    pub fn intern_resolved_ident(
        &self,
        ident: Typed<'ctx, DisambiguatedIdent<'ctx>>,
    ) -> Interned<'ctx, Typed<'ctx, DisambiguatedIdent<'ctx>>> {
        Interned::new_unchecked(
            self.ident_interner
                .intern(ident, |ident| self.ident_arena.alloc(ident)),
        )
    }

    pub fn new_expr(&self, expr: Typed<'ctx, ExprKind<'ctx>>) -> Expr<'ctx> {
        self.knorm_expr_arena.alloc_boxed(expr)
    }
}
