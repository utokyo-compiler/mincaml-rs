use data_structure::{
    arena::TypedArena,
    interning::{HashSetInterner, Interned},
};
use syntax::ExprKind;

pub struct ParsingContext<'ctx> {
    ident_arena: &'ctx TypedArena<u8>,
    ident_interner: HashSetInterner<&'ctx str>,
    expr_arena: &'ctx TypedArena<ExprKind<'ctx>>,
    expr_interner: HashSetInterner<&'ctx ExprKind<'ctx>>,
}

impl<'ctx> ParsingContext<'ctx> {
    pub fn new(
        ident_arena: &'ctx TypedArena<u8>,
        expr_arena: &'ctx TypedArena<ExprKind<'ctx>>,
    ) -> Self {
        Self {
            ident_arena,
            ident_interner: Default::default(),
            expr_arena,
            expr_interner: Default::default(),
        }
    }

    pub fn intern_ident(&self, ident: &'ctx str) -> syntax::Ident<'ctx> {
        Interned::new_unchecked(
            self.ident_interner
                .intern(ident, |ident| self.ident_arena.alloc_str(ident)),
        )
    }

    pub fn intern_expr(&self, expr: ExprKind<'ctx>) -> Interned<'ctx, ExprKind<'ctx>> {
        Interned::new_unchecked(
            self.expr_interner
                .intern(expr, |expr| self.expr_arena.alloc(expr)),
        )
    }
}
