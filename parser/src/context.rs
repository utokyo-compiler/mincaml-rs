use data_structure::{
    arena::TypedArena,
    interning::{HashSetInterner, Interned},
};
use sourcemap::Spanned;
use syntax::ExprKind;

pub struct Context<'ctx> {
    ident_arena: &'ctx TypedArena<u8>,
    ident_interner: HashSetInterner<&'ctx str>,
    expr_arena: &'ctx TypedArena<Spanned<ExprKind<'ctx>>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new(
        ident_arena: &'ctx TypedArena<u8>,
        expr_arena: &'ctx TypedArena<Spanned<ExprKind<'ctx>>>,
    ) -> Self {
        Self {
            ident_arena,
            ident_interner: Default::default(),
            expr_arena,
        }
    }

    pub fn intern_ident(&self, ident: &str) -> syntax::Ident<'ctx> {
        Interned::new_unchecked(
            self.ident_interner
                .intern_ref(ident, || self.ident_arena.alloc_str(ident)),
        )
    }

    pub fn new_expr(&self, expr: Spanned<ExprKind<'ctx>>) -> &'ctx Spanned<ExprKind<'ctx>> {
        self.expr_arena.alloc(expr)
    }
}
