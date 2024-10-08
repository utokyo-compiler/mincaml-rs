use std::sync::atomic::AtomicUsize;

use data_structure::{
    arena::{Box, TypedArena},
    interning::{HashSetInterner, Interned},
};

use crate::{Ty, TyKind, TyVarId};

/// The context for type checking.
///
/// `Ident` and `Expr` are just a placeholder for the type
/// which defined in the dependent crate.
pub struct TypingContext<'ctx, Ident, Expr> {
    ty_arena: &'ctx TypedArena<TyKind<'ctx>>,
    typed_ident_arena: &'ctx TypedArena<Ident>,
    typed_expr_arena: &'ctx TypedArena<Expr>,
    ty_interner: HashSetInterner<&'ctx TyKind<'ctx>>,
    fresh_ty_var_id: AtomicUsize,
}

impl<'ctx, Ident, Expr> TypingContext<'ctx, Ident, Expr> {
    pub fn new(
        ty_arena: &'ctx TypedArena<TyKind<'ctx>>,
        typed_ident_arena: &'ctx TypedArena<Ident>,
        typed_expr_arena: &'ctx TypedArena<Expr>,
    ) -> Self {
        Self {
            ty_arena,
            typed_ident_arena,
            typed_expr_arena,
            ty_interner: Default::default(),
            fresh_ty_var_id: Default::default(),
        }
    }

    pub fn mk_ty_from_kind(&self, kind: TyKind<'ctx>) -> Ty<'ctx> {
        Ty(Interned::new_unchecked(
            self.ty_interner
                .intern(kind, |kind| self.ty_arena.alloc(kind)),
        ))
    }

    pub fn alloc_ident(&self, expr: Ident) -> Box<'ctx, Ident> {
        self.typed_ident_arena.alloc_boxed(expr)
    }

    pub fn alloc_expr(&self, expr: Expr) -> Box<'ctx, Expr> {
        self.typed_expr_arena.alloc_boxed(expr)
    }

    pub fn fresh_ty_var(&self) -> TyVarId {
        let id = self
            .fresh_ty_var_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        TyVarId::new_unchecked(id)
    }
}

/// Common, pre-interned types.
pub struct CommonTypes<'ctx> {
    pub unit: Ty<'ctx>,
    pub bool: Ty<'ctx>,
    pub int: Ty<'ctx>,
    pub float: Ty<'ctx>,
}

impl<'ctx> CommonTypes<'ctx> {
    pub fn new<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self {
            unit: ctx.mk_ty_from_kind(TyKind::Unit),
            bool: ctx.mk_ty_from_kind(TyKind::Bool),
            int: ctx.mk_ty_from_kind(TyKind::Int),
            float: ctx.mk_ty_from_kind(TyKind::Float),
        }
    }
}
