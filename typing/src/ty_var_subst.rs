//! TyVar substitution
//!
//! The difference from the original is that the original implementation stores
//! type-variable information in `Type.t`, this implementation manages it via `Env`.

use data_structure::FxHashMap;
use ty::{Ty, TyVarId};

pub struct Env<'ctx> {
    inner: FxHashMap<TyVarId, Ty<'ctx>>,
}

impl<'ctx> Env<'ctx> {
    pub fn new() -> Self {
        Self {
            inner: Default::default(),
        }
    }
    pub fn get(&self, var: TyVarId) -> Option<Ty<'ctx>> {
        self.inner.get(&var).copied()
    }

    pub fn merge(&mut self, var: TyVarId, ty: Ty<'ctx>) {
        self.inner.insert(var, ty);
    }

    pub fn deref_ty_var(self, e: &mut ir_typed_ast::Expr<'ctx>) {
        use ir_typed_ast::MutVisitor;
        let mut visitor = DerefTyVisitor { env: self };
        visitor.visit_expr(e);
    }
}

struct DerefTyVisitor<'ctx> {
    env: Env<'ctx>,
}

impl<'ctx> ir_typed_ast::MutVisitor<'ctx> for DerefTyVisitor<'ctx> {
    fn visit_ty(&mut self, ty: &mut Ty<'ctx>) {
        while let ty::TyKind::TyVar(var) = ty.kind() {
            if let Some(new_ty) = self.env.get(*var) {
                *ty = new_ty;
            } else {
                break;
            }
        }
    }
}
