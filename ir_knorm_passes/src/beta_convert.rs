use ir_knorm::{Expr, Ident, LetBinder, MutVisitor};
use middleware::FxHashMap;

use crate::KnormPass;

pub struct BetaConvertPass;

impl<'ctx> KnormPass<'ctx> for BetaConvertPass {
    fn run_pass(&mut self, _ctx: &'ctx middleware::GlobalContext<'ctx>, expr: &mut Expr<'ctx>) {
        struct LookupVisitor<'ctx> {
            env: FxHashMap<Ident<'ctx>, Ident<'ctx>>,
        }

        impl<'ctx> MutVisitor<'ctx> for LookupVisitor<'ctx> {
            fn visit_ident(&mut self, ident: &mut Ident<'ctx>) {
                if let Some(new_ident) = self.env.get(ident) {
                    *ident = *new_ident;
                }
            }

            fn visit_binder(&mut self, binder: &mut LetBinder<'ctx>) {
                let Some(new_ident) = binder.place.as_var() else {
                    self.super_binder(binder);
                    return;
                };
                let Some(value) = binder.value.as_var() else {
                    self.super_binder(binder);
                    return;
                };
                self.env.insert(new_ident, value);
            }
        }

        LookupVisitor {
            env: FxHashMap::default(),
        }
        .visit_expr(expr);
    }
}
