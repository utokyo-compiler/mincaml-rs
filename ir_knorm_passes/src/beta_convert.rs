use ir_knorm::{Expr, Ident, LetBinding, MutVisitor};
use middleware::FxHashMap;

use crate::KnormPass;

pub struct BetaConvert;

impl<'ctx> KnormPass<'ctx> for BetaConvert {
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

            fn visit_binding(&mut self, binding: &mut LetBinding<'ctx>) {
                let Some(new_ident) = binding.pattern.as_var() else {
                    self.super_binding(binding);
                    return;
                };
                let Some(old_ident) = binding.bindee().as_var() else {
                    self.super_binding(binding);
                    return;
                };
                self.env.insert(new_ident, old_ident);
            }
        }

        LookupVisitor {
            env: FxHashMap::default(),
        }
        .visit_expr(expr);
    }
}
