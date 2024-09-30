use ir_knorm::{Context, Expr, Ident, LetBinding, MutVisitor};
use middleware::FxHashMap;
use crate::KnormPass;

fn generate_fresh_ident<'ctx>(ctx: &Context<'ctx>, tag: &'static str, ty: ir_knorm::Ty<'ctx>) -> Ident<'ctx> {
    static COMPILER_GENERATED_COUNTER: std::sync::atomic::AtomicUsize =
                std::sync::atomic::AtomicUsize::new(0);
    return ctx.intern_resolved_ident(ir_knorm::Typed::new(
        ir_knorm::DisambiguatedIdent::new_compiler_unchecked(
            tag,
            COMPILER_GENERATED_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        ),
        ty,
    ));
}

pub struct AlphaRename;

impl<'ctx> KnormPass<'ctx> for AlphaRename {
    fn run_pass(&mut self, ctx: &'ctx middleware::GlobalContext<'ctx>, expr: &mut Expr<'ctx>) {
        struct RenameVisitor<'ctx> {
            ctx: &'ctx Context<'ctx>,
            env: FxHashMap<Ident<'ctx>, Ident<'ctx>>,
        }

        impl<'ctx> MutVisitor<'ctx> for RenameVisitor<'ctx> {
            fn visit_ident(&mut self, ident: &mut Ident<'ctx>) {
                if let Some(new_ident) = self.env.get(ident) {
                    *ident = *new_ident;
                }
            }

            fn visit_binding(&mut self, binding: &mut LetBinding<'ctx>) {
                if let Some(introduced) = binding.pattern.as_var() {
                    let renamed = generate_fresh_ident(self.ctx, "inline", binding.value.ty.clone());
                    self.env.insert(introduced, renamed);
                    binding.pattern = ir_knorm::Pattern::Var(renamed);
                }
                self.super_binding(binding);
            }
        }

        RenameVisitor {
            ctx: ctx.knorm_context(),
            env: FxHashMap::default(),
        }
        .visit_expr(expr);
    }
}
