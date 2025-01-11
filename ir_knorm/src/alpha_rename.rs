use data_structure::FxHashMap;

use crate::{Context, DisambiguatedIdent, Expr, Ident, LetBinding, MutVisitor, Pattern, Ty, Typed};

fn generate_fresh_ident<'ctx>(ctx: &Context<'ctx>, tag: &'static str, ty: Ty<'ctx>) -> Ident<'ctx> {
    static COMPILER_GENERATED_COUNTER: std::sync::atomic::AtomicUsize =
        std::sync::atomic::AtomicUsize::new(0);
    return ctx.intern_resolved_ident(Typed::new(
        DisambiguatedIdent::new_compiler_unchecked(
            tag,
            COMPILER_GENERATED_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        ),
        ty,
    ));
}

pub struct AlphaRename<'ctx> {
    tag: &'static str,
    initial: FxHashMap<Ident<'ctx>, Ident<'ctx>>,
}

impl<'ctx> AlphaRename<'ctx> {
    pub fn new(tag: &'static str) -> Self {
        Self {
            tag,
            initial: FxHashMap::default(),
        }
    }

    pub fn new_from_initial(
        tag: &'static str,
        initial_env: FxHashMap<Ident<'ctx>, Ident<'ctx>>,
    ) -> Self {
        Self {
            tag,
            initial: initial_env,
        }
    }
}

impl<'ctx> AlphaRename<'ctx> {
    pub fn run(self, ctx: &'ctx Context<'ctx>, expr: &mut Expr<'ctx>) {
        struct RenameVisitor<'ctx> {
            ctx: &'ctx Context<'ctx>,
            tag: &'static str,
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
                    let renamed = generate_fresh_ident(self.ctx, self.tag, binding.bindee().ty);
                    self.env.insert(introduced, renamed);
                    binding.pattern = Pattern::Var(renamed);
                }
                self.super_binding(binding);
            }
        }

        RenameVisitor {
            ctx,
            tag: self.tag,
            env: self.initial,
        }
        .visit_expr(expr);
    }
}
