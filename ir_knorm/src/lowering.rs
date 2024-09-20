use crate::{
    context::Context, DisambiguatedIdent, Expr, ExprKind, Ident, LetBinding, Pattern, Typed,
};

pub fn lowering<'ctx>(
    ctx: &'ctx Context<'ctx>,
    typed_expr: ir_typed_ast::Expr<'ctx>,
) -> Expr<'ctx> {
    lowering_ref(ctx, &typed_expr)
}

fn lowering_ref<'ctx>(
    ctx: &'ctx Context<'ctx>,
    typed_expr: &ir_typed_ast::Expr<'ctx>,
) -> Expr<'ctx> {
    let mut binders = Binders::new();
    let bottom_expr_kind = match typed_expr.kind() {
        ir_typed_ast::ExprKind::Const(lit) => ExprKind::Const(*lit),
        ir_typed_ast::ExprKind::Unary(un_op, e) => {
            let e = insert_let(ctx, &mut binders, e);

            ExprKind::Unary(*un_op, e)
        }
        ir_typed_ast::ExprKind::Binary(bin_op, e1, e2) => {
            let e1 = insert_let(ctx, &mut binders, e1);
            let e2 = insert_let(ctx, &mut binders, e2);

            ExprKind::Binary(*bin_op, e1, e2)
        }
        ir_typed_ast::ExprKind::If(e1, e2, e3) => {
            let e1 = insert_let(ctx, &mut binders, e1);
            let e2 = lowering_ref(ctx, e2);
            let e3 = lowering_ref(ctx, e3);

            ExprKind::If(e1, e2, e3)
        }
        ir_typed_ast::ExprKind::Let(binding, follows) => {
            let binding = LetBinding {
                place: match &binding.place {
                    ir_typed_ast::Pattern::Var(ident) => {
                        Pattern::Var(ctx.intern_resolved_ident(***ident))
                    }
                    ir_typed_ast::Pattern::Tuple(idents) => Pattern::Tuple(
                        idents
                            .iter()
                            .map(|i| ctx.intern_resolved_ident(***i))
                            .collect(),
                    ),
                },
                args: binding
                    .args
                    .iter()
                    .map(|i| ctx.intern_resolved_ident(***i))
                    .collect(),
                value: lowering_ref(ctx, &binding.value),
            };
            let follows = lowering_ref(ctx, follows);

            ExprKind::Let(binding, follows)
        }
        ir_typed_ast::ExprKind::Then(e1, e2) => {
            let e1 = lowering_ref(ctx, e1);
            let e2 = lowering_ref(ctx, e2);

            ExprKind::Let(LetBinding::let_discard(e1), e2)
        }
        ir_typed_ast::ExprKind::Var(ident) => {
            let ident = ctx.intern_resolved_ident(***ident);
            ExprKind::Var(ident)
        }
        ir_typed_ast::ExprKind::App(fun, args) => {
            let fun = insert_let(ctx, &mut binders, fun);
            let args = args
                .iter()
                .map(|a| insert_let(ctx, &mut binders, a))
                .collect();
            ExprKind::App(fun, args)
        }
        ir_typed_ast::ExprKind::Tuple(es) => {
            let es = es
                .iter()
                .map(|e| insert_let(ctx, &mut binders, e))
                .collect();
            ExprKind::Tuple(es)
        }
        ir_typed_ast::ExprKind::ArrayMake(_, _) => todo!(),
        ir_typed_ast::ExprKind::Get(_, _) => todo!(),
        ir_typed_ast::ExprKind::Set(_, _, _) => todo!(),
    };
    binders.apply_binders(
        ctx,
        ctx.new_expr(Typed::new(bottom_expr_kind, typed_expr.ty)),
    )
}

fn insert_let<'ctx>(
    ctx: &'ctx Context<'ctx>,
    binders: &mut Binders<'ctx>,
    typed_expr: &ir_typed_ast::Expr<'ctx>,
) -> Ident<'ctx> {
    let transformed = lowering_ref(ctx, typed_expr);
    evaluated_ident(ctx, binders, transformed)
}

fn evaluated_ident<'ctx>(
    ctx: &'ctx Context<'ctx>,
    binders: &mut Binders<'ctx>,
    expr: Expr<'ctx>,
) -> Ident<'ctx> {
    match expr.kind() {
        ExprKind::Var(ident) => *ident,
        e => {
            static COMPILER_GENERATED_COUNTER: std::sync::atomic::AtomicUsize =
                std::sync::atomic::AtomicUsize::new(0);
            let ident = Typed::new(
                DisambiguatedIdent::new_compiler_unchecked(
                    e.name(),
                    COMPILER_GENERATED_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
                ),
                expr.ty,
            );
            let ident = ctx.intern_resolved_ident(ident);
            binders.add_binder(LetBinding::let_var(ident, expr));
            ident
        }
    }
}

/// A stack of binders. Originally called `insert_let`.
struct Binders<'ctx> {
    inner: Vec<LetBinding<'ctx>>,
}

impl<'ctx> Binders<'ctx> {
    fn new() -> Self {
        Self { inner: Vec::new() }
    }

    fn add_binder(&mut self, binder: LetBinding<'ctx>) {
        self.inner.push(binder);
    }

    fn apply_binders(mut self, ctx: &'ctx Context<'ctx>, expr: Expr<'ctx>) -> Expr<'ctx> {
        let mut expr = expr;
        while let Some(binder) = self.inner.pop() {
            let ty = expr.ty;
            expr = ctx.new_expr(Typed::new(ExprKind::Let(binder, expr), ty));
        }
        expr
    }
}
