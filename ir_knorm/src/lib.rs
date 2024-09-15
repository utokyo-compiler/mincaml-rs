pub mod context;

use context::KnormContext;
use data_structure::{arena::Box, interning::Interned};
use ir_typed_ast::DisambiguatedIdent;
use ty::Typed;

pub use ir_typed_ast::{BinOp, LitKind, UnOp};

pub type Ident<'ctx> = Interned<'ctx, Typed<'ctx, DisambiguatedIdent<'ctx>>>;

pub type Expr<'ctx> = Box<'ctx, Typed<'ctx, ExprKind<'ctx>>>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    Const(LitKind),
    Unary(UnOp, Ident<'ctx>),
    Binary(BinOp, Ident<'ctx>, Ident<'ctx>),
    If(Ident<'ctx>, Expr<'ctx>, Expr<'ctx>),
    Let(LetBinder<'ctx>, Expr<'ctx>),
    Var(Ident<'ctx>),
    App(Ident<'ctx>, Vec<Ident<'ctx>>),
    Tuple(Vec<Ident<'ctx>>),
    ArrayMake(Ident<'ctx>, Ident<'ctx>),
    Get(Ident<'ctx>, Ident<'ctx>),
    Set(Ident<'ctx>, Ident<'ctx>, Ident<'ctx>),
}

impl<'ctx> ExprKind<'ctx> {
    pub fn kind(&self) -> &Self {
        self
    }
}

impl<'ctx> ExprKind<'ctx> {
    fn name(&self) -> &'static str {
        match self {
            ExprKind::Const(lit) => lit.discriminant_name(),
            ExprKind::Unary(un_op, ..) => un_op.name(),
            ExprKind::Binary(bin_op, ..) => bin_op.name(),
            ExprKind::If(..) => "if",
            ExprKind::Let(..) => "let",
            ExprKind::Var(..) => "var",
            ExprKind::App(..) => "app",
            ExprKind::Tuple(..) => "tuple",
            ExprKind::ArrayMake(..) => "Array.make",
            ExprKind::Get(..) => "get",
            ExprKind::Set(..) => "set",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LetBinder<'ctx> {
    pub place: Pattern<'ctx>,
    pub args: Vec<Ident<'ctx>>,
    pub value: Expr<'ctx>,
}

impl<'ctx> LetBinder<'ctx> {
    fn let_var(place: Ident<'ctx>, value: Expr<'ctx>) -> Self {
        Self {
            place: Pattern::Var(place),
            args: Vec::new(),
            value,
        }
    }
    fn let_discard(value: Expr<'ctx>) -> Self {
        debug_assert!(value.ty.is_unit());
        Self {
            place: Pattern::Unit,
            args: Vec::new(),
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<'ctx> {
    Unit,
    Var(Ident<'ctx>),
    Tuple(Vec<Ident<'ctx>>),
}

pub fn knorm_transform<'ctx>(
    ctx: &'ctx KnormContext<'ctx>,
    typed_expr: ir_typed_ast::ExprRef<'ctx>,
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
            let e2 = knorm_transform(ctx, e2);
            let e3 = knorm_transform(ctx, e3);

            ExprKind::If(e1, e2, e3)
        }
        ir_typed_ast::ExprKind::Let(binder, follows) => {
            let binder = LetBinder {
                place: match &binder.place {
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
                args: binder
                    .args
                    .iter()
                    .map(|i| ctx.intern_resolved_ident(***i))
                    .collect(),
                value: knorm_transform(ctx, &binder.value),
            };
            let follows = knorm_transform(ctx, follows);

            ExprKind::Let(binder, follows)
        }
        ir_typed_ast::ExprKind::Then(e1, e2) => {
            let e1 = knorm_transform(ctx, e1);
            let e2 = knorm_transform(ctx, e2);

            ExprKind::Let(LetBinder::let_discard(e1), e2)
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
    binders.apply_binder(
        ctx,
        ctx.new_expr(Typed::new(bottom_expr_kind, typed_expr.ty)),
    )
}

fn insert_let<'ctx>(
    ctx: &'ctx KnormContext<'ctx>,
    binders: &mut Binders<'ctx>,
    typed_expr: ir_typed_ast::ExprRef<'ctx>,
) -> Ident<'ctx> {
    let transformed = knorm_transform(ctx, typed_expr);
    evaluated_ident(ctx, binders, transformed)
}

fn evaluated_ident<'ctx>(
    ctx: &'ctx KnormContext<'ctx>,
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
            binders.add_binder(LetBinder::let_var(ident, expr));
            ident
        }
    }
}

/// A stack of binders. Originally called `insert_let`.
struct Binders<'ctx> {
    inner: Vec<LetBinder<'ctx>>,
}

impl<'ctx> Binders<'ctx> {
    fn new() -> Self {
        Self { inner: Vec::new() }
    }

    fn add_binder(&mut self, binder: LetBinder<'ctx>) {
        self.inner.push(binder);
    }

    fn apply_binder(mut self, ctx: &'ctx KnormContext<'ctx>, expr: Expr<'ctx>) -> Expr<'ctx> {
        let mut expr = expr;
        while let Some(binder) = self.inner.pop() {
            let ty = expr.ty;
            expr = ctx.new_expr(Typed::new(ExprKind::Let(binder, expr), ty));
        }
        expr
    }
}
