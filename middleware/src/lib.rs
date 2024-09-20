pub use rustc_hash::FxHashMap;

use data_structure::arena::TypedArena;
use sourcemap::Spanned;
use ty::{context::CommonTypes, TyKind};

#[derive(Default)]
pub struct Arena<'ctx> {
    ident: TypedArena<u8>,
    expr: TypedArena<Spanned<syntax::ExprKind<'ctx>>>,
    type_: TypedArena<TyKind<'ctx>>,
    typed_ident: TypedArena<ir_typed_ast::Typed<'ctx, ir_typed_ast::DisambiguatedIdent<'ctx>>>,
    typed_expr: TypedArena<ir_typed_ast::TypedExprKind<'ctx>>,
    knorm_expr: TypedArena<ir_knorm::TypedExprKind<'ctx>>,
    closure_expr: TypedArena<ir_closure::TypedExprKind<'ctx>>,
}

pub struct GlobalContext<'ctx> {
    parsing_context: parser::Context<'ctx>,
    typing_context: typing::Context<'ctx>,
    knorm_context: ir_knorm::Context<'ctx>,
    closure_context: ir_closure::Context<'ctx>,
    pub common_types: CommonTypes<'ctx>,
}

impl<'ctx> GlobalContext<'ctx> {
    pub fn new(arena: &'ctx Arena<'ctx>) -> Self {
        let parsing_context = parser::Context::new(&arena.ident, &arena.expr);
        let typing_context =
            typing::Context::new(&arena.type_, &arena.typed_ident, &arena.typed_expr);
        let knorm_context = ir_knorm::Context::new(&arena.typed_ident, &arena.knorm_expr);
        let closure_context = ir_closure::Context::new(&arena.closure_expr);
        Self {
            common_types: CommonTypes::new(&typing_context),
            parsing_context,
            typing_context,
            knorm_context,
            closure_context,
        }
    }

    pub fn parsing_context(&self) -> &parser::Context<'ctx> {
        &self.parsing_context
    }

    pub fn typing_context(&self) -> &typing::Context<'ctx> {
        &self.typing_context
    }

    pub fn knorm_context(&self) -> &ir_knorm::Context<'ctx> {
        &self.knorm_context
    }

    pub fn closure_context(&self) -> &ir_closure::Context<'ctx> {
        &self.closure_context
    }
}