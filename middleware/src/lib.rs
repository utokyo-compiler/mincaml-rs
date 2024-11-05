pub use data_structure::{FxHashMap, FxHashSet};

use data_structure::arena::TypedArena;
use session::Session;
use sourcemap::Spanned;
use ty::{context::CommonTypes, TyKind};

pub use ir_typed_ast::mli::Mli;

#[derive(Default)]
pub struct Arena<'ctx> {
    ident: TypedArena<u8>,
    expr: TypedArena<Spanned<syntax::ExprKind<'ctx>>>,
    type_: TypedArena<TyKind<'ctx>>,
    typed_ident: TypedArena<ir_typed_ast::Typed<'ctx, ir_typed_ast::DisambiguatedIdent<'ctx>>>,
    typed_expr: TypedArena<ir_typed_ast::TypedExprKind<'ctx>>,
    knorm_expr: TypedArena<ir_knorm::TypedExprKind<'ctx>>,
    closure_expr: TypedArena<ir_closure::TypedExprKind<'ctx>>,
    asm_virtual_expr: TypedArena<ir_asm_virtual::TypedExprKind<'ctx>>,
}

pub struct GlobalContext<'ctx> {
    parsing_context: parser::Context<'ctx>,
    typing_context: typing::Context<'ctx>,
    knorm_context: ir_knorm::Context<'ctx>,
    closure_context: ir_closure::Context<'ctx>,
    asm_virtual_context: ir_asm_virtual::Context<'ctx>,
    typed_interface: Mli<'ctx>,
    pub common_types: CommonTypes<'ctx>,
    session: Session,
}

impl<'ctx> GlobalContext<'ctx> {
    pub fn new(arena: &'ctx Arena<'ctx>, session: Session) -> Self {
        let parsing_context = parser::Context::new(&arena.ident, &arena.expr);
        let typing_context =
            typing::Context::new(&arena.type_, &arena.typed_ident, &arena.typed_expr);
        let knorm_context = ir_knorm::Context::new(&arena.typed_ident, &arena.knorm_expr);
        let closure_context = ir_closure::Context::new(&arena.typed_ident, &arena.closure_expr);
        let asm_virtual_context =
            ir_asm_virtual::Context::new(&arena.typed_ident, &arena.asm_virtual_expr);
        Self {
            common_types: CommonTypes::new(&typing_context),
            typed_interface: ir_typed_ast::mli::Mli::default(),
            parsing_context,
            typing_context,
            knorm_context,
            closure_context,
            asm_virtual_context,
            session,
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

    pub fn asm_virtual_context(&self) -> &ir_asm_virtual::Context<'ctx> {
        &self.asm_virtual_context
    }

    pub fn session(&self) -> &Session {
        &self.session
    }

    pub fn typed_interface(&self) -> &Mli<'ctx> {
        &self.typed_interface
    }
}
