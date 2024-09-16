pub use rustc_hash::FxHashMap;

use data_structure::arena::TypedArena;
use parser::context::ParsingContext;
use sourcemap::Spanned;
use ty::{context::CommonTypes, TyKind, Typed};
use typing::TypingContext;

#[derive(Default)]
pub struct Arena<'ctx> {
    ident: TypedArena<u8>,
    expr: TypedArena<Spanned<syntax::ExprKind<'ctx>>>,
    type_: TypedArena<TyKind<'ctx>>,
    typed_ident: TypedArena<Typed<'ctx, ir_typed_ast::DisambiguatedIdent<'ctx>>>,
    typed_expr: TypedArena<Typed<'ctx, Spanned<ir_typed_ast::ExprKind<'ctx>>>>,
}

pub struct GlobalContext<'ctx> {
    parsing_context: ParsingContext<'ctx>,
    typing_context: TypingContext<'ctx>,
    pub common_types: CommonTypes<'ctx>,
}

impl<'ctx> GlobalContext<'ctx> {
    pub fn new(arena: &'ctx Arena<'ctx>) -> Self {
        let typing_context =
            TypingContext::new(&arena.type_, &arena.typed_ident, &arena.typed_expr);
        let parsing_context = ParsingContext::new(&arena.ident, &arena.expr);
        Self {
            common_types: CommonTypes::new(&typing_context),
            parsing_context,
            typing_context,
        }
    }

    pub fn parsing_context(&self) -> &ParsingContext<'ctx> {
        &self.parsing_context
    }

    pub fn typing_context(&self) -> &TypingContext<'ctx> {
        &self.typing_context
    }
}
