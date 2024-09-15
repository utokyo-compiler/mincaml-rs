use data_structure::arena::TypedArena;
use parser::context::ParsingContext;
use sourcemap::Spanned;
use ty::{context::CommonTypes, TyKind};
use typing::TypingContext;

pub struct Arena<'ctx> {
    ident: TypedArena<u8>,
    expr: TypedArena<Spanned<syntax::ExprKind<'ctx>>>,
    type_: TypedArena<TyKind<'ctx>>,
    typed_expr: TypedArena<Spanned<ir_typed_ast::ExprKind<'ctx>>>,
}

impl Arena<'_> {
    pub fn new() -> Self {
        Self {
            ident: Default::default(),
            expr: Default::default(),
            type_: Default::default(),
            typed_expr: Default::default(),
        }
    }
}

pub struct GlobalContext<'ctx> {
    arena: &'ctx Arena<'ctx>,
    parsing_context: ParsingContext<'ctx>,
    typing_context: TypingContext<'ctx>,
    pub common_types: CommonTypes<'ctx>,
}

impl<'ctx> GlobalContext<'ctx> {
    pub fn new(arena: &'ctx Arena<'ctx>) -> Self {
        let typing_context = TypingContext::new(&arena.type_, &arena.typed_expr);
        let parsing_context = ParsingContext::new(&arena.ident, &arena.expr);
        Self {
            arena,
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
