use data_structure::arena::TypedArena;
use parser::context::ParsingContext;
use syntax::ExprKind;
use ty::{
    context::{CommonTypes, TypingContext},
    TyKind,
};

pub struct Arena<'ctx> {
    expr: TypedArena<ExprKind<'ctx>>,
    type_: TypedArena<TyKind<'ctx>>,
}

impl Arena<'_> {
    pub fn new() -> Self {
        Self {
            expr: Default::default(),
            type_: Default::default(),
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
        let typing_context = TypingContext::new(&arena.type_);
        Self {
            arena,
            common_types: CommonTypes::new(&typing_context),
            typing_context,
        }
    }

    pub fn typing_context(&self) -> &TypingContext<'ctx> {
        &self.typing_context
    }
}
