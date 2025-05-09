#![feature(iterator_try_collect)]
#![feature(if_let_guard)]

mod error;
mod name_res;
mod ty_var_subst;
mod typeck;
mod unify;

macros::fluent_messages! { "../messages.ftl" }

pub use typeck::typeck;

pub type Context<'ctx> = ty::context::TypingContext<
    'ctx,
    ty::Typed<'ctx, ir_typed_ast::DisambiguatedIdent<'ctx>>,
    ty::Typed<'ctx, sourcemap::Spanned<ir_typed_ast::ExprKind<'ctx>>>,
>;
