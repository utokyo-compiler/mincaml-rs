use crate::{builder::FunctionBuilder, Context, FunctionDef};

use super::expr::{lower_expr, State};

/// Lower the given function.
pub fn lower_function<'ctx>(
    ctx: &'ctx Context<'ctx>,
    ir_closure::FunctionDef {
        name,
        args,
        args_via_closure,
        body,
    }: ir_closure::FunctionDef<'ctx>,
) -> FunctionDef<'ctx> {
    let mut builder = FunctionBuilder::new(
        ctx,
        name,
        args.into_iter(),
        args_via_closure.into_iter(),
        body.ty,
    );
    lower_expr(&body, ctx, &mut State::new(&mut builder));
    builder.finish_function()
}
