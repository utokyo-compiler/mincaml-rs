use crate::{builder::FunctionBuilder, Context, Function};

use super::expr::{lower_expr, State};

/// Lower the given function.
pub fn lower_function<'ctx>(
    ir_closure::Function {
        name,
        args,
        args_via_closure,
        body,
    }: ir_closure::Function<'ctx>,
    ctx: &'ctx Context<'ctx>,
) -> Function<'ctx> {
    let mut builder = FunctionBuilder::new(
        ctx,
        name,
        args.into_iter(),
        args_via_closure.into_iter(),
        body.ty,
    );
    lower_expr(&body, ctx, &mut builder, &mut State::with_return());
    builder.finish_function()
}
