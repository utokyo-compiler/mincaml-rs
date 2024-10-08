use crate::{builder::FunctionBuilder, Context, FunctionDef};

use super::expr::{lower_expr, State};

/// Lower the given function.
pub fn lower_function<'ctx>(
    ctx: &'ctx Context<'ctx>,
    mut function_def: ir_closure::FunctionDef<'ctx>,
) -> FunctionDef<'ctx> {
    let mut builder = FunctionBuilder::new(
        ctx,
        function_def.name,
        std::mem::take(&mut function_def.args).into_iter(),
        std::mem::take(&mut function_def.args_via_closure).into_iter(),
        function_def.body().ty,
    );
    lower_expr(function_def.body(), ctx, &mut State::new(&mut builder));
    builder.finish_function()
}
