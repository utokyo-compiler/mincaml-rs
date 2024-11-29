mod expr;
mod function;

use function::lower_function;

use crate::{context::Context, Program};

pub fn lower<'ctx>(
    ctx: &'ctx Context<'ctx>,
    closure_program: ir_closure::Program<'ctx>,
) -> Program<'ctx> {
    let functions = closure_program
        .functions
        .into_iter()
        .map(|function| lower_function(ctx, function))
        .collect();
    Program { functions }
}
