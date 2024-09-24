mod expr;
mod function;

use data_structure::index::vec::IndexVec;
use function::lower_function;

use crate::{context::Context, Program, MAIN_FN_NAME};

pub fn lowering<'ctx>(
    ctx: &'ctx Context<'ctx>,
    closure_program: ir_closure::Program<'ctx>,
) -> Program<'ctx> {
    let functions = closure_program
        .functions
        .into_iter()
        .map(|function| lower_function(function, ctx))
        .collect();
    Program {
        functions,
        main: lower_function(
            ir_closure::Function {
                name: MAIN_FN_NAME,
                args: IndexVec::new(),
                args_via_closure: IndexVec::new(),
                body: closure_program.main,
            },
            ctx,
        ),
    }
}
