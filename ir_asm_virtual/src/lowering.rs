mod expr;

use data_structure::{FxHashSet, SetLikeVec};

use crate::{
    builder, context::Context, Closure, Expr, ExprKind, FnName, Function, Ident, Program, Typed,
};

pub fn lowering<'ctx>(
    ctx: &'ctx Context<'ctx>,
    closure_program: ir_closure::Program<'ctx>,
) -> Program<'ctx> {
    let mut state = LoweringProgramState::default();
    Program {
        functions: state.functions,
        main: todo!(),
    }
}

#[derive(Default)]
struct LoweringProgramState<'ctx> {
    functions: Vec<Function<'ctx>>,
}

pub(crate) trait Lowering<'ctx> {
    type Output;
    type State;
    fn lower(
        &self,
        ctx: &'ctx Context<'ctx>,
        builder: &mut builder::FunctionBuilder<'ctx>,
        state: &mut Self::State,
    ) -> Self::Output;
}
