use ir_knorm::Expr;
use middleware::GlobalContext;

mod beta_convert;
mod constant_fold;
mod eliminate_unused;
mod inlining;
mod let_flatten;

pub trait KnormPass<'ctx> {
    fn run_pass(&mut self, ctx: &'ctx GlobalContext<'ctx>, expr: &mut Expr<'ctx>);
}
