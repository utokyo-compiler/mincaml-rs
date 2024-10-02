use ir_knorm::Expr;
use middleware::GlobalContext;

mod beta_convert;
mod let_flatten;
mod inlining;
mod alpha_rename;
mod eliminate_unused;

pub trait KnormPass<'ctx> {
    fn run_pass(&mut self, ctx: &'ctx GlobalContext<'ctx>, expr: &mut Expr<'ctx>);
}
