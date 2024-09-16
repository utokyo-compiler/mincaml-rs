use ir_knorm::Expr;
use middleware::GlobalContext;

mod beta_convert;

pub trait KnormPass<'ctx> {
    fn run_pass(&mut self, ctx: &'ctx GlobalContext<'ctx>, expr: &mut Expr<'ctx>);
}
