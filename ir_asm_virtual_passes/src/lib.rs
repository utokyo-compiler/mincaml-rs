use ir_asm_virtual::Program;
use middleware::GlobalContext;

mod remove_unit;
mod remove_unused;

pub use remove_unit::RemoveUnit;
pub use remove_unused::RemoveUnused;

pub trait VirtAsmPass<'ctx> {
    fn run_pass(&mut self, ctx: &'ctx GlobalContext<'ctx>, program: &mut Program<'ctx>);
}
