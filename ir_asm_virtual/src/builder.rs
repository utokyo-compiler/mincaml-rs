use core::range::Range;

use data_structure::{
    index::vec::{Idx, IndexVec},
    SetLikeVec,
};

use crate::{
    ArgIndex, BasicBlock, BasicBlockData, Context, DisambiguatedIdent, FnName, Function, Ident,
    Local, LocalDecl, StmtIndex, StmtKind, TerminatorKind, Ty, Typed,
};

#[derive(Default)]
pub struct BasicBlockBuilder<'ctx> {
    args: IndexVec<ArgIndex, Local>,
    stmts: IndexVec<StmtIndex, StmtKind<'ctx>>,
}

impl<'ctx> BasicBlockBuilder<'ctx> {
    pub fn set_args(&mut self, args: IndexVec<ArgIndex, Local>) {
        self.args = args;
    }
    pub fn push_stmt(&mut self, value: StmtKind<'ctx>) -> StmtIndex {
        self.stmts.push(value)
    }

    /// Terminates the current block and wraps up into a `BasicBlockData`.
    pub fn finish_block(self, terminator: TerminatorKind<'ctx>) -> BasicBlockData<'ctx> {
        BasicBlockData {
            args: self.args,
            stmts: self.stmts,
            terminator,
        }
    }
}

pub struct FunctionBuilder<'ctx> {
    name: FnName<'ctx>,
    local_decls: IndexVec<Local, LocalDecl<'ctx>>,
    args: Range<Local>,
    args_via_closure: Range<Local>,
    basic_blocks: IndexVec<BasicBlock, BasicBlockData<'ctx>>,

    seen_idents: SetLikeVec<Ident<'ctx>>,
    basic_block_builder: BasicBlockBuilder<'ctx>,
}

impl<'ctx> FunctionBuilder<'ctx> {
    pub fn new(
        ctx: &'ctx Context<'ctx>,
        name: FnName<'ctx>,
        args: impl Iterator<Item = Ident<'ctx>>,
        args_via_closure: impl Iterator<Item = Ident<'ctx>>,
        return_ty: Ty<'ctx>,
    ) -> Self {
        let mut local_decls = IndexVec::default();
        let mut seen_idents = SetLikeVec::default();

        static COMPILER_GENERATED_COUNTER: std::sync::atomic::AtomicUsize =
            std::sync::atomic::AtomicUsize::new(0);
        local_decls.push(LocalDecl {
            ident: ctx.new_ident_unchecked(Typed::new(
                DisambiguatedIdent::new_compiler_unchecked(
                    "return_place",
                    COMPILER_GENERATED_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
                ),
                return_ty,
            )),
        });

        let range_start = local_decls.len();
        for arg in args {
            seen_idents.insert(arg);
            local_decls.push(LocalDecl { ident: arg });
        }
        let range_end = local_decls.len();
        let args = Range::from(Local::new(range_start)..Local::new(range_end));

        let range_start = local_decls.len();
        for arg in args_via_closure {
            seen_idents.insert(arg);
            local_decls.push(LocalDecl { ident: arg });
        }
        let range_end = local_decls.len();
        let args_via_closure = Range::from(Local::new(range_start)..Local::new(range_end));

        Self {
            name,
            local_decls,
            args,
            args_via_closure,
            basic_blocks: IndexVec::default(),
            seen_idents,
            basic_block_builder: BasicBlockBuilder::default(),
        }
    }
    pub fn finish_function(self) -> Function<'ctx> {
        Function {
            name: self.name,
            local_decls: self.local_decls,
            args: self.args,
            args_via_closure: self.args_via_closure,
            basic_blocks: self.basic_blocks,
        }
    }

    /// Finish current basic block and start a new one.
    ///
    /// Returns the finished block.
    pub fn finish_block(&mut self, terminator: TerminatorKind<'ctx>) -> BasicBlock {
        let basic_block_data =
            std::mem::take(&mut self.basic_block_builder).finish_block(terminator);
        self.push_basic_block(basic_block_data)
    }

    fn push_basic_block(&mut self, data: BasicBlockData<'ctx>) -> BasicBlock {
        self.basic_blocks.push(data)
    }

    /// Get a local variable by its identifier.
    ///
    /// If the identifier is not seen before, it will be added
    /// to the local declarations.
    pub fn get_local(&mut self, ident: Ident<'ctx>) -> Local {
        if let Some(index) = self.seen_idents.get(&ident) {
            return Local::new(index);
        }
        self.seen_idents.insert(ident);
        self.local_decls.push(LocalDecl { ident })
    }

    pub fn set_args(&mut self, args: IndexVec<ArgIndex, Local>) {
        self.basic_block_builder.set_args(args)
    }

    pub fn push_stmt(&mut self, value: StmtKind<'ctx>) -> StmtIndex {
        self.basic_block_builder.push_stmt(value)
    }

    pub fn next_basic_block(&self) -> BasicBlock {
        BasicBlock::new(self.basic_blocks.len())
    }

    pub fn basic_blocks_mut(&mut self) -> &mut IndexVec<BasicBlock, BasicBlockData<'ctx>> {
        &mut self.basic_blocks
    }
}
