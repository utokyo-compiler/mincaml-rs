use data_structure::index::vec::{Idx, IndexVec};

use crate::syntax::*;
use std::fmt::{self, Display, Formatter};

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "^bb{}", self.index())
    }
}

impl Display for LocalDecl<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.ident.value, self.ident.ty)
    }
}

struct BasicBlockPrinter<'ctx, 'a> {
    func_names: &'a IndexVec<FnIndex, FnName<'ctx>>,
    locals: &'a IndexVec<Local, LocalDecl<'ctx>>,
}

impl BasicBlockPrinter<'_, '_> {
    fn format_projection_kind(
        &self,
        f: &mut Formatter<'_>,
        projection_kind: &ProjectionKind,
    ) -> fmt::Result {
        match projection_kind {
            ProjectionKind::TupleIndex(tuple_index) => write!(f, ".{}", tuple_index.index()),
            ProjectionKind::ArrayElem(local) => write!(f, "[{}]", self.locals[*local].ident.value),
        }
    }

    fn format_place(&self, f: &mut Formatter<'_>, place: &Place) -> fmt::Result {
        match place {
            Place::Local(x) => write!(f, "{}", self.locals[*x].ident.value),
            Place::Projection {
                base,
                projection_kind,
            } => {
                write!(f, "{}", self.locals[*base].ident.value)?;
                self.format_projection_kind(f, projection_kind)
            }
        }
    }

    fn format_branch(
        &self,
        f: &mut Formatter<'_>,
        branch: &Branch,
        prepend_br: bool,
    ) -> fmt::Result {
        let args: Vec<_> = branch
            .args
            .iter()
            .map(|arg| self.locals[*arg].to_string())
            .collect();
        if prepend_br {
            write!(f, "br ")?;
        }
        if args.is_empty() {
            write!(f, "{}", branch.target)
        } else {
            write!(f, "{}({})", branch.target, args.join(", "))
        }
    }

    fn format_function_instance(
        &self,
        f: &mut Formatter<'_>,
        function_instance: &FunctionInstance,
    ) -> fmt::Result {
        match function_instance {
            FunctionInstance::Defined(index) => write!(f, "{}", self.func_names[*index]),
            FunctionInstance::Imported(fn_name) => write!(f, "{}", fn_name.0),
        }
    }

    fn format_closure(&self, f: &mut Formatter<'_>, closure: &Closure) -> fmt::Result {
        write!(f, "(")?;
        self.format_function_instance(f, &closure.function)?;
        write!(f, ")")?;
        let args = closure
            .captured_args
            .iter()
            .map(|arg| self.locals[*arg].to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, " {{{args}}}")
    }

    fn format_expr(&self, f: &mut Formatter<'_>, expr_kind: &ExprKind) -> fmt::Result {
        match expr_kind {
            ExprKind::Const(lit_kind) => write!(f, "{lit_kind}"),
            ExprKind::Unary(un_op, x) => {
                write!(f, "{un_op} {}", self.locals[*x].ident.value)
            }
            ExprKind::Binary(bin_op, x, y) => {
                write!(
                    f,
                    "{} {bin_op} {}",
                    self.locals[*x].ident.value, self.locals[*y].ident.value
                )
            }
            ExprKind::ClosureMake(closure) => {
                write!(f, "Closure.make ")?;
                self.format_closure(f, closure)
            }
            ExprKind::Tuple(elems) => {
                let elems: Vec<_> = elems
                    .iter()
                    .map(|x| self.locals[*x].ident.to_string())
                    .collect();
                write!(f, "({})", elems.join(", "))
            }
            ExprKind::ArrayMake(len, base) => {
                write!(
                    f,
                    "Array.make [{}; {}]",
                    self.locals[*base].ident.value, self.locals[*len].ident.value
                )
            }
            ExprKind::Read(place) => self.format_place(f, place),
        }
    }

    fn format_stmt(&self, f: &mut Formatter<'_>, stmt_kind: &StmtKind) -> fmt::Result {
        match stmt_kind {
            StmtKind::Nop => write!(f, "nop"),
            StmtKind::Assign { place, value } => {
                if let Some(place) = place {
                    self.format_place(f, place)?;
                } else {
                    write!(f, "_")?;
                }
                write!(f, " := ")?;
                self.format_expr(f, &value.value)
            }
        }
    }

    fn format_terminator(&self, f: &mut Formatter<'_>, terminator: &TerminatorKind) -> fmt::Result {
        match terminator {
            TerminatorKind::Return(args) => {
                write!(f, "return")?;
                if !args.is_empty() {
                    let args: Vec<_> = args
                        .iter()
                        .map(|arg| self.locals[*arg].to_string())
                        .collect();
                    write!(f, " ({})", args.join(", "))?;
                }
                Ok(())
            }
            TerminatorKind::Branch(branch) => self.format_branch(f, branch, true),
            TerminatorKind::ConditionalBranch {
                condition,
                targets: [true_target, false_target],
            } => {
                write!(f, "if {} {{ ", self.locals[*condition].ident.value,)?;
                self.format_branch(f, true_target, true)?;
                write!(f, " }} else {{ ")?;
                self.format_branch(f, false_target, true)?;
                write!(f, " }}")
            }
            TerminatorKind::Call {
                calling_conv,
                args,
                branch,
            } => {
                match calling_conv {
                    AbsCallingConv::Direct { function } => {
                        self.format_function_instance(f, function)?;
                    }
                    AbsCallingConv::Closure { local } => {
                        write!(f, "Closure.call ({})", self.locals[*local].ident.value)?;
                    }
                }
                let args = args
                    .iter()
                    .map(|arg| self.locals[*arg].to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, " ({args}) -> ")?;
                self.format_branch(f, branch, false)
            }
        }
    }

    fn format_basic_block(
        &self,
        f: &mut Formatter<'_>,
        bb: BasicBlock,
        data: &BasicBlockData,
    ) -> fmt::Result {
        write!(f, "\t{bb}")?;
        if !data.args.is_empty() {
            let args: Vec<_> = data
                .args
                .iter()
                .map(|arg| self.locals[*arg].to_string())
                .collect();
            write!(f, "({})", args.join(", "))?;
        }
        writeln!(f, ":")?;
        data.stmts.iter().try_for_each(|stmt| {
            write!(f, "\t\t")?;
            self.format_stmt(f, stmt)?;
            writeln!(f, ";")
        })?;
        write!(f, "\t\t")?;
        self.format_terminator(f, data.terminator())?;
        writeln!(f, ";")
    }
}

fn format_function_def(
    f: &mut Formatter<'_>,
    func_names: &IndexVec<FnIndex, FnName<'_>>,
    FunctionDef {
        name,
        local_decls,
        args,
        args_via_closure,
        basic_blocks,
    }: &FunctionDef,
) -> fmt::Result {
    write!(f, "fn {name}")?;

    if !args_via_closure.is_empty() {
        let args_via_closure: Vec<_> = local_decls[*args_via_closure]
            .iter()
            .map(|arg| arg.to_string())
            .collect();
        write!(f, " {{{}}}", args_via_closure.join(", "))?;
    }

    let args: Vec<_> = local_decls[*args]
        .iter()
        .map(|arg| arg.to_string())
        .collect();
    write!(f, " ({})", args.join(", "))?;

    if let Some(ident) = name.get_inner() {
        let (_, ret_ty) = ident.ty.as_fun_ty().unwrap();
        if !ret_ty.is_unit() {
            let ret_ty = ret_ty.to_string();
            if ret_ty.starts_with("(") || !ret_ty.contains("->") {
                write!(f, ": {ret_ty}")?;
            } else {
                write!(f, ": ({ret_ty})")?;
            }
        }
    }

    writeln!(f, " {{")?;
    basic_blocks.iter_enumerated().try_for_each(|(bb, data)| {
        BasicBlockPrinter {
            func_names,
            locals: local_decls,
        }
        .format_basic_block(f, bb, data)
    })?;
    writeln!(f, "}}")
}

impl Display for Program<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let func_names = self.functions.iter().map(|func| func.name).collect();

        self.functions
            .iter()
            .try_for_each(|function| format_function_def(f, &func_names, function))
    }
}
