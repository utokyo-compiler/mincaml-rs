use data_structure::index::vec::{Idx, IndexVec};

use crate::syntax::*;
use std::fmt::{self, Display, Formatter};

// implement as a function because these types are actually defined in another crate
// and we cannot implement `Display` for it.
fn format_ty(f: &mut Formatter<'_>, ty: &Ty) -> fmt::Result {
    match ty.0 .0 {
        TyKind::Unit => write!(f, "()"),
        TyKind::Bool => write!(f, "bool"),
        TyKind::Int => write!(f, "int"),
        TyKind::Float => write!(f, "float"),
        TyKind::Fun(args, ret) => {
            write!(f, "(")?;
            args.iter().try_for_each(|arg| {
                format_ty(f, arg)?;
                write!(f, ",")
            })?;
            write!(f, ") -> ")?;
            format_ty(f, ret)
        }
        TyKind::Tuple(tys) => {
            write!(f, "(")?;
            tys.iter().try_for_each(|ty| format_ty(f, ty))?;
            write!(f, ")")
        }
        TyKind::Array(ty) => {
            write!(f, "[")?;
            format_ty(f, ty)?;
            write!(f, "]")
        }
        TyKind::TyVar(_) => panic!("TyVar should not be appeared in this context"),
    }
}

fn format_un_op(f: &mut Formatter<'_>, un_op: &UnOp) -> fmt::Result {
    match un_op {
        UnOp::Not => write!(f, "!"),
        UnOp::Neg => write!(f, "-"),
        UnOp::FNeg => write!(f, "-."),
    }
}

fn format_bin_op(f: &mut Formatter<'_>, bin_op: &BinOp) -> fmt::Result {
    use ir_closure::{FloatBinOpKind as FOp, IntBinOpKind as IOp, RelationBinOpKind as BOp};
    match bin_op {
        BinOp::Relation(bin_op) => match bin_op {
            BOp::Eq => write!(f, "=="),
            BOp::Le => write!(f, "<="),
            BOp::Ge => write!(f, ">="),
            BOp::Ne => write!(f, "!="),
            BOp::Lt => write!(f, "<"),
            BOp::Gt => write!(f, ">"),
        },
        BinOp::Int(bin_op) => match bin_op {
            IOp::Add => write!(f, "+"),
            IOp::Sub => write!(f, "-"),
            IOp::Mul => write!(f, "*"),
            IOp::Div => write!(f, "/"),
        },
        BinOp::Float(bin_op) => match bin_op {
            FOp::FAdd => write!(f, "+."),
            FOp::FSub => write!(f, "-."),
            FOp::FMul => write!(f, "*."),
            FOp::FDiv => write!(f, "/."),
        },
    }
}

impl Display for LocalDecl<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: ", self.ident.value)?;
        format_ty(f, &self.ident.ty)
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
            Place::Discard => write!(f, "_"),
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
        self.format_function_instance(f, &closure.function)?;
        write!(f, "{{")?;
        closure
            .captured_args
            .iter()
            .try_for_each(|arg| write!(f, "{}, ", self.locals[*arg].ident.value))?;
        write!(f, "}}")
    }

    fn format_expr_kind(&self, f: &mut Formatter<'_>, expr_kind: &ExprKind) -> fmt::Result {
        match expr_kind {
            ExprKind::Const(lit_kind) => write!(f, "{}", lit_kind),
            ExprKind::Unary(un_op, x) => {
                format_un_op(f, un_op)?;
                write!(f, "{}", self.locals[*x].ident.value)
            }
            ExprKind::Binary(bin_op, x, y) => {
                write!(f, "{} ", self.locals[*x].ident.value)?;
                format_bin_op(f, bin_op)?;
                write!(f, " {}", self.locals[*y].ident.value)
            }
            ExprKind::ClosureMake(closure) => {
                write!(f, "Closure.make ")?;
                self.format_closure(f, closure)
            }
            ExprKind::Tuple(xs) => {
                write!(f, "(")?;
                xs.iter()
                    .try_for_each(|x| write!(f, "{}, ", self.locals[*x].ident.value))?;
                write!(f, ")")
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

    fn format_stmt_kind(&self, f: &mut Formatter<'_>, stmt_kind: &StmtKind) -> fmt::Result {
        match stmt_kind {
            StmtKind::Nop => write!(f, "\tnop"),
            StmtKind::Assign { place, value } => {
                write!(f, "\t")?;
                self.format_place(f, place)?;
                write!(f, " := ")?;
                self.format_expr_kind(f, &value.value)
            }
        }
    }

    fn format_basic_block(
        &self,
        f: &mut Formatter<'_>,
        bb: BasicBlock,
        data: &BasicBlockData,
    ) -> fmt::Result {
        write!(f, "\tbb{}", bb.index())?;
        if !data.args.is_empty() {
            write!(f, " (")?;
            data.args
                .iter()
                .try_for_each(|arg| write!(f, "{}, ", self.locals[*arg]))?;
            write!(f, ") ")?;
        }
        writeln!(f, ":")?;
        data.stmts.iter().try_for_each(|stmt| {
            write!(f, "\t")?;
            self.format_stmt_kind(f, stmt)?;
            writeln!(f)
        })
    }
}

fn format_function_def(
    f: &mut Formatter<'_>,
    func_names: &IndexVec<FnIndex, FnName<'_>>,
    function: &FunctionDef,
) -> fmt::Result {
    write!(f, "{} {{", function.name)?;
    function.local_decls[function.args_via_closure]
        .iter()
        .try_for_each(|arg| write!(f, "{arg}, "))?;
    write!(f, "}} (")?;
    function.local_decls[function.args]
        .iter()
        .try_for_each(|arg| write!(f, "{arg}, "))?;
    writeln!(f, ") {{")?;
    function
        .basic_blocks
        .iter_enumerated()
        .try_for_each(|(bb, data)| {
            BasicBlockPrinter {
                func_names,
                locals: &function.local_decls,
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
