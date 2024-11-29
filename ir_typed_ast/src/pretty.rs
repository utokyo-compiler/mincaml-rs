use std::fmt::Display;

use crate::UnOp;

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOp::Not => write!(f, "not"),
            UnOp::Ineg => write!(f, "-"),
            UnOp::Fneg => write!(f, "-."),
        }
    }
}
