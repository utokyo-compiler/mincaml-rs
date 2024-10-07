use crate::{BinOp, Ident};

#[derive(Debug)]
/// The interface of intrinsic library.
pub struct Mli<'ctx> {
    pub declarations: Vec<Declaration<'ctx>>,
}

#[derive(Debug)]
/// A module-level item.
pub struct Declaration<'ctx> {
    pub item_ident: ItemIdent<'ctx>,
    pub ascribed_ty: AscribedTy<'ctx>,
}

#[derive(Debug)]
pub enum ItemIdent<'ctx> {
    Var(Ident<'ctx>),

    /// An operator. Currently this is just ignored.
    Operator(Operator),
}

impl<'ctx> ItemIdent<'ctx> {
    pub fn new_ident(ident: Ident<'ctx>) -> Self {
        Self::Var(ident)
    }

    pub fn new_operator(operator: Operator) -> Self {
        Self::Operator(operator)
    }

    pub fn as_var(&self) -> Option<&Ident<'ctx>> {
        if let Self::Var(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Binary(BinOp),
}

#[derive(Debug)]
/// A type ascription. Function types are supposed to be curried and
/// not to contain complex types.
pub struct AscribedTy<'ctx> {
    pub elements: Vec<Ident<'ctx>>,
}

impl<'ctx> AscribedTy<'ctx> {
    pub fn new(elements: Vec<Ident<'ctx>>) -> Self {
        Self { elements }
    }
}
