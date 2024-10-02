use crate::{BinOp, Ident};

pub struct Mli<'ctx> {
    pub declarations: Vec<Declaration<'ctx>>,
}

/// A module-level item.
pub struct Declaration<'ctx> {
    pub item_ident: ItemIdent<'ctx>,
    pub ascribed_ty: AscribedTy<'ctx>,
}

pub enum ItemIdent<'ctx> {
    Var(Ident<'ctx>),
    Operator(Operator),
}

impl<'ctx> ItemIdent<'ctx> {
    pub fn new_ident(ident: Ident<'ctx>) -> Self {
        Self::Var(ident)
    }

    pub fn new_operator(operator: Operator) -> Self {
        Self::Operator(operator)
    }
}

pub enum Operator {
    Binary(BinOp),
}

/// A type ascription. Function types are supposed to be curried.
pub struct AscribedTy<'ctx> {
    pub elements: Vec<Ident<'ctx>>,
}

impl<'ctx> AscribedTy<'ctx> {
    pub fn new(elements: Vec<Ident<'ctx>>) -> Self {
        Self { elements }
    }
}
