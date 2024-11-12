use std::fmt::Display;

pub type LocSize = usize;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Loc {
    pub char_pos: LocSize,
}

impl Loc {
    pub fn new(char_pos: LocSize) -> Self {
        Self { char_pos }
    }
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.char_pos)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

impl Span {
    pub fn new(start: Loc, end: Loc) -> Self {
        Self { start, end }
    }
}

impl Span {
    pub fn range(&self) -> std::ops::Range<LocSize>
    where
        LocSize: std::ops::Add + Copy,
    {
        self.start.char_pos..self.end.char_pos
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpanOrigin {
    UserDefined(Span),
    CompilerGenerated,
}

impl SpanOrigin {
    pub fn as_user_defined(&self) -> Option<&Span> {
        if let Self::UserDefined(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub node: T,
    pub span: SpanOrigin,
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node)
    }
}

impl<T> Spanned<T> {
    pub fn new(node: T, (start, end): (Loc, Loc)) -> Self {
        Self {
            node,
            span: SpanOrigin::UserDefined(Span { start, end }),
        }
    }
}
