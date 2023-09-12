#[derive(Debug, Clone, Copy)]
pub struct Loc<Size = usize> {
    pub char_pos: Size,
}

impl<Size> Loc<Size> {
    pub fn new(char_pos: Size) -> Self {
        Self { char_pos }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span<T> {
    pub start: T,
    pub end: T,
}

impl<U> Span<Loc<U>> {
    pub fn range(&self) -> std::ops::Range<U>
    where
        U: std::ops::Add + Copy,
    {
        self.start.char_pos..self.end.char_pos
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T, U = Loc<usize>> {
    pub node: T,
    pub span: Span<U>,
}

impl<T, U> Spanned<T, U> {
    pub fn new(node: T, (start, end): (U, U)) -> Self {
        Self {
            node,
            span: Span { start, end },
        }
    }
}

impl<T, U> Spanned<T, Loc<U>> {
    pub fn range(&self) -> std::ops::Range<U>
    where
        U: std::ops::Add + Copy,
    {
        self.span.range()
    }
}
