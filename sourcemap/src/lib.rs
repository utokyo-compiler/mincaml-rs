#[derive(Debug, Clone, Copy, Default)]
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

impl<T> Span<T> {
    pub fn new(start: T, end: T) -> Self {
        Self { start, end }
    }
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

pub struct MultipleInputFiles {
    strings: Vec<String>,
    offset_accum: Vec<usize>,
}

impl MultipleInputFiles {
    pub fn new(strings: Vec<String>) -> Self {
        let offset_accum = strings
            .iter()
            .scan(0, |offset, s| {
                let old_offset = *offset;
                *offset += s.chars().count();
                Some(old_offset)
            })
            .collect();
        Self {
            strings,
            offset_accum,
        }
    }
    pub fn concatenated(&self) -> String {
        self.strings.join("")
    }
    pub fn get(&self, loc: Loc) -> Option<(&'_ str, Loc)> {
        let idx = self
            .offset_accum
            .binary_search(&loc.char_pos)
            .unwrap_or_else(|idx| idx - 1);
        Some((
            &self.strings[idx],
            Loc::new(loc.char_pos - self.offset_accum[idx]),
        ))
    }
}
