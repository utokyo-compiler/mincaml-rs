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
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, (start, end): (Loc, Loc)) -> Self {
        Self {
            node,
            span: Span { start, end },
        }
    }
}

impl<T> Spanned<T> {
    pub fn range(&self) -> std::ops::Range<LocSize>
    where
        LocSize: std::ops::Add + Copy,
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
