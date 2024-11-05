use std::fmt::Display;
use std::path::PathBuf;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MultiSpan {
    pub spans: Vec<Span>,
}

impl MultiSpan {
    pub fn new() -> Self {
        Self {
            spans: Default::default(),
        }
    }
}

impl Default for MultiSpan {
    fn default() -> Self {
        Self::new()
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

pub struct MultipleInput {
    files: Vec<InputFile>,
    offsets: Vec<usize>,
    offset_accumulated: usize,
}

impl MultipleInput {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            offsets: Vec::new(),
            offset_accumulated: 0,
        }
    }

    pub fn add_file(&mut self, file: InputFile) {
        self.offsets.push(self.offset_accumulated);
        self.offset_accumulated += file.content().chars().count();
        self.files.push(file);
    }

    pub fn concatenated_string(&self) -> String {
        let vec: Vec<_> = self.files.iter().map(|file| file.content()).collect();
        vec.concat()
    }

    pub fn get(&self, loc: Loc) -> Option<(&'_ InputFile, Loc)> {
        let idx = self
            .offsets
            .binary_search(&loc.char_pos)
            .unwrap_or_else(|idx| idx - 1);
        Some((&self.files[idx], Loc::new(loc.char_pos - self.offsets[idx])))
    }
}

impl Default for MultipleInput {
    fn default() -> Self {
        Self::new()
    }
}

pub enum InputFile {
    File { path: PathBuf, content: String },
    String { content: String },
}

impl InputFile {
    pub fn content(&self) -> &str {
        match self {
            InputFile::File { content, .. } => content,
            InputFile::String { content } => content,
        }
    }
}
