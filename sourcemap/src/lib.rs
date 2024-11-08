use std::{fmt::Display, borrow::Cow, path::PathBuf};

use data_structure::index::{
    vec::{Idx, IndexVec},
    Indexable,
};

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

    pub fn into_range(self) -> core::ops::Range<usize> {
        self.start.char_pos..self.end.char_pos
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

pub struct MultipleInput {
    files: IndexVec<InputIndex, InputFile>,
    offsets: Vec<usize>,
    offset_accumulated: usize,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct InputIndex(usize);

impl Idx for InputIndex {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}

impl Indexable<InputIndex> for InputFile {}

impl MultipleInput {
    pub fn new() -> Self {
        Self {
            files: IndexVec::new(),
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
        self.files.iter().map(InputFile::content).collect()
    }

    fn get(&self, loc: Loc) -> InputIndex {
        InputIndex(
            self.offsets
                .binary_search(&loc.char_pos)
                .unwrap_or_else(|idx| idx - 1),
        )
    }

    pub fn get_span_index(&self, span: Span) -> Option<InputIndex> {
        let start_index = self.get(span.start);
        let end_index = self.get(span.end);
        (start_index == end_index).then_some(start_index)
    }

    pub fn files(&self) -> &IndexVec<InputIndex, InputFile> {
        &self.files
    }

    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct LocalLoc(pub Loc);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalSpan {
    pub start: LocalLoc,
    pub end: LocalLoc,
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
    pub fn origin_string(&self) -> Cow<'_, str> {
        match self {
            InputFile::File { path, .. } => path.to_string_lossy(),
            InputFile::String { .. } => Cow::Borrowed("<input>"),
        }
    }
}
