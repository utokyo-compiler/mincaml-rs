#![feature(negative_impls)]

mod annotate_snippets_emitter;
pub mod context;

pub type DiagnosticEmitter = annotate_snippets_emitter::AnnotateSnippetEmitter;

use std::borrow::Cow;

use data_structure::FxHashMap;
use fluent_bundle::FluentValue;
use sourcemap::{MultiSpan, Span};

pub trait Diagnostic {
    fn into_diag(self, dcx: &context::DiagContext, level: Level) -> Diag<'_>;
}

#[must_use]
pub struct Diag<'dcx> {
    pub dcx: &'dcx context::DiagContext,

    /// Why the `Option`? It is always `Some` until the `Diag` is consumed via
    /// `emit`, `cancel`, etc. At that point it is consumed and replaced with
    /// `None`. Then `drop` checks that it is `None`; if not, it panics because
    /// a diagnostic was built but not used.
    diag: Option<DiagInner>,
}

impl<'dcx> Diag<'dcx> {
    pub fn emit(mut self) {
        self.dcx.emit_diagnostic(self.diag.take().unwrap());
    }
}

// Cloning a `Diag` is a recipe for a diagnostic being emitted twice, which
// would be bad.
impl !Clone for Diag<'_> {}

impl std::ops::Deref for Diag<'_> {
    type Target = DiagInner;

    fn deref(&self) -> &DiagInner {
        self.diag.as_ref().unwrap()
    }
}

impl std::ops::DerefMut for Diag<'_> {
    fn deref_mut(&mut self) -> &mut DiagInner {
        self.diag.as_mut().unwrap()
    }
}

/// Destructor bomb: every `Diag` must be consumed (emitted, cancelled, etc.)
/// or we emit a bug.
impl Drop for Diag<'_> {
    fn drop(&mut self) {
        if let Some(diag) = self.diag.take() {
            self.dcx.emit_diagnostic(DiagInner::new(
                Level::Bug,
                DiagMessage::from("the following error was constructed but not emitted"),
            ));
            self.dcx.emit_diagnostic(diag);
            panic!("error was constructed but not emitted");
        }
    }
}

#[must_use]
#[derive(Clone, Debug)]
pub struct DiagInner {
    pub args: DiagArgMap,
    pub content: DiagContent,
    pub children: Vec<DiagContent>,

    /// This is not used for highlighting or rendering any error message. Rather, it can be used
    /// as a sort key to sort a buffer of diagnostics. By default, it is `Some` of the primary span of
    /// `span` if there is one. Otherwise, it is `None`.
    pub primary_span: Option<Span>,
}

#[derive(Clone, Debug)]
pub struct DiagContent {
    pub level: Level,
    pub messages: Vec<DiagMessage>,
    pub span: MultiSpan,
}

#[derive(Clone, Debug)]
pub enum Level {
    Error,
    Info,
    Bug,
}

#[derive(Clone, Debug)]
pub enum DiagMessage {
    /// Non-translatable diagnostic message.
    Str(Cow<'static, str>),

    /// Translatable message which has been already translated.
    Translated(Cow<'static, str>),

    /// Identifier for a Fluent message corresponding to the diagnostic
    /// message. Yet to be translated.
    ///
    /// <https://projectfluent.org/fluent/guide/hello.html>
    /// <https://projectfluent.org/fluent/guide/attributes.html>
    FluentIdentifier(Cow<'static, str>),
}

impl From<String> for DiagMessage {
    fn from(s: String) -> Self {
        DiagMessage::Str(Cow::Owned(s))
    }
}
impl From<&'static str> for DiagMessage {
    fn from(s: &'static str) -> Self {
        DiagMessage::Str(Cow::Borrowed(s))
    }
}
impl From<Cow<'static, str>> for DiagMessage {
    fn from(s: Cow<'static, str>) -> Self {
        DiagMessage::Str(s)
    }
}

pub type DiagArgMap = FxHashMap<DiagArgName, DiagArgValue>;

/// Name of a diagnostic argument.
pub type DiagArgName = Cow<'static, str>;

/// Simplified version of `FluentValue`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DiagArgValue {
    Str(Cow<'static, str>),
    Number(i32),
}

/// Converts a value of a type into a `DiagArg` (typically a field of an `Diag` struct).
/// Implemented as a custom trait rather than `From` so that it is implemented on the type being
/// converted rather than on `DiagArgValue`, which enables types from other crates to
/// implement this.
pub trait IntoDiagArg {
    fn into_diag_arg(self) -> DiagArgValue;
}

impl IntoDiagArg for DiagArgValue {
    fn into_diag_arg(self) -> DiagArgValue {
        self
    }
}

impl From<DiagArgValue> for FluentValue<'static> {
    fn from(val: DiagArgValue) -> Self {
        match val {
            DiagArgValue::Str(s) => From::from(s),
            DiagArgValue::Number(n) => From::from(n),
        }
    }
}

impl DiagInner {
    pub fn new(level: Level, message: impl Into<DiagMessage>) -> Self {
        Self::new_with_messages(level, vec![message.into()])
    }

    pub fn new_with_messages(level: Level, messages: Vec<DiagMessage>) -> Self {
        Self {
            args: Default::default(),
            content: DiagContent {
                level,
                messages,
                span: MultiSpan::new(),
            },
            children: Vec::default(),
            primary_span: None,
        }
    }

    pub(crate) fn arg(&mut self, name: impl Into<DiagArgName>, arg: impl IntoDiagArg) {
        self.args.insert(name.into(), arg.into_diag_arg());
    }
}
