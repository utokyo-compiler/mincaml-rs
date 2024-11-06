#![feature(negative_impls)]
#![feature(error_reporter)]

mod annotate_snippets_emitter;
mod context;
mod translation;

pub use context::DiagContext;

pub(crate) type DiagnosticEmitter<'dcx> = annotate_snippets_emitter::AnnotateSnippetEmitter<'dcx>;

pub type FluentBundle = fluent_bundle::FluentBundle<fluent_bundle::FluentResource>;
pub type FluentIdentifier = Cow<'static, str>;

use std::{
    borrow::Cow,
    ops::{Deref, DerefMut},
};

use data_structure::{FxHashMap, FxHashSet};
use fluent_bundle::FluentValue;
use sourcemap::Span;

pub trait Diagnostic {
    fn into_diag<'dcx>(self, dcx: &'dcx context::DiagContext<'dcx>, level: Level) -> Diag<'dcx>;
}

#[must_use]
pub struct Diag<'dcx> {
    pub dcx: &'dcx context::DiagContext<'dcx>,

    /// Why the `Option`? It is always `Some` until the `Diag` is consumed via
    /// `emit`, `cancel`, etc. At that point it is consumed and replaced with
    /// `None`. Then `drop` checks that it is `None`; if not, it panics because
    /// a diagnostic was built but not used.
    diag: Option<DiagInner<'dcx>>,
}

impl<'dcx> Diag<'dcx> {
    pub fn emit(mut self) {
        self.dcx.emit_diagnostic(self.diag.take().unwrap());
    }
}

// Cloning a `Diag` is a recipe for a diagnostic being emitted twice, which
// would be bad.
impl !Clone for Diag<'_> {}

impl<'dcx> Deref for Diag<'dcx> {
    type Target = DiagInner<'dcx>;

    fn deref(&self) -> &DiagInner<'dcx> {
        self.diag.as_ref().unwrap()
    }
}

impl<'dcx> DerefMut for Diag<'dcx> {
    fn deref_mut(&mut self) -> &mut DiagInner<'dcx> {
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

/// The main part of a diagnostic. Note that `Diag`, which wraps this type, is
/// used for most operations, and should be used instead whenever possible.
#[must_use]
#[derive(Clone, Debug)]
pub struct DiagInner<'dcx> {
    pub args: DiagArgMap,
    pub content: DiagContent<'dcx>,

    /// Sub-diagnostics attached to a parent diagnostic.
    /// For example, a note attached to an error.
    pub children: Vec<DiagContent<'dcx>>,
}

#[derive(Clone, Debug)]
pub struct DiagContent<'dcx> {
    pub level: Level,
    pub messages: Vec<DiagMessage<'dcx>>,
    pub span: MultiSpan<'dcx>,
}

impl<'dcx> DiagContent<'dcx> {
    /// A small diagnostic becomes a footer in the parent diagnostic.  
    pub fn is_small(&self) -> Option<(Level, &[DiagMessage<'dcx>])> {
        if self.span.is_empty() {
            Some((self.level, &self.messages))
        } else {
            None
        }
    }

    pub fn new(level: Level) -> Self {
        Self {
            level,
            messages: Vec::new(),
            span: MultiSpan::default(),
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub enum Level {
    Error,
    Info,
    Bug,
}

#[derive(Clone, Debug)]
pub enum DiagMessage<'dcx> {
    /// Non-translatable diagnostic message.
    Str(Cow<'static, str>),

    /// Translatable message which has been already translated.
    Translated(Cow<'dcx, str>),

    /// Identifier for a Fluent message corresponding to the diagnostic
    /// message. Yet to be translated.
    ///
    /// <https://projectfluent.org/fluent/guide/hello.html>
    /// <https://projectfluent.org/fluent/guide/attributes.html>
    FluentIdentifier {
        identifier: FluentIdentifier,
        attribute: Option<FluentIdentifier>,
    },
}

impl<'dcx> DiagMessage<'dcx> {
    pub fn identifier(identifier: impl Into<FluentIdentifier>) -> Self {
        DiagMessage::FluentIdentifier {
            identifier: identifier.into(),
            attribute: None,
        }
    }
    pub fn attribute(
        identifier: impl Into<FluentIdentifier>,
        attribute: impl Into<FluentIdentifier>,
    ) -> Self {
        DiagMessage::FluentIdentifier {
            identifier: identifier.into(),
            attribute: Some(attribute.into()),
        }
    }
}

impl From<String> for DiagMessage<'_> {
    fn from(s: String) -> Self {
        DiagMessage::Str(Cow::Owned(s))
    }
}
impl From<&'static str> for DiagMessage<'_> {
    fn from(s: &'static str) -> Self {
        DiagMessage::Str(Cow::Borrowed(s))
    }
}
impl From<Cow<'static, str>> for DiagMessage<'_> {
    fn from(s: Cow<'static, str>) -> Self {
        DiagMessage::Str(s)
    }
}

/// A collection of `Span`s.
///
/// Spans have two orthogonal attributes:
///
/// - They can be *primary spans*. In this case they are the locus of
///   the error, and would be rendered with `^^^`.
/// - They can have a *label*. In this case, the label is written next
///   to the mark in the snippet when we render.
#[derive(Clone, Debug, Default)]
pub struct MultiSpan<'bundle> {
    primary_span: Vec<Span>,
    span_labels: Vec<(Span, DiagMessage<'bundle>)>,
}

impl<'bundle> MultiSpan<'bundle> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.primary_span.is_empty() && self.span_labels.is_empty()
    }

    pub fn primary_span(&self) -> Option<&Span> {
        self.primary_span.first()
    }

    pub fn add_primary_span(&mut self, span: Span) {
        self.primary_span.push(span);
    }

    pub fn add_label(&mut self, span: Span, label: DiagMessage<'bundle>) {
        self.span_labels.push((span, label));
    }

    pub fn iter_labeled<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a Span, Option<&'a DiagMessage<'bundle>>)> {
        pub struct Iter<IterA, IterB> {
            iter_a: Option<IterA>,
            iter_b: Option<IterB>,
            seen: FxHashSet<Span>,
        }

        impl<IterA, IterB> Iter<IterA, IterB> {
            pub fn new(iter_a: IterA, iter_b: IterB, seen: FxHashSet<Span>) -> Self {
                Self {
                    iter_a: Some(iter_a),
                    iter_b: Some(iter_b),
                    seen,
                }
            }
        }
        impl<'a, 'bundle: 'a, IterA, IterB> Iterator for Iter<IterA, IterB>
        where
            IterA: Iterator<Item = &'a (Span, DiagMessage<'bundle>)>,
            IterB: Iterator<Item = &'a Span>,
        {
            type Item = (&'a Span, Option<&'a DiagMessage<'bundle>>);
            fn next(&mut self) -> Option<Self::Item> {
                #[inline]
                fn and_then_or_clear<T, U>(
                    opt: &mut Option<T>,
                    f: impl FnOnce(&mut T) -> Option<U>,
                ) -> Option<U> {
                    let x = f(opt.as_mut()?);
                    if x.is_none() {
                        *opt = None;
                    }
                    x
                }

                if let Some((span, label)) = and_then_or_clear(&mut self.iter_a, Iterator::next) {
                    self.seen.insert(*span);
                    Some((span, Some(label)))
                } else {
                    self.iter_b.as_mut()?.find_map(|span| {
                        if self.seen.contains(span) {
                            None
                        } else {
                            self.seen.insert(*span);
                            Some((span, None))
                        }
                    })
                }
            }
        }

        Iter::new(
            self.span_labels.iter(),
            self.primary_span.iter(),
            FxHashSet::default(),
        )
    }
}

pub type DiagArg<'a> = (&'a DiagArgName, &'a DiagArgValue);
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

/// `Diag` impls many `&mut self -> &mut Self` methods. Each one modifies an
/// existing diagnostic, either in a standalone fashion, e.g.
/// `err.code(code);`, or in a chained fashion to make multiple modifications,
/// e.g. `err.code(code).span(span);`.
///
/// This macro creates an equivalent `self -> Self` method, with a `with_`
/// prefix. This can be used in a chained fashion when making a new diagnostic,
/// e.g. `let err = struct_err(msg).with_code(code);`, or emitting a new
/// diagnostic, e.g. `struct_err(msg).with_code(code).emit();`.
///
/// Although the latter method can be used to modify an existing diagnostic,
/// e.g. `err = err.with_code(code);`, this should be avoided because the former
/// method gives shorter code, e.g. `err.code(code);`.
///
/// Note: the `with_` methods are added only when needed. If you want to use
/// one and it's not defined, feel free to add it.
///
/// Note: any doc comments must be within the `with_fn!` call.
macro_rules! with_fn {
    {
        $with_f:ident,
        $(#[$attrs:meta])*
        pub fn $f:ident(&mut $self:ident, $($name:ident: $ty:ty),* $(,)?) -> &mut Self {
            $($body:tt)*
        }
    } => {
        // The original function.
        $(#[$attrs])*
        pub fn $f(&mut $self, $($name: $ty),*) -> &mut Self {
            $($body)*
        }

        // The `with_*` variant.
        $(#[$attrs])*
        #[doc = concat!("See [`Diag::", stringify!($f), "()`].")]
        pub fn $with_f(mut $self, $($name: $ty),*) -> Self {
            $self.$f($($name),*);
            $self
        }
    };
}

impl<'dcx> Diag<'dcx> {
    with_fn! { with_arg,
    /// Add an argument.
    pub fn arg(&mut self, name: impl Into<DiagArgName>, arg: impl IntoDiagArg) -> &mut Self {
        self.deref_mut().arg(name, arg);
        self
    } }
}

impl<'dcx> DiagInner<'dcx> {
    pub fn new(level: Level, message: impl Into<DiagMessage<'dcx>>) -> Self {
        Self::new_with_messages(level, vec![message.into()])
    }

    pub fn new_with_messages(level: Level, messages: Vec<DiagMessage<'dcx>>) -> Self {
        Self {
            args: Default::default(),
            content: DiagContent {
                level,
                messages,
                span: MultiSpan::default(),
            },
            children: Vec::default(),
        }
    }

    pub(crate) fn arg(&mut self, name: impl Into<DiagArgName>, arg: impl IntoDiagArg) {
        self.args.insert(name.into(), arg.into_diag_arg());
    }
}
