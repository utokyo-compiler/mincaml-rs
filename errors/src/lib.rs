#![feature(negative_impls)]
#![feature(error_reporter)]
#![feature(trait_alias)]
#![feature(exitcode_exit_method)]

use context::{Early, Late};
use macros::counterpart;

mod annotate_snippets_emitter;
mod context;
mod diagnostic_impls;
mod translation;

#[counterpart(rustc_session::session::EarlyDiagCtxt)]
pub type EarlyDiagContext = context::DiagContext<Early<'static>>;
#[counterpart(rustc_errors::DiagCtxt)]
pub type DiagContext<'dcx> = context::DiagContext<Late<'dcx>>;

pub(crate) type SelectedEmitter<T> = annotate_snippets_emitter::AnnotateSnippetEmitter<T>;

pub type EarlyDiagnosticEmitter = annotate_snippets_emitter::AnnotateSnippetEmitter<Early<'static>>;
pub type DiagnosticEmitter<'dcx> = annotate_snippets_emitter::AnnotateSnippetEmitter<Late<'dcx>>;

pub type FluentBundle = fluent_bundle::FluentBundle<fluent_bundle::FluentResource>;
pub type FluentAstIdent = Cow<'static, str>;

use std::{
    borrow::Cow,
    ops::{Deref, DerefMut},
};

use data_structure::{FxHashMap, FxHashSet};
use fluent_bundle::FluentValue;
use sourcemap::Span;

#[counterpart(rustc_errors::diagnostic::Diagnostic)]
pub trait Diagnostic {
    fn into_diag<'dcx>(self, dcx: &'dcx DiagContext<'dcx>, level: Level) -> Diag<'dcx>;
}

#[counterpart(rustc_errors::Diag)]
#[must_use]
pub struct Diag<'dcx> {
    pub dcx: &'dcx DiagContext<'dcx>,

    /// Why the `Option`? It is always `Some` until the `Diag` is consumed via
    /// `emit`, `cancel`, etc. At that point it is consumed and replaced with
    /// `None`. Then `drop` checks that it is `None`; if not, it panics because
    /// a diagnostic was built but not used.
    ///
    /// Why the Box? `DiagInner` is a large type, and `Diag` is often used as a
    /// return value, especially within the frequently-used `PResult` type. In
    /// theory, return value optimization (RVO) should avoid unnecessary
    /// copying. In practice, it does not (at the time of writing).
    diag: Option<Box<DiagInner<'dcx>>>,
}

impl<'dcx> Diag<'dcx> {
    pub fn emit(mut self) {
        self.dcx.emit_diagnostic(*self.diag.take().unwrap());
    }

    pub fn cancel(mut self) {
        self.diag.take();
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
            self.dcx.emit_diagnostic(*diag);
            panic!("error was constructed but not emitted");
        }
    }
}

/// `Early` version of [`Diag`].
#[must_use]
pub struct EarlyDiag<'dcx> {
    pub early_dcx: &'dcx EarlyDiagContext,
    diag: Option<Box<DiagInner<'dcx>>>,
}

impl<'dcx> EarlyDiag<'dcx> {
    pub fn emit(mut self) {
        self.early_dcx
            .early_emit_diagnostic(*self.diag.take().unwrap());
    }

    pub fn cancel(mut self) {
        self.diag.take();
    }
}

impl !Clone for EarlyDiag<'_> {}

impl<'dcx> Deref for EarlyDiag<'dcx> {
    type Target = DiagInner<'dcx>;

    fn deref(&self) -> &DiagInner<'dcx> {
        self.diag.as_ref().unwrap()
    }
}

impl<'dcx> DerefMut for EarlyDiag<'dcx> {
    fn deref_mut(&mut self) -> &mut DiagInner<'dcx> {
        self.diag.as_mut().unwrap()
    }
}

impl Drop for EarlyDiag<'_> {
    fn drop(&mut self) {
        if let Some(diag) = self.diag.take() {
            self.early_dcx.early_emit_diagnostic(DiagInner::new(
                Level::Bug,
                DiagMessage::from("the following error was constructed but not emitted"),
            ));
            self.early_dcx.early_emit_diagnostic(*diag);
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
    Note,
    Help,
    Warning,
    Bug,
}

#[counterpart(rustc_error_messages::DiagMessage)]
#[derive(Clone, Debug)]
pub enum DiagStr<'dcx> {
    /// Non-translatable diagnostic message.
    NonTranslatable(Cow<'static, str>),

    /// Translatable message which has been already translated.
    Translated(Cow<'dcx, str>),
}

impl<'dcx> DiagStr<'dcx> {
    pub fn as_cow(&self) -> &Cow<'dcx, str> {
        match self {
            DiagStr::NonTranslatable(s) => s,
            DiagStr::Translated(s) => s,
        }
    }
}

#[counterpart(rustc_error_messages::DiagMessage)]
#[derive(Clone, Debug)]
pub enum DiagMessage<'dcx> {
    /// Non-translatable diagnostic message
    /// or translatable message which has been already translated.
    Str(DiagStr<'dcx>),

    /// Identifier for a Fluent message corresponding to the diagnostic
    /// message. Yet to be translated.
    ///
    /// <https://projectfluent.org/fluent/guide/hello.html>
    /// <https://projectfluent.org/fluent/guide/attributes.html>
    FluentIdentifier {
        identifier: FluentIdentifier,
        attribute: Option<FluentAttr>,
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
        attribute: impl Into<FluentAttr>,
    ) -> Self {
        DiagMessage::FluentIdentifier {
            identifier: identifier.into(),
            attribute: Some(attribute.into()),
        }
    }

    /// Given a `SubdiagMessage` which may contain a Fluent attribute, create a new
    /// `DiagMessage` that combines that attribute with the Fluent identifier of `self`.
    ///
    /// - If the `SubdiagMessage` is non-translatable then return the message as a `DiagMessage`.
    /// - If `self` is non-translatable then return `self`'s message.
    pub fn with_subdiagnostic_message(&self, sub: SubdiagMessage<'dcx>) -> Self {
        let attr = match sub {
            SubdiagMessage::Str(s) => return DiagMessage::Str(s),
            SubdiagMessage::FluentIdentifier(id) => {
                return DiagMessage::identifier(id);
            }
            SubdiagMessage::FluentAttr(attr) => attr,
        };

        match self {
            DiagMessage::Str(s) => DiagMessage::Str(s.clone()),
            DiagMessage::FluentIdentifier { identifier, .. } => {
                DiagMessage::attribute(identifier.clone(), attr)
            }
        }
    }
}

impl From<String> for DiagMessage<'_> {
    fn from(s: String) -> Self {
        DiagMessage::Str(DiagStr::NonTranslatable(Cow::Owned(s)))
    }
}
impl From<&'static str> for DiagMessage<'_> {
    fn from(s: &'static str) -> Self {
        DiagMessage::Str(DiagStr::NonTranslatable(Cow::Borrowed(s)))
    }
}
impl From<Cow<'static, str>> for DiagMessage<'_> {
    fn from(s: Cow<'static, str>) -> Self {
        DiagMessage::Str(DiagStr::NonTranslatable(s))
    }
}

impl From<FluentIdentifier> for DiagMessage<'_> {
    fn from(id: FluentIdentifier) -> Self {
        DiagMessage::identifier(id)
    }
}

#[derive(Clone, Debug)]
pub struct FluentIdentifier(pub FluentAstIdent);

impl Deref for FluentIdentifier {
    type Target = FluentAstIdent;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl FluentIdentifier {
    pub fn new(s: impl Into<FluentAstIdent>) -> Self {
        Self(s.into())
    }
}

#[derive(Clone, Debug)]
pub struct FluentAttr(pub FluentAstIdent);

impl Deref for FluentAttr {
    type Target = FluentAstIdent;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl FluentAttr {
    pub fn new(s: impl Into<FluentAstIdent>) -> Self {
        Self(s.into())
    }
}

/// Abstraction over a message in a subdiagnostic (i.e. label, note, help, etc) to support both
/// translatable and non-translatable diagnostic messages.
///
/// Translatable messages for subdiagnostics are typically attributes attached to a larger Fluent
/// message so messages of this type must be combined with a `DiagMessage` (using
/// `DiagMessage::with_subdiagnostic_message`) before rendering. However, subdiagnostics from
/// the `Subdiagnostic` derive refer to Fluent identifiers directly.
#[counterpart(rustc_error_messages::SubdiagMessage)]
pub enum SubdiagMessage<'dcx> {
    /// Non-translatable diagnostic message
    /// or translatable message which has been already translated.
    Str(DiagStr<'dcx>),

    /// Identifier of a Fluent message. Instances of this variant are generated by the
    /// `Subdiagnostic` derive.
    FluentIdentifier(FluentIdentifier),

    /// Attribute of a Fluent message. Needs to be combined with a Fluent identifier to produce an
    /// actual translated message. Instances of this variant are generated by the `fluent_messages`
    /// macro.
    ///
    /// <https://projectfluent.org/fluent/guide/attributes.html>
    FluentAttr(FluentAttr),
}

impl From<String> for SubdiagMessage<'_> {
    fn from(s: String) -> Self {
        SubdiagMessage::Str(DiagStr::NonTranslatable(Cow::Owned(s)))
    }
}
impl From<&'static str> for SubdiagMessage<'_> {
    fn from(s: &'static str) -> Self {
        SubdiagMessage::Str(DiagStr::NonTranslatable(Cow::Borrowed(s)))
    }
}
impl From<Cow<'static, str>> for SubdiagMessage<'_> {
    fn from(s: Cow<'static, str>) -> Self {
        SubdiagMessage::Str(DiagStr::NonTranslatable(s))
    }
}
impl From<FluentIdentifier> for SubdiagMessage<'_> {
    fn from(id: FluentIdentifier) -> Self {
        SubdiagMessage::FluentIdentifier(id)
    }
}
impl From<FluentAttr> for SubdiagMessage<'_> {
    fn from(id: FluentAttr) -> Self {
        SubdiagMessage::FluentAttr(id)
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
#[counterpart(rustc_error_messages::MultiSpan)]
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

pub fn new_fluent_bundle(resources: Vec<&'static str>) -> FluentBundle {
    use unic_langid::langid;
    let mut fluent_bundle = FluentBundle::new(vec![langid!("en-US")]);

    // See comment in `fluent_bundle`.
    fluent_bundle.set_use_isolating(false);

    for resource in resources {
        let resource = fluent_bundle::FluentResource::try_new(resource.to_string())
            .expect("failed to parse fallback fluent resource");
        fluent_bundle.add_resource_overriding(resource);
    }

    fluent_bundle
}

pub type DiagArg<'a> = (&'a DiagArgName, &'a DiagArgValue);
pub type DiagArgMap = FxHashMap<DiagArgName, DiagArgValue>;

/// Name of a diagnostic argument.
pub type DiagArgName = Cow<'static, str>;

/// Simplified version of `FluentValue`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[counterpart(rustc_errors::diagnostic::DiagArgValue)]
pub enum DiagArgValue {
    Str(Cow<'static, str>),
    Number(i32),
}

/// Converts a value of a type into a `DiagArg` (typically a field of an `Diag` struct).
/// Implemented as a custom trait rather than `From` so that it is implemented on the type being
/// converted rather than on `DiagArgValue`, which enables types from other crates to
/// implement this.
#[counterpart(rustc_errors::diagnostic::IntoDiagArg)]
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

#[diagnostic::on_unimplemented(
    message = "`{Self}` cannot be a primary span",
    label = "this type cannot be used as a primary span",
    note = "consider using `Span` or `IntoIterator` of it"
)]
pub trait PrimarySpan {
    fn primary_spans(self) -> impl Iterator<Item = Span>;
}

impl PrimarySpan for Span {
    fn primary_spans(self) -> impl Iterator<Item = Span> {
        std::iter::once(self)
    }
}

impl PrimarySpan for Option<Span> {
    fn primary_spans(self) -> impl Iterator<Item = Span> {
        self.into_iter()
    }
}

impl PrimarySpan for Vec<Span> {
    fn primary_spans(self) -> impl Iterator<Item = Span> {
        self.into_iter()
    }
}

#[diagnostic::on_unimplemented(
    message = "`{Self}` cannot be used as a type of #[label], #[help], etc.",
    label = "`#[label], #[help], #[note] and #[warning] does not recognize this type",
    note = "consider using `Span`, `()`, `Option<()>`"
)]
pub trait TryIntoMultiSpan<'dcx> {
    fn try_into_spans(self) -> Option<MultiSpan<'dcx>>;
}

impl<'dcx> TryIntoMultiSpan<'dcx> for Span {
    fn try_into_spans(self) -> Option<MultiSpan<'dcx>> {
        let mut multi_span = MultiSpan::default();
        multi_span.add_primary_span(self);
        Some(multi_span)
    }
}

impl<'dcx> TryIntoMultiSpan<'dcx> for () {
    fn try_into_spans(self) -> Option<MultiSpan<'dcx>> {
        Some(MultiSpan::default())
    }
}

/// A `Span` that may be missing. This is useful when a span is optional, but
/// we want to emit the subdiagnostic anyway.
///
/// This example shows how this type differs from `Option<Span>`:
///
/// ```ignore (illustrative)
/// SomeError {
///     #[warn]
///     span: AllowMissingSpan::Exist(span),
///     ..
/// }.into_diag().emit(); // emits the subdiagnostic with the span
///
/// SomeError {
///     #[warn]
///     span: AllowMissingSpan::Missing,
///     ..
/// }.into_diag().emit(); // emits the subdiagnostic without a span
///
/// SomeError {
///     #[warn]
///     span: Some(span),
///     ..
/// }.into_diag().emit(); // emits the subdiagnostic with the span
///
/// SomeError {
///     #[warn]
///     span: None,
///     ..
/// }.into_diag().emit(); // does not emit the subdiagnostic
/// ```
pub enum AllowMissingSpan {
    Exist(Span),
    Missing,
}

impl AllowMissingSpan {
    /// Converts from an `Option<Span>`.
    pub fn show_if_missing(opt: Option<Span>) -> Self {
        match opt {
            Some(span) => AllowMissingSpan::Exist(span),
            None => AllowMissingSpan::Missing,
        }
    }

    /// Converts back into an `Option<Span>`.
    pub fn hide_if_none(self) -> Option<Span> {
        match self {
            AllowMissingSpan::Exist(span) => Some(span),
            AllowMissingSpan::Missing => None,
        }
    }
}

impl From<Option<Span>> for AllowMissingSpan {
    fn from(opt: Option<Span>) -> Self {
        AllowMissingSpan::show_if_missing(opt)
    }
}

impl From<AllowMissingSpan> for Option<Span> {
    fn from(ams: AllowMissingSpan) -> Self {
        ams.hide_if_none()
    }
}

impl<'dcx> TryIntoMultiSpan<'dcx> for AllowMissingSpan {
    fn try_into_spans(self) -> Option<MultiSpan<'dcx>> {
        match self {
            AllowMissingSpan::Exist(span) => span.try_into_spans(),
            AllowMissingSpan::Missing => ().try_into_spans(),
        }
    }
}

impl<'dcx, T: TryIntoMultiSpan<'dcx>> TryIntoMultiSpan<'dcx> for Option<T> {
    fn try_into_spans(self) -> Option<MultiSpan<'dcx>> {
        self.and_then(TryIntoMultiSpan::try_into_spans)
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
#[counterpart(rustc_errors::diagnostic::with_fn)]
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

#[counterpart(rustc_errors::diagnostic::Diag)]
impl<'dcx> Diag<'dcx> {
    #[track_caller]
    pub fn new(
        dcx: &'dcx DiagContext<'dcx>,
        level: Level,
        message: impl Into<DiagMessage<'dcx>>,
    ) -> Self {
        Self::new_diagnostic(dcx, DiagInner::new(level, message))
    }

    /// Creates a new `Diag` with an already constructed diagnostic.
    #[track_caller]
    pub(crate) fn new_diagnostic(dcx: &'dcx DiagContext<'dcx>, diag: DiagInner<'dcx>) -> Self {
        Self {
            dcx,
            diag: Some(Box::new(diag)),
        }
    }

    with_fn! { with_arg,
    /// Add an argument.
    pub fn arg(&mut self, name: impl Into<DiagArgName>, arg: impl IntoDiagArg) -> &mut Self {
        self.deref_mut().arg(name, arg);
        self
    } }

    with_fn! { with_primary_span,
    /// Add a primary span.
    pub fn primary_span(&mut self, span: impl PrimarySpan) -> &mut Self {
        for span in span.primary_spans() {
            self.content.span.add_primary_span(span);
        }
        self
    } }

    with_fn! { with_label,
    /// Add a label to span.
    pub fn label(&mut self, label: impl Into<SubdiagMessage<'dcx>>, span: Span) -> &mut Self {
        let msg = self.subdiagnostic_message_to_diagnostic_message(label);
        self.content.span.add_label(span, msg);
        self
    } }

    pub fn may_add_sub(
        &mut self,
        level: Level,
        message: impl Into<SubdiagMessage<'dcx>>,
        span: impl TryIntoMultiSpan<'dcx>,
    ) {
        if let Some(span) = span.try_into_spans() {
            self.deref_mut().sub(level, message, span);
        }
    }

    with_fn! { with_note,
    /// Add a note attached to this diagnostic.
    pub fn note(&mut self, msg: impl Into<SubdiagMessage<'dcx>>, span: impl TryIntoMultiSpan<'dcx>) -> &mut Self {
        self.may_add_sub(Level::Note, msg, span);
        self
    } }

    with_fn! { with_help,
    /// Add a help attached to this diagnostic.
    pub fn help(&mut self, msg: impl Into<SubdiagMessage<'dcx>>, span: impl TryIntoMultiSpan<'dcx>) -> &mut Self {
        self.may_add_sub(Level::Help, msg, span);
        self
    } }

    with_fn! { with_warn,
    /// Add a warning attached to this diagnostic.
    pub fn warn(&mut self, msg: impl Into<SubdiagMessage<'dcx>>, span: impl TryIntoMultiSpan<'dcx>) -> &mut Self {
        self.may_add_sub(Level::Warning, msg, span);
        self
    } }
}

impl<'dcx> EarlyDiag<'dcx> {
    #[track_caller]
    pub fn new(
        early_dcx: &'dcx EarlyDiagContext,
        level: Level,
        message: impl Into<DiagMessage<'dcx>>,
    ) -> Self {
        Self::new_diagnostic(early_dcx, DiagInner::new(level, message))
    }

    /// Creates a new `EarlyDiag` with an already constructed diagnostic.
    #[track_caller]
    pub(crate) fn new_diagnostic(early_dcx: &'dcx EarlyDiagContext, diag: DiagInner<'dcx>) -> Self {
        Self {
            early_dcx,
            diag: Some(Box::new(diag)),
        }
    }

    with_fn! { with_arg,
    /// Add an argument.
    pub fn arg(&mut self, name: impl Into<DiagArgName>, arg: impl IntoDiagArg) -> &mut Self {
        self.deref_mut().arg(name, arg);
        self
    } }

    with_fn! { with_note,
    /// Add a note attached to this diagnostic.
    pub fn note(&mut self, msg: impl Into<SubdiagMessage<'dcx>>) -> &mut Self {
        self.sub(Level::Note, msg, MultiSpan::new());
        self
    } }

    with_fn! { with_help,
    /// Add a help attached to this diagnostic.
    pub fn help(&mut self, msg: impl Into<SubdiagMessage<'dcx>>) -> &mut Self {
        self.sub(Level::Help, msg, MultiSpan::new());
        self
    } }
}

impl<'dcx> DiagInner<'dcx> {
    pub(crate) fn new(level: Level, message: impl Into<DiagMessage<'dcx>>) -> Self {
        Self::new_with_messages(level, vec![message.into()])
    }

    pub(crate) fn new_with_messages(level: Level, messages: Vec<DiagMessage<'dcx>>) -> Self {
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

    pub(crate) fn sub(
        &mut self,
        level: Level,
        message: impl Into<SubdiagMessage<'dcx>>,
        span: MultiSpan<'dcx>,
    ) {
        let sub = DiagContent {
            level,
            messages: vec![self.subdiagnostic_message_to_diagnostic_message(message)],
            span,
        };
        self.children.push(sub);
    }

    pub(crate) fn subdiagnostic_message_to_diagnostic_message(
        &self,
        attr: impl Into<SubdiagMessage<'dcx>>,
    ) -> DiagMessage<'dcx> {
        let msg = self
            .content
            .messages
            .first()
            .expect("diagnostic with no messages");
        msg.with_subdiagnostic_message(attr.into())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[counterpart(rustc_errors::SuggestionStyle)]
pub enum SuggestionStyle {
    /// Hide the suggested code when displaying this suggestion inline.
    HideCodeInline,
    /// Always hide the suggested code but display the message.
    HideCodeAlways,
    /// Do not display this suggestion in the cli output, it is only meant for tools.
    CompletelyHidden,
    /// Always show the suggested code.
    /// This will *not* show the code if the suggestion is inline *and* the suggested code is
    /// empty.
    ShowCode,
    /// Always show the suggested code independently.
    ShowAlways,
}

/// Indicates the confidence in the correctness of a suggestion.
///
/// All suggestions are marked with an `Applicability`. Tools use the applicability of a suggestion
/// to determine whether it should be automatically applied or if the user should be consulted
/// before applying the suggestion.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[counterpart(rustc_lint_defs::Applicability)]
pub enum Applicability {
    /// The suggestion is definitely what the user intended, or maintains the exact meaning of the code.
    /// This suggestion should be automatically applied.
    ///
    /// In case of multiple `MachineApplicable` suggestions (whether as part of
    /// the same `multipart_suggestion` or not), all of them should be
    /// automatically applied.
    MachineApplicable,

    /// The suggestion may be what the user intended, but it is uncertain. The suggestion should
    /// result in valid code if it is applied.
    MaybeIncorrect,

    /// The suggestion contains placeholders like `(...)` or `{ /* fields */ }`. The suggestion
    /// cannot be applied automatically because it will not result in valid code. The user
    /// will need to fill in the placeholders.
    HasPlaceholders,

    /// The applicability of the suggestion is unknown.
    Unspecified,
}
