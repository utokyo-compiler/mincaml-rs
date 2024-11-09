use annotate_snippets::{Annotation, Message, Renderer};
use data_structure::{arena::TypedArena, FxIndexMap};
use fluent_bundle::FluentArgs;
use sourcemap::InputIndex;

use crate::{
    context::{Early, EarlySwitch, Late},
    translation::{to_fluent_args, Translate},
    DiagContent, DiagInner, FluentBundle, Level,
};

pub struct AnnotateSnippetEmitter<Switch: EarlySwitch> {
    fluent_bundle: FluentBundle,
    source_map: Switch::SourceMap,
}

impl<Switch: EarlySwitch> Translate for AnnotateSnippetEmitter<Switch> {
    fn fluent_bundle(&self) -> &FluentBundle {
        &self.fluent_bundle
    }
}

impl AnnotateSnippetEmitter<Early<'_>> {
    pub fn new(fluent_bundle: FluentBundle) -> Self {
        Self {
            fluent_bundle,
            source_map: (),
        }
    }
}

impl<'dcx> AnnotateSnippetEmitter<Late<'dcx>> {
    pub fn from_early(
        early: AnnotateSnippetEmitter<Early<'_>>,
        source_map: <Late<'dcx> as EarlySwitch>::SourceMap,
    ) -> Self {
        Self {
            fluent_bundle: early.fluent_bundle,
            source_map,
        }
    }
}

impl From<Level> for annotate_snippets::Level {
    fn from(value: Level) -> Self {
        match value {
            Level::Error | Level::Bug => annotate_snippets::Level::Error,
            Level::Info => annotate_snippets::Level::Info,
            Level::Note => annotate_snippets::Level::Note,
            Level::Help => annotate_snippets::Level::Help,
            Level::Warning => annotate_snippets::Level::Warning,
        }
    }
}

impl<'dcx, Switch: EarlySwitch> AnnotateSnippetEmitter<Switch>
where
    Self: AnnotateSnippetSwitch<'dcx>,
{
    pub fn emit_diagnostic(&self, diag: DiagInner) {
        let fluent_args = to_fluent_args(diag.args.iter());

        // We need to allocate strings for the snippets. We use a `TypedArena` to
        // avoid the need to manually manage the lifetimes of each string.
        let owned_str_repository = TypedArena::<u8>::new();

        let mut message = self.make_snippets(&owned_str_repository, diag.content, &fluent_args);
        for subdiag in diag.children {
            let footer = self.make_snippets(&owned_str_repository, subdiag, &fluent_args);
            message = message.footer(footer);
        }

        let renderer = Renderer::styled();
        anstream::eprintln!("{}\n", renderer.render(message))
    }
}

pub trait AnnotateSnippetSwitch<'dcx> {
    fn make_snippets<'a>(
        &self,
        owned_str_repository: &'a TypedArena<u8>,
        diag: DiagContent,
        fluent_args: &FluentArgs<'static>,
    ) -> Message<'a>
    where
        'dcx: 'a;
}

impl<'dcx> AnnotateSnippetSwitch<'dcx> for AnnotateSnippetEmitter<Early<'dcx>> {
    fn make_snippets<'a>(
        &self,
        owned_str_repository: &'a TypedArena<u8>,
        diag: DiagContent,
        fluent_args: &FluentArgs<'static>,
    ) -> Message<'a>
    where
        'dcx: 'a,
    {
        let title = self.translate_messages(&diag.messages, fluent_args);
        annotate_snippets::Level::from(diag.level).title(owned_str_repository.alloc_str(&title))
    }
}

impl<'dcx> AnnotateSnippetSwitch<'dcx> for AnnotateSnippetEmitter<Late<'dcx>> {
    fn make_snippets<'a>(
        &self,
        owned_str_repository: &'a TypedArena<u8>,
        diag: DiagContent,
        fluent_args: &FluentArgs<'static>,
    ) -> Message<'a>
    where
        'dcx: 'a,
    {
        let title = self.translate_messages(&diag.messages, fluent_args);
        let message = annotate_snippets::Level::from(diag.level)
            .title(owned_str_repository.alloc_str(&title));

        /// The original `Snippet` struct does consume `self`, which is inconvenient here.
        struct Snippet<'a> {
            origin: &'a str,
            line_start: usize,

            source: &'a str,
            annotations: Vec<Annotation<'a>>,
        }

        impl<'a> Snippet<'a> {
            pub fn into_snippet(self) -> annotate_snippets::Snippet<'a> {
                annotate_snippets::Snippet::source(self.source)
                    .origin(self.origin)
                    .line_start(self.line_start)
                    .annotations(self.annotations)
            }
        }

        let mut snippets = FxIndexMap::<InputIndex, Snippet<'_>>::with_hasher(<_>::default());

        for (span, label) in diag.span.iter_labeled() {
            let mut annotation = if diag.span.primary_span.contains(span) {
                annotate_snippets::Level::Error
            } else {
                annotate_snippets::Level::Info
            }
            .span(span.into_range());
            if let Some(label) = label {
                let label = self.translate_message(label, fluent_args).unwrap();
                annotation = annotation.label(owned_str_repository.alloc_str(&label));
            }
            let index = self.source_map.get_span_index(*span).unwrap();
            let file = &self.source_map.files()[index];
            let source = file.content();
            let line_start = source
                .chars()
                .take(span.start.char_pos)
                .filter(|c| *c == '\n')
                .count();
            snippets
                .entry(index)
                .and_modify(|snippet| {
                    if snippet.line_start > line_start {
                        snippet.line_start = line_start;
                    }
                })
                .or_insert_with(|| {
                    let file = &self.source_map.files()[index];
                    let source = file.content();
                    Snippet {
                        origin: owned_str_repository.alloc_str(&file.origin_string()),
                        line_start,
                        source,
                        annotations: Vec::new(),
                    }
                })
                .annotations
                .push(annotation);
        }

        message.snippets(snippets.into_values().map(Snippet::into_snippet))
    }
}
