//! Fatal errors

use codespan_reporting::term::DisplayStyle;

use crate::context::Context;
use crate::parser::Span;

/// A fatal, unrecoverable error
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SolveError {
    pub message: String,
    /// A trace of every clause item on the way to encountering this error
    pub trace: Vec<Span>,
}

impl<T: Into<String>> From<T> for Box<SolveError> {
    fn from(message: T) -> Self {
        Box::new(SolveError {
            message: message.into(),
            trace: vec![],
        })
    }
}
impl SolveError {
    pub fn add_trace(mut self: Box<Self>, span: Span) -> Box<Self> {
        self.trace.push(span);
        self
    }

    /// Print this error
    /// The given display style is used for all notes
    pub fn report(&self, ctx: &Context, display_style: DisplayStyle) {
        use codespan_reporting::diagnostic::Severity::*;
        use codespan_reporting::diagnostic::{Diagnostic, Label};
        use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

        let writer = StandardStream::stderr(ColorChoice::Always);
        let mut config = codespan_reporting::term::Config::default();

        for (i, span) in self.trace.iter().copied().enumerate() {
            let (severity, message) = if i == 0 {
                (Error, self.message.as_str())
            } else {
                (Note, "Called from here")
            };
            let diagnostic = Diagnostic::new(severity)
                .with_message(message)
                .with_labels(vec![Label::primary(
                    span.file_id as usize,
                    span.start..span.start + span.len as usize,
                )]);

            codespan_reporting::term::emit(&mut writer.lock(), &config, &ctx.files, &diagnostic)
                .unwrap();

            config.display_style = display_style.clone();
        }
    }
}
