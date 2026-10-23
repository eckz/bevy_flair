use ariadne::*;
use std::sync::Arc;

use crate::{Spanned, SpannedStr};
use bevy_flair_style::css_selector::SelectorErrorKind;
use cssparser::{
    BasicParseError, BasicParseErrorKind, ParseError, ParseErrorKind, SourceLocation, ToCss, Token,
};
use selectors::parser::SelectorParseErrorKind;

#[derive(Copy, Clone, Debug)]
pub(crate) struct SourceLocationU16 {
    pub line: u16,
    pub column: u16,
}

impl SourceLocationU16 {
    fn from_source_location(source_location: SourceLocation) -> Self {
        Self {
            line: u16::try_from(source_location.line).unwrap_or(u16::MAX),
            column: u16::try_from(source_location.column).unwrap_or(u16::MAX),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum CssErrorLocation {
    Range(std::range::Range<u32>),
    Unlocated,
    SourceLocationAndLen {
        source_location: SourceLocationU16,
        // Offset in bytes
        len_offset: u32,
    },
}

// Taken from subslice_range (https://doc.rust-lang.org/stable/src/core/slice/mod.rs.html#5321)
fn substr_range(original: &str, substr: &str) -> Option<core::range::Range<usize>> {
    let self_start = original.as_ptr().addr();
    let subslice_start = substr.as_ptr().addr();
    let start = subslice_start.wrapping_sub(self_start);
    let end = start.wrapping_add(substr.len());
    if start <= original.len() && end <= original.len() {
        Some(core::range::Range { start, end })
    } else {
        None
    }
}

impl CssErrorLocation {
    pub(crate) fn from_source_and_sub_str(
        source_location: SourceLocation,
        s: impl AsRef<str>,
    ) -> Self {
        Self::SourceLocationAndLen {
            source_location: SourceLocationU16::from_source_location(source_location),
            len_offset: s.as_ref().len() as u32,
        }
    }

    pub(crate) fn into_range(self, contents: &str) -> std::ops::Range<usize> {
        match self {
            CssErrorLocation::Unlocated => {
                unreachable!("Unlocated CssError")
            }
            CssErrorLocation::Range(range) => std::ops::Range {
                start: range.start as usize,
                end: range.end as usize,
            },
            CssErrorLocation::SourceLocationAndLen {
                source_location,
                len_offset,
            } => {
                // A parse error's SourceLocation can point one line past EOF (e.g. a
                // trailing block-less rule); yield an empty end-of-input span for it.
                let Some(line) = contents.lines().nth(source_location.line as usize) else {
                    return contents.len()..contents.len();
                };

                // The column number within a line starts at 1 for first the character of the line.
                // Column numbers are counted in UTF-16 code units.
                let mut utf16_code_points_remaining = source_location.column as u32 - 1;
                let mut column_byte_offset = 0;

                let mut chars = line.chars();
                while utf16_code_points_remaining > 0 {
                    let next = chars.next().expect("Invalid column provided");
                    utf16_code_points_remaining -= next.len_utf16() as u32;
                    column_byte_offset += next.len_utf8();
                }

                let line_len = line.len();
                if column_byte_offset >= line_len {
                    return substr_range(contents, &line[line_len - 1..line_len])
                        .expect("Invalid range generated")
                        .into();
                }

                substr_range(
                    contents,
                    &line[column_byte_offset..(column_byte_offset + len_offset as usize)],
                )
                .expect("Invalid range generated")
                .into()
            }
        }
    }
}

impl From<std::range::Range<u32>> for CssErrorLocation {
    fn from(range: std::range::Range<u32>) -> Self {
        Self::Range(range)
    }
}

impl From<std::ops::Range<u32>> for CssErrorLocation {
    fn from(range: std::ops::Range<u32>) -> Self {
        Self::Range(range.into())
    }
}

#[derive(Clone, Debug)]
pub(crate) struct StyleErrorData {
    code: &'static CssErrorCodeDef,
    annotated_message: Arc<str>,
}

impl StyleErrorData {
    pub(crate) fn new(
        code: &'static CssErrorCodeDef,
        annotated_message: impl Into<Arc<str>>,
    ) -> Self {
        let annotated_message = annotated_message.into();
        Self {
            code,
            annotated_message,
        }
    }
}

impl From<BasicParseErrorKind> for StyleErrorData {
    fn from(kind: BasicParseErrorKind) -> Self {
        use crate::error_codes::basic as basic_error_codes;

        let (code, annotated_message) = match kind {
            BasicParseErrorKind::UnexpectedToken => (
                basic_error_codes::UNEXPECTED_TOKEN,
                "Unexpected token".into(),
            ),
            BasicParseErrorKind::AtRuleInvalid => {
                (basic_error_codes::INVALID_AT_RULE, "Invalid at rule".into())
            }
            BasicParseErrorKind::EndOfInput => (
                basic_error_codes::END_OF_INPUT,
                "Unexpected end of input".into(),
            ),
            other => (
                basic_error_codes::BASIC_PARSE_ERROR,
                other.to_string().into(),
            ),
        };

        Self {
            code,
            annotated_message,
        }
    }
}

impl From<BasicParseError> for StyleErrorData {
    fn from(error: BasicParseError) -> Self {
        error.kind.into()
    }
}

impl From<SelectorParseErrorKind> for StyleErrorData {
    fn from(kind: SelectorParseErrorKind) -> Self {
        let code = crate::error_codes::basic::INVALID_SELECTOR;
        let selector_error = SelectorErrorKind::Selector(kind);
        Self {
            code,
            annotated_message: selector_error.to_string().into(),
        }
    }
}

impl<E: Into<StyleErrorData>> From<ParseErrorKind<E>> for StyleErrorData {
    fn from(kind: ParseErrorKind<E>) -> Self {
        match kind {
            ParseErrorKind::Basic(basic_kind) => basic_kind.into(),
            ParseErrorKind::Custom(selector_kind) => selector_kind.into(),
        }
    }
}

impl<E: Into<StyleErrorData>> From<ParseError<E>> for StyleErrorData {
    fn from(error: ParseError<E>) -> Self {
        // Cannot call .into() here because it's a method defined for `ParseErrorKind`
        StyleErrorData::from(error.kind)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum CssErrorKind {
    Internal,
    Custom,
}

/// Represents a pair of code and message for a CSS error.
/// Errors will be reported like `"[$code] $message:`
#[derive(Debug)]
pub struct CssErrorCodeDef {
    kind: CssErrorKind,
    code: u32,
    message: &'static str,
}

impl CssErrorCodeDef {
    pub const fn new_custom(code: u32, message: &'static str) -> Self {
        Self {
            kind: CssErrorKind::Custom,
            code,
            message,
        }
    }

    pub(crate) const fn new_internal(code: u32, message: &'static str) -> Self {
        Self {
            kind: CssErrorKind::Internal,
            code,
            message,
        }
    }
}

/// Represents a located error that happened while parsing CSS.
/// This class is meant to be an improvement over [`ParseError`] in order to provide better
/// error reporting.
///
/// [`ParseError`] by only contains [`SourceLocation`] which includes line and column numbers.
/// This is not enough to provide a good error report.
///
/// `CssError` contains:
///  - Optional code of the error type.
///  - Generic message of the error type.
///  - Annotated message specific to the error.
///  - Specific location of the error, specified with a byte range.
///     - It's possible to provide a less specific location, and later will automatically upgrade
///       when more information is available.
///
/// It contains handy [`From`] conversions, so it can be used with existing [`Parser`] methods like `Parser::expect_integer()?`
///
/// [`Parser`]: cssparser::Parser
#[derive(Clone, Debug)]
pub struct CssError {
    data: StyleErrorData,
    location: CssErrorLocation,
    #[cfg(test)]
    pub(crate) backtrace: Arc<std::backtrace::Backtrace>,
}

#[cfg(not(test))]
const _: () = assert!(size_of::<CssError>() <= 40);

impl CssError {
    #[track_caller]
    pub(crate) fn from_data_and_location(
        data: impl Into<StyleErrorData>,
        location: impl Into<CssErrorLocation>,
    ) -> Self {
        Self {
            data: data.into(),
            location: location.into(),
            #[cfg(test)]
            backtrace: Arc::new(std::backtrace::Backtrace::capture()),
        }
    }

    #[track_caller]
    pub(crate) fn from_parse_error_with_location(
        parse_error: ParseError<CssError>,
        source: SourceLocation,
        error_sub_str: &str,
    ) -> Self {
        match parse_error.kind {
            ParseErrorKind::Basic(basic_kind) => Self::from_data_and_location(
                basic_kind,
                CssErrorLocation::from_source_and_sub_str(source, error_sub_str),
            ),
            ParseErrorKind::Custom(error) => error,
        }
    }

    /// New error with specific location.
    /// This constructor is the preferred way to create a new error.
    ///
    /// Located items can be obtained using methods from [`ParserExt`] trait.
    ///
    /// [`ParserExt`]: crate::ParserExt
    #[track_caller]
    pub fn new<T>(
        spanned: &Spanned<T>,
        code: &'static CssErrorCodeDef,
        annotated_message: impl Into<Arc<str>>,
    ) -> Self {
        Self::from_data_and_location(
            StyleErrorData::new(code, annotated_message),
            CssErrorLocation::Range(spanned.span),
        )
    }

    #[track_caller]
    pub fn from_error_location(
        error_location: CssErrorLocation,
        code: &'static CssErrorCodeDef,
        annotated_message: impl Into<Arc<str>>,
    ) -> Self {
        Self::from_data_and_location(StyleErrorData::new(code, annotated_message), error_location)
    }

    // This should be used only when the error will not be shown to the user
    // Like when it's inside a try_parse
    pub(crate) fn unexpected<T>(spanned: &Spanned<T>) -> Self {
        Self::new(
            spanned,
            crate::error_codes::basic::UNEXPECTED_TOKEN,
            String::new(),
        )
    }

    #[track_caller]
    pub fn unexpected_token(token: &Spanned<Token>, expected: &str) -> Self {
        let token_css = token.to_css_string();
        Self::new(
            token,
            crate::error_codes::basic::UNEXPECTED_TOKEN,
            format!("Unexpected token: '{token_css}'. Was expecting {expected}"),
        )
    }

    #[track_caller]
    pub fn unexpected_ident(token: &SpannedStr, expected: &str) -> Self {
        Self::new(
            token,
            crate::error_codes::basic::UNEXPECTED_TOKEN,
            format!("Unexpected token: {token}. Was expecting {expected}"),
        )
    }

    /// New error where the location is unknown.
    /// Use with care because it might cause panics.
    /// It's meant to be used inside a `AtRuleParser` or `DeclarationParser` where the span is not known.
    #[track_caller]
    pub(crate) fn unlocated(
        code: &'static CssErrorCodeDef,
        annotated_message: impl Into<Arc<str>>,
    ) -> Self {
        Self::from_data_and_location(
            StyleErrorData::new(code, annotated_message),
            CssErrorLocation::Unlocated,
        )
    }

    pub(crate) fn is_eof(&self) -> bool {
        self.data.code.code == crate::error_codes::basic::END_OF_INPUT.code
    }

    pub(crate) fn improve_location_with_span(&mut self, span: std::range::Range<u32>) {
        if matches!(self.location, CssErrorLocation::Unlocated) {
            self.location = CssErrorLocation::Range(span);
        }
    }

    /// Converts this error into a message without any context.
    pub fn into_context_less_report(self) -> String {
        let StyleErrorData {
            code,
            annotated_message,
        } = self.data;

        let message = code.message;
        let code = code.code;

        match self.location {
            CssErrorLocation::Unlocated | CssErrorLocation::Range(_) => {
                format!("[{code:02}] {message}. {annotated_message}")
            }
            CssErrorLocation::SourceLocationAndLen {
                source_location, ..
            } => {
                let SourceLocationU16 { line, column } = source_location;
                format!("[{code:02}] {message}. {annotated_message} at {line}:{column}")
            }
        }
    }
}

impl From<Spanned<BasicParseError>> for CssError {
    fn from(value: Spanned<BasicParseError>) -> Self {
        let (value, span) = value.into_inner_and_span();
        Self::from_data_and_location(value.kind, span)
    }
}

impl From<Spanned<CssError>> for CssError {
    fn from(value: Spanned<CssError>) -> Self {
        value.into_inner()
    }
}

impl From<Spanned<ParseError<CssError>>> for CssError {
    fn from(error: Spanned<ParseError<CssError>>) -> Self {
        let (error, span) = error.into_inner_and_span();
        match error.kind {
            ParseErrorKind::Basic(basic) => Self::from_data_and_location(basic, span),
            ParseErrorKind::Custom(err) => err,
        }
    }
}

impl From<CssError> for ParseError<CssError> {
    fn from(error: CssError) -> Self {
        ParseError::custom(error)
    }
}

/// A helper struct to generate error reports out of [`CssError`].
/// It uses the [`ariadne`] crate to generate the error report.
pub struct ErrorReportGenerator<'a> {
    file_name: &'a str,
    contents: &'a str,
    config: Config,
    full_message: Vec<u8>,
    contains_errors: bool,
}

impl<'a> ErrorReportGenerator<'a> {
    /// New error report generator with the default configuration.
    pub fn new(file_name: &'a str, contents: &'a str) -> Self {
        Self::new_with_config(file_name, contents, Config::new())
    }

    /// New error report generator with the given configuration.
    pub fn new_with_config(file_name: &'a str, contents: &'a str, config: Config) -> Self {
        Self {
            contains_errors: false,
            file_name,
            contents,
            config: config.with_index_type(IndexType::Byte),
            full_message: Vec::new(),
        }
    }

    /// If no errors have been added.
    pub fn contains_errors(&self) -> bool {
        self.contains_errors
    }

    /// If there is any message, error or advice.
    pub fn is_empty(&self) -> bool {
        self.full_message.is_empty()
    }

    /// Add an error to this report.
    pub fn add_error(&mut self, error: CssError) {
        if matches!(error.location, CssErrorLocation::Unlocated) {
            // TODO: Warning and first line?
            panic!(
                "Error without location:\n{}",
                error.into_context_less_report()
            );
        }

        self.contains_errors = true;

        let StyleErrorData {
            code,
            annotated_message,
        } = error.data;
        let message = code.message;
        let code = code.code;

        let range = error.location.into_range(self.contents);

        let source = Source::from(self.contents);

        if !self.full_message.is_empty() {
            self.full_message.push(b'\n');
        }

        Report::build(ReportKind::Warning, (self.file_name, range.clone()))
            .with_config(self.config)
            .with_code(code)
            .with_message(message)
            .with_label(
                Label::new((self.file_name, range)).with_message(annotated_message), //.with_color(a),
            )
            .finish()
            .write((self.file_name, source), &mut self.full_message)
            .unwrap();
    }

    /// Add advice to this report.
    pub fn add_advice(
        &mut self,
        location: std::ops::Range<u32>,
        message: &'static str,
        annotated_message: impl Into<String>,
    ) {
        let source = Source::from(self.contents);

        if !self.full_message.is_empty() {
            self.full_message.push(b'\n');
        }
        let location = std::ops::Range {
            start: location.start as usize,
            end: location.end as usize,
        };

        Report::build(ReportKind::Advice, (self.file_name, location.clone()))
            .with_config(self.config)
            .with_message(message)
            .with_label(
                Label::new((self.file_name, location)).with_message(annotated_message.into()), //.with_color(a),
            )
            .finish()
            .write((self.file_name, source), &mut self.full_message)
            .unwrap();
    }

    /// Converts this report into the final message.
    pub fn into_message(self) -> String {
        String::from_utf8(self.full_message).expect("Invalid UTF-8 generated")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::CssParser;
    use indoc::indoc;

    const TEST_REPORT_CONFIG: Config = Config::new()
        .with_color(false)
        .with_label_attach(LabelAttach::Start)
        .with_char_set(CharSet::Ascii);

    macro_rules! into_report {
        ($error:expr, $contents:expr) => {{
            let mut report_generator =
                ErrorReportGenerator::new_with_config("test.css", &$contents, TEST_REPORT_CONFIG);
            report_generator.add_error($error);
            report_generator.into_message()
        }};
    }

    fn error_at_token(contents: &str, num_token: usize) -> CssError {
        let parser = CssParser::new(contents);
        let token = parser.into_iter().nth(num_token).expect("No token found");
        CssError::new(
            &token,
            crate::error_codes::basic::UNEXPECTED_TOKEN,
            "TestError",
        )
    }

    #[test]
    fn empty_file_eof_reporting() {
        let contents = "";

        let mut parser = CssParser::new(contents);
        let error = parser.next().expect_err("EOF expected");

        let report = into_report!(error, contents);

        assert_eq!(
            report,
            "[12] Warning: Unexpected end of input
   ,-[ test.css:1:1 ]
   |
 1 |\x20
   | |\x20
   | `- Unexpected end of input
---'
"
        );
    }

    #[test]
    fn eof_reporting() {
        let contents = "2.0";

        let mut parser = CssParser::new(contents);
        let _ = parser.next().expect("Not error expected");
        let error = parser.next().expect_err("EOF expected");

        let report = into_report!(error, contents);

        assert_eq!(
            report,
            "[12] Warning: Unexpected end of input
   ,-[ test.css:1:4 ]
   |
 1 | 2.0
   |    |\x20
   |    `- Unexpected end of input
---'
"
        );
    }

    #[test]
    fn test_source_location_conversion() {
        let contents = "#hash 12345";

        let error = error_at_token(contents, 1);

        let report = into_report!(error, contents);

        assert_eq!(
            report,
            "[01] Warning: Unexpected token
   ,-[ test.css:1:7 ]
   |
 1 | #hash 12345
   |       |^^^^\x20\x20
   |       `------ TestError
---'
"
        );
    }

    #[test]
    fn source_location_past_eof_degrades_without_panic() {
        // A parse error's SourceLocation can point one line past EOF (e.g. a
        // trailing block-less rule); into_range must not panic on it.
        let contents = "#kept 12345\n";
        let location = CssErrorLocation::SourceLocationAndLen {
            source_location: SourceLocationU16 { line: 5, column: 1 },
            len_offset: 3,
        };
        assert_eq!(
            location.into_range(contents),
            contents.len()..contents.len()
        );
    }

    #[test]
    fn test_source_location_with_unicode() {
        let contents = indoc! {r#"
            #hash 12345
            #💣💣 ßè🥳ñ
        "#};

        let error = error_at_token(contents, 3);

        let report = into_report!(error, contents);

        assert_eq!(
            report,
            "[01] Warning: Unexpected token
   ,-[ test.css:2:5 ]
   |
 2 | #💣💣 ßè🥳ñ
   |       |^^^^\x20\x20
   |       `------ TestError
---'
"
        );
    }
}
