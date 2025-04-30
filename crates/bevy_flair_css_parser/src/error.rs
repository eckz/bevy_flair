use ariadne::*;

use crate::Located;
use bevy_flair_style::css_selector::SelectorErrorKind;
use cssparser::{
    BasicParseError, BasicParseErrorKind, ParseError, ParseErrorKind, SourceLocation, ToCss, Token,
};
use selectors::parser::SelectorParseErrorKind;
use std::ops::Range;

#[derive(Default)]
struct WriteLengthCounter {
    length: usize,
}

impl std::fmt::Write for WriteLengthCounter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.length += s.len();
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub(crate) enum CssErrorLocation {
    Range(Range<usize>),
    Unlocated,
    SubStr {
        substr_ptr: usize,
        len: usize,
    },
    SourceLocation {
        source_location: SourceLocation,
        // Offset in bytes
        len_offset: usize,
    },
}

// Taken from subslice_range (https://doc.rust-lang.org/src/core/slice/mod.rs.html#4626)
fn substr_range(original: &str, substr: &str) -> Option<Range<usize>> {
    let original_start = original.as_ptr() as usize;
    let subslice_start = substr.as_ptr() as usize;

    let byte_start = subslice_start.wrapping_sub(original_start);
    let byte_end = byte_start.wrapping_add(substr.len());

    if byte_start <= original.len() && byte_end <= original.len() {
        Some(byte_start..byte_end)
    } else {
        None
    }
}

impl CssErrorLocation {
    pub(crate) fn from_source_location(source_location: SourceLocation) -> Self {
        Self::SourceLocation {
            source_location,
            len_offset: 0,
        }
    }

    pub(crate) fn from_token(source_location: SourceLocation, token: &Token<'_>) -> Self {
        // Let's measure the token length
        let mut counter = WriteLengthCounter::default();
        token.to_css(&mut counter).unwrap();

        Self::SourceLocation {
            source_location,
            len_offset: counter.length,
        }
    }

    pub(crate) fn from_str(source_location: SourceLocation, s: impl AsRef<str>) -> Self {
        Self::SourceLocation {
            source_location,
            len_offset: s.as_ref().len(),
        }
    }

    pub(crate) fn into_range(self, contents: &str) -> Range<usize> {
        match self {
            CssErrorLocation::Unlocated => {
                panic!("Unexpected unlocated CssError")
            }
            CssErrorLocation::Range(range) => range,
            CssErrorLocation::SubStr { substr_ptr, len } => {
                let contents_ptr = contents.as_ptr() as usize;
                let byte_start = substr_ptr.wrapping_sub(contents_ptr);
                let byte_end = byte_start.wrapping_add(len);

                if byte_start <= contents.len() && byte_end <= contents.len() {
                    byte_start..byte_end
                } else {
                    panic!("invalid range generated");
                }
            }
            CssErrorLocation::SourceLocation {
                source_location,
                len_offset,
            } => {
                let line = contents
                    .lines()
                    .nth(source_location.line as usize)
                    .unwrap_or_else(|| {
                        panic!("Line number {} not found in contents", source_location.line)
                    });

                // The column number within a line starts at 1 for first the character of the line.
                // Column numbers are counted in UTF-16 code units.
                let mut utf16_code_points_remaining = source_location.column - 1;
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
                        .expect("Invalid range generated");
                }

                substr_range(
                    contents,
                    &line[column_byte_offset..(column_byte_offset + len_offset)],
                )
                .expect("Invalid range generated")
            }
        }
    }
}

impl<'a> From<(SourceLocation, BasicParseErrorKind<'a>)> for CssErrorLocation {
    fn from(value: (SourceLocation, BasicParseErrorKind<'a>)) -> Self {
        let (source_location, kind) = value;
        match kind {
            BasicParseErrorKind::UnexpectedToken(token) => {
                Self::from_token(source_location, &token)
            }
            BasicParseErrorKind::AtRuleInvalid(name) => Self::from_str(source_location, name),
            _ => Self::from_source_location(source_location),
        }
    }
}

impl<'a> From<(SourceLocation, SelectorParseErrorKind<'a>)> for CssErrorLocation {
    fn from(value: (SourceLocation, SelectorParseErrorKind<'a>)) -> Self {
        let (source_location, kind) = value;
        match kind {
            SelectorParseErrorKind::NoQualifiedNameInAttributeSelector(token)
            | SelectorParseErrorKind::UnexpectedTokenInAttributeSelector(token)
            | SelectorParseErrorKind::PseudoElementExpectedColon(token)
            | SelectorParseErrorKind::PseudoElementExpectedIdent(token)
            | SelectorParseErrorKind::NoIdentForPseudo(token)
            | SelectorParseErrorKind::ExpectedBarInAttr(token)
            | SelectorParseErrorKind::BadValueInAttr(token)
            | SelectorParseErrorKind::InvalidQualNameInAttr(token)
            | SelectorParseErrorKind::ExplicitNamespaceUnexpectedToken(token)
            | SelectorParseErrorKind::ClassNeedsIdent(token) => {
                Self::from_token(source_location, &token)
            }
            SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name)
            | SelectorParseErrorKind::ExpectedNamespace(name)
            | SelectorParseErrorKind::UnexpectedIdent(name) => {
                Self::from_str(source_location, name)
            }
            _ => Self::from_source_location(source_location),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct StyleErrorData {
    code: CssErrorCode,
    annotated_message: String,
}

impl StyleErrorData {
    pub(crate) fn new(code: CssErrorCode, annotated_message: impl Into<String>) -> Self {
        let annotated_message = annotated_message.into();
        Self {
            code,
            annotated_message,
        }
    }
}

impl<'a> From<BasicParseErrorKind<'a>> for StyleErrorData {
    fn from(kind: BasicParseErrorKind<'a>) -> Self {
        use crate::error_codes::basic as basic_error_codes;
        let code = match &kind {
            BasicParseErrorKind::UnexpectedToken(_) => basic_error_codes::UNEXPECTED_TOKEN,
            BasicParseErrorKind::AtRuleInvalid(_) => basic_error_codes::INVALID_AT_RULE,
            _ => basic_error_codes::BASIC_PARSE_ERROR,
        };

        Self {
            code,
            annotated_message: kind.to_string(),
        }
    }
}

impl<'a> From<SelectorParseErrorKind<'a>> for StyleErrorData {
    fn from(kind: SelectorParseErrorKind<'a>) -> Self {
        let code = crate::error_codes::basic::INVALID_SELECTOR;

        let selector_error: SelectorErrorKind = kind.into();

        Self {
            code,
            annotated_message: selector_error.to_string(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum CssErrorCodeInner {
    Internal { code: u32, message: &'static str },
    Custom { code: u32, message: &'static str },
}

/// Represents a pair of code and message for a CSS error.
/// Errors will be reported like `"[$code] $message:`
#[derive(Copy, Clone, Debug)]
pub struct CssErrorCode(CssErrorCodeInner);

impl CssErrorCode {
    /// Creates a new custom error code.
    pub const fn new_custom(code: u32, message: &'static str) -> Self {
        // TODO: Assert that the code is not already used.
        Self(CssErrorCodeInner::Custom { code, message })
    }

    pub(crate) const fn new_internal(code: u32, message: &'static str) -> Self {
        Self(CssErrorCodeInner::Internal { code, message })
    }

    pub(crate) fn into_code_and_message(self) -> (u32, &'static str) {
        match self.0 {
            CssErrorCodeInner::Internal { code, message } => (code, message),
            CssErrorCodeInner::Custom { code, message } => (code, message),
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
#[derive(Debug)]
pub struct CssError {
    data: StyleErrorData,
    location: CssErrorLocation,
    #[cfg(test)]
    pub(crate) backtrace: Box<std::backtrace::Backtrace>,
}

impl Clone for CssError {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            location: self.location.clone(),
            #[cfg(test)]
            backtrace: Box::new(std::backtrace::Backtrace::capture()),
        }
    }
}

impl CssError {
    #[track_caller]
    fn new(data: StyleErrorData, location: CssErrorLocation) -> Self {
        Self {
            data,
            location,
            #[cfg(test)]
            backtrace: Box::new(std::backtrace::Backtrace::capture()),
        }
    }

    /// New error with specific location.
    /// This constructor is the preferred way to create a new error.
    ///
    /// Located items can be obtained using methods from [`ParserExt`] trait.
    ///
    /// [`ParserExt`]: crate::ParserExt
    #[track_caller]
    pub fn new_located<T>(
        located: &Located<T>,
        code: CssErrorCode,
        annotated_message: impl Into<String>,
    ) -> Self {
        Self::new(
            StyleErrorData::new(code, annotated_message),
            CssErrorLocation::Range(located.location.clone()),
        )
    }

    /// New error where the location is unknown.
    /// Is ok to construct errors this way when implementing [`crate::reflect::ReflectParseCss`],
    /// since the location will be automatically upgraded to the current line.
    #[track_caller]
    pub fn new_unlocated(code: CssErrorCode, annotated_message: impl Into<String>) -> Self {
        Self::new(
            StyleErrorData::new(code, annotated_message),
            CssErrorLocation::Unlocated,
        )
    }

    // Mainly used to convert from Selector errors.
    #[track_caller]
    pub(crate) fn from_parse_error<E>(error: ParseError<E>) -> Self
    where
        E: Clone + Into<StyleErrorData>,
        (SourceLocation, E): Into<CssErrorLocation>,
    {
        match error.kind {
            ParseErrorKind::Basic(basic) => {
                Self::new(basic.clone().into(), (error.location, basic).into())
            }
            ParseErrorKind::Custom(custom) => {
                Self::new(custom.clone().into(), (error.location, custom).into())
            }
        }
    }

    pub(crate) fn improve_location_with_sub_str(&mut self, substr: &str) {
        if matches!(self.location, CssErrorLocation::Unlocated) {
            self.location = CssErrorLocation::SubStr {
                substr_ptr: substr.as_ptr() as usize,
                len: substr.len(),
            };
        }
    }

    fn source_location(&self) -> SourceLocation {
        match &self.location {
            CssErrorLocation::SourceLocation {
                source_location, ..
            } => *source_location,
            _ => SourceLocation { line: 0, column: 1 },
        }
    }

    /// Converts this error into a [`ParseError`] setting itself as a custom type.
    /// It can covert back to a [`CssError`] using `CssError::from(parse_error)`.
    pub fn into_parse_error(self) -> ParseError<'static, CssError> {
        self.source_location().new_custom_error(self)
    }

    /// Converts this error into a message without any context.
    pub fn into_context_less_report(self) -> String {
        let StyleErrorData {
            code,
            annotated_message,
        } = self.data;

        let (code, message) = code.into_code_and_message();

        match self.location {
            CssErrorLocation::Unlocated
            | CssErrorLocation::Range(_)
            | CssErrorLocation::SubStr { .. } => {
                format!("[{code:02}] {message}. {annotated_message}")
            }
            CssErrorLocation::SourceLocation {
                source_location, ..
            } => {
                let SourceLocation { line, column } = source_location;
                format!("[{code:02}] {message}. {annotated_message} at {line}:{column}")
            }
        }
    }
}

impl<'a> From<BasicParseError<'a>> for CssError {
    fn from(error: BasicParseError<'a>) -> Self {
        Self::new(
            error.kind.clone().into(),
            (error.location, error.kind).into(),
        )
    }
}

impl<'a> From<ParseError<'a, CssError>> for CssError {
    fn from(error: ParseError<'a, CssError>) -> Self {
        match error.kind {
            ParseErrorKind::Basic(basic_kind) => Self::new(
                basic_kind.clone().into(),
                (error.location, basic_kind).into(),
            ),
            ParseErrorKind::Custom(custom) => custom,
        }
    }
}

impl<'a> From<ParseError<'a, ()>> for CssError {
    fn from(error: ParseError<'a, ()>) -> Self {
        match error.kind {
            ParseErrorKind::Basic(basic_kind) => Self::new(
                basic_kind.clone().into(),
                (error.location, basic_kind).into(),
            ),
            ParseErrorKind::Custom(_) => {
                panic!("Custom unit error found")
            }
        }
    }
}

/// A helper struct to generate error reports out of [`CssError`].
/// It uses the [`ariadne`] crate to generate the error report.
pub struct ErrorReportGenerator<'a> {
    file_name: &'a str,
    contents: &'a str,
    config: Config,
    full_message: Vec<u8>,
}

impl<'a> ErrorReportGenerator<'a> {
    /// New error report generator with the default configuration.
    pub fn new(file_name: &'a str, contents: &'a str) -> Self {
        Self::new_with_config(file_name, contents, Config::new())
    }

    /// New error report generator with the given configuration.
    pub fn new_with_config(file_name: &'a str, contents: &'a str, config: Config) -> Self {
        Self {
            file_name,
            contents,
            config: config.with_index_type(IndexType::Byte),
            full_message: Vec::new(),
        }
    }

    /// If no errors have been added.
    pub fn is_empty(&self) -> bool {
        self.full_message.is_empty()
    }

    /// Add an error to this report.
    pub fn add_error(&mut self, error: CssError) {
        let StyleErrorData {
            code,
            annotated_message,
        } = error.data;
        let (code, message) = code.into_code_and_message();

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

    /// Converts this report into the final message.
    pub fn into_message(self) -> String {
        String::from_utf8(self.full_message).expect("Invalid UTF-8 generated")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cssparser::*;
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

    fn error_at_token(contents: &str, num_token: u32) -> CssError {
        let mut input = ParserInput::new(contents);
        let mut parser = Parser::new(&mut input);

        for _ in 0..num_token {
            parser.next().unwrap();
        }
        parser.skip_whitespace();
        let start_location = parser.current_source_location();
        let next_token = parser.next().cloned().unwrap();

        CssError::from(start_location.new_unexpected_token_error::<()>(next_token))
    }

    #[test]
    fn test_source_location_conversion() {
        let contents = indoc! {r#"
            #hash 12345
        "#};

        let error = error_at_token(contents, 1);

        let report = into_report!(error, contents);

        assert_eq!(
            report,
            "[01] Warning: Unexpected token
   ,-[ test.css:1:7 ]
   |
 1 | #hash 12345
   |       |^^^^\x20\x20
   |       `------ unexpected token: Number { has_sign: false, value: 12345.0, int_value: Some(12345) }
---'
"
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
   |       `------ unexpected token: Ident(\"ßè🥳ñ\")
---'
"
        );
    }
}
