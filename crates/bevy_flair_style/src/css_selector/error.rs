use cssparser::{BasicParseErrorKind, ParseErrorKind, SourceLocation, ToCss};
use selectors::parser::SelectorParseErrorKind;
use std::error::Error;
use std::fmt::Display;

/// Error type that is returned when calling [`CssSelector::parse`].
///
/// [`CssSelector::parse`]: crate::css_selector::CssSelector::parse_single
#[derive(Debug, PartialEq, Clone)]
pub struct SelectorError<'a> {
    /// Details of this error
    pub(crate) kind: SelectorErrorKind<'a>,
    /// Location where this error occurred
    pub location: SourceLocation,
}

/// Error type that is returned when calling `Selector::parse`
#[derive(Debug, PartialEq, Clone)]
pub enum SelectorErrorKind<'a> {
    /// Basic parse error
    Basic(BasicParseErrorKind<'a>),
    /// Selector parse error
    Selector(SelectorParseErrorKind<'a>),
}

impl<'a> From<cssparser::ParseError<'a, SelectorParseErrorKind<'a>>> for SelectorError<'a> {
    fn from(original: cssparser::ParseError<'a, SelectorParseErrorKind<'a>>) -> Self {
        let location = original.location;
        let kind = match original.kind {
            ParseErrorKind::Basic(err) => SelectorErrorKind::Basic(err),
            ParseErrorKind::Custom(err) => SelectorErrorKind::Selector(err),
        };
        Self { kind, location }
    }
}

impl<'a> From<BasicParseErrorKind<'a>> for SelectorErrorKind<'a> {
    fn from(err: BasicParseErrorKind<'a>) -> Self {
        Self::Basic(err)
    }
}

impl<'a> From<SelectorParseErrorKind<'a>> for SelectorErrorKind<'a> {
    fn from(err: SelectorParseErrorKind<'a>) -> Self {
        Self::Selector(err)
    }
}

impl Display for SelectorErrorKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SelectorErrorKind::Basic(err) => Display::fmt(err, f),
            SelectorErrorKind::Selector(SelectorParseErrorKind::EmptySelector) => {
                write!(f, "Empty selector found")
            }
            SelectorErrorKind::Selector(SelectorParseErrorKind::ClassNeedsIdent(token)) => {
                let token_str = token.to_css_string();
                write!(f, "Expected an ident for a class, got '{token_str}'")
            }
            SelectorErrorKind::Selector(other) => {
                write!(f, "Selector error found: {other:?}")
            }
        }
    }
}

impl Error for SelectorErrorKind<'_> {}
