use cssparser::{BasicParseErrorKind, ParseErrorKind};
use selectors::parser::SelectorParseErrorKind;
use std::error::Error;
use std::fmt::Display;

/// Error type that is returned when calling [`CssSelector::parse`].
///
/// [`CssSelector::parse`]: crate::css_selector::CssSelector::parse_single
#[derive(Debug, PartialEq, Clone)]
pub struct SelectorError {
    /// Details of this error
    pub(crate) kind: SelectorErrorKind,
    // TODO: WAT?
    // /// Location where this error occurred
    // pub location: SourceLocation,
}

/// Error type that is returned when calling `Selector::parse`
#[derive(Debug, PartialEq, Clone)]
pub enum SelectorErrorKind {
    /// Basic parse error
    Basic(BasicParseErrorKind),
    /// Selector parse error
    Selector(SelectorParseErrorKind),
}

impl From<cssparser::ParseError<SelectorParseErrorKind>> for SelectorError {
    fn from(original: cssparser::ParseError<SelectorParseErrorKind>) -> Self {
        let kind = match original.kind {
            ParseErrorKind::Basic(err) => SelectorErrorKind::Basic(err),
            ParseErrorKind::Custom(err) => SelectorErrorKind::Selector(err),
        };
        Self { kind }
    }
}

impl<'a> From<BasicParseErrorKind> for SelectorErrorKind {
    fn from(err: BasicParseErrorKind) -> Self {
        Self::Basic(err)
    }
}

impl<'a> From<SelectorParseErrorKind> for SelectorErrorKind {
    fn from(err: SelectorParseErrorKind) -> Self {
        Self::Selector(err)
    }
}

impl Display for SelectorErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SelectorErrorKind::Basic(err) => Display::fmt(err, f),
            SelectorErrorKind::Selector(SelectorParseErrorKind::EmptySelector) => {
                write!(f, "Empty selector found")
            }
            SelectorErrorKind::Selector(SelectorParseErrorKind::ClassNeedsIdent) => {
                write!(f, "Expected an ident for a class")
            }
            SelectorErrorKind::Selector(
                SelectorParseErrorKind::UnsupportedPseudoClassOrElement,
            ) => {
                write!(
                    f,
                    "Unsupported pseudo-class. Pseudo-elements are unsupported. Only valid pseudo-classes are: 'hover' | 'active' | 'focus' | 'focus-visible'"
                )
            }
            SelectorErrorKind::Selector(other) => {
                write!(f, "Selector error: {other:?}")
            }
        }
    }
}

impl Error for SelectorErrorKind {}
