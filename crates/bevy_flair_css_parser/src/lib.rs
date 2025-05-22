//! # Bevy Flair CSS Parser
//! Includes a CSS parser and a Bevy plugin for parsing CSS files as assets.

use cssparser::{BasicParseError, CowRcStr, Parser, Token};
use derive_more::Deref;
pub use error::*;
pub use loader::*;
pub use reflect::*;
pub use shorthand::*;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;

mod error;
mod parser;

mod calc;
mod error_codes;
mod imports_parser;
mod loader;
mod reflect;
mod shorthand;
mod utils;
mod vars;

/// Wrapper for a value that has a location in a byte range
#[derive(Clone, Deref)]
pub struct Located<T> {
    #[deref]
    item: T,

    /// Location in byte range
    pub location: Range<usize>,
}

impl<T> Located<T> {
    /// Wraps a value with the given location.
    /// The range is the byte range from the original source.
    pub fn new(item: T, location: Range<usize>) -> Self {
        Self { item, location }
    }
}

impl<T> PartialEq for Located<T>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.item.eq(&other.item)
    }
}

impl<T> PartialEq<T> for Located<T>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &T) -> bool {
        self.item.eq(other)
    }
}

impl<T> Debug for Located<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.item, f)
    }
}

impl<T> Display for Located<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.item, f)
    }
}

pub(crate) type LocatedStr<'a> = Located<CowRcStr<'a>>;

impl PartialEq<&'static str> for LocatedStr<'_> {
    #[inline]
    fn eq(&self, other: &&'static str) -> bool {
        self.item.eq(other)
    }
}

impl<'a> From<LocatedStr<'a>> for String {
    fn from(value: LocatedStr<'a>) -> Self {
        value.as_ref().into()
    }
}

/// Convenience methods over [`Parser`].
/// Main use case is to get the [location] of the parsed item.
///
/// [location]: crate::Located
pub trait ParserExt<'i> {
    /// Peeks the next token without consuming it.
    fn peek(&mut self) -> Result<Token<'i>, BasicParseError<'i>>;

    /// Uses the inner parser function, and returns the results of such function
    /// wrapped over a [`Located`].
    ///
    /// It doesn't skip whitespaces before the inner parser function.
    fn located_with_whitespace<F, T, E>(&mut self, inner: F) -> Result<Located<T>, E>
    where
        F: for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, E>;

    /// Uses the inner parser function, and returns the results of such function
    /// wrapped over a [`Located`].
    ///
    /// It skips whitespaces before the inner parser function.
    fn located<F, T, E>(&mut self, inner: F) -> Result<Located<T>, E>
    where
        F: for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, E>;

    /// Returns the next token, if exists, wrapped over a [`Located`].
    fn located_next(&mut self) -> Result<Located<Token<'i>>, BasicParseError<'i>>;

    /// Expects the next token to be an identifier, and returns it wrapped over a [`Located`].
    fn expect_located_ident(&mut self) -> Result<LocatedStr<'i>, BasicParseError<'i>>;

    /// Convenience method to work with [`parse_nested_block`] and [`CssError`].
    ///
    /// [`parse_nested_block`]: Parser::parse_nested_block
    fn parse_nested_block_with<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F: for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, CssError>;

    /// Convenience method to work with [`try_parse`] and [`CssError`].
    ///
    /// [`try_parse`]: Parser::try_parse
    fn try_parse_with<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F: for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, CssError>;
}

impl<'i, 't> ParserExt<'i> for Parser<'i, 't> {
    fn peek(&mut self) -> Result<Token<'i>, BasicParseError<'i>> {
        let start_state = self.state();
        let result = self.next().cloned();
        self.reset(&start_state);
        result
    }

    fn located_with_whitespace<F, T, E>(&mut self, inner: F) -> Result<Located<T>, E>
    where
        F: for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, E>,
    {
        let from = self.position().byte_index();
        match inner(self) {
            Ok(item) => {
                let to = self.position().byte_index();
                Ok(Located::new(item, from..to))
            }
            Err(err) => Err(err),
        }
    }

    fn located<F, T, E>(&mut self, inner: F) -> Result<Located<T>, E>
    where
        F: for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, E>,
    {
        self.skip_whitespace();
        self.located_with_whitespace(inner)
    }

    fn located_next(&mut self) -> Result<Located<Token<'i>>, BasicParseError<'i>> {
        self.located(|parser| parser.next().cloned())
    }

    fn expect_located_ident(&mut self) -> Result<LocatedStr<'i>, BasicParseError<'i>> {
        self.located(|parser| parser.expect_ident_cloned())
    }

    fn parse_nested_block_with<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F: for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, CssError>,
    {
        self.parse_nested_block(move |parser| f(parser).map_err(|err| err.into_parse_error()))
            .map_err(CssError::from)
    }

    fn try_parse_with<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F: for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, CssError>,
    {
        self.try_parse(move |parser| f(parser).map_err(|err| err.into_parse_error()))
            .map_err(CssError::from)
    }
}

pub(crate) type CssParseResult<T> = Result<T, CssError>;

#[cfg(test)]
pub(crate) mod testing {
    use crate::utils::{ImportantLevel, try_parse_important_level};
    use crate::{CssError, ErrorReportGenerator};
    use cssparser::{Parser, ParserInput};

    #[inline(always)]
    #[track_caller]
    pub fn parse_property_content_with<T>(
        contents: &str,
        parse_fn: impl FnOnce(&mut Parser) -> Result<T, CssError>,
    ) -> T {
        // Adds a !important at the end to verify that parse functions don't try to consume the !important token.
        let contents = format!("{contents} !important");
        parse_raw_content_with(&contents, |parser| {
            let result = parse_fn(parser)?;
            let important_level = try_parse_important_level(parser);
            assert!(matches!(important_level, ImportantLevel::Important(_)));
            Ok(result)
        })
    }

    #[inline(always)]
    #[track_caller]
    pub fn parse_raw_content_with<T>(
        contents: &str,
        parse_fn: impl FnOnce(&mut Parser) -> Result<T, CssError>,
    ) -> T {
        let mut input = ParserInput::new(contents);
        let mut parser = Parser::new(&mut input);

        let result =
            parser.parse_entirely(|parser| parse_fn(parser).map_err(|err| err.into_parse_error()));

        match result {
            Ok(value) => value,
            Err(error) => {
                let mut style_error = CssError::from(error);
                style_error.improve_location_with_sub_str(contents);

                let mut report_generator = ErrorReportGenerator::new("test.css", contents);
                report_generator.add_error(style_error);

                panic!("{}", report_generator.into_message());
            }
        }
    }
}
