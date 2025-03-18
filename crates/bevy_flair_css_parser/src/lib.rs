//! # Bevy Flair CSS Parser
//! Includes a CSS parser and a Bevy plugin for parsing CSS files as assets.

use bevy::prelude::Deref;
use cssparser::{BasicParseError, CowRcStr, Parser, Token};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;

pub use error::*;
pub use loader::*;
pub use reflect::*;

mod error;
mod parser;

mod error_codes;
mod loader;
mod reflect;
mod utils;

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
pub trait ParserExt<'a> {
    /// Peeks the next token without consuming it.
    fn peek(&mut self) -> Result<Token<'a>, BasicParseError<'a>>;

    /// Uses the inner parser function, and returns the results of such function
    /// wrapped over a [`Located`].
    ///
    /// It doesn't skip whitespaces before the inner parser function.
    fn located_with_whitespace<F, T, E>(&mut self, inner: F) -> Result<Located<T>, E>
    where
        F: for<'tt> FnOnce(&mut Parser<'a, 'tt>) -> Result<T, E>;

    /// Uses the inner parser function, and returns the results of such function
    /// wrapped over a [`Located`].
    ///
    /// It skips whitespaces before the inner parser function.
    fn located<F, T, E>(&mut self, inner: F) -> Result<Located<T>, E>
    where
        F: for<'tt> FnOnce(&mut Parser<'a, 'tt>) -> Result<T, E>;

    /// Returns the next token, if exists, wrapped over a [`Located`].
    fn located_next(&mut self) -> Result<Located<Token<'a>>, BasicParseError<'a>>;

    /// Expects the next token to be an identifier, and returns it wrapped over a [`Located`].
    fn expect_located_ident(&mut self) -> Result<LocatedStr<'a>, BasicParseError<'a>>;
}

impl<'a> ParserExt<'a> for Parser<'a, '_> {
    fn peek(&mut self) -> Result<Token<'a>, BasicParseError<'a>> {
        let start_state = self.state();
        let result = self.next().cloned();
        self.reset(&start_state);

        match result {
            Ok(token) => Ok(token),
            Err(err) => Err(err),
        }
    }

    fn located_with_whitespace<F, T, E>(&mut self, inner: F) -> Result<Located<T>, E>
    where
        F: for<'tt> FnOnce(&mut Parser<'a, 'tt>) -> Result<T, E>,
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
        F: for<'tt> FnOnce(&mut Parser<'a, 'tt>) -> Result<T, E>,
    {
        self.skip_whitespace();
        self.located_with_whitespace(inner)
    }

    fn located_next(&mut self) -> Result<Located<Token<'a>>, BasicParseError<'a>> {
        self.located(|parser| parser.next().cloned())
    }

    fn expect_located_ident(&mut self) -> Result<LocatedStr<'a>, BasicParseError<'a>> {
        self.located(|parser| parser.expect_ident_cloned())
    }
}

pub(crate) type CssParseResult<T> = Result<T, CssError>;
