//! # Bevy Flair CSS Parser
//! Includes a CSS parser and a Bevy plugin for parsing CSS files as assets.

use bevy_app::{App, Plugin, PostUpdate};
use bevy_asset::AssetApp;
use bevy_ecs::prelude::AppTypeRegistry;
use bevy_ecs::schedule::IntoScheduleConfigs;
use bevy_flair_core::PropertyRegistry;
use bevy_flair_style::StyleSystems;
use cssparser::{BasicParseError, CowRcStr, Parser, Token};
use derive_more::Deref;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;

pub use error::*;
pub use inline_styles::*;
pub use loader::*;
pub use reflect::*;
pub use shorthand::*;

pub use calc::{CalcAdd, CalcMul, Calculable, parse_calc_property_value_with, parse_calc_value};
pub use utils::parse_property_value_with;

mod error;
mod parser;

mod calc;
mod error_codes;
mod imports_parser;
mod inline_styles;
mod internal_loader;
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

    /// Expects the next token to be a function, and returns it wrapped over a [`Located`].
    fn expect_located_function(&mut self) -> Result<LocatedStr<'i>, CssError>;

    /// Convenience method to work with [`parse_nested_block`] and [`CssError`].
    ///
    /// [`parse_nested_block`]: Parser::parse_nested_block
    fn parse_nested_block_with<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F: for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, CssError>;

    /// Convenience method to work with [`parse_comma_separated`] and [`CssError`].
    ///
    /// [`parse_comma_separated`]: Parser::parse_comma_separated
    fn parse_comma_separated_with<T, F>(&mut self, f: F) -> Result<Vec<T>, CssError>
    where
        F: for<'tt> FnMut(&mut Parser<'i, 'tt>) -> Result<T, CssError>;

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

    fn expect_located_function(&mut self) -> Result<LocatedStr<'i>, CssError> {
        Ok(self.located(|parser| parser.expect_function().cloned())?)
    }

    fn parse_nested_block_with<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F: for<'tt> FnOnce(&mut Parser<'i, 'tt>) -> Result<T, CssError>,
    {
        self.parse_nested_block(move |parser| f(parser).map_err(|err| err.into_parse_error()))
            .map_err(CssError::from)
    }

    fn parse_comma_separated_with<T, F>(&mut self, mut parse_one: F) -> Result<Vec<T>, CssError>
    where
        F: for<'tt> FnMut(&mut Parser<'i, 'tt>) -> Result<T, CssError>,
    {
        let mut result = Vec::with_capacity(1);
        result.push(parse_one(self)?);
        while self.try_parse(Self::expect_comma).is_ok() {
            result.push(parse_one(self)?);
        }
        Ok(result)
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

/// Add support for css parsing infrastructure.
///
/// - Registers the [`CssStyleSheetLoader`].
/// - Adds support for [`InlineStyle`].
#[derive(Default)]
pub struct FlairCssParserPlugin;

impl Plugin for FlairCssParserPlugin {
    fn build(&self, app: &mut App) {
        app.preregister_asset_loader::<CssStyleSheetLoader>(CssStyleSheetLoader::EXTENSIONS);
        app.add_plugins((ReflectParsePlugin, ShorthandPropertiesPlugin));

        app.add_systems(
            PostUpdate,
            parse_inline_style.in_set(StyleSystems::SetStyleData),
        );
    }

    fn finish(&self, app: &mut App) {
        let world = app.world();
        let type_registry_arc = world.resource::<AppTypeRegistry>().0.clone();
        let property_registry = world.resource::<PropertyRegistry>().clone();
        let shorthand_registry = world.resource::<ShorthandPropertyRegistry>().clone();
        app.register_asset_loader(CssStyleSheetLoader::new(
            type_registry_arc,
            property_registry,
            shorthand_registry,
        ));
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::utils::{ImportantLevel, try_parse_important_level};
    use crate::{CssError, ErrorReportGenerator};
    use cssparser::{ParseError, Parser, ParserInput};
    use std::backtrace::BacktraceStatus;

    #[inline(always)]
    #[track_caller]
    pub fn parse_property_content_with<T>(
        contents: &str,
        parse_fn: impl FnOnce(&mut Parser) -> Result<T, CssError>,
    ) -> T {
        // Adds a !important at the end to verify that parse functions don't try to consume the !important token.
        let contents = format!("{contents} !important");
        let result = parse_content_with(&contents, |parser| {
            let result = parse_fn(parser)?;
            let important_level = try_parse_important_level(parser);

            assert!(
                matches!(important_level, ImportantLevel::Important(_)),
                "Missing trailing !important from parser. Remaining contents: '{remaining_contents}'",
                remaining_contents = &contents[parser.position().byte_index()..]
            );
            Ok(result)
        });

        expects_parse_ok(&contents, result)
    }

    #[inline(always)]
    #[track_caller]
    pub fn parse_err_property_content_with<T: core::fmt::Debug>(
        contents: &str,
        parse_fn: impl FnOnce(&mut Parser) -> Result<T, CssError>,
    ) -> String {
        let result = parse_content_with(contents, parse_fn);
        expects_parse_err(contents, result)
    }

    const TEST_REPORT_CONFIG: ariadne::Config = ariadne::Config::new()
        .with_color(false)
        .with_label_attach(ariadne::LabelAttach::Start)
        .with_char_set(ariadne::CharSet::Ascii);

    #[inline(always)]
    #[track_caller]
    #[allow(clippy::print_stderr)]
    pub fn parse_content_with<'i, T>(
        contents: &'i str,
        parse_fn: impl FnOnce(&mut Parser) -> Result<T, CssError>,
    ) -> Result<T, ParseError<'i, CssError>> {
        let mut input = ParserInput::new(contents);
        let mut parser = Parser::new(&mut input);

        parser.parse_entirely(|parser| parse_fn(parser).map_err(CssError::into_parse_error))
    }

    #[inline(always)]
    #[track_caller]
    #[allow(clippy::print_stderr)]
    pub fn expects_parse_ok<'i, T>(
        contents: &'i str,
        result: Result<T, ParseError<'i, CssError>>,
    ) -> T {
        match result {
            Ok(value) => value,
            Err(error) => {
                let mut style_error = CssError::from(error);
                if style_error.backtrace.status() == BacktraceStatus::Captured {
                    eprintln!("CssError backtrace:");
                    eprintln!("{}", style_error.backtrace);
                }
                style_error.improve_location_with_sub_str(contents);

                let mut report_generator =
                    ErrorReportGenerator::new_with_config("test.css", contents, TEST_REPORT_CONFIG);
                report_generator.add_error(style_error);

                panic!("{}", report_generator.into_message());
            }
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn expects_parse_err<'i, T: core::fmt::Debug>(
        contents: &'i str,
        result: Result<T, ParseError<'i, CssError>>,
    ) -> String {
        match result {
            Ok(value) => {
                panic!("Parsing of '{contents}' did not failed. It produced vale '{value:?}'");
            }
            Err(error) => {
                let mut style_error = CssError::from(error);
                style_error.improve_location_with_sub_str(contents);

                let mut report_generator =
                    ErrorReportGenerator::new_with_config("test.css", contents, TEST_REPORT_CONFIG);
                report_generator.add_error(style_error);

                report_generator.into_message()
            }
        }
    }
}
