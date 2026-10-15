//! # Bevy Flair CSS Parser
//! Includes a CSS style_sheet_parser and a Bevy plugin for parsing CSS files as assets.

use bevy_app::{App, Plugin, PostUpdate};
use bevy_asset::AssetApp;
use bevy_ecs::prelude::AppTypeRegistry;
use bevy_ecs::schedule::IntoScheduleConfigs;
use bevy_flair_core::{CssPropertyRegistry, PropertyRegistry};
use bevy_flair_style::StyleSystems;
pub use cssparser::{self, BasicParseError, CowRcStr, Parser, Token};
use derive_more::{Deref, DerefMut};
pub use error::*;
pub use inline_styles::*;
pub use loader::*;
pub use reflect::*;
pub use shorthand::*;
use std::fmt::{Debug, Display, Formatter};
use std::range::Range;
use cssparser::{ParseError, ParseErrorKind};
use tracing::debug;

pub use angle::{Angle, parse_angle};
pub use calc::{
    CalcOptions, CalcSumExpr, CalcValue, MathFunction, parse_calc, parse_calc_with_options,
};
pub use color::parse_color;
pub use duration::parse_duration;
pub use utils::parse_property_value_with;

mod error;
mod style_sheet_parser;

mod angle;
mod calc;
mod color;
mod duration;
mod error_codes;
mod imports_parser;
mod inline_styles;
mod internal_loader;
mod loader;
mod reflect;
mod shorthand;
mod utils;
mod vars;
mod css_parser;
mod spanned;

pub use spanned::{Spanned, SpannedStr};


#[deprecated]
type Located<T> = Spanned<T>;

type LocatedStr<'a> = SpannedStr<'a>;

/// Convenience methods over [`Parser`].
/// Main use case is to get the [location] of the parsed item.
///
/// [location]: crate::Located
pub trait ParserExt<'i> {
    /// Peeks the next token without consuming it.
    fn peek(&mut self) -> Result<Token<'i>, Located<BasicParseError>>;

    fn catching<F, T, E>(&mut self, inner: F) -> Result<T, CssError>
    where
        F: FnOnce(&mut Parser<'i>) -> Result<T, ParseError<E>>, E: Into<StyleErrorData>;

    /// Uses the inner style_sheet_parser function, and returns the results of such function
    /// wrapped over a [`Located`].
    ///
    /// It doesn't skip whitespaces before the inner style_sheet_parser function.
    fn located_with_whitespace<F, T, E>(&mut self, inner: F) -> Result<Located<T>, Located<E>>
    where
        F:  FnOnce(&mut Parser<'i>) -> Result<T, E>;

    /// Uses the inner style_sheet_parser function, and returns the results of such function
    /// wrapped over a [`Located`].
    ///
    /// It skips whitespaces before the inner style_sheet_parser function.
    fn located<F, T, E>(&mut self, inner: F) -> Result<Located<T>, Located<E>>
    where
        F:  FnOnce(&mut Parser<'i>) -> Result<T, E>;

    /// Returns the next token, if exists, wrapped over a [`Located`].
    fn located_next(&mut self) -> Result<Located<Token<'i>>, Located<BasicParseError>>;

    /// Expects the next token to be an identifier, and returns it wrapped over a [`Located`].
    fn expect_located_ident(&mut self) -> Result<LocatedStr<'i>, Located<BasicParseError>>;

    /// Expects the next token to be a function, and returns it wrapped over a [`Located`].
    fn expect_located_function(&mut self) -> Result<LocatedStr<'i>, CssError>;

    /// Convenience method to work with [`parse_nested_block`] and [`CssError`].
    ///
    /// [`parse_nested_block`]: Parser::parse_nested_block
    fn parse_nested_block_with<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F:  FnOnce(&mut Parser<'i>) -> Result<T, CssError>;

    /// Convenience method to work with [`parse_comma_separated`] and [`CssError`].
    ///
    /// [`parse_comma_separated`]: Parser::parse_comma_separated
    fn parse_comma_separated_with<T, F>(&mut self, f: F) -> Result<Vec<T>, CssError>
    where
        F:  FnMut(&mut Parser<'i>) -> Result<T, CssError>;

    /// Convenience method to work with [`try_parse`] and [`CssError`].
    ///
    /// [`try_parse`]: Parser::try_parse
    fn try_parse_with<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F:  FnOnce(&mut Parser<'i>) -> Result<T, CssError>;
}

impl<'i> ParserExt<'i> for Parser<'i> {

    fn peek(&mut self) -> Result<Token<'i>, Located<BasicParseError>> {
        let start_state = self.state();
        let result = self.located_next();
        self.reset(&start_state);
        result.map(Located::into_inner)
    }

    fn catching<F, T, E>(&mut self, inner: F) -> Result<T, CssError>
    where
        F: FnOnce(&mut Parser<'i>) -> Result<T, ParseError<E>>, E: Into<StyleErrorData> {

        self.skip_whitespace();

        let from = self.position().byte_index();
        let result = inner(self);
        let to = self.position().byte_index();

        let location = from..to;

        match result {
            Ok(item) => {
                Ok(item)
            }
            Err(err) => {
                match err.kind {
                    ParseErrorKind::Basic(basic) => {
                        Err(CssError::new(basic, location))
                    }
                    ParseErrorKind::Custom(custom) => {
                        Err(CssError::new(custom, location))
                    }
                }
            }
        }
    }

    fn located_with_whitespace<F, T, E>(&mut self, inner: F) -> Result<Located<T>, Located<E>>
    where
        F:  FnOnce(&mut Parser<'i>) -> Result<T, E>,
    {
        let from = self.position().byte_index();
        let result = inner(self);
        let to = self.position().byte_index();

        match result {
            Ok(item) => {
                Ok(Located::new(item, from..to))
            }
            Err(err) => Err(Located::new(err, from..to)),
        }
    }

    fn located<F, T, E>(&mut self, inner: F) -> Result<Located<T>, Located<E>>
    where
        F:  FnOnce(&mut Parser<'i>) -> Result<T, E>,
    {
        self.skip_whitespace();
        self.located_with_whitespace(inner)
    }

    fn located_next(&mut self) -> Result<Located<Token<'i>>, Located<BasicParseError>> {
        self.located(|parser| parser.next().cloned())
    }

    fn expect_located_ident(&mut self) -> Result<LocatedStr<'i>, Located<BasicParseError>> {
        self.located(|parser| parser.expect_ident_cloned())
    }

    fn expect_located_function(&mut self) -> Result<LocatedStr<'i>, CssError> {
        Ok(self.located(|parser| parser.expect_function().cloned())?)
    }

    fn parse_nested_block_with<T, F>(&mut self, f: F) -> Result<T, CssError>
    where
        F:  FnOnce(&mut Parser<'i>) -> Result<T, CssError>,
    {
        self.parse_nested_block(move |parser| f(parser).map_err(|err| err.into_parse_error()))
            .map_err(CssError::from)
    }

    fn parse_comma_separated_with<T, F>(&mut self, mut parse_one: F) -> Result<Vec<T>, CssError>
    where
        F:  FnMut(&mut Parser<'i>) -> Result<T, CssError>,
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
        F: FnOnce(&mut Parser<'i>) -> Result<T, CssError>,
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
            // We need add bevy_asset::handle_internal_asset_events to properly
            (parse_inline_style, bevy_asset::handle_internal_asset_events)
                .chain()
                .in_set(StyleSystems::SetStyleData),
        );
    }

    fn finish(&self, app: &mut App) {
        let world = app.world();
        let type_registry_arc = world.resource::<AppTypeRegistry>().0.clone();
        let property_registry = world.resource::<PropertyRegistry>().clone();

        debug!(
            "PropertyRegistry contains {} registered properties",
            property_registry.iter().len()
        );

        let css_property_registry = world.resource::<CssPropertyRegistry>().clone();
        let shorthand_registry = world.resource::<ShorthandPropertyRegistry>().clone();
        app.register_asset_loader(CssStyleSheetLoader::new(
            type_registry_arc,
            property_registry,
            css_property_registry,
            shorthand_registry,
        ));
    }
}

#[cfg(test)]
pub(crate) mod test_utils {
    use crate::utils::{ImportantLevel, try_parse_important_level};
    use crate::{CssError, ErrorReportGenerator};
    use bevy_flair_style::{VarResolver, VarTokens};
    use cssparser::{ParseError, Parser};
    use std::assert_matches;
    use std::backtrace::BacktraceStatus;
    use std::sync::Arc;

    pub(crate) struct NoVarsSupportedResolver;

    impl VarResolver for NoVarsSupportedResolver {
        fn get_all_names(&self) -> Vec<Arc<str>> {
            panic!("No vars support on tests")
        }

        fn get_var_tokens(&self, _var_name: &str) -> Option<&'_ VarTokens> {
            panic!("No vars support on tests")
        }
    }

    pub(crate) trait ExpectExt<E>: IntoIterator<Item = E> + Sized {
        #[inline(always)]
        #[track_caller]
        #[allow(unused)]
        fn expect_empty(self) {
            assert_eq!(self.into_iter().count(), 0, "Contents are not empty");
        }

        #[inline(always)]
        #[track_caller]
        fn expect_n<const N: usize>(self) -> [E; N] {
            let vec: Vec<_> = self.into_iter().collect();
            vec.try_into().unwrap_or_else(|v: Vec<_>| {
                panic!("Expected {} items, but {} were found", N, v.len())
            })
        }

        #[inline(always)]
        #[track_caller]
        fn expect_one(self) -> E {
            let [one] = self.expect_n();
            one
        }
    }

    impl<T, E> ExpectExt<E> for T where T: IntoIterator<Item = E> + Sized {}

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

            assert_matches!(
                important_level,
                ImportantLevel::Important(_),
                "Missing trailing !important from style_sheet_parser. Remaining contents: '{remaining_contents}'",
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
    pub fn parse_content_with<T>(
        contents: &str,
        parse_fn: impl FnOnce(&mut Parser) -> Result<T, CssError>,
    ) -> Result<T, ParseError<CssError>> {
        let mut parser = Parser::new(contents);

        parser.parse_entirely(|parser| parse_fn(parser).map_err(CssError::into_parse_error))
    }

    #[inline(always)]
    #[track_caller]
    #[allow(clippy::print_stderr)]
    pub fn expects_parse_ok<T>(
        contents: &str,
        result: Result<T, ParseError<CssError>>,
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
    pub fn expects_parse_err<T: core::fmt::Debug>(
        contents: &str,
        result: Result<T, ParseError<CssError>>,
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
