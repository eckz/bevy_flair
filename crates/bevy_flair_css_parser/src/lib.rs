//! # Bevy Flair CSS Parser
//! Includes a CSS style_sheet_parser and a Bevy plugin for parsing CSS files as assets.

use bevy_app::{App, Plugin, PostUpdate};
use bevy_asset::AssetApp;
use bevy_ecs::prelude::AppTypeRegistry;
use bevy_ecs::schedule::IntoScheduleConfigs;
use bevy_flair_core::{CssPropertyRegistry, PropertyRegistry};
use bevy_flair_style::StyleSystems;
pub use cssparser::{self, BasicParseError, CowRcStr, Parser, Token};

pub use error::*;
pub use inline_styles::*;
pub use loader::*;
pub use reflect::*;
pub use shorthand::*;
use tracing::debug;

pub use angle::{Angle, parse_angle};
pub use calc::{
    CalcOptions, CalcSumExpr, CalcValue, MathFunction, parse_calc, parse_calc_with_options,
};
pub use color::parse_color;
pub use css_parser::CssParser;
pub use duration::parse_duration;
pub use utils::parse_property_value_with;

mod error;
mod style_sheet_parser;

mod angle;
mod calc;
mod color;
mod css_parser;
mod duration;
mod error_codes;
mod imports_parser;
mod inline_styles;
mod internal_loader;
mod loader;
mod reflect;
mod shorthand;
mod spanned;
mod utils;
mod vars;

pub use spanned::{Spanned, SpannedStr};

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
    use crate::{CssError, CssParser, ErrorReportGenerator};
    use bevy_flair_style::{VarResolver, VarTokens};

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
        parse_fn: impl FnOnce(&mut CssParser) -> Result<T, CssError>,
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
                remaining_contents = parser.slice_from_current_position()
            );
            Ok(result)
        });

        expects_parse_ok(&contents, result)
    }

    #[inline(always)]
    #[track_caller]
    pub fn parse_err_property_content_with<T: core::fmt::Debug>(
        contents: &str,
        parse_fn: impl FnOnce(&mut CssParser) -> Result<T, CssError>,
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
        parse_fn: impl FnOnce(&mut CssParser) -> Result<T, CssError>,
    ) -> Result<T, CssError> {
        let mut parser = CssParser::new(contents);

        parser.parse_entirely(parse_fn)
    }

    #[inline(always)]
    #[track_caller]
    #[allow(clippy::print_stderr)]
    pub fn expects_parse_ok<T>(contents: &str, result: Result<T, CssError>) -> T {
        match result {
            Ok(value) => value,
            Err(error) => {
                if error.backtrace.status() == BacktraceStatus::Captured {
                    eprintln!("CssError backtrace:");
                    eprintln!("{}", error.backtrace);
                }

                let mut report_generator =
                    ErrorReportGenerator::new_with_config("test.css", contents, TEST_REPORT_CONFIG);
                report_generator.add_error(error);

                panic!("{}", report_generator.into_message());
            }
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn expects_parse_err<T: core::fmt::Debug>(
        contents: &str,
        result: Result<T, CssError>,
    ) -> String {
        match result {
            Ok(value) => {
                panic!("Parsing of '{contents}' did not failed. It produced vale '{value:?}'");
            }
            Err(error) => {
                let mut report_generator =
                    ErrorReportGenerator::new_with_config("test.css", contents, TEST_REPORT_CONFIG);
                report_generator.add_error(error);

                report_generator.into_message()
            }
        }
    }
}
