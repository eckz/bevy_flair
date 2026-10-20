use crate::{error_codes::vars as error_codes, parse_calc, CalcValue, CssError, CssParser};
use bevy_asset::AssetPath;
use bevy_flair_core::PropertyValue;
use cssparser::{match_ignore_ascii_case, parse_important};
use std::path::Path;
use std::range::Range;
use variadics_please::all_tuples;


pub(crate) fn resolve_asset_path<'a>(
    parent: Option<&'a AssetPath<'a>>,
    path: &AssetPath<'a>,
) -> AssetPath<'static> {
    match parent {
        Some(parent) => parent.resolve(path),
        None => AssetPath::from_path(Path::new("")).resolve(path),
    }
}

pub(crate) fn parse_property_global_keyword<T>(
    parser: &mut CssParser,
) -> Result<PropertyValue<T>, CssError> {
    let next = parser.expect_ident()?;

    Ok(match_ignore_ascii_case! {next.as_ref(),
        "unset" => PropertyValue::Unset,
        "inherit" => PropertyValue::Inherit,
        "initial" => PropertyValue::Initial,
        _ => {
            return Err(CssError::unexpected(&next));
        }
    })
}

/// Parses a CSS property value that may be either a global keyword or a typed value.
///
/// This function first attempts to parse one of the global CSS keywords:
/// - `unset`
/// - `inherit`
/// - `initial`
///
/// If none of these match, it falls back to the provided `value_parser`
/// to parse the property as a typed value `T`.
///
///  If the produced type supports [`calc()`], it's preferable to use [`parse_calc_property_value_with()`].
///
/// [`calc()`]: crate::OldLegacyCalculable
/// [`parse_calc_property_value_with()`]: crate::legacy_parse_calc_property_with
/// # Examples
/// ```
/// # use bevy_flair_css_parser::CssParser;
/// # use bevy_flair_core::PropertyValue;
/// # use bevy_flair_css_parser::{parse_property_value_with, parse_val};
///
/// let mut parser = CssParser::new("inherit");
/// let property_value = parse_property_value_with(&mut parser, parse_val).unwrap();
/// assert_eq!(property_value, PropertyValue::Inherit);
/// ```
pub fn parse_property_value_with<T>(
    parser: &mut CssParser,
    mut value_parser: impl FnMut(&mut CssParser) -> Result<T, CssError>,
) -> Result<PropertyValue<T>, CssError> {
    if let Ok(value) = parser.try_parse(parse_property_global_keyword) {
        return Ok(value);
    };
    value_parser(parser).map(PropertyValue::Value)
}

pub(crate) fn parse_many<T, F: FnMut(&mut CssParser) -> Result<T, CssError>>(
    parser: &mut CssParser,
    mut value_parser: F,
) -> Result<Vec<T>, CssError> {
    let mut result: Vec<T> = vec![value_parser(parser)?];
    while let Ok(next_value) = parser.try_parse(&mut value_parser) {
        result.push(next_value);
    }
    Ok(result)
}

pub(crate) fn try_parse_none<T: Default>(parser: &mut CssParser) -> Option<T> {
    parser
        .try_parse(|parser| {
            parser.expect_ident_matching("none")?;
            Ok(T::default())
        })
        .ok()
}

pub(crate) fn try_parse_none_with_value<T>(parser: &mut CssParser, none_value: T) -> Option<T> {
    parser
        .try_parse(move |parser| {
            parser.expect_ident_matching("none")?;
            Ok(none_value)
        })
        .ok()
}

/// Represents the current pseudo state of an entity.
/// By default, it supports only the basic pseudo classes like `:hover`, `:active`, and `:focus`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ImportantLevel {
    /// Default (no !important found)
    Default,
    /// Important rule, with its location
    Important(Range<u32>),
}

pub(crate) fn try_parse_important_level(parser: &mut CssParser) -> ImportantLevel {
    if let Ok(located) = parser.try_parse(|parser| parser.inner_spanned(parse_important)) {
        ImportantLevel::Important(located.span)
    } else {
        ImportantLevel::Default
    }
}

pub fn parse_number(parser: &mut CssParser) -> Result<f32, CssError> {
    parse_calc(parser, |calc_value| match calc_value {
        CalcValue::Number(value) => Ok(*value),
        invalid => Err(format!("Expected a number, found '{invalid}'")),
    })
}

pub(crate) trait CombinedParse {
    type Output;

    fn combined_parse(self, parser: &mut CssParser) -> Result<Option<Self::Output>, CssError>;
}

macro_rules! impl_combined_parse {
    ($(($F:ident, $v:ident, $T:ident)),*) => {
        impl<$($F, $T),*> CombinedParse for ($($F,)*)
        where $($F: Fn(&mut CssParser) -> Result<Option<$T>, CssError>),* {
            type Output = ($(Option<$T>,)*);

            fn combined_parse(self, parser: &mut CssParser) -> Result<Option<Self::Output>, CssError> {
                let ($($v,)*) = self;
                $(
                    let $v = $v(parser)?;
                )*
                if $($v.is_some() ||)* false {
                    Ok(Some(($($v,)*)))
                } else {
                    Ok(None)
                }
            }
        }
    };
}

all_tuples!(impl_combined_parse, 1, 4, F, v, T);
