use crate::{CssError, ParserExt, error_codes::vars as error_codes};
use bevy_flair_core::PropertyValue;
use cssparser::{Parser, match_ignore_ascii_case, parse_important};
use std::ops::Range;

pub(crate) fn parse_property_global_keyword<T>(
    parser: &mut Parser,
) -> Result<PropertyValue<T>, CssError> {
    let next = parser.expect_located_ident()?;

    Ok(match_ignore_ascii_case! {next.as_ref(),
        "inherit" => PropertyValue::Inherit,
        "initial" => PropertyValue::Initial,
        _ => {
            return Err(CssError::new_unlocated(error_codes::INVALID_TOKEN, "Invalid property value token"));
        }
    })
}

/// Parses a CSS property value that may be either a global keyword or a typed value.
///
/// This function first attempts to parse one of the global CSS keywords:
/// - `inherit`
/// - `initial`
///
/// If none of these match, it falls back to the provided `value_parser`
/// to parse the property as a typed value `T`.
///
///  If the produced type supports [`calc()`], it's preferable to use [`parse_calc_property_value_with()`].
///
/// [`calc()`]: crate::Calculable
/// [`parse_calc_property_value_with()`]: crate::parse_calc_property_value_with
/// # Examples
/// ```
/// # use cssparser::{Parser, ParserInput};
/// # use bevy_flair_core::PropertyValue;
/// # use bevy_flair_css_parser::{parse_property_value_with, parse_val};
///
/// let mut input = ParserInput::new("inherit");
/// let mut parser = Parser::new(&mut input);
/// let property_value = parse_property_value_with(&mut parser, parse_val).unwrap();
/// assert_eq!(property_value, PropertyValue::Inherit);
/// ```
pub fn parse_property_value_with<T>(
    parser: &mut Parser,
    mut value_parser: impl FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<PropertyValue<T>, CssError> {
    if let Ok(value) = parser.try_parse(parse_property_global_keyword) {
        return Ok(value);
    };
    value_parser(parser).map(PropertyValue::Value)
}

pub(crate) fn parse_many<T, F: FnMut(&mut Parser) -> Result<T, CssError>>(
    parser: &mut Parser,
    mut value_parser: F,
) -> Result<Vec<T>, CssError> {
    let mut result: Vec<T> = vec![value_parser(parser)?];
    while let Ok(next_value) = parser.try_parse_with(&mut value_parser) {
        result.push(next_value);
    }
    Ok(result)
}

pub(crate) fn try_parse_none<T: Default>(parser: &mut Parser) -> Option<T> {
    parser
        .try_parse_with(|parser| {
            parser.expect_ident_matching("none")?;
            Ok(T::default())
        })
        .ok()
}

pub(crate) fn try_parse_none_with_value<T>(parser: &mut Parser, none_value: T) -> Option<T> {
    parser
        .try_parse_with(move |parser| {
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
    Important(Range<usize>),
}

pub(crate) fn try_parse_important_level(parser: &mut Parser) -> ImportantLevel {
    if let Ok(located) = parser.try_parse(|parser| parser.located(parse_important)) {
        ImportantLevel::Important(located.location)
    } else {
        ImportantLevel::Default
    }
}
