use crate::{CssError, ParserExt, error_codes::vars as error_codes};
use bevy_flair_core::PropertyValue;
use cssparser::{Parser, match_ignore_ascii_case};

fn parse_property_value_ident<T>(parser: &mut Parser) -> Result<PropertyValue<T>, CssError> {
    let next = parser.expect_located_ident()?;

    Ok(match_ignore_ascii_case! {next.as_ref(),
        "inherit" => PropertyValue::Inherit,
        _ => {
            return Err(CssError::new_unlocated(error_codes::INVALID_TOKEN, "Invalid property value token"));
        }
    })
}

pub fn parse_property_value_with<T>(
    parser: &mut Parser,
    mut value_parser: impl FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<PropertyValue<T>, CssError> {
    if let Ok(value) = parser.try_parse(parse_property_value_ident) {
        return Ok(value);
    };
    value_parser(parser).map(PropertyValue::Value)
}

pub fn parse_many<T, F: FnMut(&mut Parser) -> Result<T, CssError>>(
    parser: &mut Parser,
    mut value_parser: F,
) -> Result<Vec<T>, CssError> {
    let mut result: Vec<T> = vec![value_parser(parser)?];
    while let Ok(next_value) = parser.try_parse_with(&mut value_parser) {
        result.push(next_value);
    }
    Ok(result)
}

pub fn try_parse_none<T: Default>(parser: &mut Parser) -> Option<T> {
    parser
        .try_parse_with(|parser| {
            parser.expect_ident_matching("none")?;
            Ok(T::default())
        })
        .ok()
}

pub fn try_parse_none_with_value<T>(parser: &mut Parser, none_value: T) -> Option<T> {
    parser
        .try_parse_with(move |parser| {
            parser.expect_ident_matching("none")?;
            Ok(none_value)
        })
        .ok()
}
