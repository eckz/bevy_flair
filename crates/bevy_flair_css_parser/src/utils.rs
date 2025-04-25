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
