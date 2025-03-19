use crate::{CssError, ParserExt, error_codes::vars as error_codes};
use bevy_flair_core::PropertyValue;
use cssparser::{Parser, Token, match_ignore_ascii_case};

fn parse_property_value_ident<T>(parser: &mut Parser) -> Result<PropertyValue<T>, CssError> {
    let next = parser.expect_located_ident()?;

    Ok(match_ignore_ascii_case! {next.as_ref(),
        "unset" => PropertyValue::Unset,
        "inherit" => PropertyValue::Inherit,
        _ => {
            return Err(CssError::new_unlocated(error_codes::INVALID_TOKEN, "Invalid property value"));
        }
    })
}

fn parse_property_value_var<T>(parser: &mut Parser) -> Result<PropertyValue<T>, CssError> {
    parser.expect_function_matching("var")?;

    Ok(parser.parse_nested_block(|parser| {
        let var_name = parser.expect_located_ident()?;

        if !var_name.starts_with("--") || var_name.len() < 3 {
            return Err(CssError::new_located(
                &var_name,
                error_codes::INVALID_VAR_NAME,
                "Invalid var identifier",
            )
            .into_parse_error());
        }
        Ok(PropertyValue::Var((&var_name.as_ref()[2..]).into()))
    })?)
}

pub fn parse_property_value_with<T>(
    parser: &mut Parser,
    value_parser: impl FnOnce(&mut Parser) -> Result<T, CssError>,
) -> Result<PropertyValue<T>, CssError> {
    if let Ok(value) = parser.try_parse(parse_property_value_ident) {
        return Ok(value);
    };
    let peek = parser.peek()?;
    match peek {
        Token::Function(name) if name.eq_ignore_ascii_case("var") => {
            return parse_property_value_var(parser);
        }
        _ => {}
    }

    value_parser(parser).map(|v| PropertyValue::Value(v))
}
