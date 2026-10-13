use crate::ReflectParseCss;
use crate::error::CssError;

use crate::utils::{parse_property_value_with, try_parse_none};
use bevy_color::Color;
use bevy_flair_core::ReflectValue;

use bevy_reflect::CreateTypeData;

use crate::color::parse_color;
use cssparser::Parser;

pub fn parse_option_color(parser: &mut Parser) -> Result<Option<Color>, CssError> {
    if let Some(none_value) = try_parse_none::<Option<Color>>(parser) {
        Ok(none_value)
    } else {
        Ok(Some(parse_color(parser)?))
    }
}

impl CreateTypeData<Color> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| Ok(parse_property_value_with(parser, parse_color)?.map(ReflectValue::Color)))
    }
}

impl CreateTypeData<Option<Color>> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| {
            Ok(parse_property_value_with(parser, parse_option_color)?.map(ReflectValue::new))
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::reflect_test_utils::test_parse_reflect;
    use bevy_color::Color;

    #[test]
    fn test_srgb() {
        assert_eq!(
            test_parse_reflect::<Color>("rgb(214, 122, 127)"),
            Color::srgb_u8(214, 122, 127)
        );

        assert_eq!(
            test_parse_reflect::<Color>("rgba(10 20 30)"),
            Color::srgb_u8(10, 20, 30)
        );

        assert_eq!(
            test_parse_reflect::<Color>("rgb(from white r g b / 0.5)"),
            Color::srgba(1.0, 1.0, 1.0, 0.5)
        );

        assert_eq!(
            test_parse_reflect::<Color>("transparent"),
            Color::srgba(0.0, 0.0, 0.0, 0.0)
        );
        assert_eq!(
            test_parse_reflect::<Color>("#009912"),
            Color::srgb_u8(0, 0x99, 0x12)
        );
    }
}
