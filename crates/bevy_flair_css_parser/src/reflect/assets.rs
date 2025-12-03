use crate::error::CssError;

use crate::ReflectParseCss;
use crate::utils::parse_property_value_with;
use bevy_asset::{Asset, Handle};
use bevy_flair_core::ReflectValue;
use bevy_flair_style::placeholder::{AssetPathPlaceholder, FontTypePlaceholder};
use bevy_image::Image;
use bevy_reflect::{FromType, TypePath};
use bevy_text::Font;
use cssparser::Parser;

pub(crate) fn parse_asset_path<A: Asset + TypePath>(
    parser: &mut Parser,
) -> Result<ReflectValue, CssError> {
    let path = parser.expect_url_or_string()?;
    Ok(ReflectValue::new(AssetPathPlaceholder::<A>::new(
        path.as_ref(),
    )))
}

fn parse_font(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let path = parser.expect_ident_or_string()?;
    Ok(ReflectValue::new(FontTypePlaceholder::new(path.as_ref())))
}

impl FromType<Handle<Image>> for ReflectParseCss {
    fn from_type() -> Self {
        ReflectParseCss(|parser| parse_property_value_with(parser, parse_asset_path::<Image>))
    }
}

impl FromType<Handle<Font>> for ReflectParseCss {
    fn from_type() -> Self {
        ReflectParseCss(|parser| parse_property_value_with(parser, parse_font))
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::reflect_test_utils::test_parse_reflect_from_to;
    use bevy_asset::Handle;
    use bevy_flair_style::placeholder::{AssetPathPlaceholder, FontTypePlaceholder};
    use bevy_image::Image;
    use bevy_text::Font;

    #[test]
    fn test_font() {
        assert_eq!(
            test_parse_reflect_from_to::<Handle<Font>, FontTypePlaceholder>("\"some-font\""),
            FontTypePlaceholder::new("some-font")
        );
        assert_eq!(
            test_parse_reflect_from_to::<Handle<Font>, FontTypePlaceholder>("some-font"),
            FontTypePlaceholder::new("some-font")
        );
    }

    #[test]
    fn test_asset_path() {
        assert_eq!(
            test_parse_reflect_from_to::<Handle<Image>, AssetPathPlaceholder<Image>>(
                "url(\"some-path\")"
            ),
            AssetPathPlaceholder::new("some-path")
        );
        assert_eq!(
            test_parse_reflect_from_to::<Handle<Image>, AssetPathPlaceholder<Image>>(
                "\"some-path\""
            ),
            AssetPathPlaceholder::new("some-path")
        );
    }
}
