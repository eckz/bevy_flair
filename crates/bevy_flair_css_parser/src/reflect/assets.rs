use crate::error::CssError;

use crate::ReflectParseCss;
use crate::utils::parse_property_value_with;
use bevy_asset::Handle;
use bevy_flair_core::ReflectValue;
use bevy_flair_style::{AssetPathPlaceHolder, FontTypePlaceholder};
use bevy_image::Image;
use bevy_reflect::{FromType, TypePath};
use bevy_text::Font;
use cssparser::Parser;

pub(crate) fn parse_asset_path<A: TypePath>(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let path = parser.expect_url_or_string()?;
    Ok(ReflectValue::new(AssetPathPlaceHolder::<A>::new(
        path.as_ref(),
    )))
}

fn parse_font(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let path = parser.expect_ident_or_string()?;
    Ok(ReflectValue::new(FontTypePlaceholder(
        path.as_ref().to_owned(),
    )))
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
    use crate::reflect::testing::test_parse_reflect_from_to;
    use bevy_asset::Handle;
    use bevy_flair_style::{AssetPathPlaceHolder, FontTypePlaceholder};
    use bevy_image::Image;
    use bevy_text::Font;

    #[test]
    fn test_font() {
        assert_eq!(
            test_parse_reflect_from_to::<Handle<Font>, FontTypePlaceholder>("\"some-font\""),
            FontTypePlaceholder("some-font".into())
        );
        assert_eq!(
            test_parse_reflect_from_to::<Handle<Font>, FontTypePlaceholder>("some-font"),
            FontTypePlaceholder("some-font".into())
        );
    }

    #[test]
    fn test_asset_path() {
        assert_eq!(
            test_parse_reflect_from_to::<Handle<Image>, AssetPathPlaceHolder<Image>>(
                "url(\"some-path\")"
            ),
            AssetPathPlaceHolder::new("some-path")
        );
        assert_eq!(
            test_parse_reflect_from_to::<Handle<Image>, AssetPathPlaceHolder<Image>>(
                "\"some-path\""
            ),
            AssetPathPlaceHolder::new("some-path")
        );
    }
}
