use crate::error::CssError;
use crate::reflect::ReflectParseCss;
use bevy::asset::Handle;
use bevy::prelude::Image;
use bevy::reflect::{FromType, TypePath};
use bevy::text::Font;
use bevy_flair_core::ReflectValue;
use bevy_flair_style::{AssetPathPlaceHolder, FontTypePlaceholder};
use cssparser::Parser;

fn parse_asset_path<A: TypePath>(parser: &mut Parser) -> Result<ReflectValue, CssError> {
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
        ReflectParseCss(parse_asset_path::<Image>)
    }
}

impl FromType<Handle<Font>> for ReflectParseCss {
    fn from_type() -> Self {
        ReflectParseCss(parse_font)
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::testing::test_parse_css_2;
    use bevy::asset::Handle;
    use bevy::image::Image;
    use bevy::text::Font;
    use bevy_flair_style::{AssetPathPlaceHolder, FontTypePlaceholder};

    #[test]
    fn test_font() {
        assert_eq!(
            test_parse_css_2::<Handle<Font>, FontTypePlaceholder>("\"some-font\""),
            FontTypePlaceholder("some-font".into())
        );
        assert_eq!(
            test_parse_css_2::<Handle<Font>, FontTypePlaceholder>("some-font"),
            FontTypePlaceholder("some-font".into())
        );
    }

    #[test]
    fn test_asset_path() {
        assert_eq!(
            test_parse_css_2::<Handle<Image>, AssetPathPlaceHolder<Image>>("url(\"some-path\")"),
            AssetPathPlaceHolder::new("some-path")
        );
        assert_eq!(
            test_parse_css_2::<Handle<Image>, AssetPathPlaceHolder<Image>>("\"some-path\""),
            AssetPathPlaceHolder::new("some-path")
        );
    }
}
