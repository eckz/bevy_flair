// TODO: FontFeatures parsing

use crate::reflect::parse_calc_angle;
use crate::{
    CssError, ParserExt, ReflectParseCss, error_codes, parse_calc_property_value_with,
    parse_property_value_with,
};
use bevy_flair_style::placeholder::FontSourcePlaceholder;
use bevy_reflect::FromType;
use bevy_text::{
    FontFeatureTag, FontFeatures, FontFeaturesBuilder, FontSize, FontSource, FontStyle,
    FontVariationTag, FontVariations, FontVariationsBuilder, FontWeight, FontWidth,
};
use cssparser::{Parser, Token, match_ignore_ascii_case};

fn parse_font_source(parser: &mut Parser) -> Result<FontSourcePlaceholder, CssError> {
    let path = parser.expect_ident_or_string()?;
    Ok(match_ignore_ascii_case! { path.as_ref(),
         // basic families
        "serif"  => FontSourcePlaceholder::FontSource(FontSource::Serif),
        "sans-serif" => FontSourcePlaceholder::FontSource(FontSource::SansSerif),
        "cursive" => FontSourcePlaceholder::FontSource(FontSource::Cursive),
        "fantasy" => FontSourcePlaceholder::FontSource(FontSource::Fantasy),
        "monospace" => FontSourcePlaceholder::FontSource(FontSource::Monospace),

        // system / ui families
        "system-ui" => FontSourcePlaceholder::FontSource(FontSource::SystemUi),
        "ui-serif" => FontSourcePlaceholder::FontSource(FontSource::UiSerif),
        "ui-sans-serif" => FontSourcePlaceholder::FontSource(FontSource::UiSansSerif),
        "ui-monospace" => FontSourcePlaceholder::FontSource(FontSource::UiMonospace),
        "ui-rounded" => FontSourcePlaceholder::FontSource(FontSource::UiRounded),

        // other types
        "emoji"  => FontSourcePlaceholder::FontSource(FontSource::Emoji),
        "math" => FontSourcePlaceholder::FontSource(FontSource::Math),

        _ => FontSourcePlaceholder::FontFaceReference(path.to_string())
    })
}

pub fn parse_font_size(parser: &mut Parser) -> Result<FontSize, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Number { value, .. } => FontSize::Px(*value),
        Token::Dimension { value, unit, .. } => {
            match_ignore_ascii_case! { unit.as_ref(),
                "px" => FontSize::Px(*value),
                "vw" => FontSize::Vw(*value),
                "vh" => FontSize::Vh(*value),
                "vmin" => FontSize::VMin(*value),
                "vmax" => FontSize::VMax(*value),
                "rem" => FontSize::Rem(*value),
                _ => {
                    return Err(CssError::new_located(
                        &next,
                        error_codes::font::UNEXPECTED_FONT_SIZE_TOKEN,
                        format!("Dimension '{unit}' is not recognized for FontSize. Valid dimensions are 'px' | 'vw' | 'vh' | 'vmin' | 'vmax' | 'rem'")
                    ));
                }
            }
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::font::UNEXPECTED_FONT_SIZE_TOKEN,
                "This is not valid FontSize token. 30px, 10rem are valid examples",
            ));
        }
    })
}

pub fn parse_font_weight(parser: &mut Parser) -> Result<FontWeight, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Ident(ident) => {
            match_ignore_ascii_case! { ident.as_ref(),
                "normal" => FontWeight::NORMAL,
                "bold" => FontWeight::BOLD,
                _ => {
                    return Err(CssError::new_located(
                        &next,
                        error_codes::font::UNEXPECTED_FONT_WEIGHT_TOKEN,
                        format!("Ident '{ident}' is not recognized for FontWeight. Valid weights are 'normal' | 'bold' | 300")
                    ));
                }
            }
        }
        Token::Number {
            int_value: Some(int_value),
            ..
        } if *int_value >= 0 && *int_value <= 1000 => FontWeight(*int_value as u16),
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::font::UNEXPECTED_FONT_WEIGHT_TOKEN,
                "This is not valid FontWeight token. Valid weights are 'normal' | 'bold' | 300",
            ));
        }
    })
}

pub const EPSILON: f32 = 0.001;

fn match_font_width_percentage(value: f32) -> Option<FontWidth> {
    Some(match value {
        v if (v - 0.50).abs() < EPSILON => FontWidth::ULTRA_CONDENSED,
        v if (v - 0.625).abs() < EPSILON => FontWidth::EXTRA_CONDENSED,
        v if (v - 0.75).abs() < EPSILON => FontWidth::CONDENSED,
        v if (v - 0.875).abs() < EPSILON => FontWidth::SEMI_CONDENSED,
        v if (v - 1.0).abs() < EPSILON => FontWidth::NORMAL,
        v if (v - 1.125).abs() < EPSILON => FontWidth::SEMI_EXPANDED,
        v if (v - 1.25).abs() < EPSILON => FontWidth::EXPANDED,
        v if (v - 1.5).abs() < EPSILON => FontWidth::EXTRA_EXPANDED,
        v if (v - 2.0).abs() < EPSILON => FontWidth::ULTRA_EXPANDED,
        _ => return None,
    })
}

pub fn parse_font_width(parser: &mut Parser) -> Result<FontWidth, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Ident(ident) => {
            match_ignore_ascii_case! { ident.as_ref(),
                "ultra-condensed" => FontWidth::ULTRA_CONDENSED,
                "extra-condensed" => FontWidth::EXTRA_CONDENSED,
                "condensed" => FontWidth::CONDENSED,
                "semi-condensed" => FontWidth::SEMI_CONDENSED,
                "normal" => FontWidth::NORMAL,
                "semi-expanded" => FontWidth::SEMI_EXPANDED,
                "expanded" => FontWidth::EXPANDED,
                "extra-expanded" => FontWidth::EXTRA_EXPANDED,
                "ultra-expanded" => FontWidth::ULTRA_EXPANDED,
                _ => {
                    return Err(CssError::new_located(
                        &next,
                        error_codes::font::UNEXPECTED_FONT_WIDTH_TOKEN,
                        format!("Ident '{ident}' is not recognized for FontWidth. Valid widths are 'condensed' | 'normal' | 'semi-expanded' | 'expanded' | 100%")
                    ));
                }
            }
        }
        Token::Percentage { unit_value, .. }
            if let Some(value) = match_font_width_percentage(*unit_value) =>
        {
            value
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::font::UNEXPECTED_FONT_WIDTH_TOKEN,
                "This is not a valid FontWidth token. Valid widths are 'condensed' | 'normal' | 'semi-expanded' | 'expanded' | 100%",
            ));
        }
    })
}

pub fn parse_font_style(parser: &mut Parser) -> Result<FontStyle, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Ident(ident) => {
            match_ignore_ascii_case! { ident.as_ref(),
                "normal" => FontStyle::Normal,
                "italic" => FontStyle::Italic,
                "oblique" => {
                    if let Ok(angle) = parser.try_parse_with(parse_calc_angle) {
                        FontStyle::Oblique(Some(angle.as_degrees()))
                    } else {
                        FontStyle::Oblique(None)
                    }
                },
                _ => {
                    return Err(CssError::new_located(
                        &next,
                        error_codes::font::UNEXPECTED_FONT_STYLE_TOKEN,
                        format!("Ident '{ident}' is not recognized for FontStyle. Valid weights are 'normal' | 'italic' | oblique 45deg")
                    ));
                }
            }
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::font::UNEXPECTED_FONT_STYLE_TOKEN,
                "This is not valid FontStyle token. Valid weights are 'normal' | 'italic' | oblique 45deg",
            ));
        }
    })
}

fn parse_four_ascii_characters(parser: &mut Parser) -> Result<[u8; 4], CssError> {
    let str = parser.located(|parser| parser.expect_string_cloned())?;
    Ok(match str.as_ref().as_bytes().try_into() {
        Ok(c) => c,
        Err(_) => {
            return Err(CssError::new_located(
                &str,
                error_codes::font::FOUR_ASCII_CHARS_STRING,
                format!("Expected \"{str}\" to be four ASCII characters long"),
            ));
        }
    })
}

enum FontFeatureArg {
    On,
    Off,
    Value(u32),
}

fn parse_font_feature_arg(parser: &mut Parser) -> Result<FontFeatureArg, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Ident(ident) => {
            match_ignore_ascii_case! { ident.as_ref(),
                "on" => FontFeatureArg::On,
                "off" => FontFeatureArg::Off,
                _ => {
                    return Err(CssError::new_located(
                        &next,
                        error_codes::basic::UNEXPECTED_TOKEN,
                        "Unexpected token"
                    ));
                }
            }
        }
        Token::Number {
            int_value: Some(int_value),
            ..
        } if *int_value >= 0 && *int_value <= 5000 => FontFeatureArg::Value(*int_value as u32),
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::basic::UNEXPECTED_TOKEN,
                "Unexpected token",
            ));
        }
    })
}

fn parse_single_font_feature(
    parser: &mut Parser,
    builder: FontFeaturesBuilder,
) -> Result<FontFeaturesBuilder, CssError> {
    let feature_name = parse_four_ascii_characters(parser)?;
    let feature_arg = parser
        .try_parse_with(parse_font_feature_arg)
        .unwrap_or(FontFeatureArg::On);

    let tag = FontFeatureTag::new(&feature_name);

    Ok(match feature_arg {
        FontFeatureArg::On => builder.enable(tag),
        FontFeatureArg::Off => builder.set(tag, 0),
        FontFeatureArg::Value(value) => builder.set(tag, value),
    })
}

pub fn parse_font_features(parser: &mut Parser) -> Result<FontFeatures, CssError> {
    let mut builder = FontFeatures::builder();
    builder = parse_single_font_feature(parser, builder)?;
    while parser.try_parse(|parser| parser.expect_comma()).is_ok() {
        builder = parse_single_font_feature(parser, builder)?;
    }
    Ok(builder.build())
}

fn parse_single_font_variation(
    parser: &mut Parser,
    builder: FontVariationsBuilder,
) -> Result<FontVariationsBuilder, CssError> {
    let variation_name = parse_four_ascii_characters(parser)?;
    let value = parser.expect_number()?;
    let tag = FontVariationTag::new(&variation_name);
    Ok(builder.set(tag, value))
}

pub fn parse_font_variations(parser: &mut Parser) -> Result<FontVariations, CssError> {
    let mut builder = FontVariations::builder();
    builder = parse_single_font_variation(parser, builder)?;
    while parser.try_parse(|parser| parser.expect_comma()).is_ok() {
        builder = parse_single_font_variation(parser, builder)?;
    }
    Ok(builder.build())
}

impl FromType<FontSource> for ReflectParseCss {
    fn from_type() -> Self {
        Self(
            |parser| Ok(parse_property_value_with(parser, parse_font_source)?.into_reflect_value()),
        )
    }
}

impl FromType<FontSize> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_calc_property_value_with(parser, parse_font_size))
    }
}

impl FromType<FontWeight> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_calc_property_value_with(parser, parse_font_weight))
    }
}

impl FromType<FontWidth> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| Ok(parse_property_value_with(parser, parse_font_width)?.into_reflect_value()))
    }
}

impl FromType<FontStyle> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| Ok(parse_property_value_with(parser, parse_font_style)?.into_reflect_value()))
    }
}

impl FromType<FontFeatures> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| {
            Ok(parse_property_value_with(parser, parse_font_features)?.into_reflect_value())
        })
    }
}

impl FromType<FontVariations> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| {
            Ok(parse_property_value_with(parser, parse_font_variations)?.into_reflect_value())
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::reflect_test_utils::{
        test_err_parse_reflect, test_parse_reflect, test_parse_reflect_from_to,
    };
    use bevy_flair_style::placeholder::FontSourcePlaceholder;
    use bevy_text::{
        FontFeatureTag, FontFeatures, FontSource, FontStyle, FontVariationTag, FontVariations,
        FontWeight, FontWidth,
    };

    #[test]
    fn test_font_source() {
        assert_eq!(
            test_parse_reflect_from_to::<FontSource, FontSourcePlaceholder>("\"some-font\""),
            FontSourcePlaceholder::FontFaceReference("some-font".into())
        );
        assert_eq!(
            test_parse_reflect_from_to::<FontSource, FontSourcePlaceholder>("some-font"),
            FontSourcePlaceholder::FontFaceReference("some-font".into())
        );
        assert_eq!(
            test_parse_reflect_from_to::<FontSource, FontSourcePlaceholder>("monospace"),
            FontSourcePlaceholder::FontSource(FontSource::Monospace)
        );
        assert_eq!(
            test_parse_reflect_from_to::<FontSource, FontSourcePlaceholder>("sans-serif"),
            FontSourcePlaceholder::FontSource(FontSource::SansSerif)
        );
        assert_eq!(
            test_parse_reflect_from_to::<FontSource, FontSourcePlaceholder>("emoji"),
            FontSourcePlaceholder::FontSource(FontSource::Emoji)
        );
    }

    #[test]
    fn test_font_weight() {
        assert_eq!(
            test_parse_reflect::<FontWeight>("normal"),
            FontWeight::NORMAL
        );
        assert_eq!(test_parse_reflect::<FontWeight>("bold"), FontWeight::BOLD);
        assert_eq!(test_parse_reflect::<FontWeight>("100"), FontWeight(100));
        assert_eq!(test_parse_reflect::<FontWeight>("300"), FontWeight(300));
        assert_eq!(test_parse_reflect::<FontWeight>("400"), FontWeight::NORMAL);
        assert_eq!(test_parse_reflect::<FontWeight>("600"), FontWeight(600));
        assert_eq!(test_parse_reflect::<FontWeight>("700"), FontWeight::BOLD);
        assert_eq!(test_parse_reflect::<FontWeight>("900"), FontWeight(900));
    }

    #[test]
    fn test_font_width() {
        assert_eq!(test_parse_reflect::<FontWidth>("normal"), FontWidth::NORMAL);
        assert_eq!(
            test_parse_reflect::<FontWidth>("ultra-condensed"),
            FontWidth::ULTRA_CONDENSED
        );
        assert_eq!(
            test_parse_reflect::<FontWidth>("extra-condensed"),
            FontWidth::EXTRA_CONDENSED
        );
        assert_eq!(
            test_parse_reflect::<FontWidth>("condensed"),
            FontWidth::CONDENSED
        );
        assert_eq!(
            test_parse_reflect::<FontWidth>("semi-condensed"),
            FontWidth::SEMI_CONDENSED
        );
        assert_eq!(
            test_parse_reflect::<FontWidth>("semi-expanded"),
            FontWidth::SEMI_EXPANDED
        );
        assert_eq!(
            test_parse_reflect::<FontWidth>("expanded"),
            FontWidth::EXPANDED
        );
        assert_eq!(
            test_parse_reflect::<FontWidth>("extra-expanded"),
            FontWidth::EXTRA_EXPANDED
        );
        assert_eq!(
            test_parse_reflect::<FontWidth>("ultra-expanded"),
            FontWidth::ULTRA_EXPANDED
        );

        assert_eq!(
            test_parse_reflect::<FontWidth>("50%"),
            FontWidth::ULTRA_CONDENSED
        );
        assert_eq!(test_parse_reflect::<FontWidth>("100%"), FontWidth::NORMAL);
        assert_eq!(
            test_parse_reflect::<FontWidth>("200%"),
            FontWidth::ULTRA_EXPANDED
        );
    }

    #[test]
    fn test_font_style() {
        assert_eq!(test_parse_reflect::<FontStyle>("normal"), FontStyle::Normal);
        assert_eq!(test_parse_reflect::<FontStyle>("italic"), FontStyle::Italic);
        assert_eq!(
            test_parse_reflect::<FontStyle>("oblique"),
            FontStyle::Oblique(None)
        );
        assert_eq!(
            test_parse_reflect::<FontStyle>("oblique 45deg"),
            FontStyle::Oblique(Some(45.0))
        );
    }

    #[test]
    fn test_font_features() {
        assert_eq!(
            test_parse_reflect::<FontFeatures>("\"smcp\""),
            FontFeatures::from([FontFeatureTag::SMALL_CAPS])
        );
        assert_eq!(
            test_parse_reflect::<FontFeatures>("\"smcp\" on"),
            FontFeatures::from([FontFeatureTag::SMALL_CAPS])
        );
        assert_eq!(
            test_parse_reflect::<FontFeatures>("\"swsh\" 2"),
            FontFeatures::builder()
                .set(FontFeatureTag::SWASH, 2)
                .build()
        );
        assert_eq!(
            test_parse_reflect::<FontFeatures>("\"smcp\" on, \"swsh\" 2"),
            FontFeatures::builder()
                .enable(FontFeatureTag::SMALL_CAPS)
                .set(FontFeatureTag::SWASH, 2)
                .build()
        );

        assert_eq!(
            test_err_parse_reflect::<FontFeatures>("\"toolong\""),
            "[124] Warning: Expected a four ASCII characters
   ,-[ test.css:1:1 ]
   |
 1 | \"toolong\"
   | |^^^^^^^^\x20\x20
   | `---------- Expected \"toolong\" to be four ASCII characters long
---'
"
        );
    }
    #[test]
    fn test_font_variations() {
        assert_eq!(
            test_parse_reflect::<FontVariations>("\"wght\" 50"),
            FontVariations::builder()
                .set(FontVariationTag::WEIGHT, 50.0)
                .build()
        );

        assert_eq!(
            test_parse_reflect::<FontVariations>("\"wght\" 50, \"slnt\" 2"),
            FontVariations::builder()
                .set(FontVariationTag::WEIGHT, 50.0)
                .set(FontVariationTag::SLANT, 2.0)
                .build()
        );
    }
}
