use crate::reflect::parse_color;
use crate::reflect::ui::parse_calc_f32;
use crate::utils::{parse_property_value_with, try_parse_none_with_value};
use crate::{CssError, ParserExt, ReflectParseCss, error_codes};
use bevy_color::Color;
use bevy_flair_core::ReflectValue;
use bevy_math::Vec2;
use bevy_reflect::FromType;
use bevy_text::{LetterSpacing, LineHeight};
use bevy_ui::widget::TextShadow;
use cssparser::{Parser, Token, match_ignore_ascii_case};

const NONE_TEXT_SHADOW: TextShadow = TextShadow {
    offset: Vec2::ZERO,
    color: Color::NONE,
};

fn parse_string(parser: &mut Parser) -> Result<String, CssError> {
    let str = parser.expect_string()?;
    Ok(str.to_string())
}

fn parse_line_height(parser: &mut Parser) -> Result<LineHeight, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Ident(ident) if ident.as_ref() == "normal" => LineHeight::default(),
        Token::Number { value, .. } => LineHeight::RelativeToFont(*value),
        Token::Percentage { unit_value, .. } => LineHeight::RelativeToFont(*unit_value),
        Token::Dimension { value, unit, .. } => {
            match_ignore_ascii_case! { unit.as_ref(),
                "px" => LineHeight::Px(*value),
                "em" => LineHeight::RelativeToFont(*value),
                _ => {
                    return Err(CssError::new_located(
                        &next,
                        error_codes::ui::UNEXPECTED_LINE_HEIGHT_TOKEN,
                        format!("Dimension '{unit}' is not recognized for LineHeight. Valid dimensions are 'em' | 'px'")
                    ));
                }
            }
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::ui::UNEXPECTED_LINE_HEIGHT_TOKEN,
                "This is not valid LineHeight token. 3px, 2em, 44.2% are valid examples",
            ));
        }
    })
}

fn parse_letter_spacing(parser: &mut Parser) -> Result<LetterSpacing, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Ident(ident) if ident.as_ref() == "normal" => LetterSpacing::default(),
        Token::Dimension { value, unit, .. } => {
            match_ignore_ascii_case! { unit.as_ref(),
                "px" => LetterSpacing::Px(*value),
                "rem" => LetterSpacing::Rem(*value),
                _ => {
                    return Err(CssError::new_located(
                        &next,
                        error_codes::ui::UNEXPECTED_LETTER_SPACING_TOKEN,
                        format!("Dimension '{unit}' is not recognized for LetterSpacing. Valid dimensions are 'rem' | 'px'")
                    ));
                }
            }
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::ui::UNEXPECTED_LETTER_SPACING_TOKEN,
                "This is not valid LetterSpacing token. 'normal', 10px are valid examples",
            ));
        }
    })
}

fn parse_text_shadow(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    if let Some(none_value) = try_parse_none_with_value::<TextShadow>(parser, NONE_TEXT_SHADOW) {
        return Ok(ReflectValue::new(none_value));
    }

    if let Ok(color) = parser.try_parse_with(parse_color) {
        let offset_x = parse_calc_f32(parser)?;
        let offset_y = parse_calc_f32(parser)?;

        Ok(ReflectValue::new(TextShadow {
            offset: Vec2::new(offset_x, offset_y),
            color,
        }))
    } else {
        let offset_x = parse_calc_f32(parser)?;
        let offset_y = parse_calc_f32(parser)?;

        let color = parser
            .try_parse_with(parse_color)
            .unwrap_or_else(|_| TextShadow::default().color);

        Ok(ReflectValue::new(TextShadow {
            offset: Vec2::new(offset_x, offset_y),
            color,
        }))
    }
}

impl FromType<LineHeight> for ReflectParseCss {
    fn from_type() -> Self {
        Self(
            |parser| Ok(parse_property_value_with(parser, parse_line_height)?.into_reflect_value()),
        )
    }
}

impl FromType<LetterSpacing> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| {
            Ok(parse_property_value_with(parser, parse_letter_spacing)?.into_reflect_value())
        })
    }
}

impl FromType<TextShadow> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_property_value_with(parser, parse_text_shadow))
    }
}

impl FromType<String> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| Ok(parse_property_value_with(parser, parse_string)?.into_reflect_value()))
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::reflect_test_utils::test_parse_reflect;
    use bevy_color::palettes::css;
    use bevy_math::Vec2;
    use bevy_text::{LetterSpacing, LineHeight};
    use bevy_ui::widget::TextShadow;

    #[test]
    fn string() {
        assert_eq!(test_parse_reflect::<String>("\"a\""), "a".to_string());
    }

    #[test]
    fn test_line_height() {
        assert_eq!(
            test_parse_reflect::<LineHeight>("10px"),
            LineHeight::Px(10.0),
        );
        assert_eq!(
            test_parse_reflect::<LineHeight>("normal"),
            LineHeight::RelativeToFont(1.2),
        );
        assert_eq!(
            test_parse_reflect::<LineHeight>("2.5"),
            LineHeight::RelativeToFont(2.5),
        );
        assert_eq!(
            test_parse_reflect::<LineHeight>("120%"),
            LineHeight::RelativeToFont(1.2),
        );
    }
    #[test]
    fn test_letter_spacing() {
        assert_eq!(
            test_parse_reflect::<LetterSpacing>("10px"),
            LetterSpacing::Px(10.0)
        );
        assert_eq!(
            test_parse_reflect::<LetterSpacing>("5rem"),
            LetterSpacing::Rem(5.0)
        );
    }

    #[test]
    fn test_text_shadow() {
        assert_eq!(
            test_parse_reflect::<TextShadow>("10px 5px"),
            TextShadow {
                offset: Vec2 { x: 10.0, y: 5.0 },
                color: TextShadow::default().color
            }
        );

        assert_eq!(
            test_parse_reflect::<TextShadow>("10px 5px teal"),
            TextShadow {
                offset: Vec2 { x: 10.0, y: 5.0 },
                color: css::TEAL.into()
            }
        );

        assert_eq!(
            test_parse_reflect::<TextShadow>("white calc(10px * 2) 5px"),
            TextShadow {
                offset: Vec2 { x: 20.0, y: 5.0 },
                color: css::WHITE.into()
            }
        );
    }
}
