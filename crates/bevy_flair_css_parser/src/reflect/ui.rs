use bevy_reflect::FromType;
use bevy_ui::{BoxShadow, ShadowStyle, Val, ZIndex};

use crate::calc::parse_calc_property_value_with;
use crate::error::CssError;
use crate::error_codes::ui as error_codes;
use crate::utils::parse_property_value_with;
use crate::{ParserExt, ReflectParseCss};
use bevy_flair_core::ReflectValue;
use cssparser::{Parser, Token, match_ignore_ascii_case};
use smallvec::SmallVec;

pub(crate) fn parse_f32(parser: &mut Parser) -> Result<f32, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Number { value, .. } => *value,
        Token::Dimension { value, unit, .. } if unit.eq_ignore_ascii_case("px") => *value,
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::UNEXPECTED_F32_TOKEN,
                "This is not a valid float token. 34.2 or 34px are valid numbers",
            ));
        }
    })
}

pub(crate) fn parse_val(parser: &mut Parser) -> Result<Val, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Ident(ident) if ident.as_ref() == "auto" => Val::Auto,
        Token::Number { value, .. } if *value == 0.0 => Val::ZERO,
        Token::Number { value, .. } => Val::Px(*value),
        Token::Percentage { unit_value, .. } => Val::Percent(*unit_value * 100.0),
        Token::Dimension { value, unit, .. } => {
            match_ignore_ascii_case! { unit.as_ref(),
                "px" => Val::Px(*value),
                "vw" => Val::Vw(*value),
                "vh" => Val::Vh(*value),
                "vmin" => Val::VMin(*value),
                "vmax" => Val::VMax(*value),
                _ => {
                    return Err(CssError::new_located(&next,  error_codes::UNEXPECTED_VAL_TOKEN, format!("Dimension {unit} is not recognized. Valid dimensions are 'px' | 'vw' | 'vh' | 'vmin' | 'vmax'")));
                }
            }
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::UNEXPECTED_VAL_TOKEN,
                "This is not valid Val token. 'auto', 3px, 44.2% are valid examples",
            ));
        }
    })
}

pub(crate) fn parse_four_values<T: Copy>(
    parser: &mut Parser,
    mut f: impl FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<[T; 4], CssError> {
    let mut values = SmallVec::<[T; 4]>::new();

    values.push(f(parser)?);
    while !parser.is_exhausted() && values.len() < 4 {
        values.push(f(parser)?);
    }

    Ok(match *values.as_slice() {
        [all] => [all; 4],
        [a, b] => [a, b, a, b],
        [a, b, c] => [a, b, c, b],
        [a, b, c, d] => [a, b, c, d],
        _ => unreachable!(),
    })
}

fn parse_z_index(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    Ok(ReflectValue::new(ZIndex(parser.expect_integer()?)))
}

fn parse_single_box_shadow_style(parser: &mut Parser) -> Result<ShadowStyle, CssError> {
    let mut values = SmallVec::<[_; 4]>::new();
    let mut color = ShadowStyle::default().color;

    while !parser.is_exhausted() {
        let val_result = parser.try_parse(parse_val);
        if let Ok(val) = val_result {
            values.push(val);
            continue;
        }
        let color_result = parser.try_parse(super::color::parse_color);

        if let Ok(new_color) = color_result {
            // TODO: Error if there an existing color already
            color = new_color;
            continue;
        }

        return Err(CssError::from(parser.new_error_for_next_token::<()>()));
    }

    let shadow_style = match *values.as_slice() {
        [x_offset, y_offset] => ShadowStyle {
            color,
            x_offset,
            y_offset,
            ..Default::default()
        },
        [x_offset, y_offset, blur_radius] => ShadowStyle {
            color,
            x_offset,
            y_offset,
            blur_radius,
            ..Default::default()
        },
        [x_offset, y_offset, blur_radius, spread_radius] => ShadowStyle {
            color,
            x_offset,
            y_offset,
            blur_radius,
            spread_radius,
        },
        _ => {
            return Err(CssError::new_unlocated(
                error_codes::INVALID_NUMBER_OF_SHADOW_VALS,
                "Unexpected number of values. Between 2 and 4 values were expected",
            ));
        }
    };

    Ok(shadow_style)
}

fn parse_box_shadow(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let styles = parser.parse_comma_separated(|parser| {
        parse_single_box_shadow_style(parser).map_err(|err| err.into_parse_error())
    })?;

    Ok(ReflectValue::new(BoxShadow(styles)))
}

// TODO: OverflowClipMargin

impl FromType<f32> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_calc_property_value_with(parser, parse_f32))
    }
}

impl FromType<Val> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_calc_property_value_with(parser, parse_val))
    }
}

impl FromType<ZIndex> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_property_value_with(parser, parse_z_index))
    }
}

impl FromType<BoxShadow> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_property_value_with(parser, parse_box_shadow))
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::testing::test_parse_css;
    use bevy_color::palettes::css;
    use bevy_ui::{BoxShadow, ShadowStyle, Val, ZIndex};

    #[test]
    fn test_val() {
        assert_eq!(test_parse_css::<Val>("auto"), Val::Auto);
        assert_eq!(test_parse_css::<Val>("33.5"), Val::Px(33.5));
        assert_eq!(test_parse_css::<Val>("15px"), Val::Px(15.0));
        assert_eq!(test_parse_css::<Val>("55%"), Val::Percent(55.0));
        assert_eq!(test_parse_css::<Val>("157vw"), Val::Vw(157.0));
        assert_eq!(test_parse_css::<Val>("343.5vh"), Val::Vh(343.5));
        assert_eq!(test_parse_css::<Val>("987vmin"), Val::VMin(987.0));
        assert_eq!(test_parse_css::<Val>("9999vmax"), Val::VMax(9999.0));
    }

    #[test]
    fn test_val_with_calc() {
        assert_eq!(test_parse_css::<Val>("calc(15px * 2)"), Val::Px(30.0));
    }

    #[test]
    fn test_z_index() {
        assert_eq!(test_parse_css::<ZIndex>("2"), ZIndex(2));
        assert_eq!(test_parse_css::<ZIndex>("0"), ZIndex(0));
        assert_eq!(test_parse_css::<ZIndex>("-999999"), ZIndex(-999999));
    }

    #[test]
    fn test_box_shadow() {
        assert_eq!(
            test_parse_css::<BoxShadow>("10px 5px"),
            BoxShadow::from(ShadowStyle {
                x_offset: Val::Px(10.0),
                y_offset: Val::Px(5.0),
                ..Default::default()
            })
        );

        assert_eq!(
            test_parse_css::<BoxShadow>("60px -16px teal"),
            BoxShadow::from(ShadowStyle {
                x_offset: Val::Px(60.0),
                y_offset: Val::Px(-16.0),
                color: css::TEAL.into(),
                ..Default::default()
            })
        );

        assert_eq!(
            test_parse_css::<BoxShadow>("white 12px -22px 2px 1px"),
            BoxShadow::from(ShadowStyle {
                x_offset: Val::Px(12.0),
                y_offset: Val::Px(-22.0),
                blur_radius: Val::Px(2.0),
                spread_radius: Val::Px(1.0),
                color: css::WHITE.into(),
            })
        );
    }
}
