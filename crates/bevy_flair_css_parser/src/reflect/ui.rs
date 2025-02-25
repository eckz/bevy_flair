use bevy::prelude::ShadowStyle;
use bevy::reflect::FromType;
use bevy::ui::{BorderRadius, BoxShadow, Outline, Overflow, OverflowAxis, UiRect, Val, ZIndex};

use crate::error::CssError;
use crate::error_codes::ui as error_codes;
use crate::reflect::{ReflectParseCss, ReflectParseCssEnum};
use crate::ParserExt;
use bevy_flair_core::ReflectValue;
use cssparser::{match_ignore_ascii_case, Parser, Token};
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

fn parse_val(parser: &mut Parser) -> Result<Val, CssError> {
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

fn parse_ui_rect(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let [top, right, bottom, left] = parse_four_values(parser, parse_val)?;

    let ui_rect = UiRect {
        left,
        right,
        top,
        bottom,
    };
    Ok(ReflectValue::new(ui_rect))
}

fn parse_border_radius(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let [top_left, top_right, bottom_right, bottom_left] = parse_four_values(parser, parse_val)?;

    let border_radius = BorderRadius {
        top_left,
        top_right,
        bottom_left,
        bottom_right,
    };
    Ok(ReflectValue::new(border_radius))
}

fn parse_outline(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    fn parse_width_ident(parser: &mut Parser) -> Result<Val, CssError> {
        let ident = parser.expect_ident()?;
        Ok(match_ignore_ascii_case! { ident.as_ref(),
            "none" => Val::ZERO,
            "thin" => Val::Px(1.0),
            "medium" => Val::Px(2.0),
            "thick" => Val::Px(5.0),
            // This error does not matter much because it will be ignored
            _ => return Err(CssError::from(parser.new_error_for_next_token::<()>())),
        })
    }

    let width_result = parser
        .try_parse(parse_width_ident)
        .or_else(|_| parser.try_parse(parse_val));

    let width = match width_result {
        Ok(width) => width,
        _ => return Err(CssError::from(parser.new_error_for_next_token::<()>())),
    };

    let color = if !parser.is_exhausted() {
        super::color::parse_color(parser)?
    } else {
        Outline::default().color
    };

    let outline = Outline {
        width,
        color,
        ..Default::default()
    };

    Ok(ReflectValue::new(outline))
}

fn parse_z_index(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    Ok(ReflectValue::new(ZIndex(parser.expect_integer()?)))
}

fn parse_overflow(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    let parse_overflow_axis = <ReflectParseCssEnum as FromType<OverflowAxis>>::from_type().0;
    let first_value = parse_overflow_axis(parser)?.downcast_value().unwrap();

    if parser.is_exhausted() {
        Ok(ReflectValue::new(Overflow {
            x: first_value,
            y: first_value,
        }))
    } else {
        let second_value = parse_overflow_axis(parser)?.downcast_value().unwrap();
        Ok(ReflectValue::new(Overflow {
            x: first_value,
            y: second_value,
        }))
    }
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
            ))
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
        Self(|parser| parse_f32(parser).map(ReflectValue::Float))
    }
}

impl FromType<Val> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_val(parser).map(ReflectValue::Val))
    }
}

impl FromType<UiRect> for ReflectParseCss {
    fn from_type() -> Self {
        Self(parse_ui_rect)
    }
}

impl FromType<BorderRadius> for ReflectParseCss {
    fn from_type() -> Self {
        Self(parse_border_radius)
    }
}

impl FromType<Outline> for ReflectParseCss {
    fn from_type() -> Self {
        Self(parse_outline)
    }
}

impl FromType<ZIndex> for ReflectParseCss {
    fn from_type() -> Self {
        Self(parse_z_index)
    }
}

impl FromType<Overflow> for ReflectParseCss {
    fn from_type() -> Self {
        Self(parse_overflow)
    }
}

impl FromType<BoxShadow> for ReflectParseCss {
    fn from_type() -> Self {
        Self(parse_box_shadow)
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::testing::test_parse_css;
    use bevy::color::palettes::css;
    use bevy::prelude::ShadowStyle;
    use bevy::ui::{BorderRadius, BoxShadow, Outline, Overflow, UiRect, Val, ZIndex};

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
    fn test_ui_rect() {
        assert_eq!(test_parse_css::<UiRect>("2px"), UiRect::all(Val::Px(2.0)));

        assert_eq!(test_parse_css::<UiRect>("0"), UiRect::all(Val::ZERO));

        assert_eq!(
            test_parse_css::<UiRect>("10% auto"),
            UiRect {
                left: Val::Auto,
                right: Val::Auto,
                top: Val::Percent(10.0),
                bottom: Val::Percent(10.0),
            }
        );

        assert_eq!(
            test_parse_css::<UiRect>("10px 50px 20px"),
            UiRect {
                left: Val::Px(50.0),
                right: Val::Px(50.0),
                top: Val::Px(10.0),
                bottom: Val::Px(20.0),
            }
        );

        assert_eq!(
            test_parse_css::<UiRect>("10px 50px 30px 0"),
            UiRect {
                left: Val::ZERO,
                right: Val::Px(50.0),
                top: Val::Px(10.0),
                bottom: Val::Px(30.0),
            }
        );
    }

    #[test]
    fn test_border_radius() {
        assert_eq!(
            test_parse_css::<BorderRadius>("10px"),
            BorderRadius::all(Val::Px(10.0))
        );

        assert_eq!(
            test_parse_css::<BorderRadius>("0"),
            BorderRadius::all(Val::ZERO)
        );

        assert_eq!(
            test_parse_css::<BorderRadius>("10px 5%"),
            BorderRadius {
                top_left: Val::Px(10.0),
                top_right: Val::Percent(5.0),
                bottom_left: Val::Percent(5.0),
                bottom_right: Val::Px(10.0),
            }
        );

        assert_eq!(
            test_parse_css::<BorderRadius>("2px 50% 50px"),
            BorderRadius {
                top_left: Val::Px(2.0),
                top_right: Val::Percent(50.0),
                bottom_left: Val::Percent(50.0),
                bottom_right: Val::Px(50.0),
            }
        );

        assert_eq!(
            test_parse_css::<BorderRadius>("1px 0 3px 4px"),
            BorderRadius {
                top_left: Val::Px(1.0),
                top_right: Val::ZERO,
                bottom_left: Val::Px(4.0),
                bottom_right: Val::Px(3.0),
            }
        );
    }

    #[test]
    fn test_outline() {
        assert_eq!(
            test_parse_css::<Outline>("5px"),
            Outline {
                width: Val::Px(5.0),
                ..Default::default()
            }
        );
        assert_eq!(
            test_parse_css::<Outline>("none"),
            Outline {
                width: Val::ZERO,
                ..Default::default()
            }
        );
        assert_eq!(
            test_parse_css::<Outline>("thin"),
            Outline {
                width: Val::Px(1.0),
                ..Default::default()
            }
        );
        assert_eq!(
            test_parse_css::<Outline>("thick"),
            Outline {
                width: Val::Px(5.0),
                ..Default::default()
            }
        );
        assert_eq!(
            test_parse_css::<Outline>("3px red"),
            Outline {
                width: Val::Px(3.0),
                color: css::RED.into(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_z_index() {
        assert_eq!(test_parse_css::<ZIndex>("2"), ZIndex(2));
        assert_eq!(test_parse_css::<ZIndex>("0"), ZIndex(0));
        assert_eq!(test_parse_css::<ZIndex>("-999999"), ZIndex(-999999));
    }

    #[test]
    fn test_overflow() {
        assert_eq!(test_parse_css::<Overflow>("clip"), Overflow::clip());

        assert_eq!(test_parse_css::<Overflow>("visible"), Overflow::visible());

        assert_eq!(
            test_parse_css::<Overflow>("clip visible"),
            Overflow::clip_x()
        );
        assert_eq!(
            test_parse_css::<Overflow>("visible scroll"),
            Overflow::scroll_y()
        );
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
