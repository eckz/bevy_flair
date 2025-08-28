use crate::calc::{parse_calc_property_value_with, parse_calc_value};
use crate::error::CssError;
use crate::error_codes::ui as error_codes;
use crate::reflect::enums::parse_enum_value;
use crate::reflect::parse_color;
use crate::utils::{parse_property_value_with, try_parse_none};
use crate::{ParserExt, ReflectParseCss};
use bevy_flair_core::ReflectValue;
use bevy_math::{Rot2, Vec2};
use bevy_reflect::FromType;
use bevy_ui::{BoxShadow, OverflowClipMargin, ShadowStyle, UiTransform, Val, Val2, ZIndex};
use cssparser::{Parser, Token, match_ignore_ascii_case};
use smallvec::SmallVec;
use std::f32::consts;

pub fn parse_f32(parser: &mut Parser) -> Result<f32, CssError> {
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

/// Parses a [`Val`] (UI length/size value) from a CSS token.
///
/// This function recognizes keywords, numbers, percentages, and length units
/// supported by Bevy's [`Val`] type:
///
/// - `auto` → [`Val::Auto`]
/// - `0` → [`Val::ZERO`]
/// - `<number>` → [`Val::Px`] (pixel value)
/// - `<percentage>` → [`Val::Percent`] (percentage value)
/// - `<dimension>` → one of:
///   - `"px"` → [`Val::Px`]
///   - `"vw"` → [`Val::Vw`]
///   - `"vh"` → [`Val::Vh`]
///   - `"vmin"` → [`Val::VMin`]
///   - `"vmax"` → [`Val::VMax`]
///
/// # Example
/// ```
/// # use bevy_ui::Val;
/// # use cssparser::{Parser, ParserInput};
/// # use bevy_flair_css_parser::parse_val;
///
/// let mut input = ParserInput::new("50%");
/// let mut parser = Parser::new(&mut input);
/// let val = parse_val(&mut parser).unwrap();
/// assert_eq!(val, Val::Percent(50.0));
/// ```
pub fn parse_val(parser: &mut Parser) -> Result<Val, CssError> {
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
                    return Err(CssError::new_located(
                        &next,
                        error_codes::UNEXPECTED_VAL_TOKEN,
                        format!("Dimension '{unit}' is not recognized for Val. Valid dimensions are 'px' | 'vw' | 'vh' | 'vmin' | 'vmax'")
                    ));
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

pub(crate) fn parse_angle(parser: &mut Parser) -> Result<Rot2, CssError> {
    let next = parser.located_next()?;
    Ok(match &*next {
        Token::Number { value, .. } if *value == 0.0 => Rot2::IDENTITY,
        Token::Dimension { value, unit, .. } => {
            match_ignore_ascii_case! { unit.as_ref(),
                "deg" => Rot2::degrees(*value),
                "grad" => {
                    const RADS_PER_GRAD: f32 = consts::PI / 200.0;
                    Rot2::radians(*value * RADS_PER_GRAD)
                },
                "rad" => Rot2::radians(*value),
                "turn" => Rot2::turn_fraction(*value),
                _ => {
                    return Err(CssError::new_located(
                        &next,
                        error_codes::UNEXPECTED_ANGLE_TOKEN,
                        format!("Dimension '{unit}' is not recognized as an angle. Valid dimensions are 'deg' | 'grad' | 'rad' | 'turn'")
                    ));
                }
            }
        }
        _ => {
            return Err(CssError::new_located(
                &next,
                error_codes::UNEXPECTED_ANGLE_TOKEN,
                "This is not a valid angle token. 90deg, 100grad or 0.25turn are valid angles",
            ));
        }
    })
}

fn parse_overflow_clip_margin(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    if let Ok(margin) = parser.try_parse_with(|parser| parse_calc_value(parser, parse_f32)) {
        return Ok(ReflectValue::new(OverflowClipMargin {
            margin,
            ..OverflowClipMargin::DEFAULT
        }));
    }
    let visual_box = parse_enum_value(parser)?;
    let margin = parse_calc_value(parser, parse_f32)?;
    Ok(ReflectValue::new(OverflowClipMargin { visual_box, margin }))
}

fn parse_aspect_ratio(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    if let Ok(()) = parser.try_parse_with(|parser| {
        parser.expect_ident_matching("auto")?;
        Ok(())
    }) {
        let auto_value: Option<f32> = None;
        return Ok(ReflectValue::new(auto_value));
    }
    let dividend = parse_calc_value(parser, parse_f32)?;
    let divisor = parser
        .try_parse_with(|parser| {
            parser.expect_delim('/')?;
            parse_calc_value(parser, parse_f32)
        })
        .unwrap_or(1.0);
    let auto_value: Option<f32> = Some(dividend / divisor);
    Ok(ReflectValue::new(auto_value))
}

pub(crate) fn parse_four_values<T: Copy>(
    parser: &mut Parser,
    mut f: impl FnMut(&mut Parser) -> Result<T, CssError>,
) -> Result<[T; 4], CssError> {
    let mut values = SmallVec::<[T; 4]>::new();

    values.push(f(parser)?);
    while let Ok(val) = parser.try_parse_with(&mut f) {
        values.push(val);
        if values.len() >= 4 {
            break;
        }
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

    if let Ok(new_color) = parser.try_parse(parse_color) {
        color = new_color;
    }

    values.push(parse_calc_value(parser, parse_val)?);
    values.push(parse_calc_value(parser, parse_val)?);

    while let Ok(val) = parser.try_parse_with(|parser| parse_calc_value(parser, parse_val)) {
        values.push(val);
        if values.len() >= 4 {
            break;
        }
    }

    if let Ok(new_color) = parser.try_parse_with(parse_color) {
        color = new_color;
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
    let mut styles = Vec::with_capacity(1);
    styles.push(parse_single_box_shadow_style(parser)?);

    while let Ok(shadow_style) = parser.try_parse_with(|parser| {
        parser.expect_comma()?;
        parse_single_box_shadow_style(parser)
    }) {
        styles.push(shadow_style);
    }

    Ok(ReflectValue::new(BoxShadow(styles)))
}

// TODO: Support translateX, and translateY
fn parse_ui_transform_translation(parser: &mut Parser) -> Result<Val2, CssError> {
    let start = parser.state();
    if parser.expect_function_matching("translate").is_err() {
        parser.reset(&start);
        return Ok(Val2::ZERO);
    }
    parser.parse_nested_block_with(|parser| {
        let translate_x = parse_calc_value(parser, parse_val)?;
        let translate_y = parser
            .try_parse_with(|parser| {
                parser.expect_comma()?;
                parse_calc_value(parser, parse_val)
            })
            .unwrap_or(Val::ZERO);
        Ok(Val2::new(translate_x, translate_y))
    })
}

// TODO: Support scaleX, and scaleY
fn parse_ui_transform_scale(parser: &mut Parser) -> Result<Vec2, CssError> {
    let start = parser.state();

    if parser.expect_function_matching("scale").is_err() {
        parser.reset(&start);
        return Ok(Vec2::ONE);
    }
    parser.parse_nested_block_with(|parser| {
        let scale_x = parse_calc_value(parser, parse_f32)?;
        let scale_y = parser
            .try_parse_with(|parser| {
                parser.expect_comma()?;
                parse_calc_value(parser, parse_f32)
            })
            .unwrap_or(scale_x);

        Ok(Vec2::new(scale_x, scale_y))
    })
}

// TODO: Support rotateZ
fn parse_ui_transform_rotation(parser: &mut Parser) -> Result<Rot2, CssError> {
    let start = parser.state();
    if parser.expect_function_matching("rotate").is_err() {
        parser.reset(&start);
        return Ok(Rot2::IDENTITY);
    }
    parser.parse_nested_block_with(|parser| parse_calc_value(parser, parse_angle))
}

fn parse_ui_transform(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    if let Some(none) = try_parse_none::<UiTransform>(parser) {
        return Ok(ReflectValue::new(none));
    }

    let translation = parser.try_parse_with(parse_ui_transform_translation)?;
    let scale = parser.try_parse_with(parse_ui_transform_scale)?;
    let rotation = parser.try_parse_with(parse_ui_transform_rotation)?;

    Ok(ReflectValue::new(UiTransform {
        translation,
        scale,
        rotation,
    }))
}

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

impl FromType<OverflowClipMargin> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_property_value_with(parser, parse_overflow_clip_margin))
    }
}

impl FromType<Option<f32>> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_property_value_with(parser, parse_aspect_ratio))
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

impl FromType<UiTransform> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_property_value_with(parser, parse_ui_transform))
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::testing::test_parse_css;
    use bevy_color::palettes::css;
    use bevy_math::{Rot2, Vec2};
    use bevy_ui::{
        BoxShadow, OverflowClipBox, OverflowClipMargin, ShadowStyle, UiTransform, Val, Val2, ZIndex,
    };

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
    fn test_overflow_clip_margin() {
        assert_eq!(
            test_parse_css::<OverflowClipMargin>("2px"),
            OverflowClipMargin {
                margin: 2.0,
                ..Default::default()
            }
        );
        assert_eq!(
            test_parse_css::<OverflowClipMargin>("content-box 5px"),
            OverflowClipMargin {
                margin: 5.0,
                visual_box: OverflowClipBox::ContentBox
            }
        );
        assert_eq!(
            test_parse_css::<OverflowClipMargin>("border-box 10px"),
            OverflowClipMargin {
                margin: 10.0,
                visual_box: OverflowClipBox::BorderBox
            }
        );
    }

    #[test]
    fn test_aspect_ratio() {
        assert_eq!(test_parse_css::<Option<f32>>("auto"), None);
        assert_eq!(test_parse_css::<Option<f32>>("0.5"), Some(0.5));
        assert_eq!(test_parse_css::<Option<f32>>("16 / 9"), Some(16.0 / 9.0));
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

    #[test]
    fn test_ui_transform() {
        assert_eq!(test_parse_css::<UiTransform>("none"), UiTransform::IDENTITY);

        assert_eq!(
            test_parse_css::<UiTransform>("translate(2px)"),
            UiTransform::from_translation(Val2::px(2.0, 0.0))
        );

        assert_eq!(
            test_parse_css::<UiTransform>("translate(calc(2px * 3))"),
            UiTransform::from_translation(Val2::px(6.0, 0.0))
        );

        assert_eq!(
            test_parse_css::<UiTransform>("translate(10%, 15px)"),
            UiTransform::from_translation(Val2::new(Val::Percent(10.0), Val::Px(15.0)))
        );

        assert_eq!(
            test_parse_css::<UiTransform>("rotate(120deg)"),
            UiTransform::from_rotation(Rot2::degrees(120.0))
        );

        assert_eq!(
            test_parse_css::<UiTransform>("rotate(calc(130deg - 30deg))"),
            UiTransform::from_rotation(Rot2::degrees(100.0))
        );

        assert_eq!(
            test_parse_css::<UiTransform>("rotate(-20deg)"),
            UiTransform::from_rotation(Rot2::degrees(-20.0))
        );

        assert_eq!(
            test_parse_css::<UiTransform>("scale(1.5, 2.0)"),
            UiTransform::from_scale(Vec2::new(1.5, 2.0))
        );

        assert_eq!(
            test_parse_css::<UiTransform>("translate(10px, 0) scale(2.5) rotate(120deg)"),
            UiTransform {
                translation: Val2::new(Val::Px(10.0), Val::ZERO),
                scale: Vec2::splat(2.5),
                rotation: Rot2::degrees(120.0),
            }
        );
    }
}
