use crate::calc::{CalcValue, FromCalcValue, parse_calc_property};
use crate::error::CssError;
use crate::error_codes::ui as error_codes;
use crate::reflect::enums::parse_enum_value;
use crate::reflect::parse_color;
use crate::utils::{parse_property_value_with, try_parse_none};
use crate::{ParserExt, ReflectParseCss, parse_calc};
use bevy_flair_core::{PropertyValue, ReflectValue};
use bevy_math::{Rect, Rot2, Vec2};
use bevy_reflect::CreateTypeData;
use bevy_ui::{BoxShadow, CornerRadius, OverflowClipMargin, ShadowStyle, Val, Val2, ZIndex};
use cssparser::Parser;
use smallvec::SmallVec;
use std::f32::consts;

/// Parses a number, but also accepts <number>px and interpret it as a number.
pub fn parse_px(parser: &mut Parser) -> Result<f32, CssError> {
    struct NumberOrPx(f32);

    impl FromCalcValue for NumberOrPx {
        type Error = String;

        fn from_calc_value(calc_value: &CalcValue) -> Result<Self, Self::Error> {
            match calc_value {
                CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("px") => {
                    Ok(NumberOrPx(*value))
                }
                CalcValue::Number(value) => Ok(NumberOrPx(*value)),
                invalid => Err(format!(
                    "Expected a <number> or '<number>px', found '{invalid}'"
                )),
            }
        }
    }

    Ok(parse_calc::<NumberOrPx>(parser)?.0)
}

pub(crate) fn parse_vec2(parser: &mut Parser) -> Result<Vec2, CssError> {
    if let Some(none) = try_parse_none(parser) {
        return Ok(none);
    }

    let x = parse_px(parser)?;
    let y = parser.try_parse_with(parse_px).unwrap_or(x);
    Ok(Vec2::new(x, y))
}

pub(crate) fn parse_rect(parser: &mut Parser) -> Result<Rect, CssError> {
    if let Some(none) = try_parse_none(parser) {
        return Ok(none);
    }

    let x0 = parse_px(parser)?;
    let y0 = parse_px(parser)?;
    let x1 = parse_px(parser)?;
    let y1 = parse_px(parser)?;
    Ok(Rect::new(x0, y0, x1, y1))
}

impl FromCalcValue for Val {
    type Error = String;
    fn from_calc_value(calc_value: &CalcValue) -> Result<Self, Self::Error> {
        match calc_value {
            CalcValue::Number(value) => Ok(Val::Px(*value)),
            CalcValue::Percentage(value) => Ok(Val::Percent(*value * 100.0)),
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("px") => {
                Ok(Val::Px(*value))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("vw") => {
                Ok(Val::Vw(*value))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("vh") => {
                Ok(Val::Vh(*value))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("vmin") => {
                Ok(Val::VMin(*value))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("vmax") => {
                Ok(Val::VMax(*value))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("em") => {
                Ok(Val::Em(*value))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("rem") => {
                Ok(Val::Rem(*value))
            }
            CalcValue::Dimension { dim, .. } => Err(format!(
                "Dimension '{dim}' is not recognized for Val. Valid dimensions are 'px' | 'vw' | 'vh' | 'vmin' | 'vmax' | 'em' | 'rem'"
            )),
            CalcValue::MathFunction(math_fn) => Err(format!(
                "Expression '{math_fn}' cannot be simplified because contains different dimensions"
            )),
        }
    }
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
///
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
    if let Ok(auto) = parser.try_parse_with(|parser| {
        parser.expect_ident_matching("auto")?;
        Ok(Val::Auto)
    }) {
        return Ok(auto);
    }

    parse_calc(parser)
}

pub(crate) fn parse_val2(parser: &mut Parser) -> Result<Val2, CssError> {
    if let Some(none) = try_parse_none(parser) {
        return Ok(none);
    }

    let x = parse_val(parser)?;
    let y = parser.try_parse_with(parse_val).unwrap_or(x);
    Ok(Val2::new(x, y))
}

pub(crate) fn parse_corner_radius(parser: &mut Parser) -> Result<CornerRadius, CssError> {
    if let Some(none) = try_parse_none(parser) {
        return Ok(none);
    }

    let x = parse_val(parser)?;
    let y = parser.try_parse_with(parse_val).unwrap_or(Val::Auto);
    Ok(CornerRadius::new(x, y))
}

impl FromCalcValue for Rot2 {
    type Error = String;

    fn from_calc_value(calc_value: &CalcValue) -> Result<Self, Self::Error> {
        match calc_value {
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("deg") => {
                Ok(Rot2::degrees(*value))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("grad") => {
                const RADS_PER_GRAD: f32 = consts::PI / 200.0;
                Ok(Rot2::radians(*value * RADS_PER_GRAD))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("rad") => {
                Ok(Rot2::radians(*value))
            }
            CalcValue::Dimension { value, dim } if dim.eq_ignore_ascii_case("turn") => {
                Ok(Rot2::turn_fraction(*value))
            }
            invalid => Err(format!("Expression '{invalid}' not valid as a rotation")),
        }
    }
}

pub fn parse_angle(parser: &mut Parser) -> Result<Rot2, CssError> {
    if let Some(none) = try_parse_none(parser) {
        return Ok(none);
    }
    parse_calc::<Rot2>(parser)
}

fn parse_overflow_clip_margin(parser: &mut Parser) -> Result<ReflectValue, CssError> {
    if let Ok(margin) = parser.try_parse_with(parse_px) {
        return Ok(ReflectValue::new(OverflowClipMargin {
            margin,
            ..OverflowClipMargin::DEFAULT
        }));
    }
    let visual_box = parse_enum_value(parser)?;
    let margin = parse_px(parser)?;
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
    let dividend = parse_px(parser)?;
    let divisor = parser
        .try_parse_with(|parser| {
            parser.expect_delim('/')?;
            parse_px(parser)
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

    values.push(parse_val(parser)?);
    values.push(parse_val(parser)?);

    while let Ok(val) = parser.try_parse_with(parse_val) {
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
    if let Some(none) = try_parse_none::<BoxShadow>(parser) {
        return Ok(ReflectValue::new(none));
    }
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

impl CreateTypeData<f32> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(parse_calc_property::<f32>)
    }
}

impl CreateTypeData<Vec2> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| {
            parse_property_value_with(parser, parse_vec2).map(PropertyValue::into_reflect_value)
        })
    }
}

impl CreateTypeData<Val> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| {
            parse_property_value_with(parser, parse_val).map(PropertyValue::into_reflect_value)
        })
    }
}

impl CreateTypeData<Val2> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| {
            parse_property_value_with(parser, parse_val2).map(PropertyValue::into_reflect_value)
        })
    }
}

impl CreateTypeData<Rot2> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| {
            parse_property_value_with(parser, parse_angle).map(PropertyValue::into_reflect_value)
        })
    }
}

impl CreateTypeData<Rect> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| {
            parse_property_value_with(parser, parse_rect).map(PropertyValue::into_reflect_value)
        })
    }
}

impl CreateTypeData<CornerRadius> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| {
            parse_property_value_with(parser, parse_corner_radius)
                .map(PropertyValue::into_reflect_value)
        })
    }
}

impl CreateTypeData<OverflowClipMargin> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| parse_property_value_with(parser, parse_overflow_clip_margin))
    }
}

impl CreateTypeData<Option<f32>> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| parse_property_value_with(parser, parse_aspect_ratio))
    }
}

impl CreateTypeData<Option<Rect>> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| {
            parse_property_value_with(parser, |parser| parse_rect(parser).map(Some))
                .map(PropertyValue::into_reflect_value)
        })
    }
}

impl CreateTypeData<ZIndex> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| parse_property_value_with(parser, parse_z_index))
    }
}

impl CreateTypeData<BoxShadow> for ReflectParseCss {
    fn create_type_data(_: ()) -> Self {
        Self(|parser| parse_property_value_with(parser, parse_box_shadow))
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::reflect_test_utils::{test_err_parse_reflect, test_parse_reflect};
    use bevy_color::palettes::css;
    use bevy_math::{Rot2, Vec2};
    use bevy_ui::{
        BoxShadow, CornerRadius, OverflowClipMargin, ShadowStyle, Val, Val2, VisualBox, ZIndex,
    };

    #[test]
    fn test_f32() {
        assert_eq!(test_parse_reflect::<f32>("3"), 3.0);
        assert_eq!(test_parse_reflect::<f32>("1.5"), 1.5);
        assert_eq!(test_parse_reflect::<f32>("calc(6 / 2)"), 3.0);
    }

    #[test]
    fn test_vec2() {
        assert_eq!(test_parse_reflect::<Vec2>("none"), Vec2::ZERO);
        assert_eq!(test_parse_reflect::<Vec2>("3.0"), Vec2::splat(3.0));
        assert_eq!(test_parse_reflect::<Vec2>("3.0 8.0"), Vec2::new(3.0, 8.0));
    }

    #[test]
    fn test_rot2() {
        assert_eq!(test_parse_reflect::<Rot2>("none"), Rot2::IDENTITY);
        assert_eq!(test_parse_reflect::<Rot2>("90deg"), Rot2::degrees(90.0));
        assert_eq!(
            test_parse_reflect::<Rot2>("calc(45deg * 2)"),
            Rot2::degrees(90.0)
        );
        assert_eq!(test_parse_reflect::<Rot2>("3rad"), Rot2::radians(3.0));
        assert_eq!(test_parse_reflect::<Rot2>("0.25turn"), Rot2::degrees(90.0));
    }

    #[test]
    fn test_val() {
        assert_eq!(test_parse_reflect::<Val>("auto"), Val::Auto);
        assert_eq!(test_parse_reflect::<Val>("33.5"), Val::Px(33.5));
        assert_eq!(test_parse_reflect::<Val>("15px"), Val::Px(15.0));
        assert_eq!(test_parse_reflect::<Val>("55%"), Val::Percent(55.0));
        assert_eq!(test_parse_reflect::<Val>("157vw"), Val::Vw(157.0));
        assert_eq!(test_parse_reflect::<Val>("343.5vh"), Val::Vh(343.5));
        assert_eq!(test_parse_reflect::<Val>("987vmin"), Val::VMin(987.0));
        assert_eq!(test_parse_reflect::<Val>("9999vmax"), Val::VMax(9999.0));
        assert_eq!(test_parse_reflect::<Val>("16em"), Val::Em(16.0));
        assert_eq!(test_parse_reflect::<Val>("32rem"), Val::Rem(32.0));

        assert_eq!(test_err_parse_reflect::<Val>("2ch"), "[94] Warning: Cannot convert calc expression to final type
   ,-[ test.css:1:1 ]
   |
 1 | 2ch
   | |^^\x20\x20
   | `---- Dimension 'ch' is not recognized for Val. Valid dimensions are 'px' | 'vw' | 'vh' | 'vmin' | 'vmax' | 'em' | 'rem'
---'
"
        );

        assert_eq!(test_err_parse_reflect::<Val>("calc(3px + 2rem)"), "[94] Warning: Cannot convert calc expression to final type
   ,-[ test.css:1:1 ]
   |
 1 | calc(3px + 2rem)
   | |^^^^^^^^^^^^^^^\x20\x20
   | `----------------- Expression 'calc(3px + 2rem)' cannot be simplified because contains different dimensions
---'
"
        );
    }

    #[test]
    fn test_val2() {
        assert_eq!(test_parse_reflect::<Val2>("none"), Val2::ZERO);
        assert_eq!(
            test_parse_reflect::<Val2>("auto"),
            Val2::new(Val::Auto, Val::Auto)
        );
        assert_eq!(test_parse_reflect::<Val2>("3px 10px"), Val2::px(3.0, 10.0));
        assert_eq!(
            test_parse_reflect::<Val2>("calc(10px * 2) 10%"),
            Val2::new(Val::Px(20.0), Val::Percent(10.0))
        );
    }

    #[test]
    fn test_corner_radius() {
        assert_eq!(
            test_parse_reflect::<CornerRadius>("20%"),
            CornerRadius::circular(Val::Percent(20.0))
        );
        assert_eq!(
            test_parse_reflect::<CornerRadius>("20% 50%"),
            CornerRadius::new(Val::Percent(20.0), Val::Percent(50.0))
        );
    }

    #[test]
    fn test_val_with_calc() {
        assert_eq!(test_parse_reflect::<Val>("calc(15px * 2)"), Val::Px(30.0));
    }

    #[test]
    fn test_overflow_clip_margin() {
        assert_eq!(
            test_parse_reflect::<OverflowClipMargin>("2px"),
            OverflowClipMargin {
                margin: 2.0,
                ..Default::default()
            }
        );
        assert_eq!(
            test_parse_reflect::<OverflowClipMargin>("content-box 5px"),
            OverflowClipMargin {
                margin: 5.0,
                visual_box: VisualBox::ContentBox
            }
        );
        assert_eq!(
            test_parse_reflect::<OverflowClipMargin>("border-box 10px"),
            OverflowClipMargin {
                margin: 10.0,
                visual_box: VisualBox::BorderBox
            }
        );
    }

    #[test]
    fn test_aspect_ratio() {
        assert_eq!(test_parse_reflect::<Option<f32>>("auto"), None);
        assert_eq!(test_parse_reflect::<Option<f32>>("0.5"), Some(0.5));
        assert_eq!(
            test_parse_reflect::<Option<f32>>("16 / 9"),
            Some(16.0 / 9.0)
        );
    }

    #[test]
    fn test_z_index() {
        assert_eq!(test_parse_reflect::<ZIndex>("2"), ZIndex(2));
        assert_eq!(test_parse_reflect::<ZIndex>("0"), ZIndex(0));
        assert_eq!(test_parse_reflect::<ZIndex>("-999999"), ZIndex(-999999));

        assert_eq!(
            test_err_parse_reflect::<ZIndex>("1.2"),
            "[01] Warning: Unexpected token
   ,-[ test.css:1:1 ]
   |
 1 | 1.2
   | |^^\x20\x20
   | `---- unexpected token: Number { has_sign: false, value: 1.2, int_value: None }
---'
"
        );
    }

    #[test]
    fn test_box_shadow() {
        assert_eq!(
            test_parse_reflect::<BoxShadow>("none"),
            BoxShadow::default()
        );

        assert_eq!(
            test_parse_reflect::<BoxShadow>("10px 5px"),
            BoxShadow::from(ShadowStyle {
                x_offset: Val::Px(10.0),
                y_offset: Val::Px(5.0),
                ..Default::default()
            })
        );

        assert_eq!(
            test_parse_reflect::<BoxShadow>("60px -16px teal"),
            BoxShadow::from(ShadowStyle {
                x_offset: Val::Px(60.0),
                y_offset: Val::Px(-16.0),
                color: css::TEAL.into(),
                ..Default::default()
            })
        );

        assert_eq!(
            test_parse_reflect::<BoxShadow>("white 12px -22px 2px 1px"),
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
