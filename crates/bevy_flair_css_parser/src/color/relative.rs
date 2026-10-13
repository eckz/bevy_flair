use super::{
    convert_lab_to_css_components, convert_lch_to_css_components, convert_to_bevy_lab,
    convert_to_bevy_lch, parse_angle_or_number, parse_number_or_percentage,
};
use crate::calc::CalcConstants;
use crate::error_codes::color as error_codes;
use crate::{CssError, LocatedStr, ParserExt};
use bevy_color::*;

use cssparser::color::{OPAQUE, PredefinedColorSpace, clamp_floor_256_f32, clamp_unit_f32};
use cssparser::{Parser, match_ignore_ascii_case};
use cssparser_color::NumberOrPercentage;

pub(crate) fn looks_like_relative_color(parser: &mut Parser) -> bool {
    let initial_state = parser.state();

    let looks_like_relative_color = parser
        .try_parse_with(|parser| {
            parser.expect_function()?;
            parser.parse_nested_block_with(|parser| {
                parser.expect_ident_matching("from")?;
                // If we don't consume the whole parser, an error will be returned
                while !parser.is_exhausted() {
                    let _ = parser.next()?;
                }
                Ok(())
            })?;
            Ok(())
        })
        .is_ok();

    if looks_like_relative_color {
        parser.reset(&initial_state);
        true
    } else {
        false
    }
}

#[inline]
fn parse_alpha_component(parser: &mut Parser, constants: CalcConstants) -> Result<f32, CssError> {
    Ok(super::parse_number_or_percentage(parser, constants)?
        .unit_value()
        .clamp(0.0, OPAQUE))
}

fn parse_alpha(parser: &mut Parser, constants: CalcConstants) -> Result<f32, CssError> {
    if !parser.is_exhausted() {
        parser.expect_delim('/')?;
        parse_alpha_component(parser, constants)
    } else {
        Ok(OPAQUE)
    }
}

#[inline]
fn unwrap_percentage_as_unit(c: NumberOrPercentage) -> f32 {
    match c {
        NumberOrPercentage::Number { value } => value.round().clamp(0., 100.) / 100.0,
        NumberOrPercentage::Percentage { unit_value } => unit_value.clamp(0.0, 1.0),
    }
}

#[inline]
fn normalize_hue(hue: f32) -> f32 {
    hue.rem_euclid(360.0)
}

pub fn parse_components<F1, F2, F3, R1, R2, R3>(
    parser: &mut Parser,
    constants: CalcConstants,
    f1: F1,
    f2: F2,
    f3: F3,
) -> Result<(R1, R2, R3, f32), CssError>
where
    F1: FnOnce(&mut Parser, CalcConstants) -> Result<R1, CssError>,
    F2: FnOnce(&mut Parser, CalcConstants) -> Result<R2, CssError>,
    F3: FnOnce(&mut Parser, CalcConstants) -> Result<R3, CssError>,
{
    let r1 = f1(parser, constants)?;
    let r2 = f2(parser, constants)?;
    let r3 = f3(parser, constants)?;

    let alpha = parse_alpha(parser, constants)?;

    Ok((r1, r2, r3, alpha))
}

fn parse_relative_rgb(base: Color, parser: &mut Parser) -> Result<Color, CssError> {
    #[inline]
    fn get_component_value(c: NumberOrPercentage) -> u8 {
        match c {
            NumberOrPercentage::Number { value } => clamp_floor_256_f32(value),
            NumberOrPercentage::Percentage { unit_value } => clamp_unit_f32(unit_value),
        }
    }

    let base: Srgba = base.into();
    let constants = [
        ("r", base.red * 255.0),
        ("g", base.green * 255.0),
        ("b", base.blue * 255.0),
        ("alpha", base.alpha),
    ];

    let (red, green, blue, alpha) = parse_components(
        parser,
        &constants,
        parse_number_or_percentage,
        parse_number_or_percentage,
        parse_number_or_percentage,
    )?;

    let red = get_component_value(red);
    let green = get_component_value(green);
    let blue = get_component_value(blue);

    Ok(Color::srgb_u8(red, green, blue).with_alpha(alpha))
}

fn parse_relative_hsl(base: Color, parser: &mut Parser) -> Result<Color, CssError> {
    let base: Hsla = base.into();
    let constants = [
        ("h", base.hue),
        ("s", base.saturation * 100.0),
        ("l", base.lightness * 100.0),
        ("alpha", base.alpha),
    ];

    let (hue, saturation, lightness, alpha) = parse_components(
        parser,
        &constants,
        parse_angle_or_number,
        parse_number_or_percentage,
        parse_number_or_percentage,
    )?;

    let hue = normalize_hue(hue.degrees());
    let saturation = unwrap_percentage_as_unit(saturation);
    let lightness = unwrap_percentage_as_unit(lightness);

    Ok(Color::hsla(hue, saturation, lightness, alpha))
}

fn parse_relative_hwb(base: Color, parser: &mut Parser) -> Result<Color, CssError> {
    let base: Hwba = base.into();
    let constants = [
        ("h", base.hue),
        ("w", base.whiteness * 100.0),
        ("b", base.blackness * 100.0),
        ("alpha", base.alpha),
    ];

    let (hue, whiteness, blackness, alpha) = parse_components(
        parser,
        &constants,
        parse_angle_or_number,
        parse_number_or_percentage,
        parse_number_or_percentage,
    )?;

    let hue = normalize_hue(hue.degrees());
    let whiteness = unwrap_percentage_as_unit(whiteness);
    let blackness = unwrap_percentage_as_unit(blackness);

    Ok(Color::hwba(hue, whiteness, blackness, alpha))
}

fn parse_relative_lab_like<C: Copy + From<Color>>(
    base: Color,
    parser: &mut Parser,
    lightness_range: f32,
    a_b_range: f32,
    to_calc_components: impl FnOnce(C) -> [f32; 4],
    to_color: impl FnOnce(f32, f32, f32, f32) -> Color,
) -> Result<Color, CssError> {
    let color: C = base.into();
    let components = to_calc_components(color);
    let constants = [
        ("l", components[0]),
        ("a", components[1]),
        ("b", components[2]),
        ("alpha", components[3]),
    ];

    let (lightness, a, b, alpha) = parse_components(
        parser,
        &constants,
        parse_number_or_percentage,
        parse_number_or_percentage,
        parse_number_or_percentage,
    )?;

    let lightness = lightness.value(lightness_range);
    let a = a.value(a_b_range);
    let b = b.value(a_b_range);

    Ok(to_color(lightness, a, b, alpha))
}

fn parse_relative_lch_like<C: Copy + From<Color>>(
    base: Color,
    parser: &mut Parser,
    lightness_range: f32,
    chroma_range: f32,
    to_calc_components: impl FnOnce(C) -> [f32; 4],
    to_color: impl FnOnce(f32, f32, f32, f32) -> Color,
) -> Result<Color, CssError> {
    let color: C = base.into();
    let components = to_calc_components(color);
    let constants = [
        ("l", components[0]),
        ("c", components[1]),
        ("h", components[2]),
        ("alpha", components[3]),
    ];

    let (lightness, chroma, hue, alpha) = parse_components(
        parser,
        &constants,
        parse_number_or_percentage,
        parse_number_or_percentage,
        parse_angle_or_number,
    )?;

    let lightness = lightness.value(lightness_range);
    let chroma = chroma.value(chroma_range);
    let hue = normalize_hue(hue.degrees());

    Ok(to_color(lightness, chroma, hue, alpha))
}

#[inline]
fn parse_relative_color_with_color_space(
    base: Color,
    parser: &mut Parser,
) -> Result<Color, CssError> {
    let color_space = parser.located(PredefinedColorSpace::parse)?;

    let components = match &*color_space {
        PredefinedColorSpace::Srgb => {
            let srgb = Srgba::from(base);
            [
                ("r", srgb.red),
                ("g", srgb.green),
                ("b", srgb.blue),
                ("alpha", srgb.alpha),
            ]
        }
        PredefinedColorSpace::SrgbLinear => {
            let srgb = LinearRgba::from(base);
            [
                ("r", srgb.red),
                ("g", srgb.green),
                ("b", srgb.blue),
                ("alpha", srgb.alpha),
            ]
        }
        PredefinedColorSpace::XyzD65 => {
            let xyz = Xyza::from(base);
            [
                ("x", xyz.x),
                ("y", xyz.y),
                ("z", xyz.z),
                ("alpha", xyz.alpha),
            ]
        }
        unsupported => {
            return Err(CssError::new_located(
                &color_space,
                error_codes::UNSUPPORTED_COLOR_SPACE,
                format!(
                    "Color space '{}' is not supported",
                    cssparser::ToCss::to_css_string(unsupported)
                ),
            ));
        }
    };

    let (c1, c2, c3, alpha) = parse_components(
        parser,
        &components,
        parse_number_or_percentage,
        parse_number_or_percentage,
        parse_number_or_percentage,
    )?;

    let c1 = c1.unit_value();
    let c2 = c2.unit_value();
    let c3 = c3.unit_value();

    Ok(match &*color_space {
        PredefinedColorSpace::Srgb => Color::srgba(c1, c2, c3, alpha),
        PredefinedColorSpace::SrgbLinear => Color::linear_rgba(c1, c2, c3, alpha),
        PredefinedColorSpace::XyzD65 => Color::xyza(c1, c2, c3, alpha),
        _ => unreachable!(),
    })
}

fn parse_relative_color_function(
    name: LocatedStr,
    base_color: Color,
    parser: &mut Parser,
) -> Result<Color, CssError> {
    let color = match_ignore_ascii_case! { &name,
        "rgb" | "rgba" => parse_relative_rgb(base_color, parser),
        "hsl" | "hsla" => parse_relative_hsl(base_color, parser),
        "hwb" => parse_relative_hwb(base_color, parser),
        "lab" => parse_relative_lab_like::<Laba>(base_color, parser, 100.0, 125.0, convert_lab_to_css_components, convert_to_bevy_lab),
        "lch" => parse_relative_lch_like::<Lcha>(base_color, parser, 100.0, 150.0, convert_lch_to_css_components, convert_to_bevy_lch),
        "oklab" => parse_relative_lab_like::<Oklaba>(base_color, parser, 1.0, 0.4, ColorToComponents::to_f32_array, Color::oklaba),
        "oklch" => parse_relative_lch_like::<Oklcha>(base_color, parser, 1.0, 0.4, ColorToComponents::to_f32_array, Color::oklcha),
        "color" => parse_relative_color_with_color_space(base_color, parser),

        _ => return Err(CssError::new_located(&name, error_codes::UNSUPPORTED_COLOR_FUNCTION, format!("Color function '{name}' is not supported"))),
    }?;

    parser.expect_exhausted()?;

    Ok(color)
}

pub(crate) fn parse_relative_color(parser: &mut Parser) -> Result<Color, CssError> {
    let name = parser.expect_located_function()?;
    parser.parse_nested_block_with(|parser| {
        parser.expect_ident_matching("from")?;
        let base_color = super::parse_color(parser)?;
        parse_relative_color_function(name, base_color, parser)
    })
}
