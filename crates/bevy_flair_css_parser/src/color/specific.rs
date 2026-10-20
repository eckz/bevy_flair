use crate::{CssError, CssParser};
use crate::color::{convert_to_bevy_lab, convert_to_bevy_lch};
use crate::error_codes::color as error_codes;
use bevy_color::{Alpha, Color};
use cssparser::color::PredefinedColorSpace;
use cssparser::{ParseError, Parser};
use cssparser_color::{AngleOrNumber, FromParsedColor, NumberOrPercentage};

enum ParsedColor {
    Color(Color),
    Error(CssError),
}

impl FromParsedColor for ParsedColor {
    fn from_current_color() -> Self {
        Self::Error(CssError::unlocated(
            error_codes::CURRENT_COLOR_NOT_SUPPORTED,
            "Unsupported 'current_color' value",
        ))
    }

    fn from_rgba(red: u8, green: u8, blue: u8, alpha: f32) -> Self {
        Self::Color(Color::srgb_u8(red, green, blue).with_alpha(alpha))
    }

    fn from_hsl(
        hue: Option<f32>,
        saturation: Option<f32>,
        lightness: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let hue = hue.unwrap_or(0.0);
        let saturation = saturation.unwrap_or(0.0);
        let lightness = lightness.unwrap_or(0.0);
        ParsedColor::Color(Color::hsla(
            hue,
            saturation,
            lightness,
            alpha.unwrap_or(1.0),
        ))
    }

    fn from_hwb(
        hue: Option<f32>,
        whiteness: Option<f32>,
        blackness: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let hue = hue.unwrap_or(0.0);
        let whiteness = whiteness.unwrap_or(0.0);
        let blackness = blackness.unwrap_or(0.0);
        ParsedColor::Color(Color::hwba(hue, whiteness, blackness, alpha.unwrap_or(1.0)))
    }

    fn from_lab(
        lightness: Option<f32>,
        a: Option<f32>,
        b: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let lightness = lightness.unwrap_or(0.0);
        let a = a.unwrap_or(0.0);
        let b = b.unwrap_or(0.0);
        let alpha = alpha.unwrap_or(1.0);
        ParsedColor::Color(convert_to_bevy_lab(lightness, a, b, alpha))
    }

    fn from_lch(
        lightness: Option<f32>,
        chroma: Option<f32>,
        hue: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let lightness = lightness.unwrap_or(0.0);
        let chroma = chroma.unwrap_or(0.0);
        let hue = hue.unwrap_or(0.0);
        let alpha = alpha.unwrap_or(1.0);
        ParsedColor::Color(convert_to_bevy_lch(lightness, chroma, hue, alpha))
    }

    fn from_oklab(
        lightness: Option<f32>,
        a: Option<f32>,
        b: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let lightness = lightness.unwrap_or(0.0);
        let a = a.unwrap_or(0.0);
        let b = b.unwrap_or(0.0);
        ParsedColor::Color(Color::oklaba(lightness, a, b, alpha.unwrap_or(1.0)))
    }

    fn from_oklch(
        lightness: Option<f32>,
        chroma: Option<f32>,
        hue: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let lightness = lightness.unwrap_or(0.0);
        let chroma = chroma.unwrap_or(0.0);
        let hue = hue.unwrap_or(0.0);
        ParsedColor::Color(Color::oklcha(lightness, chroma, hue, alpha.unwrap_or(1.0)))
    }

    fn from_color_function(
        color_space: PredefinedColorSpace,
        c1: Option<f32>,
        c2: Option<f32>,
        c3: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        match color_space {
            PredefinedColorSpace::Srgb => {
                let red = c1.unwrap_or(0.0);
                let green = c2.unwrap_or(0.0);
                let blue = c3.unwrap_or(0.0);
                ParsedColor::Color(Color::srgba(red, green, blue, alpha.unwrap_or(1.0)))
            }
            PredefinedColorSpace::SrgbLinear => {
                let red = c1.unwrap_or(0.0);
                let green = c2.unwrap_or(0.0);
                let blue = c3.unwrap_or(0.0);
                ParsedColor::Color(Color::linear_rgba(red, green, blue, alpha.unwrap_or(1.0)))
            }
            PredefinedColorSpace::XyzD65 => {
                let x = c1.unwrap_or(0.0);
                let y = c2.unwrap_or(0.0);
                let z = c3.unwrap_or(0.0);
                ParsedColor::Color(Color::xyza(x, y, z, alpha.unwrap_or(1.0)))
            }
            unsupported => ParsedColor::Error(CssError::unlocated(
                error_codes::UNSUPPORTED_COLOR_SPACE,
                format!(
                    "Color space '{}' is not supported",
                    cssparser::ToCss::to_css_string(&unsupported)
                ),
            )),
        }
    }
}

struct ColorParser;

impl<'i> cssparser_color::ColorParser<'i> for ColorParser {
    type Output = ParsedColor;
    type Error = CssError;

    fn parse_angle_or_number(
        &self,
        parser: &mut Parser,
    ) -> Result<AngleOrNumber, ParseError<CssError>> {
        let mut parser = CssParser::from_parser(parser);
        Ok(super::parse_angle_or_number(&mut parser, &[])?)
    }

    fn parse_percentage(
        &self,
        parser: &mut Parser,
    ) -> Result<f32, ParseError<CssError>> {
        let mut parser = CssParser::from_parser(parser);
        Ok(super::parse_percentage(&mut parser, &[])?)
    }

    /// Parse a `<number>` value.
    fn parse_number(
        &self,
        parser: &mut Parser,
    ) -> Result<f32, ParseError<CssError>> {
        let mut parser = CssParser::from_parser(parser);
        Ok(super::parse_number(&mut parser, &[])?)
    }

    /// Parse a `<number>` value or a `<percentage>` value.
    fn parse_number_or_percentage(
        &self,
        parser: &mut Parser,
    ) -> Result<NumberOrPercentage, ParseError<CssError>> {
        let mut parser = CssParser::from_parser(parser);
        Ok(super::parse_number_or_percentage(&mut parser, &[])?)
    }
}

/// Uses `cssparser_color` to parse a color. This works parsing colors for the following use cases
///   - By keywords (such as blue or pink)
///   - By hexadecimal notations (such as #ff0000).
///   - By <color-function>, no relative colors are supported
pub fn parse_specific_color(parser: &mut CssParser) -> Result<Color, CssError> {
    let result = parser.spanned(|parser| {
        parser.inner_spanned_err(|parser| {
            cssparser_color::parse_color_with(&ColorParser, parser)
        })
    })?;
    let (result, span) = result.into_inner_and_span();
    
    match result {
        ParsedColor::Color(color) => Ok(color),
        ParsedColor::Error(mut error) => {
            error.improve_location_with_span(span);
            Err(error)
        },
    }
}
