use crate::error::CssError;
use crate::error_codes::color as error_codes;
use crate::reflect::ReflectParseCss;
use bevy::color::{Alpha, Color};
use bevy::math::FloatExt;
use bevy::reflect::FromType;
use bevy_flair_core::ReflectValue;
use cssparser::color::PredefinedColorSpace;
use cssparser::{Parser, ToCss};
use cssparser_color::FromParsedColor;

enum ParsedColor {
    Color(Color),
    Error(CssError),
}

impl ParsedColor {
    fn into_result(self) -> Result<Color, CssError> {
        match self {
            ParsedColor::Color(color) => Ok(color),
            ParsedColor::Error(error) => Err(error),
        }
    }
}

macro_rules! unwrap_component {
    ($val:expr) => {
        $val.unwrap_or(0.0)
    };
}

macro_rules! alpha {
    ($val:expr) => {
        $val.unwrap_or(1.0)
    };
}

macro_rules! remap {
    ($value:expr, input: $input:literal, output: $output:literal) => {{
        let t = f32::inverse_lerp(0.0, $input, $value);
        f32::lerp(0.0, $output, t)
    }};
}

impl FromParsedColor for ParsedColor {
    fn from_current_color() -> Self {
        Self::Error(CssError::new_unlocated(
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
        let hue = unwrap_component!(hue);
        let saturation = unwrap_component!(saturation);
        let lightness = unwrap_component!(lightness);
        ParsedColor::Color(Color::hsla(hue, saturation, lightness, alpha!(alpha)))
    }

    fn from_hwb(
        hue: Option<f32>,
        whiteness: Option<f32>,
        blackness: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let hue = unwrap_component!(hue);
        let whiteness = unwrap_component!(whiteness);
        let blackness = unwrap_component!(blackness);
        ParsedColor::Color(Color::hwba(hue, whiteness, blackness, alpha!(alpha)))
    }

    fn from_lab(
        lightness: Option<f32>,
        a: Option<f32>,
        b: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let lightness = remap!(unwrap_component!(lightness), input: 100.0, output: 1.5);
        let a = remap!(unwrap_component!(a), input: 125.0, output: 1.5);
        let b = remap!(unwrap_component!(b), input: 125.0, output: 1.5);
        ParsedColor::Color(Color::laba(lightness, a, b, alpha!(alpha)))
    }

    fn from_lch(
        lightness: Option<f32>,
        chroma: Option<f32>,
        hue: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let lightness = remap!(unwrap_component!(lightness), input: 100.0, output: 1.5);
        let chroma = remap!(unwrap_component!(chroma), input: 150.0, output: 1.5);
        let hue = unwrap_component!(hue);
        ParsedColor::Color(Color::lcha(lightness, chroma, hue, alpha!(alpha)))
    }

    fn from_oklab(
        lightness: Option<f32>,
        a: Option<f32>,
        b: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let lightness = unwrap_component!(lightness);
        let a = remap!(unwrap_component!(a), input: 0.4, output: 1.0);
        let b = remap!(unwrap_component!(b), input: 0.4, output: 1.0);
        ParsedColor::Color(Color::oklaba(lightness, a, b, alpha!(alpha)))
    }

    fn from_oklch(
        lightness: Option<f32>,
        chroma: Option<f32>,
        hue: Option<f32>,
        alpha: Option<f32>,
    ) -> Self {
        let lightness = unwrap_component!(lightness);
        let chroma = remap!(unwrap_component!(chroma), input: 0.4, output: 1.0);
        let hue = unwrap_component!(hue);
        ParsedColor::Color(Color::oklcha(lightness, chroma, hue, alpha!(alpha)))
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
                let red = unwrap_component!(c1);
                let green = unwrap_component!(c2);
                let blue = unwrap_component!(c3);
                ParsedColor::Color(Color::srgba(red, green, blue, alpha!(alpha)))
            }
            PredefinedColorSpace::SrgbLinear => {
                let red = unwrap_component!(c1);
                let green = unwrap_component!(c2);
                let blue = unwrap_component!(c3);
                ParsedColor::Color(Color::linear_rgba(red, green, blue, alpha!(alpha)))
            }
            PredefinedColorSpace::XyzD65 => {
                let x = unwrap_component!(c1);
                let y = unwrap_component!(c2);
                let z = unwrap_component!(c3);
                ParsedColor::Color(Color::xyza(x, y, z, alpha!(alpha)))
            }
            unsupported => ParsedColor::Error(CssError::new_unlocated(
                error_codes::UNSUPPORTED_COLOR_SPACE,
                format!(
                    "Color space '{}' is not supported",
                    unsupported.to_css_string()
                ),
            )),
        }
    }
}

pub(super) fn parse_color(parser: &mut Parser) -> Result<Color, CssError> {
    struct ColorParser;

    impl cssparser_color::ColorParser<'_> for ColorParser {
        type Output = ParsedColor;
        type Error = ();
    }

    let result = cssparser_color::parse_color_with(&ColorParser, parser)?;
    result.into_result()
}

impl FromType<Color> for ReflectParseCss {
    fn from_type() -> Self {
        Self(|parser| parse_color(parser).map(ReflectValue::Color))
    }
}

#[cfg(test)]
mod tests {
    use crate::reflect::testing::test_parse_css;
    use bevy::color::Color;

    macro_rules! color_approx_eq {
        ($left:expr, $right:expr) => {
            let left_srgba: bevy::color::Srgba = $left.into();
            let right_srgba: bevy::color::Srgba = $right.into();

            approx::assert_abs_diff_eq!(
                bevy::color::ColorToComponents::to_f32_array(left_srgba).as_slice(),
                bevy::color::ColorToComponents::to_f32_array(right_srgba).as_slice(),
                epsilon = 0.001,
            )
        };
    }

    #[test]
    fn test_srgb() {
        assert_eq!(
            test_parse_css::<Color>("rgb(214, 122, 127)"),
            Color::srgb_u8(214, 122, 127)
        );

        assert_eq!(
            test_parse_css::<Color>("rgba(10 20 30)"),
            Color::srgb_u8(10, 20, 30)
        );

        assert_eq!(
            test_parse_css::<Color>("rgb(255 255 255 / 50%)"),
            Color::srgba(1.0, 1.0, 1.0, 0.5)
        );

        assert_eq!(
            test_parse_css::<Color>("rgb(0, 255, 255, 50%)"),
            Color::srgba(0.0, 1.0, 1.0, 0.5)
        );
    }

    #[test]
    fn test_transparent() {
        assert_eq!(
            test_parse_css::<Color>("transparent"),
            Color::srgba(0.0, 0.0, 0.0, 0.0)
        );
    }

    #[test]
    fn test_color_name() {
        use bevy::color::palettes::css;

        for (name, color) in [
            ("red", css::RED),
            ("green", css::GREEN),
            ("blue", css::BLUE),
            ("yellow", css::YELLOW),
            ("indigo", css::INDIGO),
            ("greenyellow", css::GREEN_YELLOW),
            ("grey", css::GREY),
            ("honeydew", css::HONEYDEW),
            ("hotpink", css::HOT_PINK),
            ("indianred", css::INDIAN_RED),
            ("indigo", css::INDIGO),
            ("ivory", css::IVORY),
            ("khaki", css::KHAKI),
            ("lavender", css::LAVENDER),
            ("lavenderblush", css::LAVENDER_BLUSH),
            ("lawngreen", css::LAWN_GREEN),
            ("lemonchiffon", css::LEMON_CHIFFON),
            ("lightblue", css::LIGHT_BLUE),
            ("lightcyan", css::LIGHT_CYAN),
            ("lightseagreen", css::LIGHT_SEA_GREEN),
            ("lightskyblue", css::LIGHT_SKY_BLUE),
            ("lightslategray", css::LIGHT_SLATE_GRAY),
            ("lightslategrey", css::LIGHT_SLATE_GREY),
            ("lightsteelblue", css::LIGHT_STEEL_BLUE),
            ("lightyellow", css::LIGHT_YELLOW),
            ("limegreen", css::LIMEGREEN),
            ("linen", css::LINEN),
            ("magenta", css::MAGENTA),
            ("sienna", css::SIENNA),
            ("skyblue", css::SKY_BLUE),
            ("slateblue", css::SLATE_BLUE),
            ("slategray", css::SLATE_GRAY),
            ("slategrey", css::SLATE_GREY),
            ("snow", css::SNOW),
            ("springgreen", css::SPRING_GREEN),
            ("steelblue", css::STEEL_BLUE),
            ("tan", css::TAN),
            ("thistle", css::THISTLE),
        ] {
            color_approx_eq!(test_parse_css::<Color>(name), color);
        }
    }

    #[test]
    fn test_color_hash() {
        assert_eq!(
            test_parse_css::<Color>("#009912"),
            Color::srgb_u8(0, 0x99, 0x12)
        );
    }

    #[test]
    fn test_hsl() {
        assert_eq!(
            test_parse_css::<Color>("hsl(30deg 82% 43%)"),
            Color::hsla(30.0, 0.82, 0.43, 1.0)
        );
    }

    #[test]
    fn test_hwb() {
        assert_eq!(
            test_parse_css::<Color>("hwb(152deg 0% 58% / 70%)"),
            Color::hwba(152.0, 0., 0.58, 0.7)
        );

        assert_eq!(
            test_parse_css::<Color>("hwb(152deg none none / 70%)"),
            Color::hwba(152.0, 0., 0., 0.7)
        );
    }

    #[test]
    fn test_lab() {
        assert_eq!(
            test_parse_css::<Color>("lab(100% -100% 125)"),
            Color::lab(1.5, -1.5, 1.5)
        );
    }

    #[test]
    fn test_oklab() {
        assert_eq!(
            test_parse_css::<Color>("oklab(50% -100% 0.4)"),
            Color::oklab(0.5, -1.0, 1.0)
        );
    }

    #[test]
    fn test_rgb_linear() {
        assert_eq!(
            test_parse_css::<Color>("color(srgb-linear 0.5 0.5 0)"),
            Color::linear_rgba(0.5, 0.5, 0., 1.0)
        );
    }

    #[test]
    fn test_xyz() {
        assert_eq!(
            test_parse_css::<Color>("color(xyz 1.0 0.5 0 / 50%)"),
            Color::xyza(1.0, 0.5, 0., 0.5)
        );
    }
}
